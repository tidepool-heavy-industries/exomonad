-- | Hook event handler.
--
-- Handles hook events from Claude Code via mantle-agent.
-- Most hooks are passthrough, but some execute effect logic:
--
-- * SessionStart: Injects bead context when on a bd-* branch
-- * Stop: Enforces PR filing with templated guidance + auto-focus on subagent error
module Tidepool.Control.Handler.Hook
  ( handleHook
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import System.Environment (lookupEnv)
import Control.Exception (SomeException, try)
import Control.Monad.Freer (Eff, Member, runM)
import Data.Time.Clock (getCurrentTime)

import Tidepool.Control.Protocol hiding (role)
import Tidepool.Control.Types (ServerConfig(..))
import Tidepool.Control.Hook.Policy (HookDecision(..), evaluatePolicy)
import Tidepool.Control.Hook.CircuitBreaker (CircuitBreakerMap, SessionId, withCircuitBreaker, incrementStage)
import Tidepool.Control.ExoTools (parseBeadId)
import Tidepool.Control.Hook.SessionStart (sessionStartLogic)
import Tidepool.Control.Hook.Stop (stopHookLogic, StopHookResult(..))
import Tidepool.Control.Effects.SshExec (runSshExec)
import Tidepool.Control.Effects.Git (runGitViaSsh)
import Tidepool.Control.Effects.Justfile (runJustfileViaSsh)
import Tidepool.BD.Interpreter (runBDIO, defaultBDConfig)
import Tidepool.BD.GitInterpreter (runGitIO)
import Tidepool.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import Tidepool.Justfile.Interpreter (runJustfileIO)
import Tidepool.Effect.Types (runLog, LogLevel(..))
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import Tidepool.Effects.Zellij (Zellij, checkZellijEnv, goToTab, TabId(..))
import Tidepool.Zellij.Interpreter (runZellijIO)

-- | Handle a hook event.
--
-- Executes hook-specific logic for SessionStart and Stop.
-- Other hooks pass through with default responses.
handleHook :: ServerConfig -> HookInput -> Runtime -> Role -> CircuitBreakerMap -> IO ControlResponse
handleHook config input runtime agentRole cbMap = do
  TIO.putStrLn $ "  session=" <> input.sessionId
  TIO.putStrLn $ "  cwd=" <> input.cwd
  TIO.putStrLn $ "  role=" <> T.pack (show agentRole)
  hFlush stdout

  case input.hookEventName of
    "SessionStart" -> handleSessionStart agentRole input
    "Stop" -> handleStop input runtime cbMap
    "PreToolUse" -> handlePreToolUse config input
    _ -> pure $ hookSuccess $ makeResponse input.hookEventName input

-- | Handle PreToolUse hook using policy evaluation.
handlePreToolUse :: ServerConfig -> HookInput -> IO ControlResponse
handlePreToolUse config input = do
  let toolName = fromMaybe "unknown" input.toolName
  TIO.putStrLn $ "  [HOOK] Evaluating PreToolUse policy for tool: " <> toolName
  hFlush stdout

  let decision = evaluatePolicy config.hookPolicy toolName
  case decision of
    PolicyAllow reason modifiedInput -> do
      TIO.putStrLn $ "  [HOOK] Policy: ALLOW " <> fromMaybe "" reason
      hFlush stdout
      pure $ hookSuccess $ allowPreToolUse reason modifiedInput
    PolicyDeny reason -> do
      TIO.putStrLn $ "  [HOOK] Policy: DENY " <> reason
      hFlush stdout
      pure $ hookSuccess $ denyPreToolUse reason
    PolicyAsk reason -> do
      TIO.putStrLn $ "  [HOOK] Policy: ASK " <> fromMaybe "" reason
      hFlush stdout
      -- In Claude Code, "ask" in PreToolUse triggers its own permission prompt.
      pure $ hookSuccess defaultOutput
        { hookSpecificOutput = Just $ PreToolUseOutput "ask" reason Nothing
        }

-- | Handle SessionStart hook: inject bead context.
handleSessionStart :: Role -> HookInput -> IO ControlResponse
handleSessionStart role input = do
  TIO.putStrLn "  [HOOK] Running SessionStart context injection..."
  hFlush stdout

  -- Check if we should use SSH for execution (if TIDEPOOL_CONTAINER is set)
  mContainer <- lookupEnv "TIDEPOOL_CONTAINER"
  sshProxyUrl <- fromMaybe "http://localhost:7433" <$> lookupEnv "SSH_PROXY_URL"

  result <- try $ runM
    $ runLog Debug
    $ runBDIO defaultBDConfig
    $ case mContainer of
         Just container -> runSshExec (T.pack sshProxyUrl) $ runGitViaSsh (T.pack container) "." $ sessionStartLogic role input.cwd
         Nothing -> runGitIO $ sessionStartLogic role input.cwd

  case result of
    Left (e :: SomeException) -> do
      let errMsg = "SessionStart failed: " <> T.pack (show e)
      TIO.putStrLn $ "  [HOOK] " <> errMsg
      hFlush stdout
      -- On error, still allow session to start, just without context
      pure $ hookSuccess defaultOutput
        { hookSpecificOutput = Just $ SessionStartOutput Nothing
        }
    Right mContext -> do
      TIO.putStrLn "  [HOOK] SessionStart context injected"
      hFlush stdout
      pure $ hookSuccess defaultOutput
        { hookSpecificOutput = Just $ SessionStartOutput mContext
        }

-- | Handle Stop hook: gather state, render template, provide actionable guidance.
--
-- Uses CircuitBreaker to prevent infinite loops and concurrent execution.
handleStop :: HookInput -> Runtime -> CircuitBreakerMap -> IO ControlResponse
handleStop input runtime cbMap = do
  sessionId <- getOrCreateSession input
  TIO.putStrLn $ "  [HOOK] Running Stop hook with circuit breaker for session: " <> sessionId
  hFlush stdout

  withCircuitBreaker cbMap sessionId (runStopHookLogic input) >>= \case
    Left err -> do
      TIO.putStrLn $ "  [HOOK] Circuit breaker blocked Stop: " <> err
      hFlush stdout
      -- Block with circuit breaker error
      pure $ HookResponse
        { output = defaultOutput
            { continue_ = False
            , stopReason = Just $ "Circuit breaker: " <> err
            , systemMessage = Just $ "Tidepool circuit breaker triggered: " <> err
            , hookSpecificOutput = Just StopOutput
            }
        , exitCode = 1 -- runtimeExitCode runtime -- TODO: use runtime-aware exit code
        }
    Right result -> do
      TIO.putStrLn $ "  [HOOK] Stop hook completed, block=" <> T.pack (show result.shrShouldBlock)
      hFlush stdout

      -- Increment circuit breaker stage counter if blocking
      now <- getCurrentTime
      case (result.shrShouldBlock, result.shrStage) of
        (True, Just stage) -> incrementStage cbMap sessionId stage now
        (True, Nothing) -> incrementStage cbMap sessionId "unknown" now
        _ -> pure ()

      -- Auto-focus if not blocking
      if not result.shrShouldBlock
        then autoFocusOnSubagentStop
        else pure ()

      if result.shrShouldBlock
        then pure $ HookResponse
          { output = defaultOutput
              { continue_ = False
              , stopReason = Just result.shrMessage
              , hookSpecificOutput = Just StopOutput
              }
          , exitCode = 1
          }
        else pure $ hookSuccess defaultOutput
          { systemMessage = Just result.shrMessage
          , hookSpecificOutput = Just StopOutput
          }

-- | Get or create session ID from hook input.
--
-- Currently returns the session ID provided by the agent.
-- Future work: logic to generate stable IDs if agent's ID is ephemeral.
getOrCreateSession :: HookInput -> IO SessionId
getOrCreateSession input = pure input.sessionId

-- | Logic to run inside circuit breaker lock.
runStopHookLogic :: HookInput -> IO StopHookResult
runStopHookLogic input = do
  -- Read repo from environment, default to tidepool
  mRepoEnv <- lookupEnv "TIDEPOOL_GITHUB_REPO"
  let repoName = maybe "tidepool-heavy-industries/tidepool" T.pack mRepoEnv

  -- Check if pre-commit checks should run (default: enabled)
  mPreCommit <- lookupEnv "TIDEPOOL_STOP_PRECOMMIT"
  let runPreCommit = mPreCommit /= Just "false"

  -- Check if we should use SSH for execution (if TIDEPOOL_CONTAINER is set)
  mContainer <- lookupEnv "TIDEPOOL_CONTAINER"
  sshProxyUrl <- fromMaybe "http://localhost:7433" <$> lookupEnv "SSH_PROXY_URL"

  runM $ runLog Debug $ runBDIO defaultBDConfig $ runGitHubIO defaultGitHubConfig $
    case mContainer of
      Just container ->
        runSshExec (T.pack sshProxyUrl) $
        runGitViaSsh (T.pack container) "." $
        runJustfileViaSsh (T.pack container) "." $
        stopHookLogic repoName runPreCommit
      Nothing ->
        runGitIO $
        runJustfileIO $
        stopHookLogic repoName runPreCommit

-- | Auto-focus on subagent tab when Stop hook fires.
--
-- Only triggers if:
-- 1. We're running in Zellij
-- 2. We're in a subagent worktree (bd-* branch)
-- 3. TIDEPOOL_AUTO_FOCUS_ON_ERROR is not set to "false"
autoFocusOnSubagentStop :: IO ()
autoFocusOnSubagentStop = do
  -- Check if auto-focus is enabled (default: enabled)
  mAutoFocus <- lookupEnv "TIDEPOOL_AUTO_FOCUS_ON_ERROR"
  let autoFocusEnabled = mAutoFocus /= Just "false"

  if not autoFocusEnabled
    then do
      TIO.putStrLn "  [HOOK] Auto-focus disabled via TIDEPOOL_AUTO_FOCUS_ON_ERROR"
      hFlush stdout
    else do
      -- Check if we should use SSH for execution (if TIDEPOOL_CONTAINER is set)
      mContainer <- lookupEnv "TIDEPOOL_CONTAINER"
      sshProxyUrl <- fromMaybe "http://localhost:7433" <$> lookupEnv "SSH_PROXY_URL"

      result <- try $ runM
        $ runZellijIO
        $ case mContainer of
             Just container -> runSshExec (T.pack sshProxyUrl) $ runGitViaSsh (T.pack container) "." autoFocusLogic
             Nothing -> runGitIO autoFocusLogic

      case result of
        Left (e :: SomeException) -> do
          TIO.putStrLn $ "  [HOOK] Auto-focus failed: " <> T.pack (show e)
          hFlush stdout
        Right () -> do
          TIO.putStrLn "  [HOOK] Auto-focus completed"
          hFlush stdout

-- | Auto-focus logic: check Zellij, parse bead ID, switch focus.
autoFocusLogic :: (Member Zellij es, Member Git es) => Eff es ()
autoFocusLogic = do
  -- Check if we're in Zellij
  mZellij <- checkZellijEnv
  case mZellij of
    Nothing -> pure ()  -- Not in Zellij, skip
    Just _ -> do
      -- Get worktree info to extract branch name
      mWt <- getWorktreeInfo
      case mWt of
        Nothing -> pure ()  -- No worktree info, skip
        Just wt -> do
          -- Parse bead ID from branch name
          let branchName = wt.wiBranch
              maybeBeadId = parseBeadId branchName
          case maybeBeadId of
            Nothing -> pure ()  -- Not a bd-* branch, skip
            Just beadId -> do
              -- Switch focus to tab
              -- Tab name is the short ID (e.g., "9uv")
              -- parseBeadId returns "tidepool-9uv", so strip the prefix
              let tabName = T.stripPrefix "tidepool-" beadId
              case tabName of
                Nothing -> pure ()  -- Unexpected format, skip
                Just shortId -> do
                  _ <- goToTab (TabId shortId)
                  pure ()

-- | Create appropriate response based on hook type.
makeResponse :: Text -> HookInput -> HookOutput
makeResponse eventName input = case eventName of
  "PostToolUse" -> allowPostToolUse Nothing
  "PermissionRequest" -> defaultOutput
    { hookSpecificOutput = Just $ PermissionRequestOutput $ Allow Nothing
    }
  "UserPromptSubmit" -> defaultOutput
    { hookSpecificOutput = Just $ UserPromptSubmitOutput Nothing
    }
  "SessionStart" -> defaultOutput
    { hookSpecificOutput = Just $ SessionStartOutput $
        Just $ "Tidepool control server connected. Session: " <> input.sessionId
    }
  "SessionEnd" -> defaultOutput
    { hookSpecificOutput = Just SessionEndOutput
    }
  "Stop" -> defaultOutput
    { hookSpecificOutput = Just StopOutput
    }
  "SubagentStop" -> defaultOutput
    { hookSpecificOutput = Just SubagentStopOutput
    }
  "Notification" -> defaultOutput
    { hookSpecificOutput = Just NotificationOutput
    }
  "PreCompact" -> defaultOutput
    { hookSpecificOutput = Just PreCompactOutput
    }
  _ -> defaultOutput  -- Unknown hook type, just continue

-- | Default output (continue, no specific output)
defaultOutput :: HookOutput
defaultOutput = HookOutput
  { continue_ = True
  , stopReason = Nothing
  , suppressOutput = Nothing
  , systemMessage = Nothing
  , hookSpecificOutput = Nothing
  }