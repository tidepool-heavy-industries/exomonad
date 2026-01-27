{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Hook event handler.
--
-- Handles hook events from Claude Code via exomonad.
-- Most hooks are passthrough, but some execute effect logic:
--
-- * SessionStart: Injects bead context when on a bd-* branch
-- * Stop: Enforces PR filing with templated guidance + auto-focus on subagent error
module ExoMonad.Control.Handler.Hook
  ( handleHook
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import System.IO (hFlush, stdout)
import System.Environment (lookupEnv)
import Control.Exception (SomeException, try, throwIO)
import Control.Monad (forM_)
import Control.Monad.Freer (Eff, Member, runM)
import Control.Monad.Freer.State (runState)
import Data.Time.Clock (getCurrentTime)
import OpenTelemetry.Trace
import qualified OpenTelemetry.Context.ThreadLocal as Context

import ExoMonad.Control.Protocol hiding (role)
import ExoMonad.Control.Types (ServerConfig(..))
import ExoMonad.Control.Hook.Policy (HookDecision(..), evaluatePolicy)
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerMap, SessionId, withCircuitBreaker, incrementStage)
import ExoMonad.Control.ExoTools (parseIssueNumber)
import ExoMonad.Control.Hook.SessionStart (sessionStartLogic)
import ExoMonad.Control.Effects.SshExec (runSshExec)
import ExoMonad.Control.Effects.Git (runGitRemote)
import ExoMonad.Control.Effects.Cabal (runCabalRemote)
import ExoMonad.Control.Effects.Effector (runEffectorViaSsh, runEffectorIO)
import ExoMonad.Control.Interpreters.Traced (traceCabal, traceGit)
import ExoMonad.Cabal.Interpreter (runCabalIO, defaultCabalConfig)
import ExoMonad.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import ExoMonad.FileSystem.Interpreter (runFileSystemIO)
import ExoMonad.Env.Interpreter (runEnvIO)
import ExoMonad.Effect.Types (runLog, LogLevel(..))
import ExoMonad.Effect.NodeMeta (runGraphMeta, runNodeMeta, defaultNodeMeta, GraphMetadata(..))
import ExoMonad.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import ExoMonad.Git.Interpreter (runGitIO)
import ExoMonad.Effects.Zellij (Zellij, checkZellijEnv, goToTab, TabId(..))
import ExoMonad.Zellij.Interpreter (runZellijIO)
import ExoMonad.Graph.Interpret (runGraph)
import qualified ExoMonad.Control.Runtime.Paths as Paths

import ExoMonad.Control.StopHook.Types
import ExoMonad.Control.StopHook.Graph
import ExoMonad.Control.StopHook.Handlers
import ExoMonad.Control.StopHook.Templates (renderStopHookTemplate)

-- | Handle a hook event.
--
-- Executes hook-specific logic for SessionStart and Stop.
-- Other hooks pass through with default responses.
handleHook :: Tracer -> ServerConfig -> HookInput -> Runtime -> Role -> CircuitBreakerMap -> IO ControlResponse
handleHook tracer config input runtime agentRole cbMap = do
  TIO.putStrLn $ "  session=" <> input.sessionId
  TIO.putStrLn $ "  cwd=" <> input.cwd
  TIO.putStrLn $ "  role=" <> T.pack (show agentRole)
  hFlush stdout

  let spanName = "hook." <> input.hookEventName
  
  -- Manual span management
  ctx <- Context.getContext
  span <- createSpan tracer ctx spanName defaultSpanArguments
  
  result <- try $ do
    addAttribute span "session.id" input.sessionId
    addAttribute span "jsonl.file" input.transcriptPath
    forM_ input.toolUseId $ \tid -> addAttribute span "tool_use_id" tid
    forM_ input.toolName $ \tn -> addAttribute span "tool_name" tn
    
    case input.hookEventName of
      "SessionStart" -> handleSessionStart tracer agentRole input
      "Stop" -> handleStop tracer config input runtime cbMap
      "PreToolUse" -> handlePreToolUse config input
      _ -> pure $ hookSuccess $ makeResponse input.hookEventName input

  endSpan span Nothing
  
  case result of
    Left (e :: SomeException) -> do
      recordException span mempty Nothing e
      throwIO e
    Right r -> pure r

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
handleSessionStart :: Tracer -> Role -> HookInput -> IO ControlResponse
handleSessionStart tracer role input = do
  TIO.putStrLn "  [HOOK] Running SessionStart context injection..."
  hFlush stdout

  -- Check if we should use SSH for execution (if EXOMONAD_CONTAINER is set)
  mContainer <- lookupEnv "EXOMONAD_CONTAINER"

  -- Get binary directory (respects EXOMONAD_BIN_DIR, defaults to /usr/local/bin)
  binDir <- Paths.dockerBinDir
  let dockerCtlPath = Paths.dockerCtlBin binDir

  result <- try $ runM
    $ runLog Debug
    $ runGitHubIO defaultGitHubConfig
    $ case mContainer of 
         Just container -> 
           runSshExec dockerCtlPath
           $ runGitRemote (T.pack container) "." 
           $ traceGit tracer
           $ sessionStartLogic role input.cwd
         Nothing -> 
           runGitIO 
           $ traceGit tracer
           $ sessionStartLogic role input.cwd

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
handleStop :: Tracer -> ServerConfig -> HookInput -> Runtime -> CircuitBreakerMap -> IO ControlResponse
handleStop tracer config input runtime cbMap = do
  sessionId <- getOrCreateSession input
  TIO.putStrLn $ "  [HOOK] Running Stop hook with circuit breaker for session: " <> sessionId
  hFlush stdout

  now <- getCurrentTime
  withCircuitBreaker cbMap config.circuitBreakerConfig now sessionId (runStopHookLogic tracer input) >>= \case
    Left err -> do
      TIO.putStrLn $ "  [HOOK] Circuit breaker blocked Stop: " <> err
      hFlush stdout
      -- Block with circuit breaker error
      pure $ HookResponse
        {
          output = defaultOutput
            { continue_ = False
            , stopReason = Just $ "Circuit breaker: " <> err
            , systemMessage = Just $ "ExoMonad circuit breaker triggered: " <> err
            , hookSpecificOutput = Just StopOutput
            }
        , exitCode = 1
        }
    Right (templateName, context) -> do
      TIO.putStrLn $ "  [HOOK] Stop hook completed, template=" <> templateName
      hFlush stdout

      let rendered = renderStopHookTemplate templateName context

      -- Increment circuit breaker stage counter if blocking
      incrementTime <- getCurrentTime
      let shouldBlock = templateName `elem` ["fix-build-errors", "max-loops", "build-stuck", "fix-test-failures", "test-stuck"]
      let stageName = context.stage
      
      -- We always increment stage in the graph/workflow state, but CB map is external
      -- For now, just logging or tracking simple stage
      if shouldBlock
        then incrementStage cbMap sessionId stageName incrementTime
        else pure ()

      -- Auto-focus if not blocking
      if not shouldBlock
        then autoFocusOnSubagentStop
        else pure ()

      if shouldBlock
        then pure $ HookResponse
          {
            output = defaultOutput
              {
                continue_ = False
              , stopReason = Just $ "Workflow stage: " <> templateName
              , hookSpecificOutput = Just StopOutput
              , systemMessage = Just rendered
              }
          , exitCode = 1
          }
        else pure $ hookSuccess defaultOutput
          {
            systemMessage = Just rendered
          , hookSpecificOutput = Just StopOutput
          }

-- | Get or create session ID from hook input.
getOrCreateSession :: HookInput -> IO SessionId
getOrCreateSession input = pure input.sessionId

-- | Logic to run inside circuit breaker lock.
-- Replaces old runStopHookLogic with graph execution.
runStopHookLogic :: Tracer -> HookInput -> IO (TemplateName, StopHookContext)
runStopHookLogic tracer input = do
  -- Check environment for container/SSH
  mContainer <- lookupEnv "EXOMONAD_CONTAINER"

  -- Get binary directory (respects EXOMONAD_BIN_DIR, defaults to /usr/local/bin)
  binDir <- Paths.dockerBinDir
  let dockerCtlPath = Paths.dockerCtlBin binDir

  -- Initialize minimal workflow state
  let initialWorkflow = WorkflowState
        {
          wsGlobalStops = 0 -- TODO: Fetch from CB map?
        , wsStageRetries = Map.empty
        , wsCurrentStage = StageBuild
        , wsLastBuildResult = Nothing
        , wsLastPRStatus = Nothing
        , wsLastTestResult = Nothing
        }
  
  -- We need to fetch git info first to populate AgentState
  agentState <- case mContainer of
     Just container -> runM $ runSshExec dockerCtlPath $ runGitRemote (T.pack container) "." $ traceGit tracer $ getAgentState input
     Nothing -> runM $ runGitIO $ traceGit tracer $ getAgentState input

  (result, _finalState) <- case mContainer of
        Just container ->
          runM
          $ runSshExec dockerCtlPath
          $ runEffectorViaSsh (T.pack container)
          $ runCabalRemote (T.pack container)
          $ traceCabal tracer
          $ runGitRemote (T.pack container) "."
          $ traceGit tracer
          $ runGraphMeta (GraphMetadata "stop-hook")
          $ runNodeMeta defaultNodeMeta
          $ runState initialWorkflow
          $ runGraph stopHookHandlers agentState
        Nothing ->
          runM
          $ runState initialWorkflow
          $ runEffectorIO
          $ runCabalIO defaultCabalConfig
          $ traceCabal tracer
          $ runGitIO
          $ traceGit tracer
          $ runGraphMeta (GraphMetadata "stop-hook")
          $ runNodeMeta defaultNodeMeta
          $ runGraph stopHookHandlers agentState

  pure result

getAgentState :: Member Git es => HookInput -> Eff es AgentState
getAgentState input = do
  mWt <- getWorktreeInfo
  let branch = (.wiBranch) <$> mWt
      issueNum = branch >>= parseIssueNumber
  pure $ AgentState
    {
      asSessionId = input.sessionId
    , asCwd = T.unpack input.cwd
    , asBranch = branch
    , asIssueNum = issueNum
    }

-- | Auto-focus on subagent tab when Stop hook fires.
autoFocusOnSubagentStop :: IO ()
autoFocusOnSubagentStop = do
  mAutoFocus <- lookupEnv "EXOMONAD_AUTO_FOCUS_ON_ERROR"
  let autoFocusEnabled = mAutoFocus /= Just "false"

  if not autoFocusEnabled
    then pure ()
    else do
      mContainer <- lookupEnv "EXOMONAD_CONTAINER"

      -- Get binary directory (respects EXOMONAD_BIN_DIR, defaults to /usr/local/bin)
      binDir <- Paths.dockerBinDir
      let dockerCtlPath = Paths.dockerCtlBin binDir

      result <- try $ runM
        $ runZellijIO
        $ case mContainer of
             Just container -> runSshExec dockerCtlPath $ runGitRemote (T.pack container) "." autoFocusLogic
             Nothing -> runGitIO autoFocusLogic

      case result of
        Left (e :: SomeException) -> do
          TIO.putStrLn $ "  [HOOK] Auto-focus failed: " <> T.pack (show e)
          hFlush stdout
        Right () -> pure ()

-- | Auto-focus logic: check Zellij, parse bead ID, switch focus.
autoFocusLogic :: (Member Zellij es, Member Git es) => Eff es ()
autoFocusLogic = do
  mZellij <- checkZellijEnv
  case mZellij of
    Nothing -> pure ()
    Just _ -> do
      mWt <- getWorktreeInfo
      case mWt of
        Nothing -> pure ()
        Just wt -> do
          let branchName = wt.wiBranch
              maybeIssueNum = parseIssueNumber branchName
              maybeBeadId = T.pack . show <$> maybeIssueNum
          case maybeBeadId of
            Nothing -> pure ()
            Just bid -> do
              -- Focus on tab with bead ID name
              _ <- goToTab (TabId bid)
              pure ()
          case maybeBeadId of
            Nothing -> pure ()
            Just beadId -> do
              let tabName = T.stripPrefix "exomonad-" beadId
              case tabName of
                Nothing -> pure ()
                Just shortId -> do
                  _ <- goToTab (TabId shortId)
                  pure ()

-- | Create appropriate response based on hook type.
makeResponse :: Text -> HookInput -> HookOutput
makeResponse eventName input = case eventName of
  "PostToolUse" -> allowPostToolUse Nothing
  "PermissionRequest" -> defaultOutput
    {
      hookSpecificOutput = Just $ PermissionRequestOutput $ Allow Nothing
    }
  "UserPromptSubmit" -> defaultOutput
    {
      hookSpecificOutput = Just $ UserPromptSubmitOutput Nothing
    }
  "SessionStart" -> defaultOutput
    {
      hookSpecificOutput = Just $ SessionStartOutput $
        Just $ "ExoMonad control server connected. Session: " <> input.sessionId
    }
  "SessionEnd" -> defaultOutput
    {
      hookSpecificOutput = Just SessionEndOutput
    }
  "Stop" -> defaultOutput
    {
      hookSpecificOutput = Just StopOutput
    }
  "SubagentStop" -> defaultOutput
    {
      hookSpecificOutput = Just SubagentStopOutput
    }
  "Notification" -> defaultOutput
    {
      hookSpecificOutput = Just NotificationOutput
    }
  "PreCompact" -> defaultOutput
    {
      hookSpecificOutput = Just PreCompactOutput
    }
  _ -> defaultOutput

-- | Default output (continue, no specific output)
defaultOutput :: HookOutput
defaultOutput = HookOutput
  {
    continue_ = True
  , stopReason = Nothing
  , suppressOutput = Nothing
  , systemMessage = Nothing
  , hookSpecificOutput = Nothing
  }