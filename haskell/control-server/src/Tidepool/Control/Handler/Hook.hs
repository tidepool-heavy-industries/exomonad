-- | Hook event handler.
--
-- Handles hook events from Claude Code via mantle-agent.
-- Most hooks are passthrough, but some execute effect logic:
--
-- * SessionStart: Injects bead context when on a bd-* branch
-- * Stop: Runs reconstitute (syncs beads)
module Tidepool.Control.Handler.Hook
  ( handleHook
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Control.Exception (SomeException, try)
import Control.Monad.Freer (Eff, Member, runM)

import Tidepool.Control.Protocol
import Tidepool.Control.ExoTools (exoReconstituteLogic, ExoReconstituteArgs(..), ExoReconstituteResult, parseBeadId)
import Tidepool.Control.Hook.SessionStart (sessionStartLogic)
import Tidepool.BD.Interpreter (runBDIO, defaultBDConfig)
import Tidepool.BD.GitInterpreter (runGitIO)
import Tidepool.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import Tidepool.Effect.Types (runLog, LogLevel(..))
import Tidepool.Graph.Goto (unwrapSingleChoice)
import Tidepool.Effects.Git (Git, getWorktreeInfo, WorktreeInfo(..))
import Tidepool.Effects.Zellij (Zellij, checkZellijEnv, goToTab, TabId(..))
import Tidepool.Zellij.Interpreter (runZellijIO)
import System.Environment (lookupEnv)

-- | Handle a hook event.
--
-- Executes hook-specific logic for SessionStart and Stop.
-- Other hooks pass through with default responses.
handleHook :: HookInput -> Runtime -> IO ControlResponse
handleHook input runtime = do
  TIO.putStrLn $ "  session=" <> input.sessionId
  TIO.putStrLn $ "  cwd=" <> input.cwd
  hFlush stdout

  case input.hookEventName of
    "SessionStart" -> handleSessionStart input
    "Stop" -> handleStop input runtime
    _ -> pure $ hookSuccess $ makeResponse input.hookEventName input

-- | Handle SessionStart hook: inject bead context.
handleSessionStart :: HookInput -> IO ControlResponse
handleSessionStart input = do
  TIO.putStrLn "  [HOOK] Running SessionStart context injection..."
  hFlush stdout

  result <- try $ runM
    $ runLog Debug
    $ runBDIO defaultBDConfig
    $ runGitIO
    $ sessionStartLogic input.cwd

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

-- | Handle Stop hook: run reconstitute logic (sync beads) and auto-focus on subagent error.
handleStop :: HookInput -> Runtime -> IO ControlResponse
handleStop input runtime = do
  TIO.putStrLn "  [HOOK] Running post-stop reconstitute..."
  hFlush stdout

  -- First, run reconstitute
  result <- (try $ runM
    $ runLog Debug
    $ runBDIO defaultBDConfig
    $ runGitIO
    $ runGitHubIO defaultGitHubConfig
    $ unwrapSingleChoice <$> exoReconstituteLogic (ExoReconstituteArgs Nothing)) :: IO (Either SomeException ExoReconstituteResult)

  case result of
    Left e -> do
      let errMsg = "Reconstitute failed: " <> T.pack (show e)
      TIO.putStrLn $ "  [HOOK] " <> errMsg
      hFlush stdout
      pure $ hookError runtime errMsg
    Right _ -> do
      TIO.putStrLn "  [HOOK] Reconstitute succeeded"
      hFlush stdout

      -- Then, attempt to switch focus if in a subagent worktree
      autoFocusOnSubagentStop

      pure $ hookSuccess $ makeResponse input.hookEventName input

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
      result <- try $ runM
        $ runZellijIO
        $ runGitIO
        $ autoFocusLogic

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
  "PreToolUse" -> allowPreToolUse (Just "Allowed by Tidepool") Nothing
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
