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

import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import System.Environment (lookupEnv)
import Control.Exception (SomeException, try)
import Control.Monad.Freer (Eff, Member, runM)

import Tidepool.Control.Protocol
import Tidepool.Control.ExoTools (parseBeadId)
import Tidepool.Control.Hook.SessionStart (sessionStartLogic)
import Tidepool.BD.Interpreter (runBDIO, defaultBDConfig)
import Tidepool.BD.GitInterpreter (runGitIO)
import Tidepool.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import Tidepool.Effect.Types (runLog, LogLevel(..))
import Tidepool.Effects.BD (BD, BeadInfo(..), getBead)
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo, getDirtyFiles)
import Tidepool.Effects.GitHub (GitHub, PullRequest(..), listPullRequests, Repo(..), PRFilter(..), defaultPRFilter)
import Tidepool.Effects.Zellij (Zellij, checkZellijEnv, goToTab, TabId(..))
import Tidepool.Zellij.Interpreter (runZellijIO)

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

-- | Handle Stop hook: enforce PR filing with templated guidance + auto-focus.
handleStop :: HookInput -> Runtime -> IO ControlResponse
handleStop _input _runtime = do
  TIO.putStrLn "  [HOOK] Running Stop hook with PR check..."
  hFlush stdout

  -- Read repo from environment, default to tidepool
  mRepoEnv <- lookupEnv "TIDEPOOL_GITHUB_REPO"
  let repoName = maybe "tidepool-heavy-industries/tidepool" T.pack mRepoEnv

  result <- try
    $ runM
    $ runLog Debug
    $ runBDIO defaultBDConfig
    $ runGitIO
    $ runGitHubIO defaultGitHubConfig
    $ stopLogic repoName

  case result of
    Left (e :: SomeException) -> do
      let errMsg = "Stop hook failed: " <> T.pack (show e)
      TIO.putStrLn $ "  [HOOK] " <> errMsg
      hFlush stdout
      -- On error, allow stop but show error message
      pure $ hookSuccess defaultOutput
        { systemMessage = Just errMsg
        , hookSpecificOutput = Just StopOutput
        }
    Right response -> do
      TIO.putStrLn "  [HOOK] Stop hook succeeded"
      hFlush stdout

      -- If stop is allowed, attempt to switch focus if in a subagent worktree
      autoFocusOnSubagentStop

      pure response

-- | Shell-escape a string for safe inclusion in bash commands.
-- Wraps the string in single quotes and escapes any single quotes within.
shellEscape :: Text -> Text
shellEscape t = "'" <> T.replace "'" "'\"'\"'" t <> "'"

-- | Get bead title from bead ID, returning empty string if not found.
getBeadTitle :: Member BD es => Maybe Text -> Eff es Text
getBeadTitle Nothing = pure ""
getBeadTitle (Just beadId) = do
  mBead <- getBead beadId
  pure $ case mBead of
    Just bead -> bead.biTitle
    Nothing -> ""

-- | Core Stop hook logic with conditional branching.
stopLogic :: (Member BD es, Member Git es, Member GitHub es) => Text -> Eff es ControlResponse
stopLogic repoName = do
  -- 1. Get worktree info and dirty files
  mWorktree <- getWorktreeInfo
  dirtyFiles <- getDirtyFiles

  case mWorktree of
    Nothing -> do
      -- Not in a git repo, allow stop
      pure $ hookSuccess defaultOutput
        { hookSpecificOutput = Just StopOutput
        }

    Just wt -> do
      let branch = wt.wiBranch
          mBeadId = parseBeadId branch
          hasUncommitted = not (null dirtyFiles)
          isBeadBranch = isJust mBeadId

      if not isBeadBranch
        then do
          -- Not on a bead branch, allow stop (pass through)
          pure $ hookSuccess defaultOutput
            { hookSpecificOutput = Just StopOutput
            }
        else do
          -- On a bead branch, check for PR
          let repo = Repo repoName
              filt = defaultPRFilter { pfBase = Just "main", pfLimit = Just 100 }
          prs <- listPullRequests repo filt
          let mPR = find (\pr -> pr.prHeadRefName == branch) prs

          case (mPR, hasUncommitted) of
            (Just pr, _) -> do
              -- PR filed, good to stop
              let beadId = fromMaybe "" mBeadId
                  msg = "✓ PR #" <> T.pack (show pr.prNumber) <> " filed for " <> beadId <> "."
              pure $ hookSuccess defaultOutput
                { systemMessage = Just msg
                , hookSpecificOutput = Just StopOutput
                }

            (Nothing, True) -> do
              -- Bead branch + uncommitted + no PR = needs work (block)
              let beadId = fromMaybe "" mBeadId
              beadTitle <- getBeadTitle mBeadId
              let filesEscaped = T.intercalate " " (map (shellEscape . T.pack) dirtyFiles)
                  msg = T.unlines
                    [ "⚠️ Uncommitted changes on " <> branch <> ". File PR before stopping:"
                    , ""
                    , "```bash"
                    , "git add " <> filesEscaped
                    , "git commit -m " <> shellEscape ("[" <> beadId <> "] <description>")
                    , "git push -u origin " <> shellEscape branch
                    , "gh pr create --title " <> shellEscape ("[" <> beadId <> "] " <> beadTitle) <> " --body " <> shellEscape ("Closes " <> beadId)
                    , "```"
                    ]
              pure $ HookResponse
                { output = defaultOutput
                    { continue_ = False  -- Block stop
                    , stopReason = Just msg
                    , hookSpecificOutput = Just StopOutput
                    }
                , exitCode = 1  -- Non-zero exit code to block
                }

            (Nothing, False) -> do
              -- Bead branch + clean + no PR = suggest filing PR (allow)
              let beadId = fromMaybe "" mBeadId
              beadTitle <- getBeadTitle mBeadId
              let msg = T.unlines
                    [ "Work complete? Consider filing PR:"
                    , ""
                    , "```bash"
                    , "gh pr create --title " <> shellEscape ("[" <> beadId <> "] " <> beadTitle)
                    , "```"
                    ]
              pure $ hookSuccess defaultOutput
                { systemMessage = Just msg
                , hookSpecificOutput = Just StopOutput
                }

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
