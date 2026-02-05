-- | Zellij effect interpreter - terminal multiplexer operations.
--
-- Implements Zellij effect by calling zellij CLI commands.
-- Enables graphs to create tabs for parallel agent orchestration.
--
-- = Usage
--
-- @
-- import ExoMonad.Zellij.Interpreter (runZellijIO)
-- import ExoMonad.Effects.Zellij
--
-- main = runM $ runZellijIO $ do
--   mSession <- checkZellijEnv
--   case mSession of
--     Nothing -> error "Not in Zellij"
--     Just _ -> do
--       let cfg = TabConfig "worker" ".zellij/worktree.kdl" "/cwd" [("X", "Y")]
--       newTab cfg
-- @
--
-- = Requirements
--
-- Requires one of:
-- - Running inside a Zellij session (ZELLIJ environment variable set)
-- - Cross-container access with ZELLIJ_SESSION_NAME set and shared XDG_RUNTIME_DIR
module ExoMonad.Zellij.Interpreter
  ( -- * Interpreter
    runZellijIO,
  )
where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effects.Zellij
  ( LayoutSpec (..),
    TabConfig (..),
    TabId (..),
    Zellij (..),
    ZellijError (..),
  )
import Polysemy (Member, Sem, embed, interpret)
import Polysemy.Embed (Embed)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Zellij effects using zellij CLI commands.
--
-- All operations are wrapped in try/catch to return explicit errors.
runZellijIO :: (Member (Embed IO) r) => Sem (Zellij ': r) a -> Sem r a
runZellijIO = interpret $ \case
  CheckZellijEnv -> embed checkZellijEnvIO
  NewTab config -> embed $ newTabIO config
  GoToTab tabId -> embed $ goToTabIO tabId
  GenerateLayout spec -> embed $ generateLayoutIO spec

-- ════════════════════════════════════════════════════════════════════════════
-- IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if Zellij access is available.
--
-- Returns session info if either:
-- - Running inside a Zellij session (ZELLIJ env var set)
-- - Cross-container access configured (ZELLIJ_SESSION_NAME set)
checkZellijEnvIO :: IO (Maybe Text)
checkZellijEnvIO = do
  -- First check if we're inside a Zellij pane
  zellijEnv <- lookupEnv "ZELLIJ"
  case zellijEnv of
    Just z -> pure $ Just (T.pack z)
    Nothing -> do
      -- Check for cross-container access via session name
      sessionName <- lookupEnv "ZELLIJ_SESSION_NAME"
      pure $ T.pack <$> sessionName

-- | Create a new Zellij tab.
--
-- Uses: zellij action new-tab --layout <layout> --cwd <cwd> --name <name>
-- For cross-container: zellij --session <name> action new-tab ...
--
-- When tcCommand is set (without layout), uses new-pane instead since
-- new-tab doesn't support running commands directly.
--
-- Environment variables are passed via shell wrapper since zellij action
-- doesn't support direct environment variable passing.
newTabIO :: TabConfig -> IO (Either ZellijError TabId)
newTabIO config = do
  -- Check if layout file exists (only if layout is specified)
  layoutExists <-
    if null config.tcLayout
      then pure True
      else doesFileExist config.tcLayout

  if not layoutExists
    then pure $ Left ZellijLayoutNotFound {zlnfPath = config.tcLayout}
    else do
      result <- try @SomeException $ do
        -- Check if we need cross-container session targeting
        sessionPrefix <- getSessionPrefix

        -- Build the command based on whether we have a layout or command
        -- - With layout: use action new-tab --layout ...
        -- - With command (no layout): use action new-pane -- <cmd>
        --   (new-tab doesn't support -- <cmd>, but new-pane does)
        let args = case (null config.tcLayout, config.tcCommand) of
              -- Has layout: use new-tab with layout
              (False, _) ->
                sessionPrefix
                  ++ [ "action",
                       "new-tab",
                       "--cwd",
                       config.tcCwd,
                       "--name",
                       T.unpack config.tcName,
                       "--layout",
                       config.tcLayout
                     ]
              -- Has command, no layout: use new-pane with command
              (True, Just cmd) ->
                sessionPrefix
                  ++ [ "action",
                       "new-pane",
                       "--cwd",
                       config.tcCwd,
                       "--name",
                       T.unpack config.tcName,
                       "--",
                       "bash",
                       "-c",
                       T.unpack cmd
                     ]
              -- No layout, no command: simple new-tab
              (True, Nothing) ->
                sessionPrefix
                  ++ [ "action",
                       "new-tab",
                       "--cwd",
                       config.tcCwd,
                       "--name",
                       T.unpack config.tcName
                     ]

        -- If we have environment variables, we need to set them
        -- We do this by using env -S for the current process
        -- The layout file will inherit these env vars
        if null config.tcEnv
          then readProcessWithExitCode "zellij" args ""
          else do
            -- Use env to set variables before calling zellij
            let envArgs = concatMap (\(k, v) -> [T.unpack k <> "=" <> T.unpack v]) config.tcEnv
            readProcessWithExitCode "env" (envArgs ++ ["zellij"] ++ args) ""

      case result of
        Left e ->
          pure $
            Left
              ZellijCommandFailed
                { zceCommand = "new-tab/new-pane",
                  zceExitCode = -1,
                  zceStderr = T.pack (show e)
                }
        Right (exitCode, _stdout, stderr) ->
          case exitCode of
            ExitSuccess -> pure $ Right $ TabId config.tcName
            ExitFailure code ->
              pure $
                Left
                  ZellijCommandFailed
                    { zceCommand = "new-tab/new-pane",
                      zceExitCode = code,
                      zceStderr = T.pack stderr
                    }

-- | Switch focus to a tab by name.
--
-- Uses: zellij action go-to-tab-name <name>
-- For cross-container: zellij --session <name> action go-to-tab-name ...
goToTabIO :: TabId -> IO (Either ZellijError ())
goToTabIO (TabId tabName) = do
  result <- try @SomeException $ do
    -- Check if we need cross-container session targeting
    sessionPrefix <- getSessionPrefix
    let actionArgs = ["action", "go-to-tab-name", T.unpack tabName]
        args = sessionPrefix ++ actionArgs
    readProcessWithExitCode "zellij" args ""

  case result of
    Left e ->
      pure $
        Left
          ZellijCommandFailed
            { zceCommand = "go-to-tab-name",
              zceExitCode = -1,
              zceStderr = T.pack (show e)
            }
    Right (exitCode, _stdout, stderr) ->
      case exitCode of
        ExitSuccess -> pure $ Right ()
        ExitFailure code ->
          pure $
            Left
              ZellijCommandFailed
                { zceCommand = "go-to-tab-name",
                  zceExitCode = code,
                  zceStderr = T.pack stderr
                }

-- | Get session prefix args for cross-container access.
--
-- When running inside a Zellij pane (ZELLIJ set), returns empty list.
-- When cross-container (ZELLIJ_SESSION_NAME set), returns ["--session", "<name>"].
getSessionPrefix :: IO [String]
getSessionPrefix = do
  zellijEnv <- lookupEnv "ZELLIJ"
  case zellijEnv of
    Just _ -> pure [] -- Inside Zellij pane, no prefix needed
    Nothing -> do
      -- Check for cross-container session name
      sessionName <- lookupEnv "ZELLIJ_SESSION_NAME"
      pure $ case sessionName of
        Just name -> ["--session", name]
        Nothing -> [] -- No session configured

-- | Generate a Zellij layout file via zellij-gen.
--
-- Uses the zellij-gen CLI which bakes commands into the layout as literals,
-- solving the issue where env vars don't propagate to pane processes.
--
-- Returns the path to the generated layout file.
generateLayoutIO :: LayoutSpec -> IO (Either ZellijError FilePath)
generateLayoutIO spec = do
  let args = case spec of
        MainLayout -> ["main"]
        SubagentLayout issueId containerId ->
          ["subagent", T.unpack issueId, T.unpack containerId]

  result <- try @SomeException $ readProcessWithExitCode "zellij-gen" args ""

  case result of
    Left e ->
      pure $
        Left
          ZellijCommandFailed
            { zceCommand = "zellij-gen",
              zceExitCode = -1,
              zceStderr = T.pack (show e)
            }
    Right (exitCode, stdout, stderr) ->
      case exitCode of
        ExitSuccess ->
          -- zellij-gen prints the path to stdout
          let layoutPath = filter (/= '\n') stdout
           in pure $ Right layoutPath
        ExitFailure code ->
          pure $
            Left
              ZellijCommandFailed
                { zceCommand = "zellij-gen",
                  zceExitCode = code,
                  zceStderr = T.pack stderr
                }
