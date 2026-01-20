-- | Zellij effect interpreter - terminal multiplexer operations.
--
-- Implements Zellij effect by calling zellij CLI commands.
-- Enables graphs to create tabs for parallel agent orchestration.
--
-- = Usage
--
-- @
-- import Tidepool.Zellij.Interpreter (runZellijIO)
-- import Tidepool.Effects.Zellij
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
-- Requires running inside a Zellij session (ZELLIJ environment variable set).
module Tidepool.Zellij.Interpreter
  ( -- * Interpreter
    runZellijIO
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Tidepool.Effects.Zellij
  ( Zellij(..)
  , TabConfig(..)
  , TabId(..)
  , ZellijError(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Zellij effects using zellij CLI commands.
--
-- All operations are wrapped in try/catch to return explicit errors.
runZellijIO :: LastMember IO effs => Eff (Zellij ': effs) a -> Eff effs a
runZellijIO = interpret $ \case
  CheckZellijEnv -> sendM checkZellijEnvIO
  NewTab config -> sendM $ newTabIO config


-- ════════════════════════════════════════════════════════════════════════════
-- IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if running inside a Zellij session.
checkZellijEnvIO :: IO (Maybe Text)
checkZellijEnvIO = do
  result <- lookupEnv "ZELLIJ"
  pure $ T.pack <$> result

-- | Create a new Zellij tab.
--
-- Uses: zellij action new-tab --layout <layout> --cwd <cwd> --name <name>
--
-- Environment variables are passed via shell wrapper since zellij action
-- doesn't support direct environment variable passing.
newTabIO :: TabConfig -> IO (Either ZellijError TabId)
newTabIO config = do
  -- Check if layout file exists
  layoutExists <- doesFileExist config.tcLayout
  if not layoutExists
    then pure $ Left ZellijLayoutNotFound { zlnfPath = config.tcLayout }
    else do
      result <- try @SomeException $ do
        -- Build the command
        -- zellij action new-tab with layout and cwd
        -- The --name argument sets the tab name
        let args =
              [ "action", "new-tab"
              , "--layout", config.tcLayout
              , "--cwd", config.tcCwd
              , "--name", T.unpack config.tcName
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
        Left e -> pure $ Left ZellijCommandFailed
          { zceCommand = "new-tab"
          , zceExitCode = -1
          , zceStderr = T.pack (show e)
          }
        Right (exitCode, _stdout, stderr) ->
          case exitCode of
            ExitSuccess -> pure $ Right $ TabId config.tcName
            ExitFailure code -> pure $ Left ZellijCommandFailed
              { zceCommand = "new-tab"
              , zceExitCode = code
              , zceStderr = T.pack stderr
              }
