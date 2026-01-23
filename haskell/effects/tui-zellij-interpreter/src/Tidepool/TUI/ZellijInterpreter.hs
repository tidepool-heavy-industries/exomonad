{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
-- | TUI effect interpreter using Zellij floating panes.
--
-- This interpreter spawns Zellij panes with the tui-popup binary to display
-- interactive popups. The popup renders in a floating pane, captures user input,
-- and outputs the result to a temporary file.
--
-- == Architecture
--
-- @
-- control-server                    Zellij Pane
--     |                                  |
--     | spawn pane with tui-popup        |
--     |------------------------------->  |
--     |                                  |
--     | wait for process exit            |
--     |                                  | (user interacts)
--     |                                  |
--     |                                  | write result
--     |<-------------------------------  |
--     | read result from temp file       |
-- @
--
-- == Example Usage
--
-- @
-- import Tidepool.TUI.ZellijInterpreter (runTUIZellij)
--
-- main :: IO ()
-- main = do
--   result <- runM $ runTUIZellij $ do
--     popupResult <- showUI $ PopupDefinition {...}
--     pure popupResult
-- @
module Tidepool.TUI.ZellijInterpreter
  ( runTUIZellij
  , runTUIZellijWithBinary
  ) where

import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (encode, eitherDecodeFileStrict, object)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import qualified System.IO as IO
import System.IO.Temp (getCanonicalTemporaryDirectory)
import System.Process (readProcessWithExitCode)

import Tidepool.Effect.TUI

-- | Interpret the TUI effect by spawning Zellij floating panes.
--
-- Uses the tui-popup binary found in PATH. For custom binary locations,
-- use 'runTUIZellijWithBinary'.
runTUIZellij :: LastMember IO effs => Eff (TUI ': effs) a -> Eff effs a
runTUIZellij = runTUIZellijWithBinary "tui-popup"

-- | Interpret the TUI effect with a custom tui-popup binary path.
runTUIZellijWithBinary :: LastMember IO effs => FilePath -> Eff (TUI ': effs) a -> Eff effs a
runTUIZellijWithBinary binaryPath = interpret $ \case
  ShowUI definition -> sendM $ spawnPopupPane binaryPath definition

-- | Spawn a Zellij floating pane with tui-popup and wait for result.
spawnPopupPane :: FilePath -> PopupDefinition -> IO PopupResult
spawnPopupPane binaryPath definition = do
  -- Create temp file for result
  tmpDir <- getCanonicalTemporaryDirectory
  let resultFile = tmpDir </> "tui-popup-result.json"

  -- Serialize PopupDefinition to JSON
  let defJson = LBS.toStrict $ encode definition
      defJsonStr = T.unpack $ T.decodeUtf8 defJson

  -- Build zellij command
  -- zellij action new-pane --floating --name "popup" -- sh -c 'tui-popup --spec "{...}" > /tmp/result.json'
  let shCmd = binaryPath <> " --spec '" <> escapeShellArg defJsonStr <> "' > " <> resultFile
      zellijArgs =
        [ "action"
        , "new-pane"
        , "--floating"
        , "--name"
        , "tidepool-popup"
        , "--"
        , "sh"
        , "-c"
        , shCmd
        ]

  -- Spawn Zellij pane and wait for completion
  (exitCode, pStdout, pStderr) <- readProcessWithExitCode "zellij" zellijArgs ""

  case exitCode of
    ExitSuccess -> do
      -- Read result from temp file
      resultOrErr <- eitherDecodeFileStrict resultFile
      removeFile resultFile  -- Cleanup
      case resultOrErr of
        Right result -> pure result
        Left err -> do
          IO.hPutStrLn IO.stderr $ "Error parsing popup result: " <> err
          pure $ PopupResult "error" (object [])
    ExitFailure code -> do
      IO.hPutStrLn IO.stderr $ "Zellij pane failed with exit code " <> show code
      IO.hPutStrLn IO.stderr $ "stdout: " <> pStdout
      IO.hPutStrLn IO.stderr $ "stderr: " <> pStderr
      pure $ PopupResult "error" (object [])

-- | Escape shell argument by replacing single quotes with '\''
escapeShellArg :: String -> String
escapeShellArg = concatMap $ \c ->
  case c of
    '\'' -> "'\\''"  -- End quote, escaped quote, start quote
    _    -> [c]
