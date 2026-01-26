{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | TUI effect interpreter using FIFO-based popup spawning.
--
-- This interpreter shells out to tui-spawner, which handles:
-- 1. Writing PopupDefinition to input file
-- 2. Creating FIFO for result
-- 3. Spawning Zellij floating pane with tui-popup
-- 4. Blocking read from FIFO
-- 5. Cleanup
--
-- The result is returned via stdout (JSON).
module ExoMonad.Control.TUIInterpreter
  ( runTUIFifo
  ) where

import Control.Monad (unless)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (eitherDecode, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import ExoMonad.Control.Logging (Logger, logInfo, logDebug, logError)
import ExoMonad.Effect.TUI

-- | Interpret TUI effect using FIFO-based popup spawning.
--
-- Shells out to tui-spawner binary, which handles the cross-container
-- coordination via FIFOs.
runTUIFifo :: LastMember IO effs => Logger -> Eff (TUI ': effs) a -> Eff effs a
runTUIFifo logger = interpret $ \case
  ShowUI definition -> sendM $ spawnPopupFifo logger definition

-- | Spawn popup via tui-spawner and wait for result.
spawnPopupFifo :: Logger -> PopupDefinition -> IO PopupResult
spawnPopupFifo logger definition = do
  -- Encode definition as JSON for stdin
  let definitionJson = LBS.toStrict $ encode definition

  -- Log before subprocess call
  logInfo logger $ "[TUI] Spawning popup: " <> definition.pdTitle
  logDebug logger $ "[TUI] Definition JSON length: " <> T.pack (show $ LBS.length $ encode definition) <> " bytes"

  -- Call tui-spawner with definition via --definition flag
  -- Output is PopupResult JSON on stdout
  (exitCode, stdout, stderr) <- readProcessWithExitCode
    "tui-spawner"
    ["--definition", T.unpack $ TE.decodeUtf8 definitionJson]
    ""

  -- Log subprocess result
  logInfo logger $ "[TUI] tui-spawner exit code: " <> T.pack (show exitCode)
  unless (null stderr) $
    logDebug logger $ "[TUI] tui-spawner stderr: " <> T.pack (take 500 stderr)

  case exitCode of
    ExitFailure code -> do
      -- tui-spawner failed
      logError logger $ "[TUI] ERROR: tui-spawner failed with code " <> T.pack (show code)
      logError logger $ "[TUI] stderr: " <> T.pack (take 1000 stderr)
      pure $ PopupResult "error" $ object
        [ "message" .= T.pack ("tui-spawner failed with exit code " <> show code)
        , "stderr" .= T.pack stderr
        ]
    ExitSuccess -> do
      -- Parse stdout as PopupResult JSON
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 $ T.pack stdout) of
        Left err -> do
          logError logger $ "[TUI] ERROR: Failed to parse result JSON: " <> T.pack err
          logError logger $ "[TUI] stdout was: " <> T.pack (take 500 stdout)
          pure $ PopupResult "error" $ object
            [ "message" .= T.pack ("Failed to parse popup result: " <> err)
            , "stdout" .= T.pack stdout
            ]
        Right result -> do
          logInfo logger $ "[TUI] Success: button=" <> result.prButton
          pure result
