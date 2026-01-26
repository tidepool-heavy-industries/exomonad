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
module Tidepool.Control.TUIInterpreter
  ( runTUIFifo
  ) where

import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (eitherDecode, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Tidepool.Effect.TUI

-- | Interpret TUI effect using FIFO-based popup spawning.
--
-- Shells out to tui-spawner binary, which handles the cross-container
-- coordination via FIFOs.
runTUIFifo :: LastMember IO effs => Eff (TUI ': effs) a -> Eff effs a
runTUIFifo = interpret $ \case
  ShowUI definition -> sendM $ spawnPopupFifo definition

-- | Spawn popup via tui-spawner and wait for result.
spawnPopupFifo :: PopupDefinition -> IO PopupResult
spawnPopupFifo definition = do
  -- Encode definition as JSON for stdin
  let definitionJson = LBS.toStrict $ encode definition

  -- Call tui-spawner with definition via --definition flag
  -- Output is PopupResult JSON on stdout
  (exitCode, stdout, stderr) <- readProcessWithExitCode
    "tui-spawner"
    ["--definition", T.unpack $ TE.decodeUtf8 definitionJson]
    ""

  case exitCode of
    ExitFailure code -> do
      -- tui-spawner failed
      pure $ PopupResult "error" $ object
        [ "message" .= T.pack ("tui-spawner failed with exit code " <> show code)
        , "stderr" .= T.pack stderr
        ]
    ExitSuccess -> do
      -- Parse stdout as PopupResult JSON
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 $ T.pack stdout) of
        Left err -> pure $ PopupResult "error" $ object
          [ "message" .= T.pack ("Failed to parse popup result: " <> err)
          , "stdout" .= T.pack stdout
          ]
        Right result -> pure result
