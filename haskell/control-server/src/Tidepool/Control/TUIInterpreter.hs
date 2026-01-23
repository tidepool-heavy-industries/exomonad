{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | TUI effect interpreter using Zellij panes + WebSocket communication.
--
-- This interpreter combines Zellij for display/lifecycle with WebSocket for
-- bidirectional communication:
-- 1. Spawns Zellij floating pane with tui-popup binary
-- 2. tui-popup connects to WebSocket automatically
-- 3. Sends PopupDefinition over WebSocket
-- 4. Blocks waiting for response via correlation ID
module Tidepool.Control.TUIInterpreter
  ( runTUIWebSocket
  ) where

import Control.Concurrent.STM (atomically, newEmptyTMVarIO, readTMVar)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (object, (.=))
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Tidepool.Control.TUIState
import Tidepool.Effect.TUI

-- | Interpret TUI effect using Zellij panes + WebSocket communication.
--
-- This blocks until the popup is dismissed or times out (5 minutes).
runTUIWebSocket :: LastMember IO effs => TUIState -> Eff (TUI ': effs) a -> Eff effs a
runTUIWebSocket tuiState = interpret $ \case
  ShowUI definition -> sendM $ spawnPopupWithWebSocket tuiState definition

-- | Spawn Zellij pane and coordinate WebSocket communication.
spawnPopupWithWebSocket :: TUIState -> PopupDefinition -> IO PopupResult
spawnPopupWithWebSocket tuiState definition = do
  -- Generate request ID for correlation
  reqId <- generateRequestID

  -- Create response variable
  responseVar <- newEmptyTMVarIO

  -- Register pending request (will be matched when WebSocket connects)
  registerPendingPopup tuiState reqId definition responseVar

  -- Spawn Zellij floating pane (tui-popup will connect to WebSocket)
  let zellijArgs =
        [ "action"
        , "new-pane"
        , "--floating"
        , "--name"
        , "tidepool-popup"
        , "--"
        , "tui-popup"
        ]

  -- Spawn pane (non-blocking - tui-popup will connect separately)
  (exitCode, stdout, stderr) <- readProcessWithExitCode "zellij" zellijArgs ""

  case exitCode of
    ExitFailure code -> do
      -- Zellij spawn failed
      pure $ PopupResult "error" $ object
        [ "message" .= T.pack ("Zellij spawn failed with exit code " <> show code)
        , "stdout" .= T.pack stdout
        , "stderr" .= T.pack stderr
        ]
    ExitSuccess -> do
      -- Pane spawned successfully - now block waiting for WebSocket response
      atomically $ readTMVar responseVar
