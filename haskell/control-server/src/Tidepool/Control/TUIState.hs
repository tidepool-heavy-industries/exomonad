{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | TUI WebSocket state management for bidirectional popup communication.
--
-- This module manages WebSocket connections from tui-popup processes and
-- provides request/response correlation for MCP tools.
module Tidepool.Control.TUIState
  ( TUIState(..)
  , ConnectionContext(..)
  , PendingPopup(..)
  , ConnectionID
  , RequestID
  , ConnStatus(..)
  , newTUIState
  , registerConnection
  , unregisterConnection
  , registerPendingPopup
  , getNextPendingPopup
  , sendPopupRequest
  , waitForPopupResponse
  , dispatchPopupResponse
  , checkDisconnect
  , generateRequestID
  ) where

import Control.Concurrent.STM
import Control.Monad (void)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import Network.WebSockets (Connection, sendTextData)
import System.Timeout (timeout)
import qualified Data.Aeson as Aeson

import Tidepool.Effect.TUI (PopupDefinition, PopupResult)

-- | Connection status for disconnect detection.
data ConnStatus = Connected | Disconnected
  deriving stock (Eq, Show, Generic)

-- | Unique connection identifier (UUID).
type ConnectionID = UUID

-- | Unique request identifier for correlation (UUID).
type RequestID = UUID

-- | Context for an active WebSocket connection.
data ConnectionContext = ConnectionContext
  { connHandle :: Connection
  , connStatus :: TVar ConnStatus
  , connId :: ConnectionID
  }
  deriving stock (Generic)

-- | Pending popup request waiting for a WebSocket connection.
data PendingPopup = PendingPopup
  { ppRequestId :: RequestID
  , ppDefinition :: PopupDefinition
  , ppResponseVar :: TMVar PopupResult
  }
  deriving stock (Generic)

-- | Global TUI state managing all WebSocket connections and pending requests.
data TUIState = TUIState
  { pendingRequests :: TVar (Map RequestID (TMVar PopupResult))
  , activeConnections :: TVar (Map ConnectionID ConnectionContext)
  , pendingPopups :: TQueue PendingPopup  -- Queue of popups waiting for connections
  }
  deriving stock (Generic)

-- | Create a new TUI state with no connections.
newTUIState :: IO TUIState
newTUIState = TUIState
  <$> newTVarIO Map.empty
  <*> newTVarIO Map.empty
  <*> newTQueueIO

-- | Register a new WebSocket connection.
registerConnection :: TUIState -> Connection -> IO ConnectionID
registerConnection state conn = do
  connId <- UUID.nextRandom
  statusVar <- newTVarIO Connected
  let ctx = ConnectionContext
        { connHandle = conn
        , connStatus = statusVar
        , connId = connId
        }
  atomically $ modifyTVar (activeConnections state) $ Map.insert connId ctx
  pure connId

-- | Unregister a connection and mark it as disconnected.
unregisterConnection :: TUIState -> ConnectionID -> IO ()
unregisterConnection state cId = atomically $ do
  conns <- readTVar (activeConnections state)
  case Map.lookup cId conns of
    Just ctx -> writeTVar (connStatus ctx) Disconnected
    Nothing -> pure ()
  modifyTVar (activeConnections state) $ Map.delete cId

-- | Send a popup request via WebSocket with correlation ID.
-- Returns the request ID for later response matching.
sendPopupRequest :: Connection -> PopupDefinition -> IO RequestID
sendPopupRequest conn definition = do
  reqId <- UUID.nextRandom
  let payload = Aeson.object
        [ "type" Aeson..= ("request" :: Text)
        , "request_id" Aeson..= reqId
        , "definition" Aeson..= definition
        ]
  sendTextData conn (Aeson.encode payload)
  pure reqId

-- | Dispatch a popup response to the waiting TMVar.
dispatchPopupResponse :: TUIState -> RequestID -> PopupResult -> STM ()
dispatchPopupResponse state reqId result = do
  pending <- readTVar (pendingRequests state)
  case Map.lookup reqId pending of
    Just responseVar -> void $ tryPutTMVar responseVar result
    Nothing -> pure ()  -- Orphaned response (timeout already fired)

-- | Check if a connection is disconnected (for orElse pattern).
checkDisconnect :: TVar ConnStatus -> STM (Either Text a)
checkDisconnect statusVar = do
  s <- readTVar statusVar
  if s == Disconnected
    then pure (Left "WebSocket disconnected")
    else retry

-- | Wait for a popup response with timeout and disconnect detection.
-- Blocks until either:
-- - Response arrives (Right result)
-- - Connection disconnects (Left "disconnected")
-- - Timeout expires (Left "timeout")
waitForPopupResponse
  :: TUIState
  -> ConnectionID
  -> RequestID
  -> Int  -- Timeout in seconds
  -> IO (Either Text PopupResult)
waitForPopupResponse state connId reqId timeoutSecs = do
  -- Create response variable
  responseVar <- newEmptyTMVarIO

  -- Register pending request
  atomically $ modifyTVar (pendingRequests state) $ Map.insert reqId responseVar

  -- Get connection context for disconnect detection
  conns <- atomically $ readTVar (activeConnections state)
  case Map.lookup connId conns of
    Nothing -> pure $ Left "Connection not found"
    Just ctx -> do
      -- Block with timeout + disconnect detection (Oracle pattern)
      result <- timeout (timeoutSecs * 1000000) $ atomically $
        (Right <$> takeTMVar responseVar)
        `orElse`
        (checkDisconnect (connStatus ctx))

      -- Cleanup pending request
      atomically $ modifyTVar (pendingRequests state) $ Map.delete reqId

      case result of
        Nothing -> pure $ Left "timeout"
        Just r -> pure r

-- | Register a pending popup (spawned pane waiting for WebSocket connection).
registerPendingPopup :: TUIState -> RequestID -> PopupDefinition -> TMVar PopupResult -> IO ()
registerPendingPopup state reqId definition responseVar = do
  let pending = PendingPopup reqId definition responseVar
  atomically $ do
    writeTQueue (pendingPopups state) pending
    modifyTVar (pendingRequests state) $ Map.insert reqId responseVar

-- | Get the next pending popup (blocks if queue is empty).
getNextPendingPopup :: TUIState -> STM PendingPopup
getNextPendingPopup state = readTQueue (pendingPopups state)

-- | Generate a new request ID.
generateRequestID :: IO RequestID
generateRequestID = UUID.nextRandom
