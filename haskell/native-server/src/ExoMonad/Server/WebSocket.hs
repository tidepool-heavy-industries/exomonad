-- | WebSocket handling for exomonad server.
--
-- Handles WebSocket connections and message routing per PROTOCOL.md.
module ExoMonad.Server.WebSocket
  ( -- * WebSocket Application
    websocketApp
  , SessionHandler
    -- * Testing Support
  , withTestServer
  , TestServerConfig(..)
  , defaultTestConfig
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, void)
import Data.Aeson (encode, eitherDecode)
import qualified Network.WebSockets as WS
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run, Port)
import Network.HTTP.Types (status400)
import qualified Network.Wai.Handler.WebSockets as WaiWS

import ExoMonad.Wire.Types (UIState(..), UserAction(..), ChatMessage(..), MessageRole(..))

-- | Handler for a WebSocket session.
-- Receives actions via TVar, sends states via TVar.
type SessionHandler = TVar (Maybe UserAction) -> TVar (Maybe UIState) -> IO ()

-- | Create a WebSocket application with the given session handler.
websocketApp :: SessionHandler -> WS.ServerApp
websocketApp handler pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    -- Create channels for communication
    actionVar <- newTVarIO Nothing
    stateVar <- newTVarIO Nothing

    -- Fork handler thread
    handlerThread <- async $ handler actionVar stateVar

    -- Communication loop
    let sendLoop = forever $ do
          state <- atomically $ do
            s <- readTVar stateVar
            case s of
              Nothing -> retry
              Just st -> do
                writeTVar stateVar Nothing
                pure st
          WS.sendTextData conn (encode state)

        recvLoop = forever $ do
          msg <- WS.receiveData conn
          case eitherDecode msg of
            Left _err -> pure () -- Ignore malformed messages for now
            Right action -> atomically $ writeTVar actionVar (Just action)

    -- Run loops until connection closes
    sendThread <- async sendLoop
    recvThread <- async recvLoop

    -- Wait for handler or connection to finish
    void $ wait handlerThread `finally` do
      cancel sendThread
      cancel recvThread

-- | Echo handler for testing - echoes back text actions as assistant messages.
echoHandler :: TVar (Maybe UserAction) -> TVar (Maybe UIState) -> IO ()
echoHandler actionVar stateVar = do
  -- Send initial greeting
  let greeting = UIState
        { usMessages = [ChatMessage Assistant "Hello! I'm an echo server." "2024-01-01T00:00:00Z"]
        , usTextInput = Nothing
        , usPhotoUpload = Nothing
        , usButtons = Nothing
        , usGraphNode = "greeting"
        , usThinking = False
        }
  atomically $ writeTVar stateVar (Just greeting)

  -- Echo loop
  forever $ do
    action <- atomically $ do
      a <- readTVar actionVar
      case a of
        Nothing -> retry
        Just act -> do
          writeTVar actionVar Nothing
          pure act
    let response = case action of
          TextAction content -> UIState
            { usMessages = [ChatMessage User content "now", ChatMessage Assistant ("Echo: " <> content) "now"]
            , usTextInput = Nothing
            , usPhotoUpload = Nothing
            , usButtons = Nothing
            , usGraphNode = "echo"
            , usThinking = False
            }
          ButtonAction btnId -> UIState
            { usMessages = [ChatMessage Assistant ("Button pressed: " <> btnId) "now"]
            , usTextInput = Nothing
            , usPhotoUpload = Nothing
            , usButtons = Nothing
            , usGraphNode = "button"
            , usThinking = False
            }
          PhotoAction _ mimeType -> UIState
            { usMessages = [ChatMessage Assistant ("Photo received: " <> mimeType) "now"]
            , usTextInput = Nothing
            , usPhotoUpload = Nothing
            , usButtons = Nothing
            , usGraphNode = "photo"
            , usThinking = False
            }
    atomically $ writeTVar stateVar (Just response)

-- | Test server configuration.
data TestServerConfig = TestServerConfig
  { tscPort :: Port
  , tscStaticDir :: Maybe FilePath
  }

defaultTestConfig :: TestServerConfig
defaultTestConfig = TestServerConfig
  { tscPort = 8899
  , tscStaticDir = Nothing
  }

-- | Run a test server with echo handler and execute an action.
withTestServer :: TestServerConfig -> (Port -> IO a) -> IO a
withTestServer config action = do
  let wsApp = websocketApp echoHandler
      waiApp = WaiWS.websocketsOr WS.defaultConnectionOptions wsApp fallbackApp

  -- Start server in background
  serverThread <- async $ run (tscPort config) waiApp

  -- Give server time to start
  threadDelay 100000 -- 100ms

  -- Run action and cleanup
  result <- action (tscPort config) `finally` cancel serverThread
  pure result

-- | Fallback application for non-WebSocket requests
fallbackApp :: Application
fallbackApp _ respond = respond $ responseLBS status400 [] "WebSocket required"
