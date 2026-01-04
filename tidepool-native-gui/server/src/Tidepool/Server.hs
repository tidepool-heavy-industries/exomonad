-- | Native tidepool server - Servant + WebSocket + static files.
--
-- Ties together all executors and exposes WebSocket endpoint.
-- Uses Servant's type-level API for static file serving.
--
-- = Architecture
--
-- The server:
-- 1. Accepts WebSocket connections (via wai-websockets)
-- 2. Creates a session per connection
-- 3. Bridges WebSocket messages to the UI effect callback
-- 4. Runs the agent graph with all executors composed
-- 5. Serves static files for non-WebSocket HTTP requests
--
-- = Usage
--
-- @
-- import Tidepool.Server (runServer, ServerConfig(..), ServerMode(..))
--
-- main :: IO ()
-- main = runServer ServerConfig
--   { scPort = 8080
--   , scHost = "0.0.0.0"
--   , scExecutorConfig = defaultExecutorConfig
--   , scMode = StaticFiles "dist"
--   }
-- @
module Tidepool.Server
  ( -- * Server
    runServer
  , ServerConfig(..)
  , ServerMode(..)

    -- * Executor Configuration
  , ExecutorConfig(..)
  , defaultExecutorConfig

    -- * WebSocket Bridge
  , wsApp
  , handleConnection
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (catch, SomeException)
import Control.Monad (forever)
import Data.Aeson (encode, eitherDecode)
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, setPort, setHost, runSettings)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Data.Tagged (Tagged(..))
import Servant (Raw, serve)
import Servant.Server (Handler)
import Servant.Server.StaticFiles (serveDirectoryWebApp)
import System.Directory (doesDirectoryExist)

import Tidepool.Wire.Types
  ( UIState(..)
  , UserAction(..)
  , ChatMessage(..)
  , MessageRole(..)
  , TextInputConfig(..)
  )
import Tidepool.UI.Executor (UIContext, newUIContext, UICallback)
import Tidepool.Server.EffectRunner (ExecutorConfig(..), defaultExecutorConfig)


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Server mode - static files or dev proxy.
data ServerMode
  = StaticFiles FilePath   -- ^ Serve static files from directory (production)
  | DevProxy Int           -- ^ Proxy to Vite dev server on given port (development)
  deriving (Show, Eq)

-- | Server configuration.
data ServerConfig = ServerConfig
  { scPort :: Int
    -- ^ Port to listen on (default: 8080)
  , scHost :: Text
    -- ^ Host to bind to (default: "0.0.0.0")
  , scExecutorConfig :: ExecutorConfig
    -- ^ Executor configuration (LLM keys, Habitica creds, etc.)
  , scMode :: ServerMode
    -- ^ Static file serving mode
  }


-- ════════════════════════════════════════════════════════════════════════════
-- SERVER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the server.
--
-- This starts a Warp server with WebSocket support. Each WebSocket connection
-- gets its own session with a fresh UI context. Non-WebSocket requests are
-- served by Servant (static files in production, placeholder in dev mode).
runServer :: ServerConfig -> IO ()
runServer config = do
  putStrLn $ "[Server] Starting on " <> T.unpack (scHost config) <> ":" <> show (scPort config)
  putStrLn "[Server] WebSocket endpoint: ws://localhost:<port>/"

  case scMode config of
    StaticFiles dir -> do
      exists <- doesDirectoryExist dir
      if exists
        then putStrLn $ "[Server] Serving static files from: " <> dir
        else putStrLn $ "[Server] Warning: static directory does not exist: " <> dir <> " (will return 404)"
    DevProxy port ->
      putStrLn $ "[Server] Dev proxy mode - frontend expected at localhost:" <> show port

  putStrLn "[Server] Press Ctrl+C to stop"

  let settings = setPort (scPort config)
               $ setHost (fromString $ T.unpack $ scHost config)
               $ defaultSettings

  servantApp <- makeServantApp config
  runSettings settings $ websocketsOr WS.defaultConnectionOptions (wsApp config) servantApp

-- | Create Servant application for static file serving.
makeServantApp :: ServerConfig -> IO Application
makeServantApp config = pure $ case scMode config of
  StaticFiles dir -> serve (Proxy :: Proxy Raw) (serveDirectoryWebApp dir)
  DevProxy _port -> serve (Proxy :: Proxy Raw) devProxyHandler

-- | Placeholder handler for dev proxy mode.
--
-- In production, static files are served directly. In dev mode,
-- the frontend runs on a separate Vite server and connects directly.
devProxyHandler :: Tagged Handler Application
devProxyHandler = Tagged $ \_req respond -> respond $ responseLBS
  status200
  [("Content-Type", "text/plain")]
  "Dev proxy mode - run Vite dev server on port 3000 and connect directly."


-- ════════════════════════════════════════════════════════════════════════════
-- WEBSOCKET HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | WebSocket application handler.
--
-- Accepts incoming connections and spawns a handler for each.
wsApp :: ServerConfig -> WS.ServerApp
wsApp config pending = do
  conn <- WS.acceptRequest pending
  putStrLn "[WebSocket] Connection accepted"

  -- Fork ping thread to keep connection alive
  WS.withPingThread conn 30 (pure ()) $ do
    handleConnection config conn
      `catch` \(e :: SomeException) ->
        putStrLn $ "[WebSocket] Connection closed: " <> show e

-- | Handle a single WebSocket connection.
--
-- Creates a session with:
-- 1. UI context for accumulating state
-- 2. MVar bridge for request/response
-- 3. Reader thread for incoming messages
handleConnection :: ServerConfig -> WS.Connection -> IO ()
handleConnection config conn = do
  -- Create UI context for this session
  ctx <- newUIContext "entry"

  -- Create MVar for bridging async message reception to sync callback
  responseMVar <- newEmptyMVar :: IO (MVar UserAction)

  -- Fork a thread to read incoming messages and put them in the MVar
  _readerThread <- forkIO $ forever $ do
    msg <- WS.receiveData conn
    case eitherDecode msg of
      Left err -> do
        putStrLn $ "[WebSocket] Failed to decode UserAction: " <> err
        -- Don't put anything in MVar - let callback timeout or retry
      Right action -> do
        putStrLn $ "[WebSocket] Received: " <> show action
        putMVar responseMVar action

  -- Create the UI callback that bridges to WebSocket
  let callback :: UICallback
      callback uiState = do
        -- Send UIState over WebSocket
        putStrLn $ "[WebSocket] Sending UIState for node: " <> T.unpack (usGraphNode uiState)
        WS.sendTextData conn (encode uiState)

        -- Wait for user response
        action <- takeMVar responseMVar
        pure action

  -- Send initial greeting state
  let initialState = UIState
        { usMessages = [ChatMessage
            { cmRole = Assistant
            , cmContent = "Welcome! How can I help you today?"
            , cmTimestamp = ""
            }]
        , usTextInput = Just $ TextInputConfig { ticPlaceholder = "Type your message..." }
        , usPhotoUpload = Nothing
        , usButtons = Nothing
        , usGraphNode = "entry"
        , usThinking = False
        }
  WS.sendTextData conn (encode initialState)

  -- Main interaction loop
  -- In a real implementation, this would run the agent graph
  -- For now, we echo back user messages
  mainLoop ctx callback
  where
    mainLoop :: UIContext -> UICallback -> IO ()
    mainLoop ctx callback = do
      -- Wait for user action
      msg <- WS.receiveData conn
      case eitherDecode msg of
        Left err -> do
          putStrLn $ "[WebSocket] Decode error: " <> err
          mainLoop ctx callback

        Right (TextAction content) -> do
          putStrLn $ "[WebSocket] User said: " <> T.unpack content

          -- Echo back with a response
          let responseState = UIState
                { usMessages = [ChatMessage
                    { cmRole = User
                    , cmContent = content
                    , cmTimestamp = ""
                    }
                  , ChatMessage
                    { cmRole = Assistant
                    , cmContent = "I received your message: " <> content
                    , cmTimestamp = ""
                    }]
                , usTextInput = Just $ TextInputConfig { ticPlaceholder = "Type your message..." }
                , usPhotoUpload = Nothing
                , usButtons = Nothing
                , usGraphNode = "echo"
                , usThinking = False
                }
          WS.sendTextData conn (encode responseState)
          mainLoop ctx callback

        Right (ButtonAction buttonId) -> do
          putStrLn $ "[WebSocket] Button pressed: " <> T.unpack buttonId
          mainLoop ctx callback

        Right (PhotoAction _ _) -> do
          putStrLn "[WebSocket] Photo received"
          mainLoop ctx callback
