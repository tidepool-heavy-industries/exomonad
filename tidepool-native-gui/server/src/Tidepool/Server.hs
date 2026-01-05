-- | Native tidepool server - Servant + WebSocket + static files.
--
-- Ties together all executors and exposes WebSocket endpoint.
-- Uses Servant's type-level API for REST endpoints and static file serving.
--
-- = Architecture
--
-- The server:
-- 1. Provides REST endpoints for health check and session management
-- 2. Accepts WebSocket connections (via wai-websockets overlay)
-- 3. Creates a session per WebSocket connection
-- 4. Bridges WebSocket messages to the UI effect callback
-- 5. Runs the agent with full effect composition (UI, LLM, Observability)
-- 6. Serves static files for non-API HTTP requests
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
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically, writeTVar, readTVarIO)
import Control.Exception (catch, finally, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, eitherDecode)
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, setPort, setHost, runSettings)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Data.Tagged (Tagged(..))
import Servant
import Servant.Server.StaticFiles (serveDirectoryWith)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (ssIndices, unsafeToPiece)
import System.Directory (doesDirectoryExist)

import Tidepool.Wire.Types (UIState(..), UserAction(..))
import Tidepool.UI.Executor (UIContext, newUIContext, UICallback)
import Tidepool.Server.EffectRunner
  ( ExecutorConfig(..)
  , ExecutorEnv
  , defaultExecutorConfig
  , mkExecutorEnv
  , runEffects
  )
import Tidepool.Server.Session
  ( SessionMap
  , Session(..)
  , SessionInfo(..)
  , newSessionMap
  , createSession
  , deleteSession
  , listSessions
  , getSession
  )
import Tidepool.Server.API
  ( TidepoolAPI
  , HealthStatus(..)
  , api
  )
import Tidepool.Server.SimpleAgent (simpleAgent)


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
-- served by Servant (REST endpoints + static files).
runServer :: ServerConfig -> IO ()
runServer config = do
  putStrLn $ "[Server] Starting on " <> T.unpack (scHost config) <> ":" <> show (scPort config)
  putStrLn "[Server] REST endpoints: /health, /sessions"
  putStrLn "[Server] WebSocket endpoint: ws://localhost:<port>/"

  -- Create shared state
  sessions <- newSessionMap
  env <- mkExecutorEnv (scExecutorConfig config)

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

  -- Create the Servant application with REST endpoints + static files
  let servantApp = makeServantApp config sessions

  -- Layer WebSocket on top via WAI
  runSettings settings $
    websocketsOr WS.defaultConnectionOptions (wsApp sessions env) servantApp


-- ════════════════════════════════════════════════════════════════════════════
-- SERVANT APPLICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Combined API type: REST endpoints + static files.
type FullAPI = TidepoolAPI :<|> Raw

-- | Create Servant application with REST handlers and static files.
makeServantApp :: ServerConfig -> SessionMap -> Application
makeServantApp config sessions =
  serve (Proxy :: Proxy FullAPI) $
    restHandlers sessions :<|> staticHandler
  where
    staticHandler = case scMode config of
      StaticFiles dir -> serveDirectoryWith settings
        where
          settings = (defaultWebAppSettings dir)
            { ssIndices = [unsafeToPiece "index.html"] }
      DevProxy _port  -> devProxyHandler

-- | REST API handlers.
restHandlers :: SessionMap -> Server TidepoolAPI
restHandlers sessions =
  healthHandler :<|> (listSessionsHandler sessions :<|> getSessionHandler sessions)

-- | Health check handler.
healthHandler :: Handler HealthStatus
healthHandler = pure HealthStatus
  { hsStatus = "ok"
  , hsVersion = "0.1.0"
  }

-- | List all sessions.
listSessionsHandler :: SessionMap -> Handler [SessionInfo]
listSessionsHandler sessions = liftIO $ listSessions sessions

-- | Get a specific session.
getSessionHandler :: SessionMap -> UUID -> Handler (Maybe SessionInfo)
getSessionHandler sessions sessionId = liftIO $ do
  mSession <- getSession sessions sessionId
  case mSession of
    Nothing -> pure Nothing
    Just s -> do
      node <- readTVarIO (sGraphNode s)
      pure $ Just SessionInfo
        { siId = sId s
        , siCreatedAt = sCreatedAt s
        , siGraphNode = node
        }

-- | Placeholder handler for dev proxy mode.
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
wsApp :: SessionMap -> ExecutorEnv -> WS.ServerApp
wsApp sessions env pending = do
  conn <- WS.acceptRequest pending
  putStrLn "[WebSocket] Connection accepted"

  -- Create session for this connection
  session <- createSession sessions
  putStrLn $ "[WebSocket] Created session: " <> show (sId session)

  -- Fork ping thread to keep connection alive
  WS.withPingThread conn 30 (pure ()) $
    (handleConnection sessions env session conn
      `catch` (\(e :: SomeException) ->
        putStrLn $ "[WebSocket] Connection error: " <> show e))
      `finally` do
        deleteSession sessions (sId session)
        putStrLn $ "[WebSocket] Session cleaned up: " <> show (sId session)

-- | Handle a single WebSocket connection.
--
-- Creates a UI callback that bridges WebSocket to the effect system,
-- then runs the agent with full effect composition.
handleConnection :: SessionMap -> ExecutorEnv -> Session -> WS.Connection -> IO ()
handleConnection _sessions env session conn = do
  -- Create UI context for this session
  ctx <- newUIContext "entry"

  -- Create MVar for bridging async message reception to sync callback
  responseMVar <- newEmptyMVar :: IO (MVar UserAction)

  -- Create the UI callback that bridges to WebSocket
  let callback :: UICallback
      callback uiState = do
        -- Update session's current graph node
        atomically $ writeTVar (sGraphNode session) (usGraphNode uiState)
        atomically $ writeTVar (sStateVar session) (Just uiState)

        -- Send UIState over WebSocket
        putStrLn $ "[WebSocket] Sending UIState for node: " <> T.unpack (usGraphNode uiState)
        WS.sendTextData conn (encode uiState)

        -- Wait for user response
        msg <- WS.receiveData conn
        case eitherDecode msg of
          Left err -> do
            putStrLn $ "[WebSocket] Failed to decode UserAction: " <> err
            -- Return a default action to avoid blocking forever
            pure $ TextAction ""
          Right action -> do
            putStrLn $ "[WebSocket] Received: " <> show action
            atomically $ writeTVar (sActionVar session) (Just action)
            pure action

  -- Run simple agent with full effect stack (UI, LLM, Observability)
  runEffects env ctx callback simpleAgent
