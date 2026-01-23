{-# LANGUAGE TypeApplications #-}

-- | Unix socket control server for Claude Code++ integration.
--
-- Listens on Unix socket (default .tidepool/sockets/control.sock) and handles
-- HTTP requests via Servant.
module Tidepool.Control.Server
  ( runServer
  ) where

import Control.Concurrent.STM (atomically)
import Control.Exception (catch, bracket, finally)
import qualified Control.Exception as E
import Control.Monad (when, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, object, (.=), FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Function ((&))
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Network.Socket
import Network.Wai.Handler.Warp
import Network.WebSockets (Connection, receiveData, forkPingThread)
import Servant
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.IO.Error (isDoesNotExistError)

import Tidepool.Control.API
import Tidepool.Control.Handler (handleMessage)
import Tidepool.Control.Logging (Logger, logInfo, logDebug, logError)
import Tidepool.Control.Protocol
import Tidepool.Control.TUIState
import Tidepool.Control.Types (ServerConfig(..))
import Tidepool.Effect.TUI (PopupResult(..))
import Tidepool.Observability.Types (newTraceContext, ObservabilityConfig(..), LokiConfig(..), OTLPConfig(..))
import Tidepool.Observability.Interpreter (flushTraces)
import Tidepool.Control.Export (exportMCPTools)

-- | Load observability configuration from environment variables.
loadObservabilityConfig :: IO (Maybe ObservabilityConfig)
loadObservabilityConfig = do
  lokiUrl <- lookupEnv "LOKI_URL"
  otlpEndpoint <- lookupEnv "OTLP_ENDPOINT"
  
  case (lokiUrl, otlpEndpoint) of
    (Nothing, Nothing) -> pure Nothing
    _ -> do
      lokiUser <- lookupEnv "LOKI_USER"
      lokiToken <- lookupEnv "LOKI_TOKEN"
      otlpUser <- lookupEnv "OTLP_USER"
      otlpToken <- lookupEnv "OTLP_TOKEN"
      
      let loki = fmap (\url -> LokiConfig (T.pack url) (fmap T.pack lokiUser) (fmap T.pack lokiToken) "tidepool-control-server") lokiUrl
          otlp = fmap (\end -> OTLPConfig (T.pack end) (fmap T.pack otlpUser) (fmap T.pack otlpToken)) otlpEndpoint
          
      pure $ Just $ ObservabilityConfig loki otlp "tidepool-control-server"

-- | Run the control server. Blocks forever.
runServer :: Logger -> ServerConfig -> IO ()
runServer logger config = do
  -- Get socket path from environment
  controlSocketEnv <- lookupEnv "TIDEPOOL_CONTROL_SOCKET"
  let controlSocket = case controlSocketEnv of
        Just s -> s
        Nothing -> error "TIDEPOOL_CONTROL_SOCKET environment variable not set (should be set via start-augmented.sh or .env)"

  -- Load observability config if not already provided
  obsConfig <- case config.observabilityConfig of
    Just c  -> pure (Just c)
    Nothing -> loadObservabilityConfig

  let configWithObs = config { observabilityConfig = obsConfig }

  when (isJust obsConfig) $
    logInfo logger "Observability enabled (Loki/OTLP)"

  -- Create TUI state for WebSocket connections
  tuiState <- newTUIState

  -- Run Servant Server on Unix Socket
  bracket
    (setupUnixSocket controlSocket)
    (cleanupUnixSocket controlSocket)
    $ \sock -> do
      logInfo logger $ "Control server listening on (HTTP+WS over Unix): " <> T.pack controlSocket

      let settings = defaultSettings
            & setTimeout (5 * 60) -- 5 minutes timeout

      runSettingsSocket settings sock (app logger configWithObs tuiState)

-- | Setup Unix socket at given path.
setupUnixSocket :: FilePath -> IO Socket
setupUnixSocket path = do
  -- Ensure directory exists
  createDirectoryIfMissing True (takeDirectory path)

  -- Remove existing socket file if it exists
  cleanupSocketFile path

  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix path)
  listen sock 10
  pure sock

-- | Cleanup Unix socket at given path.
cleanupUnixSocket :: FilePath -> Socket -> IO ()
cleanupUnixSocket path sock = do
  close sock
  cleanupSocketFile path

-- | Cleanup just the socket file
cleanupSocketFile :: FilePath -> IO ()
cleanupSocketFile path = 
  catch (removeFile path) $ \e ->
    if isDoesNotExistError e
      then pure ()
      else E.throwIO e

-- | Servant application
app :: Logger -> ServerConfig -> TUIState -> Application
app logger config tuiState = serve (Proxy @TidepoolControlAPI) (server logger config tuiState)

-- | Servant server implementation
server :: Logger -> ServerConfig -> TUIState -> Server TidepoolControlAPI
server logger config tuiState =
       handleHook
  :<|> handleMcpCall
  :<|> handleMcpTools
  :<|> handleTuiWs
  :<|> handlePing
  where
    handleHook (input, runtime) = do
      res <- liftIO $ do
        logDebug logger $ "[HOOK] " <> input.hookEventName <> " runtime=" <> T.pack (show runtime)
        traceCtx <- newTraceContext
        res <- handleMessage logger config traceCtx tuiState (HookEvent input runtime)

        -- Flush traces after handling the message if OTLP is configured
        case config.observabilityConfig of
          Just obsConfig | Just otlp <- ocOTLP obsConfig ->
             flushTraces otlp obsConfig.ocServiceName traceCtx
          _ -> pure ()
        pure res

      case res of
        HookResponse out ec -> pure (out, ec)
        _ -> throwError $ err500 { errBody = "Unexpected response from handleHook" }

    handleMcpCall req = liftIO $ do
      logInfo logger $ "[MCP:" <> req.mcpId <> "] tool=" <> req.toolName
      traceCtx <- newTraceContext
      res <- handleMessage logger config traceCtx tuiState
        (McpToolCall req.mcpId req.toolName req.arguments)

      -- Flush traces
      case config.observabilityConfig of
        Just obsConfig | Just otlp <- ocOTLP obsConfig ->
           flushTraces otlp obsConfig.ocServiceName traceCtx
        _ -> pure ()

      pure res

    handleMcpTools = liftIO $ do
      logDebug logger "[MCP] tools/list request"
      exportMCPTools logger

    handleTuiWs :: Connection -> Handler ()
    handleTuiWs conn = liftIO $ do
      -- Register connection and get ID
      connId <- registerConnection tuiState conn
      logInfo logger $ "[TUI:WS] Connection registered: " <> T.pack (UUID.toString connId)

      -- CRITICAL: Fork ping thread to detect half-open connections
      forkPingThread conn 30

      -- Main WebSocket loop with cleanup
      flip finally (cleanup connId) $ do
        -- Get next pending popup from queue (blocks until available)
        pending <- atomically $ getNextPendingPopup tuiState
        logInfo logger $ "[TUI:WS] Sending popup to connection: " <> T.pack (UUID.toString (ppRequestId pending))

        -- Send PopupDefinition to this connection
        _ <- sendPopupRequest conn (ppDefinition pending)

        -- Wait for response in the main loop
        forever $ do
          -- receiveData throws ConnectionException on disconnect
          msg <- receiveData conn
          case decode msg of
            Just response -> do
              logDebug logger $ "[TUI:WS] Received response for: " <> T.pack (UUID.toString (responseId response))
              atomically $ dispatchPopupResponse tuiState (responseId response) (responseResult response)
            Nothing -> do
              logError logger $ "[TUI:WS] Invalid JSON received"

      where
        cleanup cId = do
          logInfo logger $ "[TUI:WS] Connection closed: " <> T.pack (UUID.toString cId)
          unregisterConnection tuiState cId

    handlePing :: Handler T.Text
    handlePing = pure "pong"

-- | WebSocket response message from tui-popup.
data PopupResponse = PopupResponse
  { responseId :: RequestID
  , responseResult :: PopupResult
  }
  deriving stock (Generic)

instance FromJSON PopupResponse where
  parseJSON = Aeson.withObject "PopupResponse" $ \o -> PopupResponse
    <$> o Aeson..: "request_id"
    <*> o Aeson..: "result"

instance ToJSON PopupResponse where
  toJSON r = object
    [ "request_id" .= r.responseId
    , "result" .= r.responseResult
    ]