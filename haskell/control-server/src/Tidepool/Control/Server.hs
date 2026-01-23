{-# LANGUAGE DataKinds, OverloadedRecordDot, OverloadedStrings, TypeApplications #}

-- | Unix socket control server for Claude Code++ integration.
--
-- Listens on Unix socket (default .tidepool/sockets/control.sock) and handles
-- HTTP requests via Servant.
module Tidepool.Control.Server
  ( runServer
  ) where

import Control.Exception (catch, bracket)
import qualified Control.Exception as E
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Maybe (isJust)
import qualified Data.Text as T
import Network.Socket
import Network.Wai.Handler.Warp
import Servant
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.IO.Error (isDoesNotExistError)

import Tidepool.Control.API
import Tidepool.Control.Handler (handleMessage)
import Tidepool.Control.Logging (Logger, logInfo, logDebug)
import Tidepool.Control.Protocol
import Tidepool.Control.Types (ServerConfig(..))
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

  -- Run Servant Server on Unix Socket
  bracket
    (setupUnixSocket controlSocket)
    (cleanupUnixSocket controlSocket)
    $ \sock -> do
      logInfo logger $ "Control server listening on (HTTP over Unix): " <> T.pack controlSocket

      let settings = defaultSettings
            & setTimeout (5 * 60) -- 5 minutes timeout

      runSettingsSocket settings sock (app logger configWithObs)

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
app :: Logger -> ServerConfig -> Application
app logger config = serve (Proxy @TidepoolControlAPI) (server logger config)

-- | Servant server implementation
server :: Logger -> ServerConfig -> Server TidepoolControlAPI
server logger config =
       handleHook
  :<|> handleMcpCall
  :<|> handleMcpTools
  :<|> handlePing
  where
    handleHook (input, runtime) = do
      res <- liftIO $ do
        logDebug logger $ "[HOOK] " <> input.hookEventName <> " runtime=" <> T.pack (show runtime)
        traceCtx <- newTraceContext
        res <- handleMessage logger config traceCtx (HookEvent input runtime)

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
      res <- handleMessage logger config traceCtx
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

    handlePing :: Handler T.Text
    handlePing = pure "pong"