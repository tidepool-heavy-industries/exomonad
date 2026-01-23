{-# LANGUAGE DataKinds, OverloadedRecordDot, OverloadedStrings, TypeApplications #-}

-- | Unix socket control server for Claude Code++ integration.
--
-- Listens on Unix socket (default .tidepool/sockets/control.sock) and handles
-- HTTP requests via Servant.
module Tidepool.Control.Server
  ( runServer
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTVarIO, readTVarIO, atomically, writeTVar, readTVar, TVar, writeTChan, readTChan)
import Control.Exception (catch, bracket)
import qualified Control.Exception as E
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Aeson (toJSON)
import qualified Data.Text as T
import Network.Socket
import Network.Wai.Handler.Warp
import Servant
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.IO.Error (isDoesNotExistError)
import System.Timeout (timeout)

import Tidepool.Control.API
import Tidepool.Control.Handler (handleMessage)
import Tidepool.Control.Logging (Logger, logInfo, logDebug, logError)
import Tidepool.Control.Protocol
import Tidepool.Control.Types (ServerConfig(..))
import Tidepool.TUI.Interpreter (TUIHandle(..), newTUIHandle, closeTUIHandle)
import Tidepool.Observability.Types (newTraceContext, ObservabilityConfig(..), LokiConfig(..), OTLPConfig(..))
import Tidepool.Observability.Interpreter (flushTraces)
import Tidepool.Control.Export (exportMCPTools)
import Tidepool.Effect.TUI (PopupResult(..))

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
  controlSocketEnv <- lookupEnv "TIDEPOOL_CONTROL_SOCKET"
  let controlSocket = case controlSocketEnv of
        Just s -> s
        Nothing -> error "TIDEPOOL_CONTROL_SOCKET environment variable not set"

  tuiSocketEnv <- lookupEnv "TIDEPOOL_TUI_SOCKET"
  let tuiSocket = case tuiSocketEnv of
        Just s -> s
        Nothing -> error "TIDEPOOL_TUI_SOCKET environment variable not set"

  -- Load observability config if not already provided
  obsConfig <- case config.observabilityConfig of
    Just c  -> pure (Just c)
    Nothing -> loadObservabilityConfig
  
  let configWithObs = config { observabilityConfig = obsConfig }

  when (isJust obsConfig) $ 
    logInfo logger "Observability enabled (Loki/OTLP)"

  -- Shared State
  tuiHandleVar <- newTVarIO Nothing

  -- 1. Setup TUI Listener (if enabled)
  _ <- forkIO $ when (not configWithObs.noTui) $ do
    bracket (setupUnixSocket tuiSocket) (cleanupUnixSocket tuiSocket) $ \tuiSock -> do
      logInfo logger $ "TUI sidebar listener waiting on: " <> T.pack tuiSocket
      
      -- Fork TUI acceptor loop
      forever $ do
           (conn, _) <- accept tuiSock
           logInfo logger "TUI sidebar connected"
           handle <- newTUIHandle "control-server" conn
           
           -- Update handle var
           maybeOld <- atomically $ do
             old <- readTVar tuiHandleVar
             writeTVar tuiHandleVar (Just handle)
             pure old
           
           -- Close old handle if exists
           case maybeOld of 
             Just h -> do
               logInfo logger "Closing previous TUI sidebar connection"
               closeTUIHandle h
             Nothing -> logDebug logger "No existing TUI sidebar connection"

  -- 2. Run Servant Server on Unix Socket
  bracket (setupUnixSocket controlSocket) (cleanupUnixSocket controlSocket) $ \sock -> do
    logInfo logger $ "Control server listening on (HTTP over Unix): " <> T.pack controlSocket
    
    let settings = defaultSettings
          & setTimeout (5 * 60) -- 5 minutes timeout
    
    runSettingsSocket settings sock (app logger configWithObs tuiHandleVar)

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
app :: Logger -> ServerConfig -> TVar (Maybe TUIHandle) -> Application
app logger config tuiHandleVar = serve (Proxy @TidepoolControlAPI) (server logger config tuiHandleVar)

-- | Servant server implementation
server :: Logger -> ServerConfig -> TVar (Maybe TUIHandle) -> Server TidepoolControlAPI
server logger config tuiHandleVar = 
       handleHook
  :<|> handleMcpCall
  :<|> handleMcpTools
  :<|> handleTuiSpawn
  :<|> handlePing
  where
    handleHook (input, runtime) = liftIO $ do
      logDebug logger $ "[HOOK] " <> input.hookEventName <> " runtime=" <> T.pack (show runtime)
      traceCtx <- newTraceContext
      tuiHandle <- readTVarIO tuiHandleVar
      res <- handleMessage logger config tuiHandle traceCtx (HookEvent input runtime)
      
      -- Flush traces after handling the message if OTLP is configured
      case config.observabilityConfig of
        Just obsConfig | Just otlp <- ocOTLP obsConfig -> 
           flushTraces otlp obsConfig.ocServiceName traceCtx
        _ -> pure ()
        
      case res of
        HookResponse out ec -> pure (out, ec)
        _ -> error "Unexpected response from handleHook"

    handleMcpCall req = liftIO $ do
      logInfo logger $ "[MCP:" <> req.mcpId <> "] tool=" <> req.toolName
      traceCtx <- newTraceContext
      tuiHandle <- readTVarIO tuiHandleVar
      res <- handleMessage logger config tuiHandle traceCtx 
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

    handleTuiSpawn definition = liftIO $ do
      logInfo logger "[TUI] spawn request"
      tuiHandle <- readTVarIO tuiHandleVar
      case tuiHandle of
        Just (TUIHandle _ sendChan recvChan) -> do
          atomically $ writeTChan sendChan definition
          -- Timeout handled by Warp/Servant (5 mins) but we can also use TUI timeout
          res <- timeout (300 * 1000000) $ atomically $ readTChan recvChan
          case res of
            Just r -> pure r
            Nothing -> do
              logError logger "[TUI] timeout after 300s"
              pure $ PopupResult "timeout" (toJSON (mempty :: [(String, String)]))
        Nothing -> do
          logError logger "[TUI] no sidebar connected"
          pure $ PopupResult "decline" (toJSON (mempty :: [(String, String)]))

    handlePing = pure "pong"
