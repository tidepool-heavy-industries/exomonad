{-# LANGUAGE TypeApplications #-}

-- | Unix socket control server for Claude Code++ integration.
--
-- Listens on Unix socket (default .tidepool/sockets/control.sock) and handles
-- NDJSON messages from mantle-agent. Each connection is one request-response pair.
--
-- The server maintains a long-lived LSP session for code intelligence.
module Tidepool.Control.Server
  ( runServer
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTVarIO, readTVarIO, atomically, writeTVar, readTVar, TVar)
import Control.Exception (SomeException, catch, finally, bracket)
import qualified Control.Exception as E
import Control.Monad (forever, void, when)
import Data.Maybe (isJust)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Socket hiding (ControlMessage)
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.IO.Error (isDoesNotExistError)

import Tidepool.Control.Handler (handleMessage)
import Tidepool.Control.Logging (Logger, logInfo, logDebug, logError)
import Tidepool.Control.Protocol
import Tidepool.Control.Types (ServerConfig(..))
import Tidepool.LSP.Interpreter (LSPSession, withLSPSession)
import Tidepool.TUI.Interpreter (TUIHandle, newTUIHandle, closeTUIHandle)
import Tidepool.Observability.Types (TraceContext, newTraceContext, ObservabilityConfig(..), LokiConfig(..), OTLPConfig(..))
import Tidepool.Observability.Interpreter (flushTraces)

-- | Default Unix socket paths
defaultControlSocket :: FilePath
defaultControlSocket = ".tidepool/sockets/control.sock"

defaultTuiSocket :: FilePath
defaultTuiSocket = ".tidepool/sockets/tui.sock"

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
--
-- 1. Starts LSP session for the project
-- 2. Accepts Unix socket connections on .tidepool/sockets/control.sock
runServer :: Logger -> ServerConfig -> IO ()
runServer logger config = do
  -- Get socket paths from environment or use default
  controlSocketEnv <- lookupEnv "TIDEPOOL_CONTROL_SOCKET"
  let controlSocket = maybe defaultControlSocket id controlSocketEnv

  tuiSocketEnv <- lookupEnv "TIDEPOOL_TUI_SOCKET"
  let tuiSocket = maybe defaultTuiSocket id tuiSocketEnv

  -- Load observability config if not already provided
  obsConfig <- case config.observabilityConfig of
    Just c  -> pure (Just c)
    Nothing -> loadObservabilityConfig
  
  let configWithObs = config { observabilityConfig = obsConfig }

  when (isJust obsConfig) $ 
    logInfo logger "Observability enabled (Loki/OTLP)"

  -- Shared State
  tuiHandleVar <- newTVarIO Nothing
  lspSessionVar <- newTVarIO Nothing

  -- 1. Setup Control Socket & Listener (EARLY)
  bracket (setupUnixSocket controlSocket) (cleanupUnixSocket controlSocket) $ \sock -> do
    logInfo logger $ "Control server listening on Unix socket: " <> T.pack controlSocket

    -- Fork connection acceptor loop
    _ <- forkIO $ forever $ do
      (conn, _peer) <- accept sock
      -- Snapshot state
      currentTuiHandle <- atomically $ readTVar tuiHandleVar
      traceCtx <- newTraceContext
      
      -- Handle connection in its own thread
      void $ forkIO $ handleConnection logger configWithObs lspSessionVar currentTuiHandle traceCtx conn `finally` close conn

    -- 2. Setup TUI Listener (if enabled)
    let withTui action = 
          if configWithObs.noTui
          then do
            logInfo logger "TUI sidebar listener disabled (--no-tui)"
            action
          else bracket (setupUnixSocket tuiSocket) (cleanupUnixSocket tuiSocket) $ \tuiSock -> do
            logInfo logger $ "TUI sidebar listener waiting on: " <> T.pack tuiSocket
            
            -- Fork TUI acceptor loop
            _ <- forkIO $ forever $ do
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
                 
                 -- Note: we don't handle cleanup of handle here, it's done when runServer exits
                 -- or when replaced.
            action

    -- 3. Run TUI setup and then LSP
    withTui $ do
      -- Cleanup TUI handle on exit
      let cleanupTui = do
            maybeH <- readTVarIO tuiHandleVar
            case maybeH of 
              Just h -> closeTUIHandle h
              Nothing -> pure ()

      flip finally cleanupTui $ do
         logInfo logger $ "Starting LSP session for project: " <> T.pack configWithObs.projectDir
         
         withLSPSession configWithObs.projectDir $ \lspSession -> do
             logInfo logger "LSP session initialized"
             
             -- Publish session
             atomically $ writeTVar lspSessionVar (Just lspSession)
             
             -- Wait forever (main thread blocks here, keeping LSP session alive)
             forever $ threadDelay 10000000

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


-- | Handle a single connection (one NDJSON request-response).
handleConnection :: Logger -> ServerConfig -> TVar (Maybe LSPSession) -> Maybe TUIHandle -> TraceContext -> Socket -> IO ()
handleConnection logger config lspSessionVar maybeTuiHandle traceCtx conn = do
  logDebug logger "Connection received"

  (do
    -- Read until newline (NDJSON framing)
    msgBytes <- readUntilNewline conn

    case eitherDecodeStrict msgBytes of
      Left err -> do
        logError logger $ "[Server] JSON parse error: " <> T.pack err
        logDebug logger $ "[Server] Raw input (" <> T.pack (show (BS.length msgBytes)) <> " bytes): "
          <> T.decodeUtf8 (BS.take 200 msgBytes)  -- First 200 bytes for debugging
        -- Use Gemini exit code (2) for parse errors since we can't determine runtime
        -- from malformed JSON. Exit code 2 should be treated as error by both runtimes.
        let response = hookError Gemini $ T.pack $ "JSON parse error: " <> err
        sendResponse conn response

      Right msg -> do
        logMessage logger msg
        
        -- Get current LSP session
        maybeLsp <- atomically $ readTVar lspSessionVar
        
        response <- handleMessage logger config maybeLsp maybeTuiHandle traceCtx msg
        logResponse logger response
        sendResponse conn response
        
        -- Flush traces after handling the message if OTLP is configured
        case config.observabilityConfig of
          Just obsConfig | Just otlp <- ocOTLP obsConfig -> 
             flushTraces otlp obsConfig.ocServiceName traceCtx
          _ -> pure ()
    )
  `catch` \(e :: SomeException) -> do
    logError logger $ "Connection error: " <> T.pack (show e)
    -- Try to send error response, but ignore failures (connection may be closed)
    void $ E.try @SomeException $ 
      sendResponse conn $ hookError Gemini $ "Connection error: " <> T.pack (show e)

-- | Read bytes from socket until newline.
readUntilNewline :: Socket -> IO ByteString
readUntilNewline sock = go mempty
  where
    go acc = do
      chunk <- recv sock 4096
      if BS.null chunk
        then pure acc  -- Connection closed
        else do
          let combined = acc <> chunk
          case BS8.elemIndex '\n' combined of
            Just idx -> pure $ BS.take idx combined  -- Return up to newline
            Nothing -> go combined  -- Keep reading

-- | Send NDJSON response (JSON + newline).
sendResponse :: Socket -> ControlResponse -> IO ()
sendResponse conn response = do
  let bytes = LBS.toStrict (encode response) <> "\n"
  sendAll conn bytes

-- | Log incoming message.
logMessage :: Logger -> ControlMessage -> IO ()
logMessage logger = \case
  HookEvent input r ->
    logDebug logger $ "[HOOK] " <> input.hookEventName
      <> " runtime=" <> T.pack (show r)
      <> maybe "" (" tool=" <>) input.toolName
  McpToolCall reqId name _args ->
    logInfo logger $ "[MCP:" <> reqId <> "] tool=" <> name
  ToolsListRequest ->
    logDebug logger "[MCP] tools/list request"
  Ping ->
    pure ()  -- Skip logging health check pings

-- | Log outgoing response.
logResponse :: Logger -> ControlResponse -> IO ()
logResponse logger = \case
  HookResponse output exitCode -> do
    let decision = case output.hookSpecificOutput of
          Just (PreToolUseOutput d _ _) -> " decision=" <> d
          _ -> ""
    logDebug logger $ "[HOOK] -> continue=" <> T.pack (show output.continue_)
      <> decision <> " exit=" <> T.pack (show exitCode)
  McpToolResponse _ result err -> do
    let status = case (result, err) of
          (Just _, _) -> "success"
          (_, Just e) -> "error: " <> e.errorMessage
          _ -> "empty"
    logDebug logger $ "[MCP] -> " <> status
  ToolsListResponse tools ->
    logDebug logger $ "[MCP] -> tools/list: " <> T.pack (show (length tools)) <> " tools"
  Pong ->
    pure ()  -- Skip logging health check pongs