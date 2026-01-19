-- | TCP control server for Claude Code++ integration.
--
-- Listens on TCP port 7432 (configurable via MANTLE_CONTROL_PORT) and handles
-- NDJSON messages from mantle-agent. Each connection is one request-response pair.
--
-- The server maintains a long-lived LSP session for code intelligence.
module Tidepool.Control.Server
  ( runServer
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch, finally, bracket, try)
import Control.Monad (forever, void)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Socket hiding (ControlMessage)
import Network.Socket.ByteString (recv, sendAll)
import System.Environment (lookupEnv)

import Tidepool.Control.Handler (handleMessage)
import Tidepool.Control.Logging (Logger, logInfo, logDebug, logError)
import Tidepool.Control.Protocol
import Tidepool.Control.Types (ServerConfig(..))
import Tidepool.LSP.Interpreter (LSPSession, withLSPSession)
import Tidepool.TUI.Interpreter (TUIHandle, connectTUI, newTUIHandle, closeTUIHandle)

-- | Default TCP port for mantle-agent connections.
defaultTcpPort :: Int
defaultTcpPort = 7432

-- | Default TCP port for TUI sidebar connections.
defaultTuiPort :: Int
defaultTuiPort = 7433

-- | Run the control server. Blocks forever.
--
-- 1. Starts LSP session for the project
-- 2. Accepts TCP connections on port 7432 (or MANTLE_CONTROL_PORT)
runServer :: Logger -> ServerConfig -> IO ()
runServer logger config = do
  -- Get TCP port from environment or use default
  tcpPortEnv <- lookupEnv "MANTLE_CONTROL_PORT"
  let tcpPort = maybe defaultTcpPort read tcpPortEnv

  -- Get TUI port from environment or use default
  tuiPortEnv <- lookupEnv "TIDEPOOL_TUI_PORT"
  let tuiPort = maybe (show defaultTuiPort) id tuiPortEnv

  logInfo logger $ "Starting LSP session for project: " <> T.pack config.projectDir

  -- Start LSP session and run server
  withLSPSession config.projectDir $ \lspSession -> do
    logInfo logger "LSP session initialized"

    -- Try to connect to TUI sidebar (optional)
    maybeTuiHandle <- try (connectTUI "localhost" tuiPort) >>= \case
      Left (e :: SomeException) -> do
        logInfo logger $ "TUI sidebar not found on port " <> T.pack tuiPort
          <> ", running without TUI support (" <> T.pack (show e) <> ")"
        pure Nothing
      Right tuiSock -> do
        logInfo logger $ "Connected to TUI sidebar on port " <> T.pack tuiPort
        Just <$> newTUIHandle "control-server" tuiSock

    let cleanup = do
          case maybeTuiHandle of
            Just h -> closeTUIHandle h
            Nothing -> pure ()

    flip finally cleanup $ bracket (setupTcpSocket tcpPort) close $ \sock -> do
      logInfo logger $ "Control server listening on TCP port " <> T.pack (show tcpPort)

      forever $ do
        (conn, _peer) <- accept sock
        void $ forkIO $ handleConnection logger lspSession maybeTuiHandle conn `finally` close conn

-- | Setup TCP socket on given port.
setupTcpSocket :: Int -> IO Socket
setupTcpSocket port = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (fromIntegral port) 0)  -- 0 = INADDR_ANY
  listen sock 10
  pure sock

-- | Handle a single connection (one NDJSON request-response).
handleConnection :: Logger -> LSPSession -> Maybe TUIHandle -> Socket -> IO ()
handleConnection logger lspSession maybeTuiHandle conn = do
  logDebug logger "Connection received"

  (do
    -- Read until newline (NDJSON framing)
    msgBytes <- readUntilNewline conn

    case eitherDecodeStrict msgBytes of
      Left err -> do
        logError logger $ "[Server] JSON parse error: " <> T.pack err
        logDebug logger $ "[Server] Raw input (" <> T.pack (show (BS.length msgBytes)) <> " bytes): "
          <> T.decodeUtf8 (BS.take 200 msgBytes)  -- First 200 bytes for debugging
        let response = hookError $ T.pack $ "JSON parse error: " <> err
        sendResponse conn response

      Right msg -> do
        logMessage logger msg
        response <- handleMessage logger lspSession maybeTuiHandle msg
        logResponse logger response
        sendResponse conn response
    )
  `catch` \(e :: SomeException) -> do
    logError logger $ "Connection error: " <> T.pack (show e)
    -- Send error response to client instead of leaving them hanging
    sendResponse conn $ hookError $ "Connection error: " <> T.pack (show e)

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
  HookEvent input ->
    logDebug logger $ "[HOOK] " <> input.hookEventName
      <> maybe "" (" tool=" <>) input.toolName
  McpToolCall reqId name _args ->
    logInfo logger $ "[MCP:" <> reqId <> "] tool=" <> name
  ToolsListRequest ->
    logDebug logger "[MCP] tools/list request"

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
