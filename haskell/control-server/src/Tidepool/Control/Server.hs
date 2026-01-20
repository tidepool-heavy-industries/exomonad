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
import qualified Control.Exception as E
import Control.Monad (forever, void, when)
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
import Tidepool.TUI.Interpreter (TUIHandle, connectTUIUnix, newTUIHandle, closeTUIHandle)

-- | Default Unix socket paths
defaultControlSocket :: FilePath
defaultControlSocket = ".tidepool/sockets/control.sock"

defaultTuiSocket :: FilePath
defaultTuiSocket = ".tidepool/sockets/tui.sock"

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

  logInfo logger $ "Starting LSP session for project: " <> T.pack config.projectDir

  -- Start LSP session and run server
  withLSPSession config.projectDir $ \lspSession -> do
    logInfo logger "LSP session initialized"

    -- Try to connect to TUI sidebar (optional)
    maybeTuiHandle <- try (connectTUIUnix tuiSocket) >>= \case
      Left (e :: SomeException) -> do
        logInfo logger $ "TUI sidebar not found at " <> T.pack tuiSocket
          <> ", running without TUI support (" <> T.pack (show e) <> ")"
        pure Nothing
      Right tuiSock -> do
        logInfo logger $ "Connected to TUI sidebar at " <> T.pack tuiSocket
        Just <$> newTUIHandle "control-server" tuiSock

    let cleanup = do
          case maybeTuiHandle of
            Just h -> closeTUIHandle h
            Nothing -> pure ()

    flip finally cleanup $ bracket (setupUnixSocket controlSocket) (cleanupUnixSocket controlSocket) $ \sock -> do
      logInfo logger $ "Control server listening on Unix socket: " <> T.pack controlSocket

      forever $ do
        (conn, _peer) <- accept sock
        void $ forkIO $ handleConnection logger lspSession maybeTuiHandle conn `finally` close conn

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
        -- Use Gemini exit code (2) for parse errors since we can't determine runtime
        -- from malformed JSON. Exit code 2 should be treated as error by both runtimes.
        let response = hookError Gemini $ T.pack $ "JSON parse error: " <> err
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
    -- Use Gemini exit code (2) since we can't determine runtime from connection error
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
