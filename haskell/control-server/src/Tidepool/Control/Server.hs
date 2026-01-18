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
import Control.Exception (SomeException, catch, finally, bracket)
import Control.Monad (forever, void)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Socket hiding (ControlMessage)
import Network.Socket.ByteString (recv, sendAll)
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)

import Tidepool.Control.Handler (handleMessage)
import Tidepool.Control.Protocol
import Tidepool.Control.Types (ServerConfig(..))
import Tidepool.LSP.Interpreter (LSPSession, withLSPSession)

-- | Default TCP port for mantle-agent connections.
defaultTcpPort :: Int
defaultTcpPort = 7432

-- | Run the control server. Blocks forever.
--
-- 1. Starts LSP session for the project
-- 2. Accepts TCP connections on port 7432 (or MANTLE_CONTROL_PORT)
runServer :: ServerConfig -> IO ()
runServer config = do
  -- Get TCP port from environment or use default
  tcpPortEnv <- lookupEnv "MANTLE_CONTROL_PORT"
  let tcpPort = maybe defaultTcpPort read tcpPortEnv

  TIO.putStrLn $ "Starting LSP session for project: " <> T.pack config.projectDir
  hFlush stdout

  -- Start LSP session and run server
  withLSPSession config.projectDir $ \lspSession -> do
    TIO.putStrLn "LSP session initialized"
    hFlush stdout

    bracket (setupTcpSocket tcpPort) close $ \sock -> do
      TIO.putStrLn $ "Control server listening on TCP port " <> T.pack (show tcpPort)
      hFlush stdout

      forever $ do
        (conn, _peer) <- accept sock
        void $ forkIO $ handleConnection lspSession conn `finally` close conn

-- | Setup TCP socket on given port.
setupTcpSocket :: Int -> IO Socket
setupTcpSocket port = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (fromIntegral port) 0)  -- 0 = INADDR_ANY
  listen sock 10
  pure sock

-- | Handle a single connection (one NDJSON request-response).
handleConnection :: LSPSession -> Socket -> IO ()
handleConnection lspSession conn = do
  TIO.putStrLn "Connection received"
  hFlush stdout

  (do
    -- Read until newline (NDJSON framing)
    msgBytes <- readUntilNewline conn

    case eitherDecodeStrict msgBytes of
      Left err -> do
        TIO.putStrLn $ "Parse error: " <> T.pack err
        -- Send error response
        let response = hookError $ T.pack $ "JSON parse error: " <> err
        sendResponse conn response

      Right msg -> do
        logMessage msg
        response <- handleMessage lspSession msg
        logResponse response
        sendResponse conn response
    )
  `catch` \(e :: SomeException) -> do
    TIO.putStrLn $ "Connection error: " <> T.pack (show e)
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
logMessage :: ControlMessage -> IO ()
logMessage = \case
  HookEvent input -> do
    TIO.putStrLn $ "[HOOK] " <> input.hookEventName
      <> maybe "" (\t -> " tool=" <> t) input.toolName
    hFlush stdout
  McpToolCall _ name _ -> do
    TIO.putStrLn $ "[MCP] tool=" <> name
    hFlush stdout
  ToolsListRequest -> do
    TIO.putStrLn "[MCP] tools/list request"
    hFlush stdout

-- | Log outgoing response.
logResponse :: ControlResponse -> IO ()
logResponse = \case
  HookResponse output exitCode -> do
    let decision = case output.hookSpecificOutput of
          Just (PreToolUseOutput d _ _) -> " decision=" <> d
          _ -> ""
    TIO.putStrLn $ "[HOOK] -> continue=" <> T.pack (show output.continue_)
      <> decision <> " exit=" <> T.pack (show exitCode)
    hFlush stdout
  McpToolResponse _ result err -> do
    let status = case (result, err) of
          (Just _, _) -> "success"
          (_, Just e) -> "error: " <> e.errorMessage
          _ -> "empty"
    TIO.putStrLn $ "[MCP] -> " <> status
    hFlush stdout
  ToolsListResponse tools -> do
    TIO.putStrLn $ "[MCP] -> tools/list: " <> T.pack (show (length tools)) <> " tools"
    hFlush stdout
