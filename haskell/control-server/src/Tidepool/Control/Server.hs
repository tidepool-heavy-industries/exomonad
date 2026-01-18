-- | Unix socket control server for Claude Code++ integration.
--
-- Listens on a Unix socket at .tidepool/control.sock and handles NDJSON
-- messages from mantle-agent. Each connection is one request-response pair.
--
-- The server maintains a long-lived LSP session for code intelligence.
module Tidepool.Control.Server
  ( runServer
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch, finally, bracket)
import Control.Monad (forever, void, when)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Socket hiding (ControlMessage)
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

import Tidepool.Control.Handler (handleMessage)
import Tidepool.Control.Protocol
import Tidepool.Control.Types (ServerConfig(..), TeachingSettings(..))
import Tidepool.LSP.Interpreter (LSPSession, withLSPSession)

-- | Run the control server. Blocks forever.
--
-- 1. Creates .tidepool/ directory if needed
-- 2. Cleans up stale socket file
-- 3. Starts LSP session for the project
-- 4. Accepts connections on Unix socket
runServer :: ServerConfig -> IO ()
runServer config = do
  let tidepoolDir = config.projectDir </> ".tidepool"
  let socketPath = tidepoolDir </> "control.sock"

  -- Setup .tidepool directory
  createDirectoryIfMissing True tidepoolDir
  TIO.putStrLn $ "Created .tidepool directory at " <> T.pack tidepoolDir
  hFlush stdout

  -- Clean up stale socket
  staleExists <- doesFileExist socketPath
  when staleExists $ do
    TIO.putStrLn "Removing stale socket file..."
    removeFile socketPath

  TIO.putStrLn $ "Starting LSP session for project: " <> T.pack config.projectDir
  hFlush stdout

  -- Start LSP session and run server
  withLSPSession config.projectDir $ \lspSession -> do
    TIO.putStrLn "LSP session initialized"
    hFlush stdout

    bracket (setupSocket socketPath) (closeSocket socketPath) $ \sock -> do
      TIO.putStrLn $ "Control server listening on " <> T.pack socketPath
      hFlush stdout

      forever $ do
        (conn, _peer) <- accept sock
        void $ forkIO $ handleConnection lspSession (teachingSettings config) conn `finally` close conn

-- | Setup Unix domain socket.
setupSocket :: FilePath -> IO Socket
setupSocket socketPath = do
  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix socketPath)
  listen sock 10
  pure sock

-- | Close socket and clean up socket file.
closeSocket :: FilePath -> Socket -> IO ()
closeSocket socketPath sock = do
  close sock
  -- Clean up socket file on shutdown
  exists <- doesFileExist socketPath
  when exists $ removeFile socketPath

-- | Handle a single connection (one NDJSON request-response).
handleConnection :: LSPSession -> Maybe TeachingSettings -> Socket -> IO ()
handleConnection lspSession maybeTeachSettings conn = do
  TIO.putStrLn "Connection received"
  hFlush stdout

  -- FIXME: Add socket timeout (30s) to match Rust client timeout.
  -- Currently server can block indefinitely on slow clients.
  -- Use System.Timeout.timeout (30 * 1000000) around the handler.

  (do
    -- Read until newline (NDJSON framing)
    -- Now covered by exception handler
    msgBytes <- readUntilNewline conn

    case eitherDecodeStrict msgBytes of
      Left err -> do
        TIO.putStrLn $ "Parse error: " <> T.pack err
        -- Send error response
        let response = hookError $ T.pack $ "JSON parse error: " <> err
        sendResponse conn response

      Right msg -> do
        logMessage msg
        response <- handleMessage lspSession maybeTeachSettings msg
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
