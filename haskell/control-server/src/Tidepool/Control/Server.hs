-- | TCP control server for Claude Code++ integration.
--
-- Listens on a TCP port and handles NDJSON messages from mantle-agent.
-- Each connection is one request-response pair.
module Tidepool.Control.Server
  ( ServerConfig(..)
  , defaultConfig
  , runServer
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch, finally)
import Control.Monad (forever, void)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Socket hiding (ControlMessage)
import Network.Socket.ByteString (recv, sendAll)
import System.IO (hFlush, stdout)

import Tidepool.Control.Handler (handleMessage)
import Tidepool.Control.Protocol

-- | Server configuration.
data ServerConfig = ServerConfig
  { host :: Text
  , port :: Int
  }
  deriving stock (Show, Eq)

-- | Default configuration: localhost:7432
defaultConfig :: ServerConfig
defaultConfig = ServerConfig
  { host = "127.0.0.1"
  , port = 7432
  }

-- | Run the control server. Blocks forever.
runServer :: ServerConfig -> IO ()
runServer config = do
  TIO.putStrLn $ "Control server starting on " <> config.host <> ":" <> T.pack (show config.port)
  hFlush stdout

  let hints = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }

  addrs <- getAddrInfo (Just hints) (Just $ T.unpack config.host) (Just $ show config.port)
  addr <- case addrs of
    [] -> error "No address info returned"
    (a:_) -> pure a

  sock <- openSocket addr
  setSocketOption sock ReuseAddr 1
  withFdSocket sock setCloseOnExecIfNeeded
  bind sock (addrAddress addr)
  listen sock 10

  TIO.putStrLn "Control server listening..."
  hFlush stdout

  forever $ do
    (conn, peer) <- accept sock
    void $ forkIO $ handleConnection conn peer `finally` close conn

-- | Handle a single connection (one NDJSON request-response).
handleConnection :: Socket -> SockAddr -> IO ()
handleConnection conn peer = do
  TIO.putStrLn $ "Connection from " <> T.pack (show peer)
  hFlush stdout

  -- FIXME: Add socket timeout (30s) to match Rust client timeout.
  -- Currently server can block indefinitely on slow clients.
  -- Use System.Timeout.timeout (30 * 1000000) around the handler.

  -- Read until newline (NDJSON framing)
  msgBytes <- readUntilNewline conn

  (do
    case eitherDecodeStrict msgBytes of
      Left err -> do
        TIO.putStrLn $ "Parse error: " <> T.pack err
        -- Send error response
        let response = hookError $ T.pack $ "JSON parse error: " <> err
        sendResponse conn response

      Right msg -> do
        logMessage msg
        response <- handleMessage msg
        logResponse response
        sendResponse conn response
    )
  `catch` \(e :: SomeException) -> do
    TIO.putStrLn $ "Connection error: " <> T.pack (show e)
    -- Send error response to client instead of leaving them hanging
    sendResponse conn $ hookError $ "Internal server error: " <> T.pack (show e)

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
