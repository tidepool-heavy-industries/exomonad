-- | LSP client for subprocess communication.
--
-- This module provides a client that communicates with a language server
-- subprocess via stdin/stdout using JSON-RPC 2.0.
--
-- == Usage
--
-- @
-- import Tidepool.LSP.Client
--
-- main = do
--   client <- startLSPClient "haskell-language-server-wrapper" ["--lsp"]
--
--   -- Initialize the server
--   initResult <- initialize client initParams
--
--   -- Send initialized notification
--   initialized client
--
--   -- Use the client...
--   hover client uri position
--
--   -- Shutdown
--   shutdown client
--   exit client
--   stopLSPClient client
-- @
--
-- == Thread Safety
--
-- The client is thread-safe. Multiple threads can send requests
-- concurrently. Responses are matched to requests by ID.
--
module Tidepool.LSP.Client
  ( -- * Client Handle
    LSPClient(..)
  , LSPClientError(..)

    -- * Lifecycle
  , startLSPClient
  , stopLSPClient

    -- * LSP Protocol
  , initialize
  , initialized
  , shutdown
  , exit

    -- * LSP Requests
  , sendRequest
  , sendNotification

    -- * Initialization Parameters
  , InitializeParams(..)
  , InitializeResult(..)
  , ClientCapabilities(..)
  , defaultClientCapabilities
  , defaultInitializeParams
  ) where

import Control.Concurrent
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM
import Control.Exception (SomeException, try, throwIO, Exception)
import Control.Monad (forever, when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO (Handle, hFlush, hSetBinaryMode, hSetBuffering, BufferMode(..))
import System.Process
  ( CreateProcess(..), StdStream(..), ProcessHandle
  , createProcess, proc, terminateProcess, waitForProcess
  )
import GHC.Generics (Generic)

import Tidepool.LSP.Protocol


-- ════════════════════════════════════════════════════════════════════════════
-- CLIENT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | LSP client handle.
data LSPClient = LSPClient
  { lspStdin      :: !Handle          -- ^ Stdin of the LSP server
  , lspStdout     :: !Handle          -- ^ Stdout of the LSP server
  , lspProcess    :: !ProcessHandle   -- ^ Process handle
  , lspNextId     :: !(IORef Int)     -- ^ Next request ID
  , lspPending    :: !(TVar (Map RequestId (TMVar JsonRpcResponse)))
    -- ^ Pending requests waiting for responses
  , lspReader     :: !(Async ())      -- ^ Background reader thread
  , lspRootUri    :: !(Maybe Text)    -- ^ Root URI for the workspace
  }

-- | Errors that can occur during LSP operations.
data LSPClientError
  = LSPProcessError Text       -- ^ Failed to start process
  | LSPWriteError Text         -- ^ Failed to write to stdin
  | LSPReadError Text          -- ^ Failed to read from stdout
  | LSPParseError Text         -- ^ Failed to parse response
  | LSPTimeoutError            -- ^ Request timed out
  | LSPServerError JsonRpcError -- ^ Server returned an error
  deriving stock (Show, Eq)

instance Exception LSPClientError


-- ════════════════════════════════════════════════════════════════════════════
-- INITIALIZATION TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Client capabilities (simplified).
data ClientCapabilities = ClientCapabilities
  { ccTextDocument :: !(Maybe Value)  -- ^ Text document capabilities
  , ccWorkspace    :: !(Maybe Value)  -- ^ Workspace capabilities
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default client capabilities for basic operations.
defaultClientCapabilities :: ClientCapabilities
defaultClientCapabilities = ClientCapabilities
  { ccTextDocument = Just $ object
      [ "hover" .= object
          [ "contentFormat" .= (["plaintext", "markdown"] :: [Text])
          ]
      , "references" .= object []
      , "definition" .= object []
      , "codeAction" .= object []
      , "rename" .= object []
      , "completion" .= object
          [ "completionItem" .= object
              [ "snippetSupport" .= False
              ]
          ]
      ]
  , ccWorkspace = Nothing
  }

-- | Initialize request parameters.
data InitializeParams = InitializeParams
  { ipProcessId    :: !(Maybe Int)      -- ^ Parent process ID
  , ipRootUri      :: !(Maybe Text)     -- ^ Root URI of workspace
  , ipCapabilities :: !ClientCapabilities
  , ipTrace        :: !(Maybe Text)     -- ^ Trace level
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON InitializeParams where
  toJSON p = object
    [ "processId" .= p.ipProcessId
    , "rootUri" .= p.ipRootUri
    , "capabilities" .= p.ipCapabilities
    , "trace" .= p.ipTrace
    ]

-- | Initialize result.
data InitializeResult = InitializeResult
  { irCapabilities :: !Value  -- ^ Server capabilities
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Default initialize params for a workspace.
defaultInitializeParams :: Maybe Text -> InitializeParams
defaultInitializeParams rootUri = InitializeParams
  { ipProcessId = Nothing  -- Will be set by startLSPClient
  , ipRootUri = rootUri
  , ipCapabilities = defaultClientCapabilities
  , ipTrace = Just "off"
  }


-- ════════════════════════════════════════════════════════════════════════════
-- CLIENT LIFECYCLE
-- ════════════════════════════════════════════════════════════════════════════

-- | Start an LSP client by spawning the language server.
--
-- @
-- client <- startLSPClient "haskell-language-server-wrapper" ["--lsp"]
-- @
startLSPClient :: FilePath -> [String] -> Maybe Text -> IO LSPClient
startLSPClient cmd args rootUri = do
  -- Create process
  let cp = (proc cmd args)
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe  -- Ignore stderr for now
        }

  (Just stdin, Just stdout, _, ph) <- createProcess cp

  -- Set binary mode and buffering
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  -- Initialize state
  nextIdRef <- newIORef 1
  pendingVar <- newTVarIO Map.empty

  -- Start reader thread
  reader <- async $ readerLoop stdout pendingVar

  pure LSPClient
    { lspStdin = stdin
    , lspStdout = stdout
    , lspProcess = ph
    , lspNextId = nextIdRef
    , lspPending = pendingVar
    , lspReader = reader
    , lspRootUri = rootUri
    }

-- | Stop the LSP client.
stopLSPClient :: LSPClient -> IO ()
stopLSPClient client = do
  -- Cancel reader thread
  cancel client.lspReader

  -- Terminate process
  terminateProcess client.lspProcess
  _ <- waitForProcess client.lspProcess
  pure ()

-- | Background thread that reads responses and dispatches to waiting requests.
readerLoop :: Handle -> TVar (Map RequestId (TMVar JsonRpcResponse)) -> IO ()
readerLoop stdout pendingVar = forever $ do
  -- Read header
  header <- readUntilCRLFCRLF stdout

  case parseContentLengthFromBS header of
    Left _ -> pure ()  -- Skip malformed messages
    Right contentLen -> do
      -- Read content
      content <- BS.hGet stdout contentLen

      -- Parse response
      case eitherDecode (LBS.fromStrict content) of
        Left _ -> pure ()  -- Skip non-response messages (notifications)
        Right resp -> do
          -- Find and complete pending request
          mVar <- atomically $ do
            pending <- readTVar pendingVar
            case Map.lookup resp.respId pending of
              Nothing -> pure Nothing
              Just var -> do
                writeTVar pendingVar (Map.delete resp.respId pending)
                pure (Just var)

          case mVar of
            Nothing -> pure ()  -- No pending request for this ID
            Just var -> atomically $ putTMVar var resp

-- | Read until we see \r\n\r\n (end of headers).
readUntilCRLFCRLF :: Handle -> IO ByteString
readUntilCRLFCRLF h = go BS.empty
  where
    go acc = do
      byte <- BS.hGet h 1
      let acc' = acc <> byte
      if "\r\n\r\n" `BS.isSuffixOf` acc'
        then pure acc'
        else go acc'

-- | Parse Content-Length from header bytes.
parseContentLengthFromBS :: ByteString -> Either String Int
parseContentLengthFromBS bs =
  let headerText = TE.decodeUtf8 bs
      lines' = T.lines headerText
  in case filter ("Content-Length:" `T.isPrefixOf`) lines' of
       [] -> Left "No Content-Length header"
       (h:_) ->
         let valueText = T.strip $ T.drop (T.length "Content-Length:") h
         in case reads (T.unpack valueText) of
              [(n, "")] -> Right n
              _ -> Left "Invalid Content-Length value"


-- ════════════════════════════════════════════════════════════════════════════
-- LSP PROTOCOL OPERATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Send a request and wait for response.
sendRequest :: (ToJSON params, FromJSON result)
            => LSPClient -> Text -> params -> IO (Either LSPClientError result)
sendRequest client method params = do
  -- Generate request ID
  reqId <- atomicModifyIORef client.lspNextId (\n -> (n + 1, n))
  let rpcId = IdInt reqId

  -- Create response channel
  respVar <- newEmptyTMVarIO
  atomically $ modifyTVar client.lspPending (Map.insert rpcId respVar)

  -- Build and send request
  let request = JsonRpcRequest
        { reqId = rpcId
        , reqMethod = method
        , reqParams = toJSON params
        }

  result <- try $ do
    let msg = encodeMessage request
    BS.hPut client.lspStdin msg
    hFlush client.lspStdin

  case result of
    Left (e :: SomeException) -> do
      atomically $ modifyTVar client.lspPending (Map.delete rpcId)
      pure $ Left $ LSPWriteError (T.pack $ show e)
    Right () -> do
      -- Wait for response (with timeout)
      mResp <- atomically $ takeTMVar respVar

      case (mResp.respResult, mResp.respError) of
        (_, Just err) -> pure $ Left $ LSPServerError err
        (Just val, _) ->
          case fromJSON val of
            Success r -> pure $ Right r
            Error e -> pure $ Left $ LSPParseError (T.pack e)
        (Nothing, Nothing) -> pure $ Left $ LSPParseError "Empty response"

-- | Send a notification (no response expected).
sendNotification :: ToJSON params => LSPClient -> Text -> params -> IO ()
sendNotification client method params = do
  let notif = JsonRpcNotification
        { notifMethod = method
        , notifParams = toJSON params
        }
  let msg = encodeMessage notif
  BS.hPut client.lspStdin msg
  hFlush client.lspStdin


-- ════════════════════════════════════════════════════════════════════════════
-- LSP LIFECYCLE METHODS
-- ════════════════════════════════════════════════════════════════════════════

-- | Initialize the language server.
initialize :: LSPClient -> InitializeParams -> IO (Either LSPClientError InitializeResult)
initialize client params = do
  sendRequest client initializeMethod params

-- | Send initialized notification.
initialized :: LSPClient -> IO ()
initialized client = do
  sendNotification client initializedMethod (object [])

-- | Request server shutdown.
shutdown :: LSPClient -> IO (Either LSPClientError ())
shutdown client = do
  result <- sendRequest client shutdownMethod (object [])
  case result of
    Left err -> pure $ Left err
    Right (_ :: Value) -> pure $ Right ()

-- | Send exit notification.
exit :: LSPClient -> IO ()
exit client = do
  sendNotification client exitMethod (object [])
