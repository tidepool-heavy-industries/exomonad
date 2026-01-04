-- | JSON-RPC 2.0 protocol types for LSP communication.
--
-- This module provides the low-level protocol types for communicating
-- with language servers via JSON-RPC 2.0 over stdin/stdout.
--
-- == Protocol Overview
--
-- LSP uses JSON-RPC 2.0 with a simple HTTP-like message framing:
--
-- @
-- Content-Length: <bytes>\r\n
-- \r\n
-- {"jsonrpc":"2.0","id":1,"method":"initialize","params":{...}}
-- @
--
-- == Message Types
--
-- * 'Request' - Client-to-server request expecting a response
-- * 'Response' - Server-to-client response to a request
-- * 'Notification' - One-way message (no response expected)
--
module Tidepool.LSP.Protocol
  ( -- * JSON-RPC Types
    JsonRpcRequest(..)
  , JsonRpcResponse(..)
  , JsonRpcNotification(..)
  , JsonRpcError(..)
  , ErrorCode(..)
  , RequestId(..)

    -- * Message Encoding/Decoding
  , encodeMessage
  , decodeMessage
  , parseContentLength

    -- * LSP Methods (subset for our use case)
  , initializeMethod
  , initializedMethod
  , shutdownMethod
  , exitMethod
  , diagnosticsMethod
  , hoverMethod
  , referencesMethod
  , definitionMethod
  , codeActionMethod
  , renameMethod
  , completionMethod
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)


-- ════════════════════════════════════════════════════════════════════════════
-- REQUEST ID
-- ════════════════════════════════════════════════════════════════════════════

-- | JSON-RPC request ID (can be number or string).
data RequestId
  = IdInt !Int
  | IdText !Text
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON RequestId where
  toJSON (IdInt n) = toJSON n
  toJSON (IdText t) = toJSON t

instance FromJSON RequestId where
  parseJSON (Number n) = pure $ IdInt (round n)
  parseJSON (String t) = pure $ IdText t
  parseJSON _ = fail "RequestId must be number or string"


-- ════════════════════════════════════════════════════════════════════════════
-- ERROR CODES
-- ════════════════════════════════════════════════════════════════════════════

-- | Standard JSON-RPC 2.0 error codes.
data ErrorCode
  = ParseError           -- ^ -32700 Invalid JSON
  | InvalidRequest       -- ^ -32600 Invalid request object
  | MethodNotFound       -- ^ -32601 Method doesn't exist
  | InvalidParams        -- ^ -32602 Invalid method params
  | InternalError        -- ^ -32603 Internal JSON-RPC error
  | ServerNotInitialized -- ^ -32002 Server not initialized
  | UnknownErrorCode     -- ^ -32001 Unknown error
  | RequestCancelled     -- ^ -32800 Request was cancelled
  | ContentModified      -- ^ -32801 Content was modified
  | OtherError !Int      -- ^ Other error codes
  deriving stock (Show, Eq, Generic)

instance ToJSON ErrorCode where
  toJSON ParseError = toJSON (-32700 :: Int)
  toJSON InvalidRequest = toJSON (-32600 :: Int)
  toJSON MethodNotFound = toJSON (-32601 :: Int)
  toJSON InvalidParams = toJSON (-32602 :: Int)
  toJSON InternalError = toJSON (-32603 :: Int)
  toJSON ServerNotInitialized = toJSON (-32002 :: Int)
  toJSON UnknownErrorCode = toJSON (-32001 :: Int)
  toJSON RequestCancelled = toJSON (-32800 :: Int)
  toJSON ContentModified = toJSON (-32801 :: Int)
  toJSON (OtherError n) = toJSON n

instance FromJSON ErrorCode where
  parseJSON v = do
    n <- parseJSON v
    pure $ case (n :: Int) of
      -32700 -> ParseError
      -32600 -> InvalidRequest
      -32601 -> MethodNotFound
      -32602 -> InvalidParams
      -32603 -> InternalError
      -32002 -> ServerNotInitialized
      -32001 -> UnknownErrorCode
      -32800 -> RequestCancelled
      -32801 -> ContentModified
      other  -> OtherError other


-- ════════════════════════════════════════════════════════════════════════════
-- JSON-RPC MESSAGE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | JSON-RPC 2.0 request.
data JsonRpcRequest = JsonRpcRequest
  { reqId     :: !RequestId  -- ^ Request ID
  , reqMethod :: !Text       -- ^ Method name
  , reqParams :: !Value      -- ^ Parameters (object or array)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON JsonRpcRequest where
  toJSON r = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= r.reqId
    , "method" .= r.reqMethod
    , "params" .= r.reqParams
    ]

instance FromJSON JsonRpcRequest where
  parseJSON = withObject "JsonRpcRequest" $ \v -> do
    _ <- v .: "jsonrpc" :: Parser Text
    JsonRpcRequest
      <$> v .: "id"
      <*> v .: "method"
      <*> v .:? "params" .!= Null

-- | JSON-RPC 2.0 error object.
data JsonRpcError = JsonRpcError
  { errCode    :: !ErrorCode   -- ^ Error code
  , errMessage :: !Text        -- ^ Error message
  , errData    :: !(Maybe Value) -- ^ Optional additional data
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON JsonRpcError where
  toJSON e = object $
    [ "code" .= e.errCode
    , "message" .= e.errMessage
    ] ++ maybe [] (\d -> ["data" .= d]) e.errData

instance FromJSON JsonRpcError where
  parseJSON = withObject "JsonRpcError" $ \v ->
    JsonRpcError
      <$> v .: "code"
      <*> v .: "message"
      <*> v .:? "data"

-- | JSON-RPC 2.0 response.
data JsonRpcResponse = JsonRpcResponse
  { respId     :: !RequestId           -- ^ Request ID (matches request)
  , respResult :: !(Maybe Value)       -- ^ Result (if success)
  , respError  :: !(Maybe JsonRpcError) -- ^ Error (if failure)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON JsonRpcResponse where
  toJSON r = object $
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= r.respId
    ] ++ case (r.respResult, r.respError) of
      (Just result, _) -> ["result" .= result]
      (_, Just err)    -> ["error" .= err]
      (Nothing, Nothing) -> ["result" .= Null]

instance FromJSON JsonRpcResponse where
  parseJSON = withObject "JsonRpcResponse" $ \v -> do
    _ <- v .: "jsonrpc" :: Parser Text
    JsonRpcResponse
      <$> v .: "id"
      <*> v .:? "result"
      <*> v .:? "error"

-- | JSON-RPC 2.0 notification (no response expected).
data JsonRpcNotification = JsonRpcNotification
  { notifMethod :: !Text   -- ^ Method name
  , notifParams :: !Value  -- ^ Parameters
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON JsonRpcNotification where
  toJSON n = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "method" .= n.notifMethod
    , "params" .= n.notifParams
    ]

instance FromJSON JsonRpcNotification where
  parseJSON = withObject "JsonRpcNotification" $ \v -> do
    _ <- v .: "jsonrpc" :: Parser Text
    JsonRpcNotification
      <$> v .: "method"
      <*> v .:? "params" .!= Null


-- ════════════════════════════════════════════════════════════════════════════
-- MESSAGE ENCODING/DECODING
-- ════════════════════════════════════════════════════════════════════════════

-- | Encode a JSON message with LSP headers.
--
-- Produces:
-- @
-- Content-Length: <length>\r\n
-- \r\n
-- <json>
-- @
encodeMessage :: ToJSON a => a -> ByteString
encodeMessage msg =
  let content = LBS.toStrict (encode msg)
      contentLen = BS.length content
      header = "Content-Length: " <> BS8.pack (show contentLen) <> "\r\n\r\n"
  in header <> content

-- | Decode a JSON message from bytes.
decodeMessage :: FromJSON a => ByteString -> Either String a
decodeMessage bs = eitherDecode (LBS.fromStrict bs)

-- | Parse Content-Length header from bytes.
--
-- Returns (content_length, remaining_bytes after header).
parseContentLength :: ByteString -> Either String (Int, ByteString)
parseContentLength bs =
  case BS8.breakSubstring "\r\n\r\n" bs of
    (header, rest)
      | BS.null rest -> Left "No header terminator found"
      | otherwise ->
          let body = BS.drop 4 rest  -- Drop the \r\n\r\n
              headerText = TE.decodeUtf8 header
          in case parseContentLengthHeader headerText of
               Left err -> Left err
               Right len -> Right (len, body)

-- | Parse the Content-Length value from header text.
parseContentLengthHeader :: Text -> Either String Int
parseContentLengthHeader header =
  let prefix = "Content-Length: "
      cleaned = T.strip header
  in if prefix `T.isPrefixOf` cleaned
     then case reads (T.unpack $ T.drop (T.length prefix) cleaned) of
       [(n, "")] -> Right n
       _ -> Left "Invalid Content-Length value"
     else Left "Missing Content-Length header"


-- ════════════════════════════════════════════════════════════════════════════
-- LSP METHOD NAMES
-- ════════════════════════════════════════════════════════════════════════════

-- | Initialize method.
initializeMethod :: Text
initializeMethod = "initialize"

-- | Initialized notification.
initializedMethod :: Text
initializedMethod = "initialized"

-- | Shutdown method.
shutdownMethod :: Text
shutdownMethod = "shutdown"

-- | Exit notification.
exitMethod :: Text
exitMethod = "exit"

-- | Text document diagnostics.
diagnosticsMethod :: Text
diagnosticsMethod = "textDocument/publishDiagnostics"

-- | Hover request.
hoverMethod :: Text
hoverMethod = "textDocument/hover"

-- | Find references request.
referencesMethod :: Text
referencesMethod = "textDocument/references"

-- | Go to definition request.
definitionMethod :: Text
definitionMethod = "textDocument/definition"

-- | Code action request.
codeActionMethod :: Text
codeActionMethod = "textDocument/codeAction"

-- | Rename request.
renameMethod :: Text
renameMethod = "textDocument/rename"

-- | Completion request.
completionMethod :: Text
completionMethod = "textDocument/completion"
