{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module ExoMonad.Effects.SocketClient
  ( SocketConfig(..)
  , ServiceRequest(..)
  , ServiceResponse(..)
  , ServiceError(..)
  , sendRequest
  , withSocketConnection
  ) where

import Control.Exception (bracket, try, SomeException)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Socket
import Network.Socket.ByteString.Lazy (sendAll, recv)
import System.Timeout (timeout)

data SocketConfig = SocketConfig
  { scSocketPath :: FilePath
  , scTimeout :: Int  -- milliseconds
  } deriving (Show, Eq, Generic)

-- | Mirror of Rust protocol types (to be implemented in exomonad-shared)
data ServiceRequest
  = AnthropicChat 
      { model :: Text
      , messages :: [Value]
      , maxTokens :: Int
      , tools :: Maybe [Value]
      , system :: Maybe Text
      , thinking :: Maybe Value
      }
  | GitHubGetIssue { owner :: Text, repo :: Text, number :: Int }
  | GitHubListIssues { owner :: Text, repo :: Text, state :: Maybe Text, labels :: [Text] }
  | OllamaGenerate { model :: Text, prompt :: Text, system :: Maybe Text }
  | OtelSpan { traceId :: Text, spanId :: Text, name :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON ServiceRequest where
  toJSON = \case
    AnthropicChat m ms mt tls sys th -> object
      [ "type" .= ("AnthropicChat" :: Text)
      , "model" .= m
      , "messages" .= ms
      , "max_tokens" .= mt
      , "tools" .= tls
      , "system" .= sys
      , "thinking" .= th
      ]
    GitHubGetIssue o r n -> object
      [ "type" .= ("GitHubGetIssue" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "number" .= n
      ]
    GitHubListIssues o r s ls -> object
      [ "type" .= ("GitHubListIssues" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "state" .= s
      , "labels" .= ls
      ]
    OllamaGenerate m p s -> object
      [ "type" .= ("OllamaGenerate" :: Text)
      , "model" .= m
      , "prompt" .= p
      , "system" .= s
      ]
    OtelSpan tid sid n -> object
      [ "type" .= ("OtelSpan" :: Text)
      , "trace_id" .= tid
      , "span_id" .= sid
      , "name" .= n
      ]

data ServiceResponse
  = AnthropicChatResponse { content :: [Value], stopReason :: Text, usage :: Value }
  | GitHubIssueResponse { issueNumber :: Int, title :: Text, body :: Text }
  | GitHubIssuesResponse { issues :: [Value] }
  | OllamaGenerateResponse { response :: Text, done :: Bool }
  | OtelAckResponse
  | ErrorResponse { code :: Int, message :: Text }
  deriving (Show, Eq, Generic)

instance FromJSON ServiceResponse where
  parseJSON = withObject "ServiceResponse" $ \v -> do
    t <- v .: "type"
    case (t :: Text) of 
      "AnthropicChatResponse" -> AnthropicChatResponse <$> v .: "content" <*> v .: "stop_reason" <*> v .: "usage"
      "GitHubIssueResponse" -> GitHubIssueResponse <$> v .: "number" <*> v .: "title" <*> v .: "body"
      "GitHubIssuesResponse" -> GitHubIssuesResponse <$> v .: "issues"
      "OllamaGenerateResponse" -> OllamaGenerateResponse <$> v .: "response" <*> v .: "done"
      "OtelAckResponse" -> pure OtelAckResponse
      "ErrorResponse" -> ErrorResponse <$> v .: "code" <*> v .: "message"
      _ -> fail $ "Unknown response type: " <> T.unpack t

data ServiceError
  = SocketError Text
  | DecodeError String
  | TimeoutError
  deriving (Show, Eq, Generic)

-- | NDJSON over Unix socket
sendRequest :: SocketConfig -> ServiceRequest -> IO (Either ServiceError ServiceResponse)
sendRequest config req = do
  res <- timeout (scTimeout config * 1000) $ try $ withSocketConnection config $ \sock -> do
    let encoded = encode req <> "\n"
    sendAll sock encoded
    
    -- Receive response line
    respLine <- recvLine sock
    case eitherDecode respLine of
      Left err -> pure $ Left $ DecodeError err
      Right val -> pure $ Right val
      
  case res of
    Nothing -> pure $ Left TimeoutError
    Just (Left (e :: SomeException)) -> pure $ Left $ SocketError (T.pack $ show e)
    Just (Right result) -> pure result

withSocketConnection :: SocketConfig -> (Socket -> IO a) -> IO a
withSocketConnection config action = bracket open Network.Socket.close action
  where
    open = do
      sock <- socket AF_UNIX Stream defaultProtocol
      connect sock (SockAddrUnix (scSocketPath config))
      pure sock

-- | Read a single line from the socket
recvLine :: Socket -> IO ByteString
recvLine sock = go LBS.empty
  where
    go acc = do
      chunk <- recv sock 1024
      if LBS.null chunk
        then pure acc
        else do
          let (line, rest) = LBS8.break (== '\n') chunk
          if LBS.null rest
            then go (acc <> line)
            else pure (acc <> line)
