{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module ExoMonad.Effects.SocketClient
  ( SocketConfig(..)
  , ServiceRequest(..)
  , ServiceResponse(..)
  , ServiceError(..)
  , sendRequest
  , withSocketConnection
  ) where

import Control.Exception (bracket, try, SomeException)
import Deriving.Aeson
import Data.Aeson (eitherDecode, encode, Value, Object)
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty (NonEmpty(..))
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
      , messages :: NonEmpty Value
      , maxTokens :: Int
      , tools :: Maybe [Value]
      , system :: Maybe Text
      , thinking :: Maybe Value
      }
  | GitHubGetIssue { owner :: Text, repo :: Text, number :: Int, includeComments :: Bool }
  | GitHubCreateIssue
      { owner :: Text
      , repo :: Text
      , title :: Maybe Text -- Use Maybe to unify with Update
      , body :: Maybe Text
      , labels :: Maybe [Text]
      }
  | GitHubUpdateIssue
      { owner :: Text
      , repo :: Text
      , number :: Int
      , title :: Maybe Text
      , body :: Maybe Text
      , state :: Maybe Text
      , labels :: Maybe [Text]
      , assignees :: Maybe [Text]
      }
  | GitHubAddIssueLabel { owner :: Text, repo :: Text, number :: Int, label :: Text }
  | GitHubRemoveIssueLabel { owner :: Text, repo :: Text, number :: Int, label :: Text }
  | GitHubAddIssueAssignee { owner :: Text, repo :: Text, number :: Int, assignee :: Text }
  | GitHubRemoveIssueAssignee { owner :: Text, repo :: Text, number :: Int, assignee :: Text }
  | GitHubListIssues { owner :: Text, repo :: Text, state :: Maybe Text, labels :: Maybe [Text] }
  | GitHubCreatePR { owner :: Text, repo :: Text, title :: Maybe Text, body :: Maybe Text, head :: Text, base :: Text }
  | GitHubGetPR { owner :: Text, repo :: Text, number :: Int, includeDetails :: Bool }
  | GitHubListPullRequests { owner :: Text, repo :: Text, state :: Maybe Text, limit :: Maybe Int }
  | GitHubGetPullRequestReviews { owner :: Text, repo :: Text, number :: Int }
  | GitHubGetDiscussion { owner :: Text, repo :: Text, number :: Int }
  | GitHubCheckAuth
  | OllamaGenerate { model :: Text, prompt :: Text, system :: Maybe Text }
  | OtelSpan { traceId :: Text, spanId :: Text, name :: Text, startNs :: Integer, endNs :: Integer, attributes :: Object }
  | OtelMetric { name :: Text, value :: Double, otelLabels :: Object }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via CustomJSON '[SumTaggedObject "type" "", OmitNothingFields] ServiceRequest

data ServiceResponse
  = AnthropicChatResponse { content :: NonEmpty Value, stop_reason :: Text, usage :: Value }
  | GitHubIssueResponse { number :: Int, title :: Text, body :: Text, state :: Text, labels :: Maybe [Text], url :: Text, author :: Text, comments :: Maybe [Value] }
  | GitHubIssuesResponse { issues :: [Value] }
  | GitHubPRResponse { number :: Int, title :: Text, body :: Text, author :: Text, url :: Text, state :: Text, head_ref_name :: Text, base_ref_name :: Text, created_at :: Text, merged_at :: Maybe Text, labels :: Maybe [Text], comments :: Maybe [Value], reviews :: Maybe [Value] }
  | GitHubPullRequestsResponse { pull_requests :: [Value] }
  | GitHubReviewsResponse { reviews :: Maybe [Value] }
  | GitHubDiscussionResponse { number :: Int, title :: Text, body :: Text, author :: Text, url :: Text, comments :: Maybe [Value] }
  | GitHubAuthResponse { authenticated :: Bool, user :: Maybe Text }
  | OllamaGenerateResponse { response :: Text, done :: Bool }
  | OtelAckResponse
  | ErrorResponse { code :: Int, message :: Text }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via CustomJSON '[SumTaggedObject "type" ""] ServiceResponse

data ServiceError
  = SocketError Text
  | DecodeError String
  | TimeoutError
  deriving (Show, Eq, Generic)

-- | NDJSON over Unix socket
sendRequest :: SocketConfig -> ServiceRequest -> IO (Either ServiceError ServiceResponse)
sendRequest config req = do
  res <- timeout (config.scTimeout * 1000) $ try $ withSocketConnection config $ \sock -> do
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
      connect sock (SockAddrUnix (config.scSocketPath))
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
