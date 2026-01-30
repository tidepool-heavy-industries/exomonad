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
  , AnthropicChatReq(..)
  , GitHubGetIssueReq(..)
  , GitHubCreateIssueReq(..)
  , GitHubUpdateIssueReq(..)
  , GitHubAddIssueLabelReq(..)
  , GitHubRemoveIssueLabelReq(..)
  , GitHubAddIssueAssigneeReq(..)
  , GitHubRemoveIssueAssigneeReq(..)
  , GitHubListIssuesReq(..)
  , GitHubCreatePRReq(..)
  , GitHubGetPRReq(..)
  , GitHubListPullRequestsReq(..)
  , GitHubGetPullRequestReviewsReq(..)
  , GitHubGetDiscussionReq(..)
  , OllamaGenerateReq(..)
  , OtelSpanReq(..)
  , OtelMetricReq(..)
  , sendRequest
  , withSocketConnection
  ) where

import Control.Exception (bracket, try, SomeException)
import Deriving.Aeson
import Data.Aeson (eitherDecode, encode, Value, Object)
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

-- REQUEST PAYLOAD TYPES

data AnthropicChatReq = AnthropicChatReq
  { model :: Text
  , messages :: [Value]
  , maxTokens :: Int
  , tools :: Maybe [Value]
  , system :: Maybe Text
  , thinking :: Maybe Value
  } deriving (Show, Eq, Generic, ToJSON)

data GitHubGetIssueReq = GitHubGetIssueReq { owner :: Text, repo :: Text, number :: Int, includeComments :: Bool }
  deriving (Show, Eq, Generic, ToJSON)

data GitHubCreateIssueReq = GitHubCreateIssueReq
  { owner :: Text
  , repo :: Text
  , title :: Text
  , body :: Text
  , labels :: [Text]
  } deriving (Show, Eq, Generic, ToJSON)

data GitHubUpdateIssueReq = GitHubUpdateIssueReq
  { owner :: Text
  , repo :: Text
  , number :: Int
  , title :: Maybe Text
  , body :: Maybe Text
  , state :: Maybe Text
  , labels :: Maybe [Text]
  , assignees :: Maybe [Text]
  } deriving (Show, Eq, Generic, ToJSON)

data GitHubAddIssueLabelReq = GitHubAddIssueLabelReq { owner :: Text, repo :: Text, number :: Int, label :: Text }
  deriving (Show, Eq, Generic, ToJSON)

data GitHubRemoveIssueLabelReq = GitHubRemoveIssueLabelReq { owner :: Text, repo :: Text, number :: Int, label :: Text }
  deriving (Show, Eq, Generic, ToJSON)

data GitHubAddIssueAssigneeReq = GitHubAddIssueAssigneeReq { owner :: Text, repo :: Text, number :: Int, assignee :: Text }
  deriving (Show, Eq, Generic, ToJSON)

data GitHubRemoveIssueAssigneeReq = GitHubRemoveIssueAssigneeReq { owner :: Text, repo :: Text, number :: Int, assignee :: Text }
  deriving (Show, Eq, Generic, ToJSON)

data GitHubListIssuesReq = GitHubListIssuesReq { owner :: Text, repo :: Text, state :: Maybe Text, labels :: [Text] }
  deriving (Show, Eq, Generic, ToJSON)

data GitHubCreatePRReq = GitHubCreatePRReq { owner :: Text, repo :: Text, title :: Text, body :: Text, head :: Text, base :: Text }
  deriving (Show, Eq, Generic, ToJSON)

data GitHubGetPRReq = GitHubGetPRReq { owner :: Text, repo :: Text, number :: Int, includeDetails :: Bool }
  deriving (Show, Eq, Generic, ToJSON)

data GitHubListPullRequestsReq = GitHubListPullRequestsReq { owner :: Text, repo :: Text, state :: Maybe Text, limit :: Maybe Int }
  deriving (Show, Eq, Generic, ToJSON)

data GitHubGetPullRequestReviewsReq = GitHubGetPullRequestReviewsReq { owner :: Text, repo :: Text, number :: Int }
  deriving (Show, Eq, Generic, ToJSON)

data GitHubGetDiscussionReq = GitHubGetDiscussionReq { owner :: Text, repo :: Text, number :: Int }
  deriving (Show, Eq, Generic, ToJSON)

data OllamaGenerateReq = OllamaGenerateReq { model :: Text, prompt :: Text, system :: Maybe Text }
  deriving (Show, Eq, Generic, ToJSON)

data OtelSpanReq = OtelSpanReq { traceId :: Text, spanId :: Text, name :: Text, startNs :: Integer, endNs :: Integer, attributes :: Object }
  deriving (Show, Eq, Generic, ToJSON)

data OtelMetricReq = OtelMetricReq { name :: Text, value :: Double, otelLabels :: Object }
  deriving (Show, Eq, Generic, ToJSON)


-- | Mirror of Rust protocol types (to be implemented in exomonad-shared)
data ServiceRequest
  = AnthropicChat AnthropicChatReq
  | GitHubGetIssue GitHubGetIssueReq
  | GitHubCreateIssue GitHubCreateIssueReq
  | GitHubUpdateIssue GitHubUpdateIssueReq
  | GitHubAddIssueLabel GitHubAddIssueLabelReq
  | GitHubRemoveIssueLabel GitHubRemoveIssueLabelReq
  | GitHubAddIssueAssignee GitHubAddIssueAssigneeReq
  | GitHubRemoveIssueAssignee GitHubRemoveIssueAssigneeReq
  | GitHubListIssues GitHubListIssuesReq
  | GitHubCreatePR GitHubCreatePRReq
  | GitHubGetPR GitHubGetPRReq
  | GitHubListPullRequests GitHubListPullRequestsReq
  | GitHubGetPullRequestReviews GitHubGetPullRequestReviewsReq
  | GitHubGetDiscussion GitHubGetDiscussionReq
  | GitHubCheckAuth
  | OllamaGenerate OllamaGenerateReq
  | OtelSpan OtelSpanReq
  | OtelMetric OtelMetricReq
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via CustomJSON '[SumTaggedObject "type" "contents", UnwrapUnaryRecords] ServiceRequest

data ServiceResponse
  = AnthropicChatResponse { content :: [Value], stop_reason :: Text, usage :: Value }
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
  deriving (FromJSON) via CustomJSON '[SumTaggedObject "type" "contents"] ServiceResponse

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
