{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
  | GitHubGetIssue { owner :: Text, repo :: Text, number :: Int, includeComments :: Bool }
  | GitHubCreateIssue { owner :: Text, repo :: Text, title :: Text, body :: Text, labels :: [Text] }
  | GitHubUpdateIssue { owner :: Text, repo :: Text, number :: Int, updateTitle :: Maybe Text, updateBody :: Maybe Text, state :: Maybe Text, updateLabels :: Maybe [Text], assignees :: Maybe [Text] }
  | GitHubAddIssueLabel { owner :: Text, repo :: Text, number :: Int, label :: Text }
  | GitHubRemoveIssueLabel { owner :: Text, repo :: Text, number :: Int, label :: Text }
  | GitHubAddIssueAssignee { owner :: Text, repo :: Text, number :: Int, assignee :: Text }
  | GitHubRemoveIssueAssignee { owner :: Text, repo :: Text, number :: Int, assignee :: Text }
  | GitHubListIssues { owner :: Text, repo :: Text, state :: Maybe Text, issueLabels :: [Text] }
  | GitHubCreatePR { owner :: Text, repo :: Text, title :: Text, body :: Text, head :: Text, base :: Text }
  | GitHubGetPR { owner :: Text, repo :: Text, number :: Int, includeDetails :: Bool }
  | GitHubListPullRequests { owner :: Text, repo :: Text, state :: Maybe Text, limit :: Maybe Int }
  | GitHubGetPullRequestReviews { owner :: Text, repo :: Text, number :: Int }
  | GitHubGetDiscussion { owner :: Text, repo :: Text, number :: Int }
  | GitHubCheckAuth
  | OllamaGenerate { model :: Text, prompt :: Text, system :: Maybe Text }
  | OtelSpan { traceId :: Text, spanId :: Text, name :: Text, startNs :: Integer, endNs :: Integer, attributes :: Object }
  | OtelMetric { name :: Text, value :: Double, otelLabels :: Object }
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
    GitHubGetIssue o r n ic -> object
      [ "type" .= ("GitHubGetIssue" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "number" .= n
      , "include_comments" .= ic
      ]
    GitHubCreateIssue o r t b ls -> object
      [ "type" .= ("GitHubCreateIssue" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "title" .= t
      , "body" .= b
      , "labels" .= ls
      ]
    GitHubUpdateIssue o r n t b s ls as -> object
      [ "type" .= ("GitHubUpdateIssue" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "number" .= n
      , "title" .= t
      , "body" .= b
      , "state" .= s
      , "labels" .= ls
      , "assignees" .= as
      ]
    GitHubAddIssueLabel o r n l -> object
      [ "type" .= ("GitHubAddIssueLabel" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "number" .= n
      , "label" .= l
      ]
    GitHubRemoveIssueLabel o r n l -> object
      [ "type" .= ("GitHubRemoveIssueLabel" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "number" .= n
      , "label" .= l
      ]
    GitHubAddIssueAssignee o r n a -> object
      [ "type" .= ("GitHubAddIssueAssignee" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "number" .= n
      , "assignee" .= a
      ]
    GitHubRemoveIssueAssignee o r n a -> object
      [ "type" .= ("GitHubRemoveIssueAssignee" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "number" .= n
      , "assignee" .= a
      ]
    GitHubListIssues o r s ls -> object
      [ "type" .= ("GitHubListIssues" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "state" .= s
      , "labels" .= ls
      ]
    GitHubCreatePR o r t b h b' -> object
      [ "type" .= ("GitHubCreatePR" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "title" .= t
      , "body" .= b
      , "head" .= h
      , "base" .= b'
      ]
    GitHubGetPR o r n id' -> object
      [ "type" .= ("GitHubGetPR" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "number" .= n
      , "include_details" .= id'
      ]
    GitHubListPullRequests o r s l -> object
      [ "type" .= ("GitHubListPullRequests" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "state" .= s
      , "limit" .= l
      ]
    GitHubGetPullRequestReviews o r n -> object
      [ "type" .= ("GitHubGetPullRequestReviews" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "number" .= n
      ]
    GitHubGetDiscussion o r n -> object
      [ "type" .= ("GitHubGetDiscussion" :: Text)
      , "owner" .= o
      , "repo" .= r
      , "number" .= n
      ]
    GitHubCheckAuth -> object
      [ "type" .= ("GitHubCheckAuth" :: Text)
      ]
    OllamaGenerate m p s -> object
      [ "type" .= ("OllamaGenerate" :: Text)
      , "model" .= m
      , "prompt" .= p
      , "system" .= s
      ]
    OtelSpan tid sid n sns ens attrs -> object
      [ "type" .= ("OtelSpan" :: Text)
      , "trace_id" .= tid
      , "span_id" .= sid
      , "name" .= n
      , "start_ns" .= sns
      , "end_ns" .= ens
      , "attributes" .= attrs
      ]
    OtelMetric n v ls -> object
      [ "type" .= ("OtelMetric" :: Text)
      , "name" .= n
      , "value" .= v
      , "labels" .= ls
      ]

data ServiceResponse
  = AnthropicChatResponse { content :: [Value], stopReason :: Text, usage :: Value }
  | GitHubIssueResponse { issueNumber :: Int, title :: Text, body :: Text, state :: Text, labels :: [Text], url :: Text, issueAuthor :: Text, issueComments :: [Value] }
  | GitHubIssuesResponse { issues :: [Value] }
  | GitHubPRResponse { number :: Int, title :: Text, body :: Text, author :: Text, url :: Text, state :: Text, head_ref_name :: Text, base_ref_name :: Text, created_at :: Text, merged_at :: Maybe Text, prLabels :: [Text], prComments :: [Value], prReviews :: [Value] }
  | GitHubPullRequestsResponse { pull_requests :: [Value] }
  | GitHubReviewsResponse { reviews :: [Value] }
  | GitHubDiscussionResponse { number :: Int, title :: Text, body :: Text, author :: Text, url :: Text, comments :: [Value] }
  | GitHubAuthResponse { authenticated :: Bool, user :: Maybe Text }
  | OllamaGenerateResponse { response :: Text, done :: Bool }
  | OtelAckResponse
  | ErrorResponse { code :: Int, message :: Text }
  deriving (Show, Eq, Generic)

instance FromJSON ServiceResponse where
  parseJSON = withObject "ServiceResponse" $ \v -> do
    t <- v .: "type"
    case (t :: Text) of 
      "AnthropicChatResponse" -> AnthropicChatResponse <$> v .: "content" <*> v .: "stop_reason" <*> v .: "usage"
      "GitHubIssueResponse" -> GitHubIssueResponse <$> v .: "number" <*> v .: "title" <*> v .: "body" <*> v .: "state" <*> v .: "labels" <*> v .: "url" <*> v .: "author" <*> v .:? "comments" .!= []
      "GitHubIssuesResponse" -> GitHubIssuesResponse <$> v .: "issues"
      "GitHubPRResponse" -> GitHubPRResponse <$> v .: "number" <*> v .: "title" <*> v .: "body" <*> v .: "author" <*> v .: "url" <*> v .: "state" <*> v .: "head_ref_name" <*> v .: "base_ref_name" <*> v .: "created_at" <*> v .:? "merged_at" <*> v .:? "labels" .!= [] <*> v .:? "comments" .!= [] <*> v .:? "reviews" .!= []
      "GitHubPullRequestsResponse" -> GitHubPullRequestsResponse <$> v .: "pull_requests"
      "GitHubReviewsResponse" -> GitHubReviewsResponse <$> v .: "reviews"
      "GitHubDiscussionResponse" -> GitHubDiscussionResponse <$> v .: "number" <*> v .: "title" <*> v .: "body" <*> v .: "author" <*> v .: "url" <*> v .: "comments"
      "GitHubAuthResponse" -> GitHubAuthResponse <$> v .: "authenticated" <*> v .: "user"
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
