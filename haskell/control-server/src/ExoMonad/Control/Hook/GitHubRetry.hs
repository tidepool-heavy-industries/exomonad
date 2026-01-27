{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- | GitHub retry logic with exponential backoff.
module ExoMonad.Control.Hook.GitHubRetry
  ( RetryConfig(..)
  , defaultRetryConfig
  , withRetry
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Freer (Eff, Member, interpose, send, sendM, LastMember)
import Data.Text (Text)
import qualified Data.Text as T
import OpenTelemetry.Trace
import OpenTelemetry.Context.ThreadLocal (getContext)

import ExoMonad.Effects.GitHub (GitHub(..), GitHubError(..), isRetryable, Repo(..))
import ExoMonad.Effect.Types (Log, logWarn)

-- | Configuration for GitHub retries.
data RetryConfig = RetryConfig
  { maxRetries    :: Int
  , baseDelayUs   :: Int
  , maxDelayUs    :: Int
  , tracer        :: Maybe Tracer
  } 

instance Show RetryConfig where
  show c = "RetryConfig { maxRetries = " ++ show c.maxRetries ++ " }"

instance Eq RetryConfig where
  a == b = a.maxRetries == b.maxRetries && a.baseDelayUs == b.baseDelayUs

-- | Default retry configuration: 3 retries, starting at 1s, capped at 8s.
defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig
  { maxRetries    = 3
  , baseDelayUs   = 1000000 -- 1s
  , maxDelayUs    = 8000000 -- 8s
  , tracer        = Nothing
  }

-- | Retry a GitHub operation with exponential backoff.
--
-- Only retries transient errors as defined by 'isRetryable'.
-- Log each retry attempt with attempt number and delay.
-- Also records OpenTelemetry spans if a tracer is provided.
withRetry
  :: forall es a. (Member GitHub es, Member Log es, LastMember IO es)
  => RetryConfig
  -> Eff es a
  -> Eff es a
withRetry config = interpose $ \(op :: GitHub x) -> do
  let 
    -- Helper to run an attempt with tracing and retrying for Either GitHubError results
    runEitherAttempt 
      :: forall b. GitHub (Either GitHubError b) 
      -> Int 
      -> Int 
      -> Eff es (Either GitHubError b)
    runEitherAttempt operation attempt delay = do
      res <- case config.tracer of
        Nothing -> send operation
        Just t -> do
          ctx <- sendM getContext
          let spanName = "github." <> getOpName operation
          span <- sendM $ createSpan t ctx spanName defaultSpanArguments
          
          sendM $ addAttribute span "github.op" (getOpName operation)
          sendM $ addAttribute span "github.retry_count" (attempt - 1)
          addOpAttributes span operation
          
          r <- send operation
          
          case r of
            Left err -> sendM $ do
              addAttribute span "error" True
              addAttribute span "error.message" (T.pack (show err))
            Right _ -> pure ()
            
          sendM $ endSpan span Nothing
          pure r

      case res of
        Left err | isRetryable err && attempt <= config.maxRetries -> do
          logWarn $ "[GitHub] Request failed (attempt " 
                 <> T.pack (show attempt) <> "/" <> T.pack (show config.maxRetries) 
                 <> "), retrying in " <> T.pack (show (delay `div` 1000000)) 
                 <> "s: " <> T.pack (show err)
          sendM $ threadDelay delay
          runEitherAttempt operation (attempt + 1) (min config.maxDelayUs (delay * 2))
        _ -> pure res

  case op of
    CreateIssue {} -> runEitherAttempt op 1 (baseDelayUs config)
    UpdateIssue {} -> runEitherAttempt op 1 (baseDelayUs config)
    CloseIssue {} -> runEitherAttempt op 1 (baseDelayUs config)
    ReopenIssue {} -> runEitherAttempt op 1 (baseDelayUs config)
    AddIssueLabel {} -> runEitherAttempt op 1 (baseDelayUs config)
    RemoveIssueLabel {} -> runEitherAttempt op 1 (baseDelayUs config)
    AddIssueAssignee {} -> runEitherAttempt op 1 (baseDelayUs config)
    RemoveIssueAssignee {} -> runEitherAttempt op 1 (baseDelayUs config)
    GetIssue {} -> runEitherAttempt op 1 (baseDelayUs config)
    ListIssues {} -> runEitherAttempt op 1 (baseDelayUs config)
    CreatePR {} -> runEitherAttempt op 1 (baseDelayUs config)
    GetPullRequest {} -> runEitherAttempt op 1 (baseDelayUs config)
    ListPullRequests {} -> runEitherAttempt op 1 (baseDelayUs config)
    GetPullRequestReviews {} -> runEitherAttempt op 1 (baseDelayUs config)
    CheckAuth -> case config.tracer of
      Nothing -> send op
      Just t -> do
        ctx <- sendM getContext
        span <- sendM $ createSpan t ctx "github.check_auth" defaultSpanArguments
        res <- send op
        sendM $ endSpan span Nothing
        pure res

-- | Helper to get operation name for tracing.
getOpName :: GitHub r -> Text
getOpName = \case
  CreateIssue _ -> "create_issue"
  UpdateIssue _ _ _ -> "update_issue"
  CloseIssue _ _ -> "close_issue"
  ReopenIssue _ _ -> "reopen_issue"
  AddIssueLabel _ _ _ -> "add_label"
  RemoveIssueLabel _ _ _ -> "remove_label"
  AddIssueAssignee _ _ _ -> "add_assignee"
  RemoveIssueAssignee _ _ _ -> "remove_assignee"
  GetIssue _ _ _ -> "get_issue"
  ListIssues _ _ -> "list_issues"
  CreatePR _ -> "create_pr"
  GetPullRequest _ _ _ -> "get_pr"
  ListPullRequests _ _ -> "list_prs"
  GetPullRequestReviews _ _ -> "get_pr_reviews"
  CheckAuth -> "check_auth"

-- | Helper to add operation-specific attributes.
addOpAttributes :: LastMember IO es => Span -> GitHub r -> Eff es ()
addOpAttributes span = \case
  GetIssue _ num _ -> sendM $ addAttribute span "github.issue_number" (fromIntegral num :: Int)
  UpdateIssue _ num _ -> sendM $ addAttribute span "github.issue_number" (fromIntegral num :: Int)
  CloseIssue _ num -> sendM $ addAttribute span "github.issue_number" (fromIntegral num :: Int)
  ReopenIssue _ num -> sendM $ addAttribute span "github.issue_number" (fromIntegral num :: Int)
  ListIssues repo _ -> sendM $ addAttribute span "github.repo" repo.unRepo
  GetPullRequest _ num _ -> sendM $ addAttribute span "github.pr_number" (fromIntegral num :: Int)
  _ -> pure ()