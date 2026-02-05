{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Middleware.Logging
  ( logLLMCalls,
    logGitHubCalls,
  )
where

import Data.Aeson (ToJSON, toJSON)
import Data.Text qualified as T
import ExoMonad.Effect.Log
import ExoMonad.Effects.GitHub
import ExoMonad.LLM.Effect
import ExoMonad.LLM.Types (modelToText)
import Polysemy
import Polysemy.Internal (send)

-- | Intercept LLM calls to log requests and responses.
logLLMCalls :: (Member LLMCall r, Member Log r) => Sem r a -> Sem r a
logLLMCalls = intercept $ \case
  PerformLLMCall mdl maxTok (System sys) (User usr) schema -> do
    logInfoWith "LLM Request"
      [ ("model", toJSON $ modelToText mdl),
        ("system", toJSON sys),
        ("user", toJSON usr)
      ]
    -- Re-emit the effect
    result <- send (PerformLLMCall mdl maxTok (System sys) (User usr) schema)
    case result of
      Left err -> logError $ "LLM Error: " <> T.pack (show err)
      Right _ -> logInfo "LLM Response: success"
    pure result
  PerformLLMCallWithTools mdl maxTok (System sys) (User usr) schema tools -> do
    logInfoWith "LLM Request (with tools)"
      [ ("model", toJSON $ modelToText mdl),
        ("system", toJSON sys),
        ("user", toJSON usr),
        ("tool_count", toJSON $ length tools)
      ]
    result <- send (PerformLLMCallWithTools mdl maxTok (System sys) (User usr) schema tools)
    case result of
      Left err -> logError $ "LLM Error: " <> T.pack (show err)
      Right _ -> logInfo "LLM Response: success"
    pure result

-- | Intercept GitHub calls to log operations.
logGitHubCalls :: (Member GitHub r, Member Log r) => Sem r a -> Sem r a
logGitHubCalls = intercept $ \case
  CreateIssue input -> do
    logInfo $ "Creating issue in " <> input.ciiRepo.unRepo <> ": " <> input.ciiTitle
    send (CreateIssue input)
  UpdateIssue repo num input -> do
    logInfo $ "Updating issue " <> repo.unRepo <> "#" <> T.pack (show num)
    send (UpdateIssue repo num input)
  CloseIssue repo num -> do
    logInfo $ "Closing issue " <> repo.unRepo <> "#" <> T.pack (show num)
    send (CloseIssue repo num)
  ReopenIssue repo num -> do
    logInfo $ "Reopening issue " <> repo.unRepo <> "#" <> T.pack (show num)
    send (ReopenIssue repo num)
  AddIssueLabel repo num label -> do
    logInfo $ "Adding label " <> label <> " to " <> repo.unRepo <> "#" <> T.pack (show num)
    send (AddIssueLabel repo num label)
  RemoveIssueLabel repo num label -> do
    logInfo $ "Removing label " <> label <> " from " <> repo.unRepo <> "#" <> T.pack (show num)
    send (RemoveIssueLabel repo num label)
  AddIssueAssignee repo num assignee -> do
    logInfo $ "Adding assignee " <> assignee <> " to " <> repo.unRepo <> "#" <> T.pack (show num)
    send (AddIssueAssignee repo num assignee)
  RemoveIssueAssignee repo num assignee -> do
    logInfo $ "Removing assignee " <> assignee <> " from " <> repo.unRepo <> "#" <> T.pack (show num)
    send (RemoveIssueAssignee repo num assignee)
  GetIssue repo num includeComments -> do
    logInfo $ "Getting issue " <> repo.unRepo <> "#" <> T.pack (show num)
    send (GetIssue repo num includeComments)
  ListIssues repo filter_ -> do
    logInfo $ "Listing issues for " <> repo.unRepo
    send (ListIssues repo filter_)
  CreatePR spec -> do
    logInfo $ "Creating PR in " <> spec.prcsRepo.unRepo <> ": " <> spec.prcsTitle
    send (CreatePR spec)
  GetPullRequest repo num includeComments -> do
    logInfo $ "Getting PR " <> repo.unRepo <> "#" <> T.pack (show num)
    send (GetPullRequest repo num includeComments)
  ListPullRequests repo filter_ -> do
    logInfo $ "Listing PRs for " <> repo.unRepo
    send (ListPullRequests repo filter_)
  GetPullRequestReviews repo num -> do
    logInfo $ "Getting review comments for " <> repo.unRepo <> "#" <> T.pack (show num)
    send (GetPullRequestReviews repo num)
  GetDiscussion repo num -> do
    logInfo $ "Getting discussion " <> repo.unRepo <> "#" <> T.pack (show num)
    send (GetDiscussion repo num)
  CheckAuth -> do
    logInfo "Checking GitHub auth"
    send CheckAuth
