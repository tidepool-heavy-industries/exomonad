{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Middleware.Logging
  ( logLLMCalls,
    logGitHubCalls,
  )
where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import ExoMonad.Effect.Log
import ExoMonad.Effects.GitHub
import ExoMonad.LLM.Effect
import Polysemy
import Polysemy.Internal (send)

-- | Intercept LLM calls to log requests and responses.
logLLMCalls :: (Member LLMCall r, Member Log r) => Sem r a -> Sem r a
logLLMCalls = intercept $ \case
  PerformLLMCall mdl maxTok sys usr schema -> do
    logInfoWith "LLM Request" $
      [ ("model", modelToText mdl),
        ("system", unSystem sys),
        ("user", unUser usr)
      ]
    -- Re-emit the effect
    result <- send (PerformLLMCall mdl maxTok sys usr schema)
    case result of
      Left err -> logError $ "LLM Error: " <> T.pack (show err)
      Right out -> logInfo $ "LLM Response: " <> T.pack (show out)
    pure result
  PerformLLMCallWithTools mdl maxTok sys usr schema tools -> do
    logInfoWith "LLM Request (with tools)" $
      [ ("model", modelToText mdl),
        ("system", unSystem sys),
        ("user", unUser usr),
        ("tool_count", T.pack $ show $ length tools)
      ]
    result <- send (PerformLLMCallWithTools mdl maxTok sys usr schema tools)
    case result of
      Left err -> logError $ "LLM Error: " <> T.pack (show err)
      Right out -> logInfo $ "LLM Response: " <> T.pack (show out)
    pure result

-- | Intercept GitHub calls to log operations.
logGitHubCalls :: (Member GitHub r, Member Log r) => Sem r a -> Sem r a
logGitHubCalls = intercept $ \case
  ListIssues repo -> do
    logInfo $ "Listing issues for " <> repo
    send (ListIssues repo)
  GetIssue repo num -> do
    logInfo $ "Getting issue " <> repo <> "#" <> T.pack (show num)
    send (GetIssue repo num)
  CreatePR repo title body branch base -> do
    logInfo $ "Creating PR in " <> repo <> ": " <> title
    send (CreatePR repo title body branch base)
  ListPRs repo -> do
    logInfo $ "Listing PRs for " <> repo
    send (ListPRs repo)
  GetPRForBranch repo branch -> do
    logInfo $ "Getting PR for branch " <> branch <> " in " <> repo
    send (GetPRForBranch repo branch)
  GetPRReviewComments repo num -> do
    logInfo $ "Getting review comments for " <> repo <> "#" <> T.pack (show num)
    send (GetPRReviewComments repo num)
