{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Freer (Eff, run, reinterpret)
import Control.Monad.Freer.State (modify, runState)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Tidepool.Effects.GitHub
import Tidepool.Effects.Env
import Tidepool.Control.PMTools (labelNeedsTLReview)
import Tidepool.Control.PMPropose

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PMPropose"
  [ testCase "creates issue with correct details" test_createsIssue
  , testCase "includes needs-tl-review label" test_includesLabel
  , testCase "formats description correctly" test_formatsDescription
  ]

-- | Mock State to capture created issues
data MockState = MockState
  { createdIssues :: [CreateIssueInput]
  }

initialState :: MockState
initialState = MockState []

-- | Mock GitHub and Env interpreter
runMockStack :: Eff '[GitHub, Env] a -> (a, MockState)
runMockStack eff = run $ runState initialState $ reinterpret (\case
  GetEnv _ -> pure Nothing
  ) $ reinterpret (\case
  CreateIssue input -> do
    modify $ \s -> s { createdIssues = input : createdIssues s }
    pure 123
  GetRepo _ -> pure $ Repo "tidepool/tidepool"
  _ -> error "Not implemented in mock"
  ) eff

test_createsIssue :: Assertion
test_createsIssue = do
  let args = PMProposeArgs
        { ppaTitle = "My Feature"
        , ppaIntent = "Implement X"
        , ppaSuggestedPriority = Just 1
        , ppaSuggestedLabels = Just ["frontend"]
        , ppaContext = Nothing
        , ppaScopeHint = Nothing
        , ppaRepo = Nothing
        }
  
  let (_, state) = runMockStack $ pmProposeLogic args
  
  case createdIssues state of
    [input] -> do
      assertEqual "Title matches" "My Feature" input.ciiTitle
      assertBool "Priority label P1 should be added" ("P1" `elem` input.ciiLabels)
    [] -> assertFailure "No issue created"
    _ -> assertFailure "Multiple issues created"

test_includesLabel :: Assertion
test_includesLabel = do
  let args = PMProposeArgs
        { ppaTitle = "My Feature"
        , ppaIntent = "Implement X"
        , ppaSuggestedPriority = Nothing
        , ppaSuggestedLabels = Just ["frontend"]
        , ppaContext = Nothing
        , ppaScopeHint = Nothing
        , ppaRepo = Nothing
        }
  
  let (_, state) = runMockStack $ pmProposeLogic args
  
  case createdIssues state of
    [input] -> do
      assertBool "Includes needs-tl-review" (labelNeedsTLReview `elem` input.ciiLabels)
      assertBool "Includes suggested label" ("frontend" `elem` input.ciiLabels)
    _ -> assertFailure "Expected one issue created"

test_formatsDescription :: Assertion
test_formatsDescription = do
  let args = PMProposeArgs
        { ppaTitle = "My Feature"
        , ppaIntent = "Do the thing"
        , ppaSuggestedPriority = Nothing
        , ppaSuggestedLabels = Nothing
        , ppaContext = Just "Because user said so"
        , ppaScopeHint = Just "Small"
        , ppaRepo = Nothing
        }
  
  let (_, state) = runMockStack $ pmProposeLogic args
  
  case createdIssues state of
    [input] -> do
      let d = input.ciiBody
      assertBool "Contains intent" ("Do the thing" `T.isInfixOf` d)
      assertBool "Contains context header" ("## Context" `T.isInfixOf` d)
      assertBool "Contains context body" ("Because user said so" `T.isInfixOf` d)
      assertBool "Contains scope header" ("## Scope Hint" `T.isInfixOf` d)
      assertBool "Contains scope body" ("Small" `T.isInfixOf` d)
    _ -> assertFailure "Expected one issue created"