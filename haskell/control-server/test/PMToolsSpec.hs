{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Freer (Eff, run, reinterpret)
import Control.Monad.Freer.State (get, modify, runState)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Tidepool.Effects.BD (BD(..))
import Tidepool.Control.PMTools

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PMTools"
  [ testGroup "WorkflowState"
      [ testCase "getWorkflowState returns Nothing when no labels" test_getNoLabels
      , testCase "getWorkflowState returns NeedsTLReview" test_getTLReview
      , testCase "setWorkflowState updates labels correctly" test_setWorkflow
      ]
  ]

-- | Mock BD interpreter that uses State to track labels
runMockBD :: [Text] -> Eff '[BD] a -> (a, [Text])
runMockBD initialLabels eff = run $ runState initialLabels $ reinterpret (\case
  GetLabels _ -> get
  AddLabel _ label -> modify (label :)
  RemoveLabel _ label -> modify (filter (/= label))
  ListByStatus _ -> pure []
  ListByType _ -> pure []
  GetBead _ -> pure Nothing
  GetDeps _ -> pure []
  GetBlocking _ -> pure []
  GetChildren _ -> pure []
  CreateBead _ -> pure "new-bead"
  UpdateBead _ _ -> pure ()
  CloseBead _ -> pure ()
  ReopenBead _ -> pure ()
  AddDep _ _ _ -> pure ()
  RemoveDep _ _ -> pure ()
  Sync -> pure ()
  ) eff

test_getNoLabels :: Assertion
test_getNoLabels = do
  let (res, _) = runMockBD [] (getWorkflowState "test-bead")
  assertEqual "Should be Nothing" Nothing res

test_getTLReview :: Assertion
test_getTLReview = do
  let (res, _) = runMockBD [labelNeedsTLReview, "other-label"] (getWorkflowState "test-bead")
  assertEqual "Should be NeedsTLReview" (Just NeedsTLReview) res

test_setWorkflow :: Assertion
test_setWorkflow = do
  -- Start with PM Approval, set to Ready
  let initialLabels = [labelNeedsPMApproval, "some-tag"]
  let (res, finalLabels) = runMockBD initialLabels $ do
        setWorkflowState "test-bead" Ready
        getWorkflowState "test-bead"
  
  assertEqual "Should now be Ready" (Just Ready) res
  assertBool "Old label should be removed" (labelNeedsPMApproval `notElem` finalLabels)
  assertBool "New label should be added" (labelReady `elem` finalLabels)
  assertBool "Other labels should be preserved" ("some-tag" `elem` finalLabels)