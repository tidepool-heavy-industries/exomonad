{-# LANGUAGE GADTs, OverloadedStrings, OverloadedRecordDot, TypeOperators, FlexibleContexts, RankNTypes, LambdaCase #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad.Freer (Eff, run, reinterpret)
import Control.Monad.Freer.State (get, gets, modify, runState)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Tidepool.Effects.BD (BD(..), BeadInfo(..), BeadStatus(..), BeadType(..), UpdateBeadInput(..))
import Tidepool.Graph.Goto (unwrapSingleChoice)
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
  , testGroup "Prioritize"
      [ testCase "pmPrioritizeLogic updates priority and description" test_prioritize
      , testCase "pmPrioritizeLogic handles multiple beads" test_prioritizeMultiple
      ]
  ]

-- | State for mock BD
data MockState = MockState
  { msLabels :: [Text]
  , msBeads  :: [(Text, BeadInfo)]
  }

-- | Mock BD interpreter that uses State to track labels and beads
runMockBD :: MockState -> Eff '[BD] a -> (a, MockState)
runMockBD initial eff = run $ runState initial $ reinterpret (\case
  GetLabels _ -> gets msLabels
  AddLabel _ label -> modify (\s -> s { msLabels = label : s.msLabels })
  RemoveLabel _ label -> modify (\s -> s { msLabels = filter (/= label) s.msLabels })
  GetBead bid -> gets (lookup bid . msBeads)
  UpdateBead bid input -> modify $ \s ->
    let newBeads = map (\(bid', info) ->
          if bid' == bid
          then (bid', applyUpdate input info)
          else (bid', info)) s.msBeads
    in s { msBeads = newBeads }
  ListByStatus _ -> pure []
  ListByType _ -> pure []
  GetDeps _ -> pure []
  GetBlocking _ -> pure []
  GetChildren _ -> pure []
  CreateBead _ -> pure "new-bead"
  CloseBead _ -> pure ()
  ReopenBead _ -> pure ()
  AddDep _ _ _ -> pure ()
  RemoveDep _ _ -> pure ()
  Sync -> pure ()
  ) eff

applyUpdate :: UpdateBeadInput -> BeadInfo -> BeadInfo
applyUpdate input info = info
  { biTitle = fromMaybe info.biTitle input.ubiTitle
  , biDescription = input.ubiDescription <|> info.biDescription
  , biStatus = fromMaybe info.biStatus input.ubiStatus
  , biPriority = fromMaybe info.biPriority input.ubiPriority
  , biAssignee = input.ubiAssignee <|> info.biAssignee
  }

test_getNoLabels :: Assertion
test_getNoLabels = do
  let (res, _) = runMockBD (MockState [] []) (getWorkflowState "test-bead")
  assertEqual "Should be Nothing" Nothing res

test_getTLReview :: Assertion
test_getTLReview = do
  let (res, _) = runMockBD (MockState [labelNeedsTLReview, "other-label"] []) (getWorkflowState "test-bead")
  assertEqual "Should be NeedsTLReview" (Just NeedsTLReview) res

test_setWorkflow :: Assertion
test_setWorkflow = do
  -- Start with PM Approval, set to Ready
  let initialLabels = [labelNeedsPMApproval, "some-tag"]
  let (res, finalState) = runMockBD (MockState initialLabels []) $ do
        setWorkflowState "test-bead" Ready
        getWorkflowState "test-bead"
  
  assertEqual "Should now be Ready" (Just Ready) res
  let finalLabels = finalState.msLabels
  assertBool "Old label should be removed" (labelNeedsPMApproval `notElem` finalLabels)
  assertBool "New label should be added" (labelReady `elem` finalLabels)
  assertBool "Other labels should be preserved" ("some-tag" `elem` finalLabels)

test_prioritize :: Assertion
test_prioritize = do
  let initialBead = BeadInfo
        { biId = "tidepool-123"
        , biTitle = "Test Bead"
        , biDescription = Just "Original description"
        , biStatus = StatusOpen
        , biPriority = 2
        , biType = TypeTask
        , biAssignee = Nothing
        , biCreatedAt = Nothing
        , biCreatedBy = Nothing
        , biUpdatedAt = Nothing
        , biParent = Nothing
        , biDependencies = []
        , biDependents = []
        }
  let initialState = MockState [] [("tidepool-123", initialBead)]
  let (resChoice, finalState) = runMockBD initialState $ do
        pmPrioritizeLogic $ PmPrioritizeArgs
          [ PrioritizeItem "tidepool-123" 4 "Urgent fix needed" ]
  let res = unwrapSingleChoice resChoice

  assertEqual "Should have 1 result" 1 (length res.pprResults)
  let resultItem = head res.pprResults
  assertEqual "Should be success" True resultItem.priSuccess

  let finalBead = snd $ head finalState.msBeads
  assertEqual "Priority should be 4" 4 finalBead.biPriority
  let desc = fromMaybe "" finalBead.biDescription
  assertBool "Description should contain rationale" ("Urgent fix needed" `T.isInfixOf` desc)
  assertBool "Description should contain header" ("## Priority History" `T.isInfixOf` desc)

test_prioritizeMultiple :: Assertion
test_prioritizeMultiple = do
  let bead1 = BeadInfo "b1" "T1" (Just "D1") StatusOpen 2 TypeTask Nothing Nothing Nothing Nothing Nothing [] []
  let bead2 = BeadInfo "b2" "T2" (Just "D2") StatusOpen 2 TypeTask Nothing Nothing Nothing Nothing Nothing [] []
  let initialState = MockState [] [("b1", bead1), ("b2", bead2)]

  let (resChoice, finalState) = runMockBD initialState $ do
        pmPrioritizeLogic $ PmPrioritizeArgs
          [ PrioritizeItem "b1" 1 "Lowering"
          , PrioritizeItem "b2" 3 "Raising"
          , PrioritizeItem "b3" 0 "Missing"
          ]
  let res = unwrapSingleChoice resChoice

  assertEqual "Should have 3 results" 3 (length res.pprResults)

  let r1 = findResult "b1" res.pprResults
  let r2 = findResult "b2" res.pprResults
  let r3 = findResult "b3" res.pprResults

  assertEqual "b1 success" (Just True) ((\r -> r.priSuccess) <$> r1)
  assertEqual "b2 success" (Just True) ((\r -> r.priSuccess) <$> r2)
  assertEqual "b3 success" (Just False) ((\r -> r.priSuccess) <$> r3)

  let b1Final = lookup "b1" finalState.msBeads
  let b2Final = lookup "b2" finalState.msBeads

  assertEqual "b1 priority" (Just 1) ((\b -> b.biPriority) <$> b1Final)
  assertEqual "b2 priority" (Just 3) ((\b -> b.biPriority) <$> b2Final)
  where
    findResult bid = find (\r -> r.priBeadId == bid)