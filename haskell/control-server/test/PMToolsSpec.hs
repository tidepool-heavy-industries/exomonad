{-# LANGUAGE GADTs, OverloadedStrings, OverloadedRecordDot, TypeOperators, FlexibleContexts, RankNTypes, LambdaCase #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad.Freer (Eff, run, reinterpret)
import Control.Monad.Freer.State (gets, modify, runState)
import Data.Aeson (toJSON, fromJSON, Result(..))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Tidepool.Effects.BD (BD(..), BeadInfo(..), BeadStatus(..), BeadType(..), UpdateBeadInput(..))
import Tidepool.Graph.Goto (unwrapSingleChoice)
import Tidepool.Graph.MCPReify (reifyMCPTools, MCPToolInfo(..))
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
      , testCase "pmPrioritizeLogic validates priority range" test_prioritizeValidation
      ]
  , testGroup "Rationale Appending"
      [ testCase "appendRationale handles Nothing" test_appendNothing
      , testCase "appendRationale handles empty string" test_appendEmpty
      , testCase "appendRationale handles whitespace" test_appendWhitespace
      , testCase "appendRationale appends to existing history" test_appendExisting
      , testCase "appendRationale ensures newline before appending" test_appendNoNewline
      ]
  , testGroup "MCP Discovery & Serialization"
      [ testCase "pm_prioritize is discoverable" test_discovery_pm
      , testCase "pm_prioritize result serialization" test_serialization_pm
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

test_prioritizeValidation :: Assertion
test_prioritizeValidation = do
  let initialState = MockState [] []
  let (resChoice, _) = runMockBD initialState $ do
        pmPrioritizeLogic $ PmPrioritizeArgs
          [ PrioritizeItem "b1" 5 "Too high"
          , PrioritizeItem "b2" (-1) "Too low"
          ]
  let res = unwrapSingleChoice resChoice
  assertEqual "Should have 2 results" 2 (length res.pprResults)
  assertBool "b1 should fail" (not (head res.pprResults).priSuccess)
  assertBool "b2 should fail" (not (res.pprResults !! 1).priSuccess)
  assertEqual "Error message" (Just "Invalid priority: must be between 0 and 4") (head res.pprResults).priError

test_appendNothing :: Assertion
test_appendNothing = do
  let res = appendRationale Nothing 3 "Reason"
  assertEqual "Result" "## Priority History\n- Priority 3: Reason" res

test_appendEmpty :: Assertion
test_appendEmpty = do
  let res = appendRationale (Just "") 3 "Reason"
  assertEqual "Result" "## Priority History\n- Priority 3: Reason" res

test_appendWhitespace :: Assertion
test_appendWhitespace = do
  let res = appendRationale (Just "  \n ") 3 "Reason"
  assertEqual "Result" "## Priority History\n- Priority 3: Reason" res

test_appendExisting :: Assertion
test_appendExisting = do
  let orig = "## Priority History\n- Priority 2: Old"
  let res = appendRationale (Just orig) 3 "New"
  assertEqual "Result" "## Priority History\n- Priority 2: Old\n- Priority 3: New" res

test_appendNoNewline :: Assertion
test_appendNoNewline = do
  let orig = "Some desc\n## Priority History\n- Priority 2: Old"
  let res = appendRationale (Just orig) 3 "New"
  assertEqual "Result" "Some desc\n## Priority History\n- Priority 2: Old\n- Priority 3: New" res

test_discovery_pm :: Assertion
test_discovery_pm = do
  let pmTools = reifyMCPTools (Proxy @PmPrioritizeGraph)
  assertEqual "Should find one tool" 1 (length pmTools)
  let tool = head pmTools
  assertEqual "Name should be pm_prioritize" "pm_prioritize" tool.mtdName

test_serialization_pm :: Assertion
test_serialization_pm = do
  let res = PmPrioritizeResult [PrioritizeResultItem "b1" True Nothing]
  let json = toJSON res
  case fromJSON @PmPrioritizeResult json of
    Success res' -> assertEqual "Should roundtrip" res res'
    Error err -> assertFailure $ "Failed to parse: " ++ err
  