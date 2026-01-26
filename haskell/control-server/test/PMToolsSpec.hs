{-# LANGUAGE GADTs, OverloadedStrings, OverloadedRecordDot, TypeOperators, FlexibleContexts, RankNTypes, LambdaCase #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad.Freer (Eff, run, reinterpret, send)
import Control.Monad.Freer.State (gets, modify, runState)
import Data.Aeson (toJSON, fromJSON, Result(..))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import ExoMonad.Effects.GitHub
import ExoMonad.Graph.Goto (unwrapSingleChoice)
import ExoMonad.Graph.MCPReify (reifyMCPTools, MCPToolInfo(..))
import ExoMonad.Control.PMTools

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
      , testCase "pmPrioritizeLogic handles multiple issues" test_prioritizeMultiple
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

-- | State for mock GitHub
data MockState = MockState
  { msIssues :: [(Int, Issue)]
  }

-- | Mock GitHub interpreter that uses State to track issues
runMockGitHub :: MockState -> Eff '[GitHub] a -> (a, MockState)
runMockGitHub initial eff = run $ runState initial $ reinterpret (\case
  GetIssue _ num _ -> gets (lookup num . msIssues)
  AddIssueLabel _ num label -> modify $ \s ->
    let newIssues = map (\(n, i) ->
          if n == num
          then (n, i { issueLabels = label : i.issueLabels })
          else (n, i)) s.msIssues
    in s { msIssues = newIssues }
  RemoveIssueLabel _ num label -> modify $ \s ->
    let newIssues = map (\(n, i) ->
          if n == num
          then (n, i { issueLabels = filter (/= label) i.issueLabels })
          else (n, i)) s.msIssues
    in s { msIssues = newIssues }
  UpdateIssue _ num input -> modify $ \s ->
    let newIssues = map (\(n, i) ->
          if n == num
          then (n, applyUpdate input i)
          else (n, i)) s.msIssues
    in s { msIssues = newIssues }
  ListIssues _ _ -> pure []
  CreateIssue _ -> pure 123
  CloseIssue _ num -> pure ()
  ReopenIssue _ num -> pure ()
  ) eff

applyUpdate :: UpdateIssueInput -> Issue -> Issue
applyUpdate input i = i
  { issueTitle = fromMaybe i.issueTitle input.uiiTitle
  , issueBody = fromMaybe i.issueBody input.uiiBody
  , issueState = fromMaybe i.issueState input.uiiState
  }

test_getNoLabels :: Assertion
test_getNoLabels = do
  let issue = (defaultIssue 123) { issueLabels = [] }
  let (res, _) = runMockGitHub (MockState [(123, issue)]) (getWorkflowState (Repo "t/t") 123)
  assertEqual "Should be Nothing" Nothing res

test_getTLReview :: Assertion
test_getTLReview = do
  let issue = (defaultIssue 123) { issueLabels = [labelNeedsTLReview, "other-label"] }
  let (res, _) = runMockGitHub (MockState [(123, issue)]) (getWorkflowState (Repo "t/t") 123)
  assertEqual "Should be NeedsTLReview" (Just NeedsTLReview) res

test_setWorkflow :: Assertion
test_setWorkflow = do
  -- Start with PM Approval, set to Ready
  let initialIssue = (defaultIssue 123) { issueLabels = [labelNeedsPMApproval, "some-tag"] }
  let (res, finalState) = runMockGitHub (MockState [(123, initialIssue)]) $ do
        setWorkflowState (Repo "t/t") 123 Ready
        getWorkflowState (Repo "t/t") 123
  
  assertEqual "Should now be Ready" (Just Ready) res
  let finalLabels = (snd $ head finalState.msIssues).issueLabels
  assertBool "Old label should be removed" (labelNeedsPMApproval `notElem` finalLabels)
  assertBool "New label should be added" (labelReady `elem` finalLabels)
  assertBool "Other labels should be preserved" ("some-tag" `elem` finalLabels)

test_prioritize :: Assertion
test_prioritize = do
  let initialIssue = (defaultIssue 123) 
        { issueTitle = "Test Issue"
        , issueBody = "Original description"
        , issueLabels = ["P2"]
        }
  let initialState = MockState [(123, initialIssue)]
  let (resChoice, finalState) = runMockGitHub initialState $ do
        pmPrioritizeLogic $ PmPrioritizeArgs
          [ PrioritizeItem 123 4 "Urgent fix needed" ]
  let res = unwrapSingleChoice resChoice

  assertEqual "Should have 1 result" 1 (length res.pprResults)
  let resultItem = head res.pprResults
  assertEqual "Should be success" True resultItem.priSuccess

  let finalIssue = snd $ head finalState.msIssues
  assertBool "Priority label P4 should be added" ("P4" `elem` finalIssue.issueLabels)
  assertBool "Priority label P2 should be removed" ("P2" `notElem` finalIssue.issueLabels)
  let desc = finalIssue.issueBody
  assertBool "Description should contain rationale" ("Urgent fix needed" `T.isInfixOf` desc)
  assertBool "Description should contain header" ("## Priority History" `T.isInfixOf` desc)

test_prioritizeMultiple :: Assertion
test_prioritizeMultiple = do
  let i1 = (defaultIssue 1) { issueBody = "D1", issueLabels = ["P2"] }
  let i2 = (defaultIssue 2) { issueBody = "D2", issueLabels = ["P2"] }
  let initialState = MockState [(1, i1), (2, i2)]
  let (resChoice, finalState) = runMockGitHub initialState $ do
        pmPrioritizeLogic $ PmPrioritizeArgs
          [ PrioritizeItem 1 1 "Lowering"
          , PrioritizeItem 2 3 "Raising"
          , PrioritizeItem 3 0 "Missing"
          ]
  let res = unwrapSingleChoice resChoice

  assertEqual "Should have 3 results" 3 (length res.pprResults)

  let r1 = findResult 1 res.pprResults
  let r2 = findResult 2 res.pprResults
  let r3 = findResult 3 res.pprResults

  assertEqual "1 success" (Just True) ((\r -> r.priSuccess) <$> r1)
  assertEqual "2 success" (Just True) ((\r -> r.priSuccess) <$> r2)
  assertEqual "3 success" (Just False) ((\r -> r.priSuccess) <$> r3)

  let i1Final = lookup 1 finalState.msIssues
  let i2Final = lookup 2 finalState.msIssues

  assertBool "1 should have P1" (maybe False (\i -> "P1" `elem` i.issueLabels) i1Final)
  assertBool "2 should have P3" (maybe False (\i -> "P3" `elem` i.issueLabels) i2Final)
  where
    findResult num = find (\r -> r.priIssueNum == num)

test_prioritizeValidation :: Assertion
test_prioritizeValidation = do
  let initialState = MockState []
  let (resChoice, _) = runMockGitHub initialState $ do
        pmPrioritizeLogic $ PmPrioritizeArgs
          [ PrioritizeItem 1 5 "Too high"
          , PrioritizeItem 2 (-1) "Too low"
          ]
  let res = unwrapSingleChoice resChoice
  assertEqual "Should have 2 results" 2 (length res.pprResults)
  assertBool "1 should fail" (not (head res.pprResults).priSuccess)
  assertBool "2 should fail" (not (res.pprResults !! 1).priSuccess)
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
  let res = PmPrioritizeResult [PrioritizeResultItem 1 True Nothing]
  let json = toJSON res
  case fromJSON @PmPrioritizeResult json of
    Success res' -> assertEqual "Should roundtrip" res res'
    Error err -> assertFailure $ "Failed to parse: " ++ err

defaultIssue :: Int -> Issue
defaultIssue num = Issue
  {
  issueNumber = num
  , issueTitle = ""
  , issueBody = ""
  , issueState = IssueOpen
  , issueLabels = []
  , issueAuthor = Author "ghost" Nothing
  , issueUrl = ""
  }