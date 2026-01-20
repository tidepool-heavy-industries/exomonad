{-# LANGUAGE GADTs, OverloadedStrings, OverloadedRecordDot, TypeOperators, FlexibleContexts, RankNTypes, LambdaCase #-}

module Main where

import Control.Monad.Freer (Eff, run, reinterpret, interpret)
import Control.Monad.Freer.State (gets, runState)
import Data.List (find)
import Data.Text (Text)
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale)
import Test.Tasty hiding (DependencyType)
import Test.Tasty.HUnit

import Tidepool.Effects.BD (BD(..), BeadInfo(..), BeadStatus(..), BeadType(..), DependencyInfo(..), DependencyType(..))
import Tidepool.Effect.Types (Time(..))
import Tidepool.Graph.Goto (unwrapSingleChoice)
import Tidepool.Control.PMReviewDAG

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PMReviewDAG"
  [ testCase "pm_review_dag analyzes simple DAG correctly" test_simpleDAG
  , testCase "pm_review_dag detects priority inversion" test_priorityInversion
  , testCase "pm_review_dag calculates depth and critical path" test_depthAndCriticalPath
  ]

-- | State for mock BD
data MockState = MockState
  { msBeads  :: [BeadInfo]
  , msNow    :: UTCTime
  }

-- | Mock BD + Time interpreter
runMockEffects :: MockState -> Eff '[BD, Time] a -> (a, MockState)
runMockEffects initial eff = 
  let 
    -- 1. Interpret BD into State, leaving Time exposed
    -- Result: Eff '[State MockState, Time] a
    e1 = reinterpret (\case
          ListBeads _ -> gets msBeads
          GetBead bid -> gets (find (\b -> b.biId == bid) . msBeads)
          _ -> error "Not implemented in mock"
          ) eff
    
    -- 2. Run State, which "threads" through the unhandled Time effects
    -- Result: Eff '[Time] (a, MockState)
    e2 = runState initial e1

    -- 3. Interpret Time (now at head) using the initial time (read-only)
    -- Result: Eff '[] (a, MockState)
    e3 = interpret (\case
          GetCurrentTime -> pure initial.msNow
          ) e2
  in run e3

t0 :: UTCTime
t0 = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2026-01-20 10:00:00"

mkBead :: Text -> BeadStatus -> Int -> [Text] -> [DependencyInfo] -> BeadInfo
mkBead bid status priority labels deps = BeadInfo
  { biId = bid
  , biTitle = "Title " <> bid
  , biDescription = Nothing
  , biStatus = status
  , biPriority = priority
  , biType = TypeTask
  , biAssignee = Nothing
  , biCreatedAt = Just t0
  , biCreatedBy = Nothing
  , biUpdatedAt = Just t0
  , biParent = Nothing
  , biLabels = labels
  , biDependencies = deps
  , biDependents = []
  }

mkDep :: Text -> DependencyType -> DependencyInfo
mkDep bid dtype = DependencyInfo
  { diId = bid
  , diTitle = "Dep " <> bid
  , diStatus = StatusOpen
  , diPriority = 2
  , diType = TypeTask
  , diDepType = dtype
  , diLabels = []
  }

test_simpleDAG :: Assertion
test_simpleDAG = do
  let b1 = mkBead "b1" StatusOpen 2 ["ready"] []
      b2 = mkBead "b2" StatusOpen 2 [] [mkDep "b1" DepDependsOn]
      state = MockState [b1, b2] t0
      args = PmReviewDagArgs Nothing Nothing Nothing
      (resChoice, _) = runMockEffects state (pmReviewDagLogic args)
      res = unwrapSingleChoice resChoice
  
  assertEqual "b1 should be ready" ["b1"] res.prdrReady
  assertEqual "b2 should be blocked" 1 (length res.prdrBlocked)
  let blocked = head res.prdrBlocked
  assertEqual "blocked bead should be b2" "b2" blocked.biBeadId
  assertEqual "blocked depth should be 1" 1 blocked.biDepth

test_priorityInversion :: Assertion
test_priorityInversion = do
  -- b1 (P2) blocks b2 (P0) -> Priority Inversion
  let b1 = mkBead "b1" StatusOpen 2 [] []
      b2 = mkBead "b2" StatusOpen 0 [] [mkDep "b1" DepDependsOn]
      state = MockState [b1, b2] t0
      args = PmReviewDagArgs Nothing Nothing Nothing
      (resChoice, _) = runMockEffects state (pmReviewDagLogic args)
      res = unwrapSingleChoice resChoice
  
  assertEqual "Should have 1 priority gap" 1 (length res.prdrPriorityGaps)
  let gap = head res.prdrPriorityGaps
  assertEqual "Blocked should be b2" "b2" gap.pgBlockedId
  assertEqual "Blocker should be b1" "b1" gap.pgBlockerId
  assertEqual "Blocked priority should be 0" 0 gap.pgBlockedPriority
  assertEqual "Blocker priority should be 2" 2 gap.pgBlockerPriority

test_depthAndCriticalPath :: Assertion
test_depthAndCriticalPath = do
  -- b1 -> b2 -> b3
  -- b4 -> b3
  let b1 = mkBead "b1" StatusOpen 2 [] []
      b2 = mkBead "b2" StatusOpen 2 [] [mkDep "b1" DepDependsOn]
      b3 = mkBead "b3" StatusOpen 2 [] [mkDep "b2" DepDependsOn, mkDep "b4" DepDependsOn]
      b4 = mkBead "b4" StatusOpen 2 [] []
      state = MockState [b1, b2, b3, b4] t0
      args = PmReviewDagArgs Nothing Nothing Nothing
      (resChoice, _) = runMockEffects state (pmReviewDagLogic args)
      res = unwrapSingleChoice resChoice
  
  -- b3 depends on b2 (depth 1) and b4 (depth 0). 
  -- depth(b1) = 0
  -- depth(b4) = 0
  -- depth(b2) = 1 + depth(b1) = 1
  -- depth(b3) = 1 + max(depth(b2), depth(b4)) = 2
  
  let b3Info = find (\bi -> bi.biBeadId == "b3") res.prdrBlocked
  assertEqual "b3 depth should be 2" (Just 2) (biDepth <$> b3Info)
  
  assertEqual "Critical path should be [b3, b2, b1]" ["b3", "b2", "b1"] res.prdrCriticalPath
