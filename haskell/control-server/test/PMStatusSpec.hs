{-# LANGUAGE GADTs, OverloadedStrings, OverloadedRecordDot, TypeOperators, FlexibleContexts, RankNTypes, LambdaCase, DataKinds #-}

module Main where

import Control.Monad.Freer (Eff, run, interpret)
import Data.Text (Text)
import Data.Time (UTCTime, nominalDay, addUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Test.Tasty
import Test.Tasty.HUnit

import Tidepool.Effects.BD (BD(..), BeadInfo(..), BeadStatus(..), BeadType(..))
import Tidepool.Effects.GitHub (GitHub(..), PullRequest(..), PRState(..), Author(..))
import Tidepool.Effect.Types (Time(..))
import Tidepool.Graph.Goto (unwrapSingleChoice)
import Tidepool.Control.PMStatus
import Tidepool.Control.PMTools (labelReady, labelNeedsTLReview, labelNeedsPMApproval)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PMStatus"
  [ testCase "pmStatusLogic calculates correct metrics" test_pmStatus
  ]

-- | Mock state
data MockState = MockState
  { msNow   :: UTCTime
  , msBeads :: [BeadInfo]
  , msPrs   :: [PullRequest]
  }

runMockAll :: MockState -> Eff [BD, GitHub, Time] a -> a
runMockAll initial eff = run $ 
  interpret (\case
    GetCurrentTime -> pure initial.msNow
  ) $
  interpret (\case
    ListPullRequests _ _ -> pure initial.msPrs
    GetPullRequest _ _ _ -> pure Nothing
    GetIssue _ _ _ -> pure Nothing
    ListIssues _ _ -> pure []
    CreatePR _ -> error "CreatePR not mocked"
    CreateIssue _ _ _ _ -> error "CreateIssue not mocked"
    GetPullRequestReviews _ _ -> pure []
    CheckAuth -> pure True
  ) $
  interpret (\case
    ListBeads _ -> pure initial.msBeads
    GetBead _ -> pure Nothing
    GetDeps _ -> pure []
    GetBlocking _ -> pure []
    GetLabels _ -> pure []
    GetChildren _ -> pure []
    ListByStatus _ -> pure []
    ListByType _ -> pure []
    CreateBead _ -> pure "new"
    UpdateBead _ _ -> pure ()
    CloseBead _ _ -> pure ()
    ReopenBead _ -> pure ()
    AddLabel _ _ -> pure ()
    RemoveLabel _ _ -> pure ()
    AddDep _ _ _ -> pure ()
    RemoveDep _ _ -> pure ()
    Sync -> pure ()
  ) eff

parseTime :: String -> UTCTime
parseTime s = case iso8601ParseM s of
  Just t -> t
  Nothing -> error $ "Failed to parse time: " ++ s

test_pmStatus :: Assertion
test_pmStatus = do
  let baseTime = parseTime "2020-01-01T12:00:00Z"
      now      = addUTCTime (19 * nominalDay) baseTime
      day1     = addUTCTime (-nominalDay) now
      day2     = addUTCTime (-2 * nominalDay) now
      day8     = addUTCTime (-8 * nominalDay) now
      
      bead1 = BeadInfo "b1" "T1" Nothing StatusClosed 2 TypeTask Nothing (Just day2) Nothing (Just day1) (Just day1) Nothing [] [] []
      bead2 = BeadInfo "b2" "T2" Nothing StatusInProgress 2 TypeTask Nothing (Just day1) Nothing (Just now) Nothing Nothing ["pm-tools"] [] []
      bead3 = BeadInfo "b3" "T3" Nothing StatusOpen 2 TypeTask Nothing (Just day2) Nothing (Just day1) Nothing Nothing [labelReady] [] []
      bead4 = BeadInfo "b4" "T4" Nothing StatusClosed 2 TypeTask Nothing (Just day8) Nothing (Just day8) (Just day8) Nothing [] [] []
      
      author = Author "alice" Nothing
      pr1 = PullRequest 1 "PR1" "" author [] PRMerged "url1" "head" "base" day2 (Just day1) [] []
      
      mockState = MockState now [bead1, bead2, bead3, bead4] [pr1]
      
      args = PmStatusArgs 7 True Nothing Nothing
      res = unwrapSingleChoice $ runMockAll mockState (pmStatusLogic args)
      
  -- Velocity: 1 bead closed in last 7 days (bead1). bead4 was 8 days ago.
  -- 1 / 7 = 0.1428...
  assertEqual "Velocity" (1/7 :: Double) res.psrVelocity
  
  -- Current State
  let CurrentStateMetrics inFlight ready blocked _ _ = res.psrCurrentState
  assertEqual "In Flight" (1 :: Int) inFlight
  assertEqual "Ready" (1 :: Int) ready
  assertEqual "Blocked" (0 :: Int) blocked
  
  -- Cycle Time (bead1): day1 - day2 = 1 day
  let CycleTimeMetrics ctMedian _ = res.psrCycleTime
  assertEqual "Cycle Time Median" (1 :: Double) ctMedian
  
  -- PR Lag (pr1): day1 - day2 = 1 day = 24 hours
  let PrLagMetrics prMedian _ = res.psrPrLag
  assertEqual "PR Lag Median" (24 :: Double) prMedian
  
  -- Breakdown
  let breakdown = res.psrBreakdown
  assertBool "Has breakdown" (maybe False (not . null) breakdown)
  assertEqual "Breakdown count" (Just [("pm-tools", 1)]) breakdown
