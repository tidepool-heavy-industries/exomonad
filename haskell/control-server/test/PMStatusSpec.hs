{-# LANGUAGE GADTs, OverloadedStrings, OverloadedRecordDot, TypeOperators, FlexibleContexts, RankNTypes, LambdaCase, DataKinds #-}

module Main where

import Control.Monad.Freer (Eff, run, interpret)
import Data.Text (Text)
import Data.Time (UTCTime, nominalDay, addUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Test.Tasty
import Test.Tasty.HUnit

import ExoMonad.Effects.GitHub
import ExoMonad.Effect.Types (Time(..))
import ExoMonad.Graph.Goto (unwrapSingleChoice)
import ExoMonad.Control.PMStatus

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PMStatus"
  [ testCase "pmStatusLogic calculates correct metrics" test_pmStatus
  ]

-- | Mock state
data MockState = MockState
  { msNow    :: UTCTime
  , msIssues :: [Issue]
  , msPrs    :: [PullRequest]
  }

runMockAll :: MockState -> Eff [GitHub, Time] a -> a
runMockAll initial eff = run $ 
  interpret (\case
    GetCurrentTime -> pure initial.msNow
  ) $
  interpret (\case
    ListPullRequests _ _ -> pure $ Right initial.msPrs
    ListIssues _ _ -> pure $ Right initial.msIssues
    GetIssue _ _ _ -> pure $ Right Nothing
    _ -> error "Not implemented in mock"
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
      
      issue1 = (defaultIssue 1) { issueState = IssueClosed }
      issue2 = (defaultIssue 2) { issueLabels = ["in-progress"] }
      issue3 = (defaultIssue 3) { issueLabels = ["ready"] }
      issue4 = (defaultIssue 4) { issueState = IssueClosed }
      
      author = Author "alice" Nothing
      pr1 = PullRequest 1 "PR1" "" author [] PRMerged "url1" "head" "base" day2 (Just day1) [] []
      
      mockState = MockState now [issue1, issue2, issue3, issue4] [pr1]
      
      args = PmStatusArgs 7 True Nothing Nothing
      res = runMockAll mockState (pmStatusLogic args)
      
  -- Velocity: 2 issues closed total (logic currently just counts closed issues)
  -- 2 / 7 = 0.2857...
  assertEqual "Velocity" (2/7 :: Double) res.velocity
  
  -- Current State
  let CurrentStateMetrics inFlight ready blocked _ _ = res.currentState
  assertEqual "In Flight" (1 :: Int) inFlight
  assertEqual "Ready" (1 :: Int) ready
  assertEqual "Blocked" (0 :: Int) blocked
  
  -- PR Lag (pr1): day1 - day2 = 1 day = 24 hours
  let PrLagMetrics prMedian _ = res.prLag
  assertEqual "PR Lag Median" (24 :: Double) prMedian

defaultIssue :: Int -> Issue
defaultIssue num = Issue
  { issueNumber = num
  , issueTitle = ""
  , issueBody = ""
  , issueState = IssueOpen
  , issueLabels = []
  , issueAuthor = Author "ghost" Nothing
  , issueUrl = ""
  }