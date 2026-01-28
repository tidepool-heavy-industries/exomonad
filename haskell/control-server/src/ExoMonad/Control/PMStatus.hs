{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | PM Status tool for sprint health observability.
module ExoMonad.Control.PMStatus
  ( PmStatusArgs(..)
  , PmStatusResult(..)
  , CycleTimeMetrics(..)
  , CurrentStateMetrics(..)
  , PrLagMetrics(..)
  , pmStatusLogic
  ) where

import Control.Monad.Freer (Eff, Member)
import Control.Lens (folded, filtered, lengthOf, traversed, (^..))
import Data.Generics.Labels ()
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime, addUTCTime, nominalDay)

import ExoMonad.Effects.GitHub (GitHub, listIssues, defaultIssueFilter, IssueFilter(..), IssueState(..), Issue(..), listPullRequests, PRFilter(..), defaultPRFilter, PRState(..), PullRequest(..), Repo(..), defaultRepo)
import ExoMonad.Effect.Types (Time, getCurrentTime)

import ExoMonad.Control.PMStatus.Types

-- | Core logic for pm_status.
pmStatusLogic
  :: (Member GitHub es, Member Time es)
  => PmStatusArgs
  -> Eff es PmStatusResult
pmStatusLogic args = do
  now <- getCurrentTime
  let periodSeconds = fromIntegral args.periodDays * nominalDay
      periodStart = addUTCTime (-periodSeconds) now
      prevPeriodStart = addUTCTime (-2 * periodSeconds) now

  let repo = maybe defaultRepo Repo args.repo

  -- Fetch all issues
  let listInput = defaultIssueFilter
                    { ifLabels = maybe [] (:[]) args.labelTrack }
  allIssuesResult <- listIssues repo listInput
  let allIssues = case allIssuesResult of
        Left _err -> []  -- Silently degrade if GitHub unavailable
        Right is -> is

  -- 1. Velocity & Trend (Simulated using state for now as Issue doesn't have closedAt yet)
  let closedIssues = filter (\i -> i.issueState == IssueClosed) allIssues
      velocity = fromIntegral (length closedIssues) / fromIntegral args.periodDays
      trend = 0 -- Need timestamps for real trend

  -- 2. Cycle Time
  let (ctMedian, ctP90) = (0, 0) -- Need timestamps

  -- 3. Current State
  let openIssues = filter (\i -> i.issueState == IssueOpen) allIssues
      countWithLabel lbl = lengthOf (folded . filtered (\i -> lbl `elem` i.issueLabels))
      inFlight = countWithLabel "in-progress" openIssues
      ready = countWithLabel "ready" openIssues
      blocked = countWithLabel "blocked" openIssues
      needsTLReview = countWithLabel "needs-review" openIssues
      needsPMApproval = countWithLabel "needs-approval" openIssues

  -- 4. PR Lag
  allPrsResult <- listPullRequests repo (defaultPRFilter { pfState = Just PRMerged })
  let allPrs = case allPrsResult of
        Left _err -> []  -- Silently degrade if GitHub unavailable
        Right ps -> ps
  let prsInPeriod = filter (isPrMergedBetween periodStart now) allPrs
      prLags = mapMaybe calcPrLag prsInPeriod
      (prMedian, prP90) = calcMetrics prLags (3600) -- in hours

  -- 5. Breakdown (optional)
  let breakdown = if args.includeBreakdown
                  then Just $ aggregateLabels allIssues
                  else Nothing

  pure $ PmStatusResult
    { velocity = velocity
    , trend = trend
    , cycleTime = CycleTimeMetrics ctMedian ctP90
    , currentState = CurrentStateMetrics inFlight ready blocked needsTLReview needsPMApproval
    , prLag = PrLagMetrics prMedian prP90
    , breakdown = breakdown
    }

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

isPrMergedBetween :: UTCTime -> UTCTime -> PullRequest -> Bool
isPrMergedBetween start end pr =
  case pr.prMergedAt of
    Just t -> t >= start && t <= end
    Nothing -> False

calcPrLag :: PullRequest -> Maybe Double
calcPrLag pr = do
  let start = pr.prCreatedAt
  end <- pr.prMergedAt
  pure $ realToFrac $ diffUTCTime end start

calcMetrics :: [Double] -> Double -> (Double, Double)
calcMetrics [] _ = (0, 0)
calcMetrics xs unit =
  let sorted = sort xs
      len = length sorted
      -- Use lower-middle element for median to avoid upper-middle bias
      medianIdx = (len - 1) `div` 2
      -- Compute a 0-based index for the 90th percentile and clamp into bounds
      p90Pos = ceiling (0.9 * fromIntegral len :: Double)  -- 1-based position
      rawP90Idx = p90Pos - 1                               -- convert to 0-based
      p90Idx = max 0 (min (len - 1) rawP90Idx)
      m = sorted !! medianIdx
      p = sorted !! p90Idx
  in (m / unit, p / unit)

aggregateLabels :: [Issue] -> [(Text, Int)]
aggregateLabels issues =
  let allLabels = issues ^.. traversed . #issueLabels . traversed
      groups = group' $ sort allLabels
  in mapMaybe (\case { (x:xs) -> Just (x, 1 + length xs); [] -> Nothing }) groups
  where
    group' [] = []
    group' (x:xs) = let (ys, zs) = span (== x) xs in (x:ys) : group' zs