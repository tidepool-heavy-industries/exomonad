{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | PM Status tool for sprint health observability.
module ExoMonad.Control.PMStatus
  ( PmStatusArgs(..)
  , PmStatusResult(..)
  , CycleTimeMetrics(..)
  , CurrentStateMetrics(..)
  , PrLagMetrics(..)
  , PmStatusGraph(..)
  , pmStatusLogic
  , pmStatusHandlers
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), (.!=), object, withObject)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime, addUTCTime, nominalDay)
import GHC.Generics (Generic)

import ExoMonad.Effects.GitHub (GitHub, listIssues, defaultIssueFilter, IssueFilter(..), IssueState(..), Issue(..), listPullRequests, PRFilter(..), defaultPRFilter, PRState(..), PullRequest(..), Repo(..), defaultRepo)
import ExoMonad.Effect.Types (Time, getCurrentTime)
import ExoMonad.Role (Role(..))
import ExoMonad.Graph.Generic (AsHandler, type (:-))
import ExoMonad.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import ExoMonad.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import ExoMonad.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef, MCPRoleHint)
import ExoMonad.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..), describeField)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for pm_status tool.
data PmStatusArgs = PmStatusArgs
  { psaPeriodDays       :: Int
  , psaIncludeBreakdown :: Bool
  , psaLabelTrack       :: Maybe Text
  , psaRepo             :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema PmStatusArgs where
  jsonSchema = objectSchema
    [ ("period_days", describeField "period_days" "Period in days for velocity and trend calculation (default 7)" (emptySchema TInteger))
    , ("include_breakdown", describeField "include_breakdown" "Whether to include label breakdown" (emptySchema TBoolean))
    , ("label_track", describeField "label_track" "Optional label to filter metrics (e.g. a specific track or epic label)" (emptySchema TString))
    , ("repo", describeField "repo" "Optional GitHub repository name (owner/repo) to fetch PR metrics from" (emptySchema TString))
    ]
    []

instance FromJSON PmStatusArgs where
  parseJSON = withObject "PmStatusArgs" $ \v ->
    PmStatusArgs
      <$> v .:? "period_days" .!= 7
      <*> v .:? "include_breakdown" .!= False
      <*> v .:? "label_track"
      <*> v .:? "repo"

instance ToJSON PmStatusArgs where
  toJSON args = object
    [ "period_days" .= psaPeriodDays args
    , "include_breakdown" .= psaIncludeBreakdown args
    , "label_track" .= psaLabelTrack args
    , "repo" .= psaRepo args
    ]

-- | Result of pm_status tool.
data PmStatusResult = PmStatusResult
  { psrVelocity     :: Double
  , psrTrend        :: Double
  , psrCycleTime    :: CycleTimeMetrics
  , psrCurrentState :: CurrentStateMetrics
  , psrPrLag        :: PrLagMetrics
  , psrBreakdown    :: Maybe [(Text, Int)]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PmStatusResult where
  toJSON res = object
    [ "velocity"      .= psrVelocity res
    , "trend"         .= psrTrend res
    , "cycle_time"    .= psrCycleTime res
    , "current_state" .= psrCurrentState res
    , "pr_lag"        .= psrPrLag res
    , "breakdown"     .= psrBreakdown res
    ]

data CycleTimeMetrics = CycleTimeMetrics
  { ctmMedianDays :: Double
  , ctmP90Days    :: Double
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CycleTimeMetrics where
  toJSON m = object
    [ "median_days" .= ctmMedianDays m
    , "p90_days"    .= ctmP90Days m
    ]

data CurrentStateMetrics = CurrentStateMetrics
  { csmInFlight       :: Int
  , csmReady          :: Int
  , csmBlocked        :: Int
  , csmNeedsTLReview  :: Int
  , csmNeedsPMApproval :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CurrentStateMetrics where
  toJSON m = object
    [ "in_flight"        .= csmInFlight m
    , "ready"            .= csmReady m
    , "blocked"          .= csmBlocked m
    , "needs_tl_review"  .= csmNeedsTLReview m
    , "needs_pm_approval" .= csmNeedsPMApproval m
    ]

data PrLagMetrics = PrLagMetrics
  { plmMedianHours :: Double
  , plmP90Hours    :: Double
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PrLagMetrics where
  toJSON m = object
    [ "median_hours" .= plmMedianHours m
    , "p90_hours"    .= plmP90Hours m
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for pm_status tool.
data PmStatusGraph mode = PmStatusGraph
  { psEntry :: mode :- EntryNode PmStatusArgs
      :@ MCPExport
      :@ MCPToolDef '("pm_status", "Sprint health dashboard for PM observability. Calculates velocity, cycle time, and state distribution.")
      :@ MCPRoleHint 'PM

  , psRun :: mode :- LogicNode
      :@ Input PmStatusArgs
      :@ UsesEffects '[GitHub, Time, Goto Exit PmStatusResult]

  , psExit :: mode :- ExitNode PmStatusResult
  }
  deriving Generic

-- | Handlers for pm_status graph.
pmStatusHandlers :: (Member GitHub es, Member Time es) => PmStatusGraph (AsHandler es)
pmStatusHandlers = PmStatusGraph
  { psEntry = ()
  , psRun = pmStatusLogic
  , psExit = ()
  }

-- | Core logic for pm_status.
pmStatusLogic
  :: (Member GitHub es, Member Time es)
  => PmStatusArgs
  -> Eff es (GotoChoice '[To Exit PmStatusResult])
pmStatusLogic args = do
  now <- getCurrentTime
  let periodSeconds = fromIntegral args.psaPeriodDays * nominalDay
      periodStart = addUTCTime (-periodSeconds) now
      prevPeriodStart = addUTCTime (-2 * periodSeconds) now

  let repo = maybe defaultRepo Repo args.psaRepo

  -- Fetch all issues
  let listInput = defaultIssueFilter
                    { ifLabels = maybe [] (:[]) args.psaLabelTrack }
  allIssuesResult <- listIssues repo listInput
  let allIssues = case allIssuesResult of
        Left _err -> []  -- Silently degrade if GitHub unavailable
        Right is -> is

  -- 1. Velocity & Trend (Simulated using state for now as Issue doesn't have closedAt yet)
  let closedIssues = filter (\i -> i.issueState == IssueClosed) allIssues
      velocity = fromIntegral (length closedIssues) / fromIntegral args.psaPeriodDays
      trend = 0 -- Need timestamps for real trend

  -- 2. Cycle Time
  let (ctMedian, ctP90) = (0, 0) -- Need timestamps

  -- 3. Current State
  let openIssues = filter (\i -> i.issueState == IssueOpen) allIssues
      inFlight = count (\i -> "in-progress" `elem` i.issueLabels) openIssues
      ready = count (\i -> "ready" `elem` i.issueLabels) openIssues
      blocked = count (\i -> "blocked" `elem` i.issueLabels) openIssues
      needsTLReview = count (\i -> "needs-review" `elem` i.issueLabels) openIssues
      needsPMApproval = count (\i -> "needs-approval" `elem` i.issueLabels) openIssues

  -- 4. PR Lag
  allPrsResult <- listPullRequests repo (defaultPRFilter { pfState = Just PRMerged })
  let allPrs = case allPrsResult of
        Left _err -> []  -- Silently degrade if GitHub unavailable
        Right ps -> ps
  let prsInPeriod = filter (isPrMergedBetween periodStart now) allPrs
      prLags = mapMaybe calcPrLag prsInPeriod
      (prMedian, prP90) = calcMetrics prLags (3600) -- in hours

  -- 5. Breakdown (optional)
  let breakdown = if args.psaIncludeBreakdown
                  then Just $ aggregateLabels allIssues
                  else Nothing

  pure $ gotoExit PmStatusResult
    { psrVelocity = velocity
    , psrTrend = trend
    , psrCycleTime = CycleTimeMetrics ctMedian ctP90
    , psrCurrentState = CurrentStateMetrics inFlight ready blocked needsTLReview needsPMApproval
    , psrPrLag = PrLagMetrics prMedian prP90
    , psrBreakdown = breakdown
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

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

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
  let allLabels = concatMap (\i -> i.issueLabels) issues
      groups = group' $ sort allLabels
  in mapMaybe (\case { (x:xs) -> Just (x, 1 + length xs); [] -> Nothing }) groups
  where
    group' [] = []
    group' (x:xs) = let (ys, zs) = span (== x) xs in (x:ys) : group' zs
