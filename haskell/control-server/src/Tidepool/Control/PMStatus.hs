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
module Tidepool.Control.PMStatus
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

import Tidepool.Effects.BD (BD, listBeads, ListBeadsInput(..), defaultListBeadsInput, BeadStatus(..), BeadInfo(..))
import Tidepool.Effects.GitHub (GitHub, listPullRequests, PRFilter(..), defaultPRFilter, PRState(..), PullRequest(..), Repo(..))
import Tidepool.Effect.Types (Time, getCurrentTime)
import Tidepool.Role (Role(..))
import Tidepool.Control.PMTools (labelReady, labelNeedsTLReview, labelNeedsPMApproval)
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef, MCPRoleHint)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..), describeField)

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
      :@ UsesEffects '[BD, GitHub, Time, Goto Exit PmStatusResult]

  , psExit :: mode :- ExitNode PmStatusResult
  }
  deriving Generic

-- | Handlers for pm_status graph.
pmStatusHandlers :: (Member BD es, Member GitHub es, Member Time es) => PmStatusGraph (AsHandler es)
pmStatusHandlers = PmStatusGraph
  { psEntry = ()
  , psRun = pmStatusLogic
  , psExit = ()
  }

-- | Core logic for pm_status.
pmStatusLogic
  :: (Member BD es, Member GitHub es, Member Time es)
  => PmStatusArgs
  -> Eff es (GotoChoice '[To Exit PmStatusResult])
pmStatusLogic args = do
  now <- getCurrentTime
  let periodSeconds = fromIntegral args.psaPeriodDays * nominalDay
      periodStart = addUTCTime (-periodSeconds) now
      prevPeriodStart = addUTCTime (-2 * periodSeconds) now

  -- Fetch all beads (we'll filter in memory for simplicity/flexibility)
  let listInput = defaultListBeadsInput 
                    { lbiLabels = maybe [] (:[]) args.psaLabelTrack }
  allBeads <- listBeads listInput

  -- 1. Velocity & Trend
  let closedInPeriod = filter (isClosedBetween periodStart now) allBeads
      closedInPrevPeriod = filter (isClosedBetween prevPeriodStart periodStart) allBeads
      velocity = fromIntegral (length closedInPeriod) / fromIntegral args.psaPeriodDays
      prevVelocity = fromIntegral (length closedInPrevPeriod) / fromIntegral args.psaPeriodDays
      trend = if prevVelocity == 0 then 0 else (velocity - prevVelocity) / prevVelocity

  -- 2. Cycle Time
  let cycleTimes = mapMaybe calcCycleTime closedInPeriod
      (ctMedian, ctP90) = calcMetrics cycleTimes (realToFrac nominalDay) -- in days

  -- 3. Current State
  let inFlight = count (\b -> b.biStatus == StatusInProgress) allBeads
      ready = count (\b -> labelReady `elem` b.biLabels) allBeads
      blocked = count (\b -> b.biStatus == StatusBlocked) allBeads
      needsTLReview = count (\b -> labelNeedsTLReview `elem` b.biLabels) allBeads
      needsPMApproval = count (\b -> labelNeedsPMApproval `elem` b.biLabels) allBeads

  -- 4. PR Lag
  -- For Tidepool, it's usually tidepool-heavy-industries/tidepool, but allow override via args.
  let defaultRepo = Repo "tidepool-heavy-industries/tidepool"
      repo = maybe defaultRepo Repo args.psaRepo
  allPrs <- listPullRequests repo (defaultPRFilter { pfState = Just PRMerged })
  let prsInPeriod = filter (isPrMergedBetween periodStart now) allPrs
      prLags = mapMaybe calcPrLag prsInPeriod
      (prMedian, prP90) = calcMetrics prLags (3600) -- in hours

  -- 5. Breakdown (optional)
  let breakdown = if args.psaIncludeBreakdown
                  then Just $ aggregateLabels allBeads
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

isClosedBetween :: UTCTime -> UTCTime -> BeadInfo -> Bool
isClosedBetween start end b =
  case b.biClosedAt of
    Just t -> t >= start && t <= end
    Nothing -> False

isPrMergedBetween :: UTCTime -> UTCTime -> PullRequest -> Bool
isPrMergedBetween start end pr =
  case pr.prMergedAt of
    Just t -> t >= start && t <= end
    Nothing -> False

calcCycleTime :: BeadInfo -> Maybe Double
calcCycleTime b = do
  start <- b.biCreatedAt
  end <- b.biClosedAt
  pure $ realToFrac $ diffUTCTime end start

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

aggregateLabels :: [BeadInfo] -> [(Text, Int)]
aggregateLabels beads =
  let allLabels = concatMap (\b -> b.biLabels) beads
      uniqueLabels = sort $ filter (not . isWorkflowLabel) allLabels
      groups = group' uniqueLabels
  in mapMaybe (\case { (x:xs) -> Just (x, 1 + length xs); [] -> Nothing }) groups
  where
    isWorkflowLabel l = l `elem` [labelReady, labelNeedsTLReview, labelNeedsPMApproval]
    group' [] = []
    group' (x:xs) = let (ys, zs) = span (== x) xs in (x:ys) : group' zs
