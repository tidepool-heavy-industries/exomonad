{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module ExoMonad.Control.PMStatus.Types
  ( PmStatusArgs(..)
  , PmStatusResult(..)
  , CycleTimeMetrics(..)
  , CurrentStateMetrics(..)
  , PrLagMetrics(..)
  ) where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), (~>), (?))
import Language.Haskell.TH (mkName)

-- | Arguments for pm_status tool.
data PmStatusArgs = PmStatusArgs
  { periodDays       :: Int
  , includeBreakdown :: Bool
  , labelTrack       :: Maybe Text
  , repo             :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions ''PmStatusArgs
  [ mkName "periodDays"       ~> "period_days"       ? "Period in days for velocity and trend calculation (default 7)"
  , mkName "includeBreakdown" ~> "include_breakdown" ? "Whether to include label breakdown"
  , mkName "labelTrack"       ~> "label_track"       ? "Optional label to filter metrics (e.g. a specific track or epic label)"
  , mkName "repo"             ?? "Optional GitHub repository name (owner/repo) to fetch PR metrics from"
  ])

-- | Result of pm_status tool.
data PmStatusResult = PmStatusResult
  { velocity     :: Double
  , trend        :: Double
  , cycleTime    :: CycleTimeMetrics
  , currentState :: CurrentStateMetrics
  , prLag        :: PrLagMetrics
  , breakdown    :: Maybe [(Text, Int)]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PmStatusResult where
  toJSON res = object
    [ "velocity"      .= res.velocity
    , "trend"         .= res.trend
    , "cycle_time"    .= res.cycleTime
    , "current_state" .= res.currentState
    , "pr_lag"        .= res.prLag
    , "breakdown"     .= res.breakdown
    ]

data CycleTimeMetrics = CycleTimeMetrics
  { medianDays :: Double
  , p90Days    :: Double
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CycleTimeMetrics where
  toJSON m = object
    [ "median_days" .= m.medianDays
    , "p90_days"    .= m.p90Days
    ]

data CurrentStateMetrics = CurrentStateMetrics
  { inFlight       :: Int
  , ready          :: Int
  , blocked        :: Int
  , needsTLReview  :: Int
  , needsPMApproval :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CurrentStateMetrics where
  toJSON m = object
    [ "in_flight"        .= m.inFlight
    , "ready"            .= m.ready
    , "blocked"          .= m.blocked
    , "needs_tl_review"  .= m.needsTLReview
    , "needs_pm_approval" .= m.needsPMApproval
    ]

data PrLagMetrics = PrLagMetrics
  { medianHours :: Double
  , p90Hours    :: Double
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PrLagMetrics where
  toJSON m = object
    [ "median_hours" .= m.medianHours
    , "p90_hours"    .= m.p90Hours
    ]
