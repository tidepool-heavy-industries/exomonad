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

import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), (.!=), object, withObject)
import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), (~>), (?), MCPOptions(..), HasJSONSchema(..), emptySchema, SchemaType(..), describeField)

-- | Arguments for pm_status tool.
data PmStatusArgs = PmStatusArgs
  { psaPeriodDays       :: Int
  , psaIncludeBreakdown :: Bool
  , psaLabelTrack       :: Maybe Text
  , psaRepo             :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "psa" } ''PmStatusArgs
  [ 'psaPeriodDays       ~> "period_days"       ? "Period in days for velocity and trend calculation (default 7)"
  , 'psaIncludeBreakdown ~> "include_breakdown" ? "Whether to include label breakdown"
  , 'psaLabelTrack       ~> "label_track"       ? "Optional label to filter metrics (e.g. a specific track or epic label)"
  , 'psaRepo             ?? "Optional GitHub repository name (owner/repo) to fetch PR metrics from"
  ])

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
