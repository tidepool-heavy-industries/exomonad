{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module ExoMonad.Control.ExoTools.PrReviewStatus.Types
  ( PrReviewStatusArgs(..)
  , PrReviewStatusResult(..)
  , AuthorFeedback(..)
  , FeedbackSummary(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import GHC.Generics (Generic)
import ExoMonad.Effects.GitHub (ReviewComment)
import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), MCPOptions(..), HasJSONSchema(..))
import Language.Haskell.TH (mkName)

-- | Arguments for pr_review_status tool.
data PrReviewStatusArgs = PrReviewStatusArgs
  { prNumber :: Int
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions ''PrReviewStatusArgs
  [ mkName "prNumber" ?? "Pull Request number."
  ])

-- | Feedback from a single author category (Copilot or human).
data AuthorFeedback = AuthorFeedback
  { pending :: [ReviewComment]
  , resolved :: [ReviewComment]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AuthorFeedback where
  toJSON af = object
    [ "pending" .= af.pending
    , "resolved" .= af.resolved
    ]

-- | Summary counts for quick triage.
data FeedbackSummary = FeedbackSummary
  { copilotPending :: Int
  , copilotResolved :: Int
  , humanPending :: Int
  , humanResolved :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FeedbackSummary where
  toJSON fs = object
    [ "copilot_pending" .= fs.copilotPending
    , "copilot_resolved" .= fs.copilotResolved
    , "human_pending" .= fs.humanPending
    , "human_resolved" .= fs.humanResolved
    ]

-- | Result of pr_review_status tool.
-- Returns comprehensive feedback grouped by author type and status.
data PrReviewStatusResult = PrReviewStatusResult
  { prNumber :: Int
  , copilot :: AuthorFeedback
  , humans :: AuthorFeedback
  , summary :: FeedbackSummary
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PrReviewStatusResult where
  toJSON res = object
    [ "pr_number" .= res.prNumber
    , "copilot" .= res.copilot
    , "humans" .= res.humans
    , "summary" .= res.summary
    ]
