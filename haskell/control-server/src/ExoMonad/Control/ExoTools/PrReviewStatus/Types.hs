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

-- | Arguments for pr_review_status tool.
data PrReviewStatusArgs = PrReviewStatusArgs
  { prsaPrNumber :: Int
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "prsa" } ''PrReviewStatusArgs
  [ 'prsaPrNumber ?? "Pull Request number."
  ])

-- | Feedback from a single author category (Copilot or human).
data AuthorFeedback = AuthorFeedback
  { afPending :: [ReviewComment]
  , afResolved :: [ReviewComment]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AuthorFeedback where
  toJSON af = object
    [ "pending" .= afPending af
    , "resolved" .= afResolved af
    ]

instance FromJSON AuthorFeedback where
  parseJSON = withObject "AuthorFeedback" $ \v ->
    AuthorFeedback
      <$> v .: "pending"
      <*> v .: "resolved"

-- | Summary counts for quick triage.
data FeedbackSummary = FeedbackSummary
  { fsCopilotPending :: Int
  , fsCopilotResolved :: Int
  , fsHumanPending :: Int
  , fsHumanResolved :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FeedbackSummary where
  toJSON fs = object
    [ "copilot_pending" .= fsCopilotPending fs
    , "copilot_resolved" .= fsCopilotResolved fs
    , "human_pending" .= fsHumanPending fs
    , "human_resolved" .= fsHumanResolved fs
    ]

instance FromJSON FeedbackSummary where
  parseJSON = withObject "FeedbackSummary" $ \v ->
    FeedbackSummary
      <$> v .: "copilot_pending"
      <*> v .: "copilot_resolved"
      <*> v .: "human_pending"
      <*> v .: "human_resolved"

-- | Result of pr_review_status tool.
-- Returns comprehensive feedback grouped by author type and status.
data PrReviewStatusResult = PrReviewStatusResult
  { prsrPrNumber :: Int
  , prsrCopilot :: AuthorFeedback
  , prsrHumans :: AuthorFeedback
  , prsrSummary :: FeedbackSummary
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PrReviewStatusResult where
  toJSON res = object
    [ "pr_number" .= prsrPrNumber res
    , "copilot" .= prsrCopilot res
    , "humans" .= prsrHumans res
    , "summary" .= prsrSummary res
    ]

instance FromJSON PrReviewStatusResult where
  parseJSON = withObject "PrReviewStatusResult" $ \v ->
    PrReviewStatusResult
      <$> v .: "pr_number"
      <*> v .: "copilot"
      <*> v .: "humans"
      <*> v .: "summary"
