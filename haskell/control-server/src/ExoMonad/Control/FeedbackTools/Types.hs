{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module ExoMonad.Control.FeedbackTools.Types
  ( TokenCategoryEstimate(..)
  , RegisterFeedbackArgs(..)
  , RegisterFeedbackResult(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), withObject)
import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), (~>), (?), MCPOptions(..), HasJSONSchema(..), arraySchema, emptySchema, SchemaType(..), describeField, jsonSchema)

-- | Estimate of token spend for a specific category.
data TokenCategoryEstimate = TokenCategoryEstimate
  { tceCategory :: Text -- ^ Category name (reading, searching, editing, testing, planning, waiting)
  , tceEstimate :: Text -- ^ Estimate: high, medium, low
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "tce" } ''TokenCategoryEstimate
  [ 'tceCategory ?? "Category name (reading, searching, editing, testing, planning, waiting)"
  , 'tceEstimate ?? "Estimate of token spend: high, medium, low"
  ])

-- | Arguments for register_feedback tool.
data RegisterFeedbackArgs = RegisterFeedbackArgs
  { rfaIssueId :: Text            -- ^ The ID of the issue being worked on (e.g. gh-123)
  , rfaSuggestions :: [Text]     -- ^ Suggestions for making the task easier
  , rfaIdeas :: [Text]           -- ^ Ideas for new tools that should exist
  , rfaNits :: [Text]            -- ^ Small annoyances or points of confusion
  , rfaTokenCategories :: [TokenCategoryEstimate] -- ^ Token spend estimates by category
  , rfaOverallExperience :: Text -- ^ Overall experience: smooth, bumpy, blocked
  , rfaNotes :: Maybe Text       -- ^ Free-form notes and reflections
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "rfa" } ''RegisterFeedbackArgs
  [ 'rfaIssueId           ?? "The ID of the issue being worked on (e.g. gh-123)"
  , 'rfaSuggestions       ?? "Suggestions for making the task easier"
  , 'rfaIdeas             ?? "Ideas for new tools that should exist"
  , 'rfaNits              ?? "Small annoyances or points of confusion"
  , 'rfaTokenCategories   ?? "Token spend estimates by category (reading, searching, editing, testing, planning, waiting)"
  , 'rfaOverallExperience ?? "Overall experience: smooth, bumpy, blocked"
  , 'rfaNotes             ?? "Free-form notes and reflections"
  ])

-- | Result of register_feedback tool.
data RegisterFeedbackResult = RegisterFeedbackResult
  { rfrSuccess :: Bool
  , rfrPath :: Text
  , rfrError :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
