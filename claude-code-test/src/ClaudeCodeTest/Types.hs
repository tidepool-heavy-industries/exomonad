{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Domain types for the ClaudeCode test graph.
--
-- These types flow through the graph:
--   ExploreInput → Findings → ActionInput → ActionResult
module ClaudeCodeTest.Types
  ( -- * Input Types
    ExploreInput(..)
  , ActionInput(..)

    -- * Output Types (with JSON Schema for ClaudeCode)
  , Findings(..)
  , ActionResult(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Schema
  ( HasJSONSchema(..)
  , objectSchema
  , arraySchema
  , describeField
  , emptySchema
  , SchemaType(..)
  )


-- ============================================================================
-- Input Types
-- ============================================================================

-- | Input to the explore node.
--
-- Specifies what directory to explore and what to look for.
data ExploreInput = ExploreInput
  { eiDirectory :: FilePath
    -- ^ Directory path to explore
  , eiObjective :: Text
    -- ^ What to look for / analyze
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Input to the action node.
--
-- Contains the findings from exploration and what action to take.
data ActionInput = ActionInput
  { aiFindings :: Findings
    -- ^ Results from the exploration phase
  , aiAction :: Text
    -- ^ Description of action to take
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- ============================================================================
-- Output Types (Schema required for ClaudeCode structured output)
-- ============================================================================

-- | Findings from the exploration phase.
--
-- Structured output from the explore ClaudeCode node.
data Findings = Findings
  { fSummary :: Text
    -- ^ High-level summary of what was found
  , fFileCount :: Int
    -- ^ Number of files discovered
  , fFiles :: [Text]
    -- ^ List of interesting file paths
  , fRecommendation :: Text
    -- ^ Recommended next action based on findings
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Manual HasJSONSchema instance for Findings.
--
-- This schema tells ClaudeCode what structured output to produce.
instance HasJSONSchema Findings where
  jsonSchema = objectSchema
    [ ("fSummary", describeField "fSummary"
        "High-level summary of what was found in the directory"
        (emptySchema TString))
    , ("fFileCount", describeField "fFileCount"
        "Number of files discovered (0 or more)"
        (emptySchema TInteger))
    , ("fFiles", describeField "fFiles"
        "List of interesting file paths found"
        (arraySchema (emptySchema TString)))
    , ("fRecommendation", describeField "fRecommendation"
        "Recommended next action based on what was found"
        (emptySchema TString))
    ]
    ["fSummary", "fFileCount", "fFiles", "fRecommendation"]


-- | Result from the action phase.
--
-- Structured output from the action ClaudeCode node.
data ActionResult = ActionResult
  { arSuccess :: Bool
    -- ^ Whether the action completed successfully
  , arDescription :: Text
    -- ^ Description of what was done
  , arArtifact :: Maybe Text
    -- ^ Optional output artifact (file contents, etc.)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Manual HasJSONSchema instance for ActionResult.
instance HasJSONSchema ActionResult where
  jsonSchema = objectSchema
    [ ("arSuccess", describeField "arSuccess"
        "Whether the action completed successfully (true/false)"
        (emptySchema TBoolean))
    , ("arDescription", describeField "arDescription"
        "Description of what action was taken and the outcome"
        (emptySchema TString))
    , ("arArtifact", describeField "arArtifact"
        "Optional output artifact such as generated content or file contents"
        (emptySchema TString))
    ]
    ["arSuccess", "arDescription"]  -- arArtifact is optional (Maybe)
