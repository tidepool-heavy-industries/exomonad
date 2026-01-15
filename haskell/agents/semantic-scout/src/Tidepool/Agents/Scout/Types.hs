{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Types for the semantic scout tool
module Tidepool.Agents.Scout.Types
  ( ScoutQuery(..)
  , ScoutResponse(..)
  , Pointer(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Tidepool.Schema (HasJSONSchema)

-- | Input query for semantic code exploration
data ScoutQuery = ScoutQuery
  { sqQuery :: Text
    -- ^ Natural language question about the codebase
  , sqTags :: [Text]
    -- ^ Interest signals (e.g., "exhaustive", "pattern-match")
  , sqBudget :: Maybe Int
    -- ^ Maximum number of LSP calls to make (default: 20)
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, HasJSONSchema)

-- | Response from semantic scout
data ScoutResponse = ScoutResponse
  { srSummary :: Text
    -- ^ Markdown summary answering the query
  , srPointers :: [Pointer]
    -- ^ Actionable locations found
  , srNodesVisited :: Int
    -- ^ Number of LSP operations performed
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, HasJSONSchema)

-- | A pointer to a relevant code location
data Pointer = Pointer
  { pLocation :: Text
    -- ^ File and line (e.g., "Edges.hs:89")
  , pWhat :: Text
    -- ^ What was found at this location
  , pRisk :: Text
    -- ^ Risk level: "high", "medium", or "low"
  , pAction :: Maybe Text
    -- ^ Suggested action (optional)
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, HasJSONSchema)
