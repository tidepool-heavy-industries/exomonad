{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

-- | Types for the semantic scout tool.
--
-- Uses the shared Tag enum from training-generator for consistency
-- between exploration and training data generation.
module Tidepool.Agents.Scout.Types
  ( -- * Query Input
    ScoutQuery(..)
  , defaultBudget

    -- * Response Output
  , ScoutResponse(..)
  , Pointer(..)

    -- * Exploration State
  , ExploreState(..)
  , initialExploreState
  , VisitedNode(..)

    -- * Re-exports from Training Types
  , Tag(..)
  , Rubric(..)
  , NodeContext(..)
  , QueryContext(..)
  , TrainingExample(..)
  , allTags
  , tagToText
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..))

-- Re-export training types for consistency
import Tidepool.Training.Types
  ( Tag(..)
  , Rubric(..)
  , NodeContext(..)
  , QueryContext(..)
  , TrainingExample(..)
  , allTags
  , tagToText
  )


-- | Input query for semantic code exploration.
data ScoutQuery = ScoutQuery
  { sqQuery :: Text
    -- ^ Natural language question about the codebase
  , sqTags :: [Tag]
    -- ^ Interest signals using the fixed Tag enum
  , sqBudget :: Maybe Int
    -- ^ Maximum number of LSP calls to make (default: 20)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Default exploration budget.
defaultBudget :: Int
defaultBudget = 20

-- | Response from semantic scout.
data ScoutResponse = ScoutResponse
  { srSummary :: Text
    -- ^ Markdown summary answering the query
  , srPointers :: [Pointer]
    -- ^ Actionable locations found
  , srNodesVisited :: Int
    -- ^ Number of LSP operations performed
  , srTrainingExamples :: [TrainingExample]
    -- ^ Training examples generated during exploration (for fine-tuning)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | A pointer to a relevant code location.
data Pointer = Pointer
  { pLocation :: Text
    -- ^ File and line (e.g., "Edges.hs:89")
  , pWhat :: Text
    -- ^ What was found at this location
  , pRisk :: Int
    -- ^ Risk level: 1-5 (from rubric)
  , pRelevance :: Int
    -- ^ Relevance level: 1-5 (from rubric)
  , pTags :: [Tag]
    -- ^ Tags that apply to this location
  , pAction :: Maybe Text
    -- ^ Suggested action (optional)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | A visited node during exploration.
data VisitedNode = VisitedNode
  { vnContext :: NodeContext
    -- ^ The node's context (location, hover, snippet)
  , vnRubric :: Rubric
    -- ^ The computed rubric for this node
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | State maintained during exploration.
data ExploreState = ExploreState
  { esQuery :: QueryContext
    -- ^ The original query context
  , esVisited :: [VisitedNode]
    -- ^ Nodes visited so far
  , esBudget :: Int
    -- ^ Remaining exploration budget
  , esDepth :: Int
    -- ^ Current exploration depth
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Create initial exploration state from a query.
initialExploreState :: ScoutQuery -> ExploreState
initialExploreState sq = ExploreState
  { esQuery = QueryContext
      { qcQuery = sqQuery sq
      , qcTags = sqTags sq
      }
  , esVisited = []
  , esBudget = maybe defaultBudget id (sqBudget sq)
  , esDepth = 0
  }

-- ════════════════════════════════════════════════════════════════════════════
-- HasJSONSchema Instance
-- ════════════════════════════════════════════════════════════════════════════

-- Manual instance for ScoutQuery (required by MCP server).
-- The schema defines the JSON input format for the scout tool.
instance HasJSONSchema ScoutQuery where
  jsonSchema = objectSchema
    [ ("sqQuery", emptySchema TString)
    , ("sqTags", arraySchema (emptySchema TString))  -- Tags as strings
    , ("sqBudget", emptySchema TInteger)
    ]
    ["sqQuery"]  -- Only sqQuery is required; tags and budget have defaults
