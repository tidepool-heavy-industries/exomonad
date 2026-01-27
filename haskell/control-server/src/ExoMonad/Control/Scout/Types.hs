{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

-- | Types for the semantic scout tool.
module ExoMonad.Control.Scout.Types
  ( -- * Query Input
    ScoutQuery(..)
  , Depth(..)
  , depthToBudget

    -- * Response Output
  , ScoutResponse(..)
  , Pointer(..)

    -- * Exploration State (legacy, for compatibility)
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

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Schema (deriveMCPTypeWith, deriveHasJSONSchema, defaultMCPOptions, (??), MCPOptions(..), HasJSONSchema(..))

-- Re-export training types for consistency
import ExoMonad.Training.Types
  ( Tag(..)
  , Rubric(..)
  , NodeContext(..)
  , QueryContext(..)
  , TrainingExample(..)
  , allTags
  , tagToText
  )

-- | Exploration depth controls budget.
data Depth = Low | Medium | High
  deriving stock (Show, Eq, Generic, Bounded, Enum)

$(deriveHasJSONSchema ''Depth)

instance FromJSON Depth where
  parseJSON = \case
    String "low"    -> pure Low
    String "medium" -> pure Medium
    String "high"   -> pure High
    String "Low"    -> pure Low
    String "Medium" -> pure Medium
    String "High"   -> pure High
    _               -> pure Medium  -- Default to medium

instance ToJSON Depth where
  toJSON Low    = String "low"
  toJSON Medium = String "medium"
  toJSON High   = String "high"

-- | Convert semantic depth to concrete budget.
depthToBudget :: Depth -> Int
depthToBudget Low    = 10
depthToBudget Medium = 30
depthToBudget High   = 100

-- | Input query for semantic code exploration.
data ScoutQuery = ScoutQuery
  { sqQuery :: Text
    -- ^ Natural language question (for relevance scoring, not symbol lookup)
  , sqSymbols :: [Text]
    -- ^ Entry point symbols (explicit, resolved via workspaceSymbol)
  , sqDepth :: Depth
    -- ^ Exploration depth: Low (~10), Medium (~30), High (~100)
  } deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "sq" } ''ScoutQuery
  [ 'sqQuery   ?? "Natural language question (for relevance scoring)"
  , 'sqSymbols ?? "Entry point symbols (explicit, resolved via workspaceSymbol)"
  , 'sqDepth   ?? "Exploration depth: Low (~10), Medium (~30), High (~100)"
  ])

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
      , qcTags = []  -- Tags inferred from rubrics during exploration
      }
  , esVisited = []
  , esBudget = depthToBudget (sqDepth sq)
  , esDepth = 0
  }