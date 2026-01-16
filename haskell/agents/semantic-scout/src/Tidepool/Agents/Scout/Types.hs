{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

-- | Types for the semantic scout tool.
--
-- Design: FunctionGemma as coalgebra in an anamorphism.
-- The model handles one step at a time: (Query, Edge) → Rubric
-- The exploration loop handles the recursion.
--
-- Uses the shared Tag enum from training-generator for consistency
-- between exploration and training data generation.
module Tidepool.Agents.Scout.Types
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

import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value(..)
  , withObject, (.:), (.:?), (.!=), (.=), object, parseJSON
  )
import Data.Aeson.Types (Parser)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
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


-- | Exploration depth controls budget.
--
-- Semantic levels that map to concrete node limits:
--   * Low: Quick scan (~10 nodes)
--   * Medium: Thorough exploration (~30 nodes)
--   * High: Exhaustive search (~100 nodes)
data Depth = Low | Medium | High
  deriving stock (Show, Eq, Generic, Bounded, Enum)

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
--
-- Caller provides:
--   * query: Natural language question (for relevance scoring only)
--   * symbols: Explicit entry point symbols (no NL parsing needed)
--   * depth: How thoroughly to explore (Low/Medium/High)
--
-- FunctionGemma scores edges for relevance to the query.
-- The exploration loop follows high-scoring edges.
data ScoutQuery = ScoutQuery
  { sqQuery :: Text
    -- ^ Natural language question (for relevance scoring, not symbol lookup)
  , sqSymbols :: [Text]
    -- ^ Entry point symbols (explicit, resolved via workspaceSymbol)
  , sqDepth :: Depth
    -- ^ Exploration depth: Low (~10), Medium (~30), High (~100)
  } deriving stock (Show, Eq, Generic)

-- | Parse ScoutQuery from JSON.
-- Symbols can be array or comma-separated string for MCP compatibility.
instance FromJSON ScoutQuery where
  parseJSON = withObject "ScoutQuery" $ \v -> ScoutQuery
    <$> v .: "query"
    <*> (parseSymbols =<< v .:? "symbols" .!= Array mempty)
    <*> v .:? "depth" .!= Medium
    where
      -- Parse symbols from either comma-separated string or JSON array
      parseSymbols :: Value -> Parser [Text]
      parseSymbols (String t)
        | T.null t = pure []
        | otherwise = pure $ map T.strip (T.splitOn "," t)
      parseSymbols (Array arr) = mapM parseJSON (toList arr)
      parseSymbols _ = pure []

instance ToJSON ScoutQuery where
  toJSON sq = object
    [ "query" .= sqQuery sq
    , "symbols" .= sqSymbols sq
    , "depth" .= sqDepth sq
    ]

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

-- ════════════════════════════════════════════════════════════════════════════
-- HasJSONSchema Instance
-- ════════════════════════════════════════════════════════════════════════════

-- Manual instance for ScoutQuery (required by MCP server).
-- The schema defines the JSON input format for the scout tool.
-- Field names match the Aeson encoding: query, symbols, depth (no "sq" prefix)
instance HasJSONSchema ScoutQuery where
  jsonSchema = objectSchema
    [ ("query", emptySchema TString)     -- NL question for relevance scoring
    , ("symbols", arraySchema (emptySchema TString))  -- Entry point symbols
    , ("depth", emptySchema TString)     -- "low" | "medium" | "high"
    ]
    ["query", "symbols"]  -- Both required; depth defaults to medium
