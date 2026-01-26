{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

-- | Types for DocGen graph nodes.
--
-- This module defines the input/output types that flow between nodes in the
-- DocGen exploration graph. The graph explores code via LSP and uses an LLM
-- to select relevant type dependencies.
--
-- @
-- TeachQuery ─► dgInit ─► ProcessInput ─► dgProcess ─┬─► SelectInput ─► dgSelect
--                                                     │                    │
--                                                     │    ◄─ SelectOutput ◄┘
--                                                     │
--                                                     └─► dgExpand ─► ExpandInput
--                                                                         │
--                                                     ◄────────────────────┘
--                                                     │
--                                                     └─► FinalizeInput ─► dgFinalize ─► TeachingDoc
-- @
module ExoMonad.Control.Scout.Graph.Types
  ( -- * Node Input Types
    ProcessInput(..)
  , SelectInput(..)
  , ExpandInput(..)
  , FinalizeInput(..)

    -- * Node Output Types
  , SelectOutput(..)
  , FinalizeReason(..)

    -- * Graph Memory State
  , ExploreState(..)
  , initialExploreState
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..))
import ExoMonad.StructuredOutput (StructuredOutput)
import ExoMonad.Control.Scout.DocGen.Types (SymbolKey, LSPSymbol)


-- ════════════════════════════════════════════════════════════════════════════
-- NODE INPUT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Input to dgProcess node: a symbol to explore.
data ProcessInput = ProcessInput
  { piSymbolKey :: SymbolKey
    -- ^ Symbol to look up (file path, symbol name)
  , piDepth :: Int
    -- ^ Current BFS depth from seeds
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Input to dgSelect node (LLM): candidates to filter.
data SelectInput = SelectInput
  { siTopic :: Text
    -- ^ Topic context (e.g., "the scoring system")
  , siSymbol :: LSPSymbol
    -- ^ Symbol being analyzed (from LSP hover)
  , siCandidates :: [Text]
    -- ^ Type names extracted from signature
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Input to dgExpand node: selected symbols to add to frontier.
data ExpandInput = ExpandInput
  { eiSelected :: [Text]
    -- ^ Symbol names selected by LLM
  , eiCurrentDepth :: Int
    -- ^ Depth of the current symbol (new symbols get depth+1)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Input to dgFinalize node: why exploration is ending.
newtype FinalizeInput = FinalizeInput
  { fiReason :: FinalizeReason
    -- ^ Why we're stopping
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- NODE OUTPUT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Output from dgSelect node (LLM): which candidates are relevant.
--
-- This is the structured output schema for the LLM call. Haiku returns
-- this as JSON which gets parsed and flows to dgExpand.
data SelectOutput = SelectOutput
  { soSelected :: [Text]
    -- ^ Symbol names that are relevant to the topic
  , soReasoning :: Maybe Text
    -- ^ Optional: why these were selected (useful for training data)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | JSON schema for LLM structured output.
--
-- Note: No oneOf - Anthropic structured output compatible.
instance HasJSONSchema SelectOutput where
  jsonSchema = objectSchema
    [ ("selected", arraySchema (emptySchema TString))
    , ("reasoning", emptySchema TString)
    ]
    ["selected"]  -- selected is required; reasoning is optional


-- | Why exploration ended.
data FinalizeReason
  = FrontierEmpty
    -- ^ No more symbols to explore
  | BudgetExhausted
    -- ^ Hit the symbol exploration limit
  | DepthLimitReached
    -- ^ Hit maximum BFS depth
  deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH MEMORY STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Shared memory state for the DocGen graph.
--
-- This state is accessible to all nodes via the Memory effect.
-- It tracks the exploration progress and accumulated results.
data ExploreState = ExploreState
  { esGraph :: Map SymbolKey (Int, LSPSymbol)
    -- ^ Explored symbols: key -> (depth, symbol data)
  , esFrontier :: [(SymbolKey, Int)]
    -- ^ Symbols queued for exploration: (key, depth)
  , esVisited :: Set SymbolKey
    -- ^ Symbols already processed (avoid cycles)
  , esBudget :: Int
    -- ^ Remaining exploration budget
  , esTopic :: Text
    -- ^ Topic being explored (from original query)
  , esMaxDepth :: Int
    -- ^ Maximum BFS depth allowed
  } deriving stock (Show, Eq, Generic)

-- | Create initial state from a query.
--
-- Seeds are resolved to SymbolKeys by the caller before graph execution.
initialExploreState
  :: Text           -- ^ Topic
  -> [SymbolKey]    -- ^ Resolved seed symbols
  -> Int            -- ^ Budget
  -> Int            -- ^ Max depth
  -> ExploreState
initialExploreState topic seeds budget maxDepth = ExploreState
  { esGraph = Map.empty
  , esFrontier = map (, 0) seeds  -- Seeds at depth 0
  , esVisited = Set.empty
  , esBudget = budget
  , esTopic = topic
  , esMaxDepth = maxDepth
  }
