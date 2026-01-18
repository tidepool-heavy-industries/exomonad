{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

-- | Types for teaching document generation via LSP + Gemma.
--
-- Core insight: Gemma extracts edges (related symbols). Haskell traverses the graph.
--
-- Key simplification: Gemma's ONLY job is to return symbol names to follow.
-- Everything else (teaching order) is derived from BFS depth.
--
-- @
-- Seeds: [compositeScore]
--          │
--          ▼
--     ┌─────────┐     ┌─────────┐     ┌──────────────────┐
--     │  LSP    │────▶│  Gemma  │────▶│  "ScoreConfig    │
--     │  hover  │     │ (simple)│     │   Rubric         │
--     └─────────┘     └─────────┘     │   EdgeContext"   │
--                                     └────────┬─────────┘
--                                              │
--          ┌───────────────────────────────────┘
--          ▼
--     Parse tokens, resolve via LSP, add to frontier
--          │
--          ▼
--     BFS depth = teaching order (closer to seeds = more foundational)
-- @
module Tidepool.Control.Scout.DocGen.Types
  ( -- * LSP Symbol Data
    LSPSymbol(..)

    -- * Exploration State
  , SymbolKey
  , TeachState(..)
  , initialTeachState

    -- * Teaching Document
  , TeachingDoc(..)
  , TeachingUnit(..)

    -- * Query Input
  , TeachQuery(..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.:), (.:?), (.!=), (.=)
  , object, withObject
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..))
import Tidepool.Effect.LSP (SymbolKind, Location)


-- ════════════════════════════════════════════════════════════════════════════
-- LSP SYMBOL DATA
-- ════════════════════════════════════════════════════════════════════════════

-- | Symbol data from LSP lookups.
data LSPSymbol = LSPSymbol
  { lsName      :: Text
    -- ^ Symbol name (e.g., "compositeScore")
  , lsKind      :: SymbolKind
    -- ^ Symbol kind (Function, Class, etc.)
  , lsLocation  :: Location
    -- ^ File location
  , lsSignature :: Text
    -- ^ Type signature from hover info
  , lsDocComment :: Maybe Text
    -- ^ Documentation comment from hover (if available)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- EXPLORATION STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Symbol key for disambiguation.
--
-- Combines file path with symbol name to handle same-named symbols
-- in different modules.
type SymbolKey = (FilePath, Text)

-- | State maintained during teaching exploration.
--
-- Simplified: depth tracked with symbols, no role classification.
data TeachState = TeachState
  { tsGraph    :: Map SymbolKey (Int, LSPSymbol)
    -- ^ Explored symbols: (depth, symbol data)
  , tsFrontier :: [(SymbolKey, Int)]
    -- ^ Symbols queued for exploration: (key, depth)
  , tsVisited  :: Set SymbolKey
    -- ^ Symbols already processed
  , tsBudget   :: Int
    -- ^ Remaining exploration budget
  } deriving stock (Show, Eq, Generic)

-- | Create initial state from seeds and budget.
initialTeachState :: [SymbolKey] -> Int -> TeachState
initialTeachState seeds budget = TeachState
  { tsGraph    = Map.empty
  , tsFrontier = map (\k -> (k, 0)) seeds  -- Seeds are at depth 0
  , tsVisited  = Set.empty
  , tsBudget   = budget
  }


-- ════════════════════════════════════════════════════════════════════════════
-- TEACHING DOCUMENT
-- ════════════════════════════════════════════════════════════════════════════

-- | A complete teaching document.
--
-- Organized by BFS depth:
--   - Depth 0: Seed symbols (core)
--   - Depth 1-2: Direct dependencies (prerequisites)
--   - Depth 3+: Transitive dependencies (support)
data TeachingDoc = TeachingDoc
  { tdTitle     :: Text
    -- ^ Title (based on topic)
  , tdTopic     :: Text
    -- ^ Original topic query
  , tdCore      :: [TeachingUnit]
    -- ^ Core content: seed symbols (depth 0)
  , tdPrereqs   :: [TeachingUnit]
    -- ^ Prerequisites: depth 1-2
  , tdSupport   :: [TeachingUnit]
    -- ^ Supporting material: depth 3+
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | A single teaching unit (one symbol to explain).
--
-- Simplified: just depth, no role classification.
data TeachingUnit = TeachingUnit
  { tuSymbol    :: LSPSymbol
    -- ^ The symbol being taught
  , tuDepth     :: Int
    -- ^ BFS depth from seeds (0 = seed symbol)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- QUERY INPUT
-- ════════════════════════════════════════════════════════════════════════════

-- | Input query for teaching document generation.
data TeachQuery = TeachQuery
  { tqTopic   :: Text
    -- ^ Topic to teach (e.g., "the scoring system")
  , tqSeeds   :: [Text]
    -- ^ Entry point symbols (e.g., ["compositeScore"])
  , tqBudget  :: Int
    -- ^ Maximum symbols to explore (default: 20)
  } deriving stock (Show, Eq, Generic)

instance FromJSON TeachQuery where
  parseJSON = withObject "TeachQuery" $ \v -> TeachQuery
    <$> v .: "topic"
    <*> v .: "seeds"
    <*> (v .:? "budget" .!= 20)

instance ToJSON TeachQuery where
  toJSON q = object
    [ "topic"  .= tqTopic q
    , "seeds"  .= tqSeeds q
    , "budget" .= tqBudget q
    ]

-- | JSON schema for MCP tool.
instance HasJSONSchema TeachQuery where
  jsonSchema = objectSchema
    [ ("topic", emptySchema TString)
    , ("seeds", arraySchema (emptySchema TString))
    , ("budget", emptySchema TInteger)
    ]
    ["topic", "seeds"]  -- topic and seeds required; budget defaults to 20


