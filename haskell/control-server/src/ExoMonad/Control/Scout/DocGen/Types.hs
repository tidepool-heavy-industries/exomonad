{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Types for teaching document generation via LSP + Gemma.
module ExoMonad.Control.Scout.DocGen.Types
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

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), (.!=), withObject)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), MCPOptions(..), HasJSONSchema(..), arraySchema, emptySchema, SchemaType(..), describeField)
import ExoMonad.Effect.LSP (SymbolKind, Location)


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

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "tq" } ''TeachQuery
  [ 'tqTopic  ?? "Topic to teach (e.g., 'the scoring system')"
  , 'tqSeeds  ?? "Entry point symbols (e.g., ['compositeScore'])"
  , 'tqBudget ?? "Maximum symbols to explore (default: 20)"
  ])