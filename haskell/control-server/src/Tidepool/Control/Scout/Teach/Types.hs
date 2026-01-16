{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

-- | Types for teaching document generation via LSP + Gemma.
--
-- Core insight: Gemma extracts edges (what symbols are needed to explain this symbol).
-- Haskell traverses the graph and generates topologically-sorted teaching documents.
--
-- @
-- Seeds: [compositeScore]
--          │
--          ▼
--     ┌─────────┐     ┌─────────┐     ┌──────────────────┐
--     │  LSP    │────▶│  Gemma  │────▶│  mentions:       │
--     │  hover  │     │ classify│     │  [ScoreConfig,   │
--     └─────────┘     └─────────┘     │   Rubric,        │
--                                     │   EdgeContext]   │
--                                     └────────┬─────────┘
--                                              │
--          ┌───────────────────────────────────┘
--          ▼
--     Add mentions to frontier, repeat until empty
--          │
--          ▼
--     Topological sort by mentions → Teaching order
-- @
module Tidepool.Control.Scout.Teach.Types
  ( -- * Gemma Output
    TeachOutput(..)
  , SymbolRole(..)
  , symbolRoleToText

    -- * LSP Symbol Data
  , LSPSymbol(..)

    -- * Exploration State
  , SymbolKey
  , TeachState(..)
  , initialTeachState

    -- * Teaching Document
  , TeachingDoc(..)
  , TeachingUnit(..)

    -- * Query Input
  , TeachQuery(..)

    -- * Errors
  , TeachError(..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.:), (.=)
  , object, withObject, withText
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..))
import Tidepool.Effect.LSP (SymbolKind, Location)


-- ════════════════════════════════════════════════════════════════════════════
-- GEMMA OUTPUT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Symbol role in understanding the topic.
--
-- Used by Gemma to classify how a symbol relates to the query topic.
data SymbolRole
  = CoreConcept      -- ^ This IS the topic (e.g., the seed symbol)
  | Implementation   -- ^ How the topic works (internal details)
  | Interface        -- ^ How the topic connects (API surface)
  | Helper           -- ^ Utility code used by the topic
  | Unrelated        -- ^ Skip this symbol
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Convert role to lowercase text for wire format.
symbolRoleToText :: SymbolRole -> Text
symbolRoleToText = \case
  CoreConcept    -> "core"
  Implementation -> "implementation"
  Interface      -> "interface"
  Helper         -> "helper"
  Unrelated      -> "unrelated"

instance FromJSON SymbolRole where
  parseJSON = withText "SymbolRole" $ \t -> case T.toLower t of
    "core"           -> pure CoreConcept
    "coreconcept"    -> pure CoreConcept
    "core_concept"   -> pure CoreConcept
    "implementation" -> pure Implementation
    "impl"           -> pure Implementation
    "interface"      -> pure Interface
    "helper"         -> pure Helper
    "utility"        -> pure Helper
    "unrelated"      -> pure Unrelated
    "skip"           -> pure Unrelated
    _                -> fail $ "Unknown SymbolRole: " <> T.unpack t

instance ToJSON SymbolRole where
  toJSON = toJSON . symbolRoleToText


-- | Output from Gemma's symbol classification.
--
-- Strict parsing - no fallbacks. If Gemma fails to provide valid output,
-- the exploration fails with an error.
data TeachOutput = TeachOutput
  { toRole      :: SymbolRole
    -- ^ What role does this symbol play in understanding the topic?
  , toIsPrereq  :: Bool
    -- ^ Must understand this symbol BEFORE understanding the topic?
  , toMentions  :: [Text]
    -- ^ Other symbols needed to explain THIS symbol
  } deriving stock (Show, Eq, Generic)

instance FromJSON TeachOutput where
  parseJSON = withObject "TeachOutput" $ \v -> TeachOutput
    <$> v .: "role"
    <*> v .: "is_prerequisite"
    <*> v .: "mentions"

instance ToJSON TeachOutput where
  toJSON t = object
    [ "role"           .= toRole t
    , "is_prerequisite" .= toIsPrereq t
    , "mentions"       .= toMentions t
    ]


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
data TeachState = TeachState
  { tsGraph    :: Map SymbolKey (TeachOutput, LSPSymbol)
    -- ^ Explored symbols with their classifications
  , tsFrontier :: Set SymbolKey
    -- ^ Symbols queued for exploration
  , tsVisited  :: Set SymbolKey
    -- ^ Symbols already processed
  , tsBudget   :: Int
    -- ^ Remaining exploration budget
  , tsCache    :: Map SymbolKey TeachOutput
    -- ^ Gemma response cache (avoid redundant calls)
  } deriving stock (Show, Eq, Generic)

-- | Create initial state from seeds and budget.
initialTeachState :: [SymbolKey] -> Int -> TeachState
initialTeachState seeds budget = TeachState
  { tsGraph    = Map.empty
  , tsFrontier = Set.fromList seeds
  , tsVisited  = Set.empty
  , tsBudget   = budget
  , tsCache    = Map.empty
  }


-- ════════════════════════════════════════════════════════════════════════════
-- TEACHING DOCUMENT
-- ════════════════════════════════════════════════════════════════════════════

-- | A complete teaching document.
--
-- Organized in prerequisite order: understand prereqs before core content.
data TeachingDoc = TeachingDoc
  { tdTitle     :: Text
    -- ^ Title (based on topic)
  , tdTopic     :: Text
    -- ^ Original topic query
  , tdPrereqs   :: [TeachingUnit]
    -- ^ Prerequisites: understand these first
  , tdCore      :: [TeachingUnit]
    -- ^ Core content: the main topic
  , tdSupport   :: [TeachingUnit]
    -- ^ Supporting material: helpers, utilities
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | A single teaching unit (one symbol to explain).
data TeachingUnit = TeachingUnit
  { tuSymbol    :: LSPSymbol
    -- ^ The symbol being taught
  , tuRole      :: SymbolRole
    -- ^ Role in understanding the topic
  , tuMentions  :: [Text]
    -- ^ Symbols this unit references
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
    <*> (v .: "budget" >>= \b -> pure $ if b <= 0 then 20 else b)

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


-- ════════════════════════════════════════════════════════════════════════════
-- ERRORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during teaching exploration.
data TeachError
  = TeachGemmaParseError Text Text
    -- ^ Gemma returned unparseable response: (raw response, parse error)
  | TeachLSPError Text
    -- ^ LSP lookup failed: (error message)
  | TeachNoSymbolsFound Text
    -- ^ No symbols found for seed: (seed name)
  | TeachBudgetExhausted Int
    -- ^ Budget exhausted before completion: (symbols explored)
  deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
