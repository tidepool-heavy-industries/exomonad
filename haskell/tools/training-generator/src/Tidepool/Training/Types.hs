{-# LANGUAGE StrictData #-}

-- | Core types for FunctionGemma training data generation.
--
-- These types define the LOCKED SCHEMA for the rating model.
-- Do NOT change field names or enum variants without retraining.
-- The 270M model is syntax-sensitive.
module Tidepool.Training.Types
  ( -- * Tag Enum (Fixed)
    Tag(..)
  , allTags
  , tagToText

    -- * Rubric (Output Schema)
  , Rubric(..)

    -- * Input Context
  , NodeContext(..)
  , QueryContext(..)

    -- * Training Example
  , TrainingExample(..)

    -- * Edge Types (Flat Schema for FunctionGemma)
    -- These types are designed for the 270M model which performs best
    -- with flat, shallow data structures. Individual booleans instead of tag lists.
  , EdgeType(..)
  , allEdgeTypes
  , edgeTypeToText
  , ScoreEdgeInput(..)
  , ScoreEdgeOutput(..)
  , EdgeTrainingExample(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)


-- | Fixed tag enum for semantic classification.
--
-- IMPORTANT: Do not add/remove/rename variants without retraining.
-- The model learns to output exactly these strings.
data Tag
  = Exhaustive        -- ^ Exhaustive pattern match (must handle all cases)
  | PatternMatch      -- ^ Case/pattern matching construct
  | TypeFamily        -- ^ Type family definition or instance
  | BreaksOnAdd       -- ^ Adding a variant/field would break this
  | Import            -- ^ Import statement (typically low relevance)
  | ReExport          -- ^ Re-export (typically low relevance)
  | Signature         -- ^ Type signature only (no implementation)
  | Implementation    -- ^ Actual implementation code
  | Constructor       -- ^ Data constructor usage
  | Recursive         -- ^ Recursive function call
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | All tags in enum order.
allTags :: [Tag]
allTags = [minBound .. maxBound]

-- | Convert tag to lowercase text for output.
tagToText :: Tag -> Text
tagToText = T.toLower . T.pack . show


-- | Rubric output schema (locked).
--
-- All fields are 1-5 integers. Tags must be from the fixed enum.
data Rubric = Rubric
  { rRelevance  :: Int   -- ^ 1-5: How on-topic for the query
  , rRisk       :: Int   -- ^ 1-5: How likely to break if changed
  , rComplexity :: Int   -- ^ 1-5: How much context needed to understand
  , rConfidence :: Int   -- ^ 1-5: How confident in this rating
  , rTags       :: [Tag] -- ^ Which tags apply (from fixed enum)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Input context for a code location to be rated.
data NodeContext = NodeContext
  { ncLocation    :: Text  -- ^ File location, e.g. "Edges.hs:89"
  , ncHover       :: Text  -- ^ Hover info from LSP (markdown)
  , ncCodeSnippet :: Text  -- ^ Surrounding code lines
  , ncDepth       :: Int   -- ^ Exploration depth (0 = root)
  , ncBreadth     :: Int   -- ^ Number of siblings at this level
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Query context from the user.
data QueryContext = QueryContext
  { qcQuery :: Text   -- ^ Natural language query
  , qcTags  :: [Tag]  -- ^ Interest tags from caller
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Complete training example.
data TrainingExample = TrainingExample
  { teQuery  :: QueryContext  -- ^ The query being answered
  , teNode   :: NodeContext   -- ^ The code location being rated
  , teRubric :: Rubric        -- ^ Ground truth rating
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- -----------------------------------------------------------------------------
-- Edge Types (Flat Schema for FunctionGemma 270M)
-- -----------------------------------------------------------------------------

-- | Edge type enum for LSP-derived relationships.
--
-- IMPORTANT: Do not add/remove/rename variants without retraining.
-- The model learns to output exactly these strings.
data EdgeType
  = Definition      -- ^ Go-to-definition edge (symbol → its definition)
  | Reference       -- ^ Find-references edge (definition → usage sites)
  | Usage           -- ^ Usage/call site (function → where it's called)
  | Instance        -- ^ Interface implementation (typeclass → instance)
  | TypeConstraint  -- ^ Typeclass constraint (function → required instances)
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | All edge types in enum order.
allEdgeTypes :: [EdgeType]
allEdgeTypes = [minBound .. maxBound]

-- | Convert edge type to lowercase text for wire format.
edgeTypeToText :: EdgeType -> Text
edgeTypeToText = T.toLower . T.pack . show


-- | Edge scoring input (FLAT - no nested records or lists).
--
-- Designed for FunctionGemma 270M which performs best with shallow structures.
-- All string values will be wrapped in \<escape\> tokens in wire format.
data ScoreEdgeInput = ScoreEdgeInput
  { seiQuery       :: Text      -- ^ Natural language query
  , seiSourceFile  :: Text      -- ^ Source location file path
  , seiSourceLine  :: Int       -- ^ Source location line number
  , seiSourceHover :: Text      -- ^ Hover info at source (type sig, docs)
  , seiTargetFile  :: Text      -- ^ Target location file path
  , seiTargetLine  :: Int       -- ^ Target location line number
  , seiTargetHover :: Text      -- ^ Hover info at target (type sig, docs)
  , seiEdgeType    :: EdgeType  -- ^ Type of edge (Definition, Reference, etc.)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Edge scoring output (FLAT - individual bools instead of tag list).
--
-- Designed for FunctionGemma 270M. Tags are flattened to boolean fields
-- to eliminate list-parsing errors and simplify the classification task.
data ScoreEdgeOutput = ScoreEdgeOutput
  { seoRelevance    :: Int   -- ^ 1-5: How relevant to the query
  , seoRisk         :: Int   -- ^ 1-5: How risky to modify this code
  , seoReasoning    :: Text  -- ^ Natural language justification
  -- Flattened tag flags (instead of [Tag] list)
  , seoIsExhaustive :: Bool  -- ^ Pattern match exhaustiveness concern
  , seoIsTypeFamily :: Bool  -- ^ Type-level computation involved
  , seoIsExported   :: Bool  -- ^ Part of public API surface
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Complete edge training example for FunctionGemma.
--
-- Captures a single edge scoring decision: given a query and an edge
-- (source → target with context), what rubric should the model produce?
data EdgeTrainingExample = EdgeTrainingExample
  { eteInput  :: ScoreEdgeInput   -- ^ The edge context being scored
  , eteOutput :: ScoreEdgeOutput  -- ^ Ground truth scoring rubric
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
