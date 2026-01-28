{-# LANGUAGE StrictData #-}

-- | Core types for FunctionGemma training data generation.
--
-- These types define the LOCKED SCHEMA for the rating model.
-- Do NOT change field names or enum variants without retraining.
-- The 270M model is syntax-sensitive.
module ExoMonad.Training.Types
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
  , EdgeTrainingExample(..)

    -- * Score Edge (Flat Format for 270M model)
  , ScoreEdgeInput(..)
  , ScoreEdgeOutput(..)

    -- * Select Symbols Example (for FunctionGemma symbol selection)
  , SelectSymbolsExample(..)

    -- * Candidate Groups (LSP orchestration output)
  , CandidateGroups(..)

    -- * Edge Types (LSP-derived relationship types)
  , EdgeType(..)
  , allEdgeTypes
  , edgeTypeToText
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))

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
  { relevance  :: Int   -- ^ 1-5: How on-topic for the query
  , risk       :: Int   -- ^ 1-5: How likely to break if changed
  , complexity :: Int   -- ^ 1-5: How much context needed to understand
  , confidence :: Int   -- ^ 1-5: How confident in this rating
  , tags       :: [Tag] -- ^ Which tags apply (from fixed enum)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Input context for a code location to be rated.
data NodeContext = NodeContext
  { location    :: Text  -- ^ File location, e.g. "Edges.hs:89"
  , hover       :: Text  -- ^ Hover info from LSP (markdown)
  , codeSnippet :: Text  -- ^ Surrounding code lines
  , depth       :: Int   -- ^ Exploration depth (0 = root)
  , breadth     :: Int   -- ^ Number of siblings at this level
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Query context from the user.
data QueryContext = QueryContext
  { query :: Text   -- ^ Natural language query
  , tags  :: [Tag]  -- ^ Interest tags from caller
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Complete training example.
data TrainingExample = TrainingExample
  { query  :: QueryContext  -- ^ The query being answered
  , node   :: NodeContext   -- ^ The code location being rated
  , rubric :: Rubric        -- ^ Ground truth rating
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Input context for scoring an edge (flattened for 270M model).
data ScoreEdgeInput = ScoreEdgeInput
  { query       :: Text      -- ^ Natural language query
  , sourceFile  :: Text      -- ^ Source location file
  , sourceLine  :: Int       -- ^ Source location line
  , sourceHover :: Text      -- ^ Hover info at source
  , targetFile  :: Text      -- ^ Target location file
  , targetLine  :: Int       -- ^ Target location line
  , targetHover :: Text      -- ^ Hover info at target
  , edgeType    :: EdgeType  -- ^ Relationship type
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Output rubric for an edge (flattened for 270M model).
data ScoreEdgeOutput = ScoreEdgeOutput
  { relevance    :: Int   -- ^ 1-5: How relevant to query
  , risk         :: Int   -- ^ 1-5: How risky to modify
  , reasoning    :: Text  -- ^ Natural language justification
  , isExhaustive :: Bool  -- ^ Pattern match exhaustiveness
  , isTypeFamily :: Bool  -- ^ Type-level computation
  , isExported   :: Bool  -- ^ Public API surface
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Complete training example for edge scoring.
data EdgeTrainingExample = EdgeTrainingExample
  { input  :: ScoreEdgeInput
  , output :: ScoreEdgeOutput
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Training example input for select_symbols tool (from LSP exploration).
data SelectSymbolsExample = SelectSymbolsExample
  { symbolName :: Text   -- ^ Symbol name from LSP
  , location   :: Text   -- ^ File location "File.hs:42"
  , signature  :: Text   -- ^ Type signature from LSP hover
  , candidates :: [Text] -- ^ Extracted type names
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Grouped candidates by edge type for rich training data.
--
-- Extracted via LSP orchestration:
-- - Fields: from documentSymbol (record fields, constructors)
-- - Inputs: argument types from signature (dependencies)
-- - Output: return type from signature (what this produces)
-- - References: from findReferences (usage sites, capped)
--
-- The Input/Output split helps the navigator model answer directional queries:
-- "How do I get a User?" → look at Output
-- "What needs a Config?" → look at Inputs
data CandidateGroups = CandidateGroups
  { fields     :: [Text]            -- ^ Record fields from documentSymbol
  , inputs     :: [Text]            -- ^ Argument types (dependencies)
  , output     :: [Text]            -- ^ Return type (what this produces)
  , references :: Either Text [Text] -- ^ Left = "[many refs]", Right = ref list
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Edge type enum for LSP-derived relationships.
data EdgeType
  = Definition
  | Reference
  | Usage
  | Instance
  | TypeConstraint
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON)

allEdgeTypes :: [EdgeType]
allEdgeTypes = [minBound .. maxBound]

edgeTypeToText :: EdgeType -> Text
edgeTypeToText = T.toLower . T.pack . show
