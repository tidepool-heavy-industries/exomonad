{-# LANGUAGE StrictData #-}

-- | Core types for FunctionGemma training data generation.
--
-- These types define the LOCKED SCHEMA for the rating model.
-- Do NOT change field names or enum variants without retraining.
-- The 270M model is syntax-sensitive.
module ExoMonad.Training.Types
  ( -- * Tag Enum (Fixed)
    Tag (..),
    allTags,
    tagToText,

    -- * Rubric (Output Schema)
    Rubric (..),

    -- * Input Context
    NodeContext (..),
    QueryContext (..),

    -- * Training Example
    TrainingExample (..),
    EdgeTrainingExample (..),

    -- * Score Edge (Flat Format for 270M model)
    ScoreEdgeInput (..),
    ScoreEdgeOutput (..),

    -- * Select Symbols Example (for FunctionGemma symbol selection)
    SelectSymbolsExample (..),

    -- * Candidate Groups (LSP orchestration output)
    CandidateGroups (..),

    -- * Edge Types (LSP-derived relationship types)
    EdgeType (..),
    allEdgeTypes,
    edgeTypeToText,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | Fixed tag enum for semantic classification.
--
-- IMPORTANT: Do not add/remove/rename variants without retraining.
-- The model learns to output exactly these strings.
data Tag
  = -- | Exhaustive pattern match (must handle all cases)
    Exhaustive
  | -- | Case/pattern matching construct
    PatternMatch
  | -- | Type family definition or instance
    TypeFamily
  | -- | Adding a variant/field would break this
    BreaksOnAdd
  | -- | Import statement (typically low relevance)
    Import
  | -- | Re-export (typically low relevance)
    ReExport
  | -- | Type signature only (no implementation)
    Signature
  | -- | Actual implementation code
    Implementation
  | -- | Data constructor usage
    Constructor
  | -- | Recursive function call
    Recursive
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
  { -- | 1-5: How on-topic for the query
    relevance :: Int,
    -- | 1-5: How likely to break if changed
    risk :: Int,
    -- | 1-5: How much context needed to understand
    complexity :: Int,
    -- | 1-5: How confident in this rating
    confidence :: Int,
    -- | Which tags apply (from fixed enum)
    tags :: [Tag]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Input context for a code location to be rated.
data NodeContext = NodeContext
  { -- | File location, e.g. "Edges.hs:89"
    location :: Text,
    -- | Hover info from LSP (markdown)
    hover :: Text,
    -- | Surrounding code lines
    codeSnippet :: Text,
    -- | Exploration depth (0 = root)
    depth :: Int,
    -- | Number of siblings at this level
    breadth :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Query context from the user.
data QueryContext = QueryContext
  { -- | Natural language query
    query :: Text,
    -- | Interest tags from caller
    tags :: [Tag]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Complete training example.
data TrainingExample = TrainingExample
  { -- | The query being answered
    query :: QueryContext,
    -- | The code location being rated
    node :: NodeContext,
    -- | Ground truth rating
    rubric :: Rubric
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Input context for scoring an edge (flattened for 270M model).
data ScoreEdgeInput = ScoreEdgeInput
  { -- | Natural language query
    query :: Text,
    -- | Source location file
    sourceFile :: Text,
    -- | Source location line
    sourceLine :: Int,
    -- | Hover info at source
    sourceHover :: Text,
    -- | Target location file
    targetFile :: Text,
    -- | Target location line
    targetLine :: Int,
    -- | Hover info at target
    targetHover :: Text,
    -- | Relationship type
    edgeType :: EdgeType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Output rubric for an edge (flattened for 270M model).
data ScoreEdgeOutput = ScoreEdgeOutput
  { -- | 1-5: How relevant to query
    relevance :: Int,
    -- | 1-5: How risky to modify
    risk :: Int,
    -- | Natural language justification
    reasoning :: Text,
    -- | Pattern match exhaustiveness
    isExhaustive :: Bool,
    -- | Type-level computation
    isTypeFamily :: Bool,
    -- | Public API surface
    isExported :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Complete training example for edge scoring.
data EdgeTrainingExample = EdgeTrainingExample
  { input :: ScoreEdgeInput,
    output :: ScoreEdgeOutput
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Training example input for select_symbols tool (from LSP exploration).
data SelectSymbolsExample = SelectSymbolsExample
  { -- | Symbol name from LSP
    symbolName :: Text,
    -- | File location "File.hs:42"
    location :: Text,
    -- | Type signature from LSP hover
    signature :: Text,
    -- | Extracted type names
    candidates :: [Text]
  }
  deriving stock (Eq, Show, Generic)
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
  { -- | Record fields from documentSymbol
    fields :: [Text],
    -- | Argument types (dependencies)
    inputs :: [Text],
    -- | Return type (what this produces)
    output :: [Text],
    -- | Left = "[many refs]", Right = ref list
    references :: Either Text [Text]
  }
  deriving stock (Eq, Show, Generic)
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
