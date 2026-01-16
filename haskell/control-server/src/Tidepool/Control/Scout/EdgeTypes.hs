{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

-- | Edge types for semantic code exploration.
--
-- Each LSP reference type generates different template context for FunctionGemma.
-- The model learns type-specific patterns for relevance scoring.
module Tidepool.Control.Scout.EdgeTypes
  ( -- * Edge Classification
    EdgeType(..)
  , edgeTypeToText
  , inferEdgeType

    -- * Edge Context
  , EdgeContext(..)
  , mkEdgeContext
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)


-- | Different edge types from LSP, each gets different template context.
--
-- FunctionGemma learns to score edges differently based on type:
--   * TypeReference: Adding a variant breaks type refs
--   * PatternMatchSite: Exhaustive matches need new cases
--   * DefinitionSite: Where the symbol is defined
--   * etc.
data EdgeType
  = TypeReference      -- ^ Reference to a type (data, newtype, type alias)
  | ValueReference     -- ^ Reference to a value (function, variable)
  | DefinitionSite     -- ^ Where something is defined
  | UsageSite          -- ^ Where something is used
  | ImportEdge         -- ^ Import statement
  | ExportEdge         -- ^ Export/re-export
  | ConstructorRef     -- ^ Data constructor usage
  | PatternMatchSite   -- ^ Pattern match on a type
  | InstanceSite       -- ^ Typeclass instance
  | UnknownEdge        -- ^ Fallback when type can't be inferred
  deriving stock (Show, Eq, Generic, Bounded, Enum)
  deriving anyclass (FromJSON, ToJSON)

-- | Convert edge type to text for templates.
edgeTypeToText :: EdgeType -> Text
edgeTypeToText = \case
  TypeReference    -> "type reference"
  ValueReference   -> "value reference"
  DefinitionSite   -> "definition"
  UsageSite        -> "usage"
  ImportEdge       -> "import"
  ExportEdge       -> "export"
  ConstructorRef   -> "constructor"
  PatternMatchSite -> "pattern match"
  InstanceSite     -> "instance"
  UnknownEdge      -> "reference"

-- | Infer edge type from hover text and code snippet.
--
-- Uses pattern matching on LSP hover output to classify edges.
-- This is heuristic-based; could be improved with semantic analysis.
inferEdgeType :: Text -> Text -> EdgeType
inferEdgeType hover snippet
  -- Pattern matches
  | "case" `T.isInfixOf` snippet && "{" `T.isInfixOf` snippet = PatternMatchSite
  | "case" `T.isInfixOf` snippet = PatternMatchSite
  | "\\" `T.isPrefixOf` T.strip snippet && "->" `T.isInfixOf` snippet = PatternMatchSite

  -- Type definitions and references
  | "type family" `T.isInfixOf` hover = TypeReference
  | "data " `T.isPrefixOf` hover = TypeReference
  | "newtype " `T.isPrefixOf` hover = TypeReference
  | "type " `T.isPrefixOf` hover = TypeReference
  | "class " `T.isPrefixOf` hover = TypeReference

  -- Instance sites
  | "instance " `T.isPrefixOf` hover = InstanceSite
  | "instance" `T.isInfixOf` snippet = InstanceSite

  -- Import/export
  | "import " `T.isPrefixOf` T.strip snippet = ImportEdge
  | "module " `T.isPrefixOf` T.strip snippet = ExportEdge

  -- Constructor usage
  | any (`T.isPrefixOf` hover) ["Just ", "Nothing", "Left ", "Right "] = ConstructorRef
  | any (`T.isInfixOf` snippet) [" = Just", " = Nothing", " = Left", " = Right"] = ConstructorRef

  -- Function/value definitions
  | " :: " `T.isInfixOf` hover && " = " `T.isInfixOf` snippet = DefinitionSite
  | " :: " `T.isInfixOf` hover = ValueReference

  -- Fallback
  | otherwise = UsageSite


-- | Context for an edge being evaluated by FunctionGemma.
--
-- Contains all information needed to render the scoring template.
-- Type-specific fields are populated based on EdgeType.
data EdgeContext = EdgeContext
  { ecEdgeType    :: EdgeType
    -- ^ Classification of this edge
  , ecLocation    :: Text
    -- ^ File:line location
  , ecHover       :: Text
    -- ^ LSP hover info (type signature, docs)
  , ecSnippet     :: Text
    -- ^ Code context (5-10 surrounding lines)
  , ecDepth       :: Int
    -- ^ Exploration depth (0 = entry point)
  , ecParent      :: Maybe Text
    -- ^ Parent location (where we came from)
  -- Type-specific fields
  , ecTypeName    :: Maybe Text
    -- ^ For TypeReference: the type name
  , ecFunctionSig :: Maybe Text
    -- ^ For ValueReference: the function signature
  , ecPatterns    :: Maybe [Text]
    -- ^ For PatternMatchSite: the pattern cases
  , ecConstraints :: Maybe [Text]
    -- ^ For InstanceSite: the constraints
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Create edge context from LSP data.
--
-- Infers edge type and extracts type-specific fields.
mkEdgeContext
  :: Text       -- ^ Location (file:line)
  -> Text       -- ^ Hover info
  -> Text       -- ^ Code snippet
  -> Int        -- ^ Depth
  -> Maybe Text -- ^ Parent location
  -> EdgeContext
mkEdgeContext loc hover snippet depth parent =
  let edgeType = inferEdgeType hover snippet
  in EdgeContext
    { ecEdgeType    = edgeType
    , ecLocation    = loc
    , ecHover       = hover
    , ecSnippet     = snippet
    , ecDepth       = depth
    , ecParent      = parent
    -- Extract type-specific fields
    , ecTypeName    = extractTypeName edgeType hover
    , ecFunctionSig = extractFunctionSig edgeType hover
    , ecPatterns    = extractPatterns edgeType snippet
    , ecConstraints = extractConstraints edgeType hover
    }

-- | Extract type name from hover for TypeReference edges.
extractTypeName :: EdgeType -> Text -> Maybe Text
extractTypeName TypeReference hover =
  case T.words hover of
    ("data":name:_)    -> Just name
    ("newtype":name:_) -> Just name
    ("type":name:_)    -> Just name
    ("class":name:_)   -> Just name
    _                  -> Nothing
extractTypeName _ _ = Nothing

-- | Extract function signature from hover for ValueReference edges.
extractFunctionSig :: EdgeType -> Text -> Maybe Text
extractFunctionSig ValueReference hover
  | " :: " `T.isInfixOf` hover = Just $ T.strip $ snd $ T.breakOn " :: " hover
extractFunctionSig DefinitionSite hover
  | " :: " `T.isInfixOf` hover = Just $ T.strip $ snd $ T.breakOn " :: " hover
extractFunctionSig _ _ = Nothing

-- | Extract pattern cases from snippet for PatternMatchSite edges.
extractPatterns :: EdgeType -> Text -> Maybe [Text]
extractPatterns PatternMatchSite snippet =
  let lns = T.lines snippet
      patternLines = filter (\l -> "->" `T.isInfixOf` l || "= case" `T.isInfixOf` l) lns
  in if null patternLines then Nothing else Just patternLines
extractPatterns _ _ = Nothing

-- | Extract constraints from hover for InstanceSite edges.
extractConstraints :: EdgeType -> Text -> Maybe [Text]
extractConstraints InstanceSite hover
  | "=>" `T.isInfixOf` hover =
      let (constraints, _) = T.breakOn "=>" hover
      in Just $ map T.strip $ T.splitOn "," constraints
extractConstraints _ _ = Nothing
