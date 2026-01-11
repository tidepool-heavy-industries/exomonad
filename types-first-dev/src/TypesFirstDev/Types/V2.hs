{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FieldSelectors #-}

-- | V2 Type definitions for tree-based decomposition workflow.
--
-- Architecture:
-- * Scaffold phase: writes TDD tests + stubs + plan, commits, outputs rubric
-- * Implement phase: if children exist, spawn/await/merge; then implement glue or full
-- * Exit: yields MR to parent
--
-- Key insight: Tests are part of scaffolding, not a parallel phase.
-- Children implement stubbed subsystems; parent implements glue.
--
-- Design principle: LLM reports structure (sensing), handler decides (policy).
module TypesFirstDev.Types.V2
  ( -- * Input Types
    Spec(..)
  , ScaffoldingCtx(..)
  , ImplementCtx(..)

    -- * Decomposition Oracle
  , ScaffoldingRubric(..)
  , Facet(..)
  , SubsystemSpec(..)
  , shouldDecompose
  , deriveSubsystemSpecs

    -- * LLM Output Schemas
  , ScaffoldingOutput(..)
  , ImplementOutput(..)

    -- * Internal State Types
  , ScaffoldingResult(..)
  , ChildResult(..)
  , V2Result(..)

    -- * Conversions
  , specFromSubsystem
  ) where

import Control.Monad.Writer (Writer)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.!=), withObject, object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal)
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

import Tidepool.StructuredOutput (StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- INPUT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Root specification for a task.
-- Passed from parent (or root) to start a graph instance.
--
-- Each graph instance receives a Spec and:
-- 1. Scaffolds (tests + stubs + rubric)
-- 2. Maybe spawns children for subsystems
-- 3. Implements (glue if children, or full if leaf)
-- 4. Yields MR to parent
data Spec = Spec
  { specDescription        :: Text         -- ^ What we're building
  , specAcceptanceCriteria :: [Text]       -- ^ Must-have features
  , specTargetPath         :: FilePath     -- ^ Where to write code
  , specTestPath           :: FilePath     -- ^ Where to write tests
  , specParentBranch       :: Maybe Text   -- ^ Parent git branch (Nothing for root)
  , specDepth              :: Int          -- ^ Recursion depth (0 for root)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (StructuredOutput)

-- | Custom FromJSON for user-friendly YAML field names.
--
-- YAML format:
-- @
-- description: "An effect-based URL shortener..."
-- acceptance_criteria:
--   - "UrlService effect GADT..."
--   - "Persistence effect..."
-- target_path: src/UrlShortener
-- test_path: test/UrlShortener
-- parent_branch: main          # optional
-- depth: 0                     # optional, defaults to 0
-- @
instance FromJSON Spec where
  parseJSON = withObject "Spec" $ \o -> Spec
    <$> o .: "description"
    <*> o .: "acceptance_criteria"
    <*> o .: "target_path"
    <*> o .: "test_path"
    <*> o .:? "parent_branch"
    <*> o .:? "depth" .!= 0

-- | Custom ToJSON to match FromJSON (snake_case field names).
instance ToJSON Spec where
  toJSON spec = object
    [ "description"         .= spec.specDescription
    , "acceptance_criteria" .= spec.specAcceptanceCriteria
    , "target_path"         .= spec.specTargetPath
    , "test_path"           .= spec.specTestPath
    , "parent_branch"       .= spec.specParentBranch
    , "depth"               .= spec.specDepth
    ]


-- | Context passed to scaffolding template.
--
-- Scaffolding does ALL of:
-- * Write TDD tests (QuickCheck properties for acceptance criteria)
-- * Write stubs (types + function signatures with undefined)
-- * Identify subsystems (facets that may become child specs)
-- * Make a commit
data ScaffoldingCtx = ScaffoldingCtx
  { scSpec          :: Spec
  , scParentContext :: Maybe Text  -- ^ Summary of parent's plan if depth > 0
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)


-- | Context passed to implementation template.
--
-- Implementation receives:
-- * Original spec and scaffolding result
-- * If decomposed: results from all child MRs (subsystems now implemented!)
-- * Task: implement glue code that uses the subsystems, OR full impl if leaf
data ImplementCtx = ImplementCtx
  { icSpec              :: Spec
  , icScaffoldingResult :: ScaffoldingResult
  , icChildResults      :: [ChildResult]      -- ^ Empty for leaf nodes
  , icIsGlueLayer       :: Bool               -- ^ True if children existed (impl is glue)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)


-- ════════════════════════════════════════════════════════════════════════════
-- DECOMPOSITION ORACLE
-- ════════════════════════════════════════════════════════════════════════════

-- | Rubric fields reported by LLM during scaffolding.
-- LLM reports what it SEES (structure). Handler DECIDES (policy).
--
-- Value-neutral design: LLM has no reason to game these fields
-- because it doesn't know the consequences.
data ScaffoldingRubric = ScaffoldingRubric
  { srParsedCriteria     :: [Text]   -- ^ Acceptance criteria as LLM parsed them
  , srIdentifiedFacets   :: [Facet]  -- ^ Natural decomposition boundaries
  , srOpenQuestions      :: [Text]   -- ^ Uncertainties that might need clarification
  , srExternalDeps       :: [Text]   -- ^ Dependencies outside spec scope
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)


-- | A facet is a natural decomposition boundary.
-- Each facet can become a child spec if handler decides to decompose.
--
-- Facets correspond to stubs that scaffolding created. If we decompose,
-- each facet's child will implement that stub. If we don't decompose,
-- the parent implements all stubs directly.
data Facet = Facet
  { facetName           :: Text      -- ^ Short name ("persistence", "validation")
  , facetResponsibility :: Text      -- ^ What this piece handles
  , facetTargetPath     :: FilePath  -- ^ Where the stub was written
  , facetTestPath       :: FilePath  -- ^ Where tests for this facet live
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)


-- | Subsystem spec derived from a facet.
-- Ready to be converted to Spec for child spawning.
data SubsystemSpec = SubsystemSpec
  { ssName              :: Text       -- ^ Facet name
  , ssDescription       :: Text       -- ^ What to implement
  , ssAcceptanceCriteria :: [Text]    -- ^ Scoped criteria
  , ssTargetPath        :: FilePath   -- ^ Where to write impl
  , ssTestPath          :: FilePath   -- ^ Where tests live
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)


-- | Handler policy: should we decompose this spec into child facets?
--
-- Score = criteriaCount + (facetCount * 3)
-- Threshold = 10 - (depth * 2)  -- linear penalty
-- Decompose if: score > threshold && facets > 1 && depth < 5
shouldDecompose :: Int -> ScaffoldingRubric -> Bool
shouldDecompose depth rubric =
  let criteriaCount = length rubric.srParsedCriteria
      facetCount = length rubric.srIdentifiedFacets
      score = criteriaCount + (facetCount * 3)
      threshold = 10 - (depth * 2)  -- linear penalty: deeper = stricter
      hasFacets = facetCount > 1
      hardLimit = 5
  in score > threshold && hasFacets && depth < hardLimit


-- | Derive subsystem specs from facets.
-- Called by handler when shouldDecompose returns True.
deriveSubsystemSpecs :: Spec -> [Facet] -> [SubsystemSpec]
deriveSubsystemSpecs _parentSpec facets =
  [ SubsystemSpec
      { ssName = facet.facetName
      , ssDescription = facet.facetResponsibility
      , ssAcceptanceCriteria = []  -- Child will re-parse from its description
      , ssTargetPath = facet.facetTargetPath
      , ssTestPath = facet.facetTestPath
      }
  | facet <- facets
  ]


-- | Convert SubsystemSpec to Spec for child spawning.
specFromSubsystem :: Spec -> SubsystemSpec -> Spec
specFromSubsystem parent ss = Spec
  { specDescription = ss.ssDescription
  , specAcceptanceCriteria = ss.ssAcceptanceCriteria
  , specTargetPath = ss.ssTargetPath
  , specTestPath = ss.ssTestPath
  , specParentBranch = Just $ maybe "main" id parent.specParentBranch
  , specDepth = parent.specDepth + 1
  }


-- ════════════════════════════════════════════════════════════════════════════
-- LLM OUTPUT SCHEMAS
-- ════════════════════════════════════════════════════════════════════════════

-- | Scaffolding agent output.
--
-- The scaffold phase writes to disk:
-- * TDD tests (QuickCheck properties that fail on undefined stubs)
-- * Type stubs (signatures with undefined implementations)
-- * Module structure
--
-- Then commits and returns this metadata.
data ScaffoldingOutput = ScaffoldingOutput
  { soPlan              :: Text              -- ^ High-level approach (2-3 sentences)
  , soTestsWritten      :: [Text]            -- ^ Property/test names written
  , soStubsCreated      :: [Text]            -- ^ Function/type names stubbed
  , soRubric            :: ScaffoldingRubric -- ^ Structure analysis for decomposition oracle
  , soCommitHash        :: Text              -- ^ Commit containing scaffold
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)


-- | Implementation agent output.
--
-- The implement phase:
-- * If leaf: implements all stubs to pass tests
-- * If glue: implements orchestration using now-real subsystems
--
-- Then commits and returns this metadata.
data ImplementOutput = ImplementOutput
  { ioFunctionsImpl     :: [Text]   -- ^ Functions implemented
  , ioTestsPassing      :: Bool     -- ^ Did tests pass after impl?
  , ioCommitHash        :: Text     -- ^ Commit containing impl
  , ioIterations        :: Int      -- ^ How many CI iterations to pass
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)


-- ════════════════════════════════════════════════════════════════════════════
-- INTERNAL STATE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler wraps scaffolding output with infrastructure.
data ScaffoldingResult = ScaffoldingResult
  { srOutput      :: ScaffoldingOutput
  , srSessionId   :: Text
  , srBranch      :: Text
  , srSpec        :: Spec               -- ^ Original spec for reference
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)


-- | Result from a child graph instance after MR merge.
data ChildResult = ChildResult
  { crSubsystemName :: Text    -- ^ Which subsystem this was
  , crBranch        :: Text    -- ^ Branch that was merged
  , crCommitHash    :: Text    -- ^ Commit hash after merge
  , crSuccess       :: Bool    -- ^ Did child succeed?
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)


-- | Final result of a V2 graph instance.
-- This becomes the MR that gets merged into parent.
data V2Result = V2Result
  { vrSuccess       :: Bool
  , vrDescription   :: Text    -- ^ What was built
  , vrBranch        :: Text    -- ^ Final branch (MR source)
  , vrCommitHash    :: Text    -- ^ Final commit
  , vrSubsystemName :: Maybe Text  -- ^ If this was a child, which subsystem
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)


-- ════════════════════════════════════════════════════════════════════════════
-- GINGER TEMPLATE INSTANCES
-- ════════════════════════════════════════════════════════════════════════════

-- | ToGVal instances for ginger template rendering.
-- Generic derivation via GHC.Generics - empty instances use default implementation.

-- Template context types
instance ToGVal (Run SourcePos (Writer Text) Text) ScaffoldingCtx
instance ToGVal (Run SourcePos (Writer Text) Text) ImplementCtx

-- Types referenced by context types (for nested access in templates)
instance ToGVal (Run SourcePos (Writer Text) Text) Spec
instance ToGVal (Run SourcePos (Writer Text) Text) ScaffoldingResult
instance ToGVal (Run SourcePos (Writer Text) Text) ScaffoldingOutput
instance ToGVal (Run SourcePos (Writer Text) Text) ScaffoldingRubric
instance ToGVal (Run SourcePos (Writer Text) Text) Facet
instance ToGVal (Run SourcePos (Writer Text) Text) ChildResult
