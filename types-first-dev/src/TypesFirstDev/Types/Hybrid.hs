{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FieldSelectors #-}

-- | Type definitions for the hybrid TDD graph.
--
-- All types are organized into:
-- 1. Schema types - What LLMs return (structured output)
-- 2. Internal types - Handler state (not LLM-produced)
-- 3. Template context types - What templates render
--
-- Design principle: LLM outputs describe WHAT was written, not the code itself.
-- The code lives on disk. The output is semantic metadata for analysis and templating.
module TypesFirstDev.Types.Hybrid
  ( -- * Entry & Configuration
    StackSpec(..)
  , StrictnessConfig(..)
  , defaultStrictness
  , strictMode
  , CoordSpec(..)
  , ScopeLevel(..)

    -- * Value-Neutral Rubric Types
  , DesignChoice(..)
  , PropertyCategory(..)
  , FailureCorrelation(..)
  , FailureCause(..)

    -- * Shared Contract Types
  , ConcreteExample(..)
  , PropertySketch(..)
  , PropertyType(..)
  , FunctionSpec(..)

    -- * Types Agent Output (Schema)
  , TypesAgentOutput(..)

    -- * Type Adversary Output (Schema)
  , TypeHole(..)
  , HoleType(..)
  , Severity(..)
  , TypeAdversaryOutput(..)

    -- * Tests Agent Output (Schema)
  , PropertyWritten(..)
  , CoverageReport(..)   -- Handler-computed, not LLM-reported
  , TestsAgentOutput(..)

    -- * Impl Agent Output (Schema)
  , ImplAgentOutput(..)

    -- * Fix Agent Output (Schema)
  , FixApplied(..)
  , FixType(..)
  , FixAgentOutput(..)

    -- * Mutation Adversary Output (Schema)
  , MutationType(..)
  , SurvivingMutant(..)
  , MutationAdversaryOutput(..)

    -- * Conflict Resolution Output (Schema)
  , ConflictResolveOutput(..)

    -- * Structured Failures
  , StructuredFailure(..)
  , FailureType(..)

    -- * Echo Channels & Hardening
  , EchoChannel(..)
  , HardeningHint(..)

    -- * Internal State Types
  , TypesResult(..)
  , SkeletonState(..)
  , TypeAdversaryResult(..)
  , TypeSystemVerdict(..)
  , GatedState(..)
  , TypeHolesFound(..)
  , TestsResult(..)
  , ImplResult(..)
  , BlindResults(..)
  , VerificationStatus(..)
  , VerifiedResults(..)
  , MergedState(..)
  , ConflictState(..)
  , UnderstandingState(..)
  , FailurePattern(..)
  , initialUnderstanding
  , ValidationFailure(..)
  , ValidatedState(..)
  , MutationAdversaryResult(..)
  , TestSuiteVerdict(..)
  , TrivialTestsError(..)
  , TrivialTestsFeedback(..)

    -- * Witness Types
  , NodeObservation(..)
  , WitnessReport(..)

    -- * Final Result
  , HybridResult(..)

    -- * Template Context Types
  , TypesTemplateCtx(..)
  , TestsTemplateCtx(..)
  , ImplTemplateCtx(..)
  , MutationTemplateCtx(..)
  , TypeAdversaryTemplateCtx(..)
  , TypesFixTemplateCtx(..)
  , ConflictResolveTemplateCtx(..)
  , FixTemplateCtx(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.StructuredOutput (StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- ENTRY & CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Entry specification for the hybrid TDD workflow.
-- Includes strictness configuration for per-task behavior.
data StackSpec = StackSpec
  { specModuleName        :: Text               -- "Data.Stack"
  , specDescription       :: Text               -- What we're building
  , specAcceptanceCriteria :: [Text]            -- Must-have features
  , specImplPath          :: FilePath           -- Where to write impl
  , specTestPath          :: FilePath           -- Where to write tests
  , specStrictness        :: StrictnessConfig   -- Per-task strictness
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Per-task strictness configuration.
-- Controls when mutation adversary blocks vs advises.
data StrictnessConfig = StrictnessConfig
  { scMutationBlocking :: Bool        -- If True, 0 survivors required for exit
  , scMaxFixAttempts   :: Int         -- Default: 5
  , scRequireCoverage  :: Bool        -- Block if PropertySketches unimplemented
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Default strictness for normal development.
defaultStrictness :: StrictnessConfig
defaultStrictness = StrictnessConfig
  { scMutationBlocking = False  -- Advisory mode
  , scMaxFixAttempts   = 5
  , scRequireCoverage  = True   -- PropertySketches must be implemented
  }

-- | High-stakes strictness for critical code.
strictMode :: StrictnessConfig
strictMode = StrictnessConfig
  { scMutationBlocking = True   -- 0 survivors required
  , scMaxFixAttempts   = 5
  , scRequireCoverage  = True
  }

-- | Coordination spec for cross-cutting functions after children merge.
data CoordSpec = CoordSpec
  { coordModuleName   :: Text             -- "Data.Stack.Persist"
  , coordDescription  :: Text             -- What we're gluing
  , coordGiven        :: [FilePath]       -- Already-implemented child modules
  , coordFunctions    :: [FunctionSpec]   -- Cross-cutting functions to implement
  , coordImplPath     :: FilePath         -- Where to write glue code
  , coordTestPath     :: FilePath         -- Where to write relationship tests
  , coordStrictness   :: StrictnessConfig
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Scope level for adversary aperture.
data ScopeLevel = Leaf | Coordination | System
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- VALUE-NEUTRAL RUBRIC TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Structured design choice - replaces vague prose "designNotes".
-- Each choice is a concrete decision that can be evaluated.
data DesignChoice = DesignChoice
  { dcArea     :: Text     -- "data representation", "error handling"
  , dcChoice   :: Text     -- "recursive ADT", "Either-based errors"
  , dcTradeoff :: Text     -- "memory safety over speed"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Category of properties covered - for coverage analysis without asking "how complete?"
data PropertyCategory
  = PCInvariant          -- Core invariants (pushPop inverse)
  | PCEdgeCase           -- Edge case handling (empty stack)
  | PCBoundary           -- Boundary conditions (capacity limits)
  | PCComposition        -- Function composition laws
  | PCErrorHandling      -- Error path testing
  | PCOther Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Failure correlation - tracks whether fixes actually addressed failures.
-- Handler computes this, not LLM.
data FailureCorrelation = FailureCorrelation
  { fcPriorFix     :: Text        -- What we tried
  , fcStillFailing :: [Text]      -- Property names still broken
  , fcLikelihood   :: FailureCause
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Classification of why a fix didn't work.
data FailureCause
  = WrongFix           -- Fix was incorrect or incomplete
  | PartialFix         -- Fix helped but didn't fully resolve
  | UnrelatedFailure   -- This failure isn't related to the fix
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- SHARED CONTRACT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Concrete example: specific input/output pair.
-- Used for alignment - both agents see the same concrete cases.
data ConcreteExample = ConcreteExample
  { ceDescription :: Text   -- "Pushing onto empty stack"
  , ceInput       :: Text   -- "push 1 empty"
  , ceExpected    :: Text   -- "Stack containing just 1"
  , ceEdgeCase    :: Bool   -- Is this an edge case worth extra attention?
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Property sketch: informal description of a property to test.
-- NOT code - prose/pseudocode that tests agent interprets.
-- NOTE: Field names match template variables for TH validation.
data PropertySketch = PropertySketch
  { name          :: Text           -- "pushPopInverse"
  , psDescription :: Text           -- "Pushing then popping returns original stack"
  , invariant     :: Text           -- "forall x s. pop (push x s) == (x, s)"
  , psType        :: PropertyType   -- Classification for analysis (kept prefixed - not used in templates)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Classification of property types (for coverage analysis).
data PropertyType
  = Inverse         -- f . g = id (or similar)
  | Idempotent      -- f . f = f
  | Commutative     -- f x y = f y x
  | Associative     -- f (f x y) z = f x (f y z)
  | Identity        -- f identity x = x
  | Preservation    -- Some property preserved across operation
  | Boundary        -- Edge case behavior
  | Monotonic       -- Order preservation
  | Other Text      -- Freeform classification
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Complete specification for a function.
-- Single source of truth - no duplicate name/signature fields.
--
-- NOTE: Uses regular lists for LLM schema simplicity.
-- Handler can validate non-empty if needed.
data FunctionSpec = FunctionSpec
  { name        :: Text                       -- "push"
  , signature   :: Text                       -- "a -> Stack a -> Stack a"
  , brief       :: Text                       -- One-line for code comments
  , behavior    :: Text                       -- Detailed prose for agents
  , examples    :: [Text]                     -- Example descriptions (simplified)
  , properties  :: [Text]                     -- Property descriptions (simplified)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES AGENT OUTPUT (Schema)
-- ════════════════════════════════════════════════════════════════════════════

-- | Output from types agent. Semantic descriptions, not code.
-- The agent WRITES code to disk; this describes what was written.
--
-- SIMPLIFIED SCHEMA: Flat structure for reliable LLM JSON generation.
data TypesAgentOutput = TypesAgentOutput
  { typeName        :: Text             -- "Stack"
  , typeDescription :: Text             -- Prose: what this type represents
  , functions       :: [FunctionSpec]   -- Specs for each function
  , designNotes     :: Text             -- Free-form design rationale
  , blocker         :: Maybe Text       -- If blocked, explain
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPE ADVERSARY OUTPUT (Schema)
-- ════════════════════════════════════════════════════════════════════════════

-- | A hole found in the type system.
-- Describes the hole semantically - counterexample is pseudocode/prose.
-- NOTE: Field names match template variables for TH validation.
data TypeHole = TypeHole
  { holeType        :: HoleType       -- Classification
  , description     :: Text           -- "Empty stack can claim non-empty"
  , invariantBroken :: Text           -- "NonEmpty should guarantee >=1 element"
  , exploitSketch   :: Text           -- Pseudocode showing exploit (not compilable)
  , severity        :: Severity
  , suggestedFix    :: Text           -- "Use newtype with smart constructor"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

data HoleType
  = RepresentableInvalid   -- Can construct value violating invariant
  | LeakyAbstraction       -- Internal representation exposed
  | PartialFunction        -- Function undefined for some valid inputs
  | TypeConfusion          -- Different semantic meanings share type
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

data Severity = Critical | Major | Minor | Informational
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Type adversary output. No verdict field - derived from holes.
-- VALUE-NEUTRAL: Instead of asking "are you confident?", we ask
-- "what areas did you check?" and "what did you skip?" - actionable lists.
data TypeAdversaryOutput = TypeAdversaryOutput
  { tadHoles          :: [TypeHole]
  , areasExamined     :: [Text]    -- VALUE-NEUTRAL: What was actually checked
  , uncheckedAreas    :: [Text]    -- VALUE-NEUTRAL: What was skipped (actionable!)
  , analysisApproach  :: Text      -- High-level strategy (prose ok here)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- TESTS AGENT OUTPUT (Schema)
-- ════════════════════════════════════════════════════════════════════════════

-- | Description of a property that was written.
-- NOT the code - semantic metadata about the property.
data PropertyWritten = PropertyWritten
  { pwName            :: Text           -- "prop_pushPopInverse"
  , pwTargetFunctions :: [Text]         -- ["push", "pop"]
  , pwPropertyType    :: PropertyType   -- Inverse
  , pwDescription     :: Text           -- "Verifies push/pop are inverses"
  , pwCoversExamples  :: [Int]          -- Indices into FunctionSpec.fnExamples
  , pwCoversSketch    :: Maybe Text     -- Which PropertySketch this implements
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Coverage report - what fraction of spec is tested?
data CoverageReport = CoverageReport
  { crFunctionsCovered   :: [Text]      -- Functions with >=1 property
  , crFunctionsUncovered :: [Text]      -- Functions with no properties
  , crExamplesCovered    :: Int         -- Examples referenced by properties
  , crExamplesTotal      :: Int         -- Total examples in spec
  , crSketchesCovered    :: [Text]      -- PropertySketch names implemented
  , crSketchesUncovered  :: [Text]      -- PropertySketch names NOT implemented
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Tests agent output. SIMPLIFIED for reliable LLM JSON generation.
data TestsAgentOutput = TestsAgentOutput
  { propertiesWritten :: [Text]       -- List of property names written
  , commitMessage     :: Text         -- Git commit message
  , blocker           :: Maybe Text   -- If blocked, explain
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- IMPL AGENT OUTPUT (Schema)
-- ════════════════════════════════════════════════════════════════════════════

-- | Impl agent output. SIMPLIFIED for reliable LLM JSON generation.
data ImplAgentOutput = ImplAgentOutput
  { functionsImplemented :: [Text]       -- List of function names implemented
  , designNotes          :: Text         -- How you approached the implementation
  , commitMessage        :: Text         -- Git commit message
  , blocker              :: Maybe Text   -- If blocked, explain
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- FIX AGENT OUTPUT (Schema)
-- ════════════════════════════════════════════════════════════════════════════

-- | Description of a fix applied.
-- VALUE-NEUTRAL: faRelatedFailures lets handler correlate fixes to failures.
data FixApplied = FixApplied
  { faFunction        :: Text           -- "push"
  , faWhatChanged     :: Text           -- "Fixed off-by-one in size tracking"
  , faWhyFailed       :: Text           -- "Wasn't handling empty stack edge case"
  , faFixType         :: FixType
  , faRelatedFailures :: [Text]         -- VALUE-NEUTRAL: Which failures this addresses
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

data FixType
  = EdgeCaseFix         -- Missing edge case handling
  | LogicFix            -- Incorrect logic/algorithm
  | TypeFix             -- Type mismatch or conversion issue
  | BoundaryFix         -- Off-by-one or boundary condition
  | InitializationFix   -- Wrong initial value
  | OtherFix Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Fix agent output.
-- VALUE-NEUTRAL: Removed fixBuildPassed (handler verifies mechanically).
data FixAgentOutput = FixAgentOutput
  { fixChanges   :: [FixApplied]
  , fixCommitMsg :: Text
  , fixBlocker   :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- MUTATION ADVERSARY OUTPUT (Schema)
-- ════════════════════════════════════════════════════════════════════════════

-- | Classification of mutation types.
data MutationType
  = BoundaryMutation     -- Changed >, <, >=, <=
  | ConditionFlip        -- Negated a condition
  | OffByOne             -- +1/-1 changes
  | SwappedArgs          -- Arguments in wrong order
  | RemovedCheck         -- Deleted a guard/validation
  | ChangedOperator      -- +/-, *//, etc.
  | ReturnedWrongBranch  -- Swapped if/else results
  | OtherMutation Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | A mutation that survived (tests didn't catch).
-- Semantic description, not the actual mutated code.
data SurvivingMutant = SurvivingMutant
  { smFunction       :: Text            -- "push"
  , smMutationType   :: MutationType    -- BoundaryMutation
  , smDescription    :: Text            -- "Changed > to >= in length check"
  , smWhyDangerous   :: Text            -- "Allows stack overflow"
  , smMissingTest    :: Text            -- "Need property for max capacity"
  , smSuggestedProp  :: PropertySketch  -- Informal spec for missing property
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Mutation adversary output. Survivor count derived from list.
-- VALUE-NEUTRAL: Replaced mutMutantsTried (gameable count) with
-- mutationTypesAttempted (what kinds of mutations - handler counts).
data MutationAdversaryOutput = MutationAdversaryOutput
  { mutationTypesAttempted :: [MutationType]  -- VALUE-NEUTRAL: What kinds (handler counts)
  , mutSurvivors           :: [SurvivingMutant]
  , mutAnalysis            :: Text            -- Areas tested, approach
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- CONFLICT RESOLUTION OUTPUT (Schema)
-- ════════════════════════════════════════════════════════════════════════════

-- | Conflict resolution agent output (schema).
-- Semantic metadata about how conflicts were resolved.
-- VALUE-NEUTRAL: Removed croBuildPassed (handler verifies mechanically).
data ConflictResolveOutput = ConflictResolveOutput
  { croFilesResolved   :: [Text]             -- Paths that were resolved
  , croResolutionNotes :: Text               -- How each conflict was resolved
  , croBlocker         :: Maybe Text         -- If resolution failed, why
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- STRUCTURED FAILURES
-- ════════════════════════════════════════════════════════════════════════════

-- | Structured test failure - parsed from QuickCheck output.
-- Handler produces this, not LLM.
data StructuredFailure = StructuredFailure
  { sfPropertyName  :: Text             -- "prop_pushPopInverse"
  , sfFailureType   :: FailureType
  , sfCounterexample :: Maybe Text      -- Shrunk counterexample if available
  , sfExpected      :: Maybe Text       -- What was expected
  , sfActual        :: Maybe Text       -- What actually happened
  , sfMessage       :: Text             -- Raw failure message
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

data FailureType
  = PropertyFailed      -- QuickCheck found counterexample
  | ExceptionThrown     -- Property threw exception
  | Timeout             -- Property didn't terminate
  | UndefinedHit        -- Hit undefined/error
  | ParseError          -- Couldn't parse test output
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- ECHO CHANNELS & HARDENING
-- ════════════════════════════════════════════════════════════════════════════

-- | Echo channel - shape without substance.
-- Agents share what they're DOING (names), not HOW (implementations).
-- NOTE: Field names match template variables for TH validation.
data EchoChannel = EchoChannel
  { fromImpl  :: [Text]  -- Function names impl is working on
  , fromTests :: [Text]  -- Property names tests is asserting
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Hardening hint from adversary agents.
-- Instead of just blocking, adversaries TEACH downstream agents.
data HardeningHint = HardeningHint
  { hhContext   :: Text    -- "The type allows representing invalid empty stacks"
  , hhGuidance  :: Text    -- "Consider: tests for NonEmpty invariant, impl guards"
  , hhSource    :: Text    -- "typeAdversary" or "mutationAdversary"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- INTERNAL STATE TYPES (Handler-managed, not LLM-produced)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler combines types output with infrastructure.
data TypesResult = TypesResult
  { trOutput      :: TypesAgentOutput   -- What LLM returned
  , trSessionId   :: Text               -- ClaudeCode session ID
  , trCost        :: Double             -- API cost
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | State after skeleton generation (handler-produced).
data SkeletonState = SkeletonState
  { ssTypesResult :: TypesResult
  , ssImplPath    :: FilePath           -- Generated skeleton location
  , ssTestPath    :: FilePath           -- Generated test scaffold location
  , ssProjectPath :: FilePath
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Handler combines type adversary output with derived verdict.
-- Includes skeleton state for join point routing.
data TypeAdversaryResult = TypeAdversaryResult
  { tarOutput   :: TypeAdversaryOutput
  , tarVerdict  :: TypeSystemVerdict     -- DERIVED from tarOutput.tadHoles
  , tarSkeleton :: SkeletonState         -- JOIN: needed by hGate for routing
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

data TypeSystemVerdict
  = TypeSystemSound      -- No holes
  | TypeSystemMinorHoles -- Only Minor/Informational
  | TypeSystemHasHoles   -- Has Critical or Major
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | State after gate passes (type adversary found no significant holes).
data GatedState = GatedState
  { gsSkeleton       :: SkeletonState
  , gsTypeAdversary  :: TypeAdversaryResult
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Input to typesFix when adversary found holes.
-- Includes attempt tracking for retry budget.
data TypeHolesFound = TypeHolesFound
  { thfOriginalTypes :: TypesAgentOutput
  , thfHoles         :: [TypeHole]
  , thfAttempt       :: Int       -- Current attempt (1-indexed)
  , thfMaxAttempts   :: Int       -- From StrictnessConfig.scMaxFixAttempts
  , thfPriorFixes    :: [Text]    -- What was tried before (for context)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Handler combines tests output with verification.
data TestsResult = TestsResult
  { testsOutput       :: TestsAgentOutput
  , testsWorktree     :: FilePath
  , testsCommitHash   :: Text
  , testsSessionId    :: Text
  , testsCost         :: Double
  , testsFailureProof :: [StructuredFailure]  -- From running tests on skeleton
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Handler combines impl output with metadata.
data ImplResult = ImplResult
  { implOutput     :: ImplAgentOutput
  , implWorktree   :: FilePath
  , implCommitHash :: Text
  , implSessionId  :: Text
  , implCost       :: Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Combined blind results.
data BlindResults = BlindResults
  { brTests :: TestsResult
  , brImpl  :: ImplResult
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Verification status - strict mode, no nuance.
data VerificationStatus = Verified | NotVerified
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | After external TDD verification.
-- Strict mode: we only get here if tests failed on skeleton (good).
data VerifiedResults = VerifiedResults
  { vrBlindResults     :: BlindResults
  , vrExternalVerified :: Bool          -- Always True in strict mode
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | After merge into fresh worktree.
data MergedState = MergedState
  { msVerifiedResults :: VerifiedResults
  , msMergeWorktree   :: FilePath
  , msUnderstanding   :: UnderstandingState
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | State when cherry-pick produces git conflicts.
data ConflictState = ConflictState
  { csVerifiedResults  :: VerifiedResults
  , csMergeWorktree    :: FilePath
  , csConflictedFiles  :: [(FilePath, Text)]
  , csTestsContext     :: Text
  , csImplContext      :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Understanding accumulated through the fix loop.
data UnderstandingState = UnderstandingState
  { usFailuresSeen  :: [FailurePattern]
  , usFixesApplied  :: [FixApplied]
  , usLearnings     :: [Text]
  , usConverging    :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Pattern extracted from test failures.
data FailurePattern = FailurePattern
  { fpSignature   :: Text
  , fpAffectedFns :: [Text]
  , fpCategory    :: FailureType
  , fpOccurrences :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Initial understanding state.
initialUnderstanding :: UnderstandingState
initialUnderstanding = UnderstandingState [] [] [] True

-- | Validation failure context for fix agent.
data ValidationFailure = ValidationFailure
  { vfMergedState   :: MergedState
  , vfFailures      :: [StructuredFailure]
  , vfNewPatterns   :: [FailurePattern]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | After validation passes.
data ValidatedState = ValidatedState
  { vsMergedState  :: MergedState
  , vsTestsPassed  :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Handler combines mutation output with derived verdict.
data MutationAdversaryResult = MutationAdversaryResult
  { marOutput  :: MutationAdversaryOutput
  , marVerdict :: TestSuiteVerdict
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

data TestSuiteVerdict
  = TestSuiteRobust   -- No survivors
  | TestSuiteHasGaps  -- Some survivors
  | TestSuiteWeak     -- Many survivors (>50% survival rate)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Error when tests pass on skeleton.
data TrivialTestsError = TrivialTestsError
  { tteBlindResults :: BlindResults
  , tteMessage      :: Text
  , tteAttempt      :: Int
  , tteFeedback     :: TrivialTestsFeedback
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Feedback for tests agent when previous tests were trivial.
-- NOTE: Field names match template variables for TH validation.
data TrivialTestsFeedback = TrivialTestsFeedback
  { whyRejected     :: Text
  , propertiesWrote :: [Text]
  , suggestion      :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- WITNESS TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Observation from a single node.
data NodeObservation = NodeObservation
  { noNode      :: Text
  , noPhase     :: Text
  , noProgress  :: Text
  , noConcerns  :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Accumulated report from witness.
data WitnessReport = WitnessReport
  { wrObservations :: [NodeObservation]
  , wrNarrative    :: Text
  , wrConcerns     :: [Text]
  , wrSuggestions  :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- FINAL RESULT
-- ════════════════════════════════════════════════════════════════════════════

data HybridResult = HybridResult
  { hrSuccess           :: Bool
  , hrSpec              :: [FunctionSpec]
  , hrTestsCoverage     :: CoverageReport
  , hrUnderstanding     :: UnderstandingState
  , hrTypeAdversary     :: Maybe TypeAdversaryResult
  , hrMutationAdversary :: Maybe MutationAdversaryResult
  , hrWitness           :: WitnessReport
  , hrTotalCost         :: Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- TEMPLATE CONTEXT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Context for TypesTpl - what the types agent sees.
-- NOTE: Field names match template variables for TH validation.
data TypesTemplateCtx = TypesTemplateCtx
  { moduleName        :: Text
  , description       :: Text
  , acceptanceCriteria :: [Text]
  , implPath          :: FilePath
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Context for TestsTpl - what the tests agent sees.
-- NOTE: Field names match template variables for TH validation.
data TestsTemplateCtx = TestsTemplateCtx
  { typeName       :: Text
  , functions      :: [FunctionSpec]
  , testPath       :: FilePath
  , priorFeedback  :: Maybe TrivialTestsFeedback
  , echoes         :: Maybe EchoChannel
  , hardeningHints :: [HardeningHint]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Context for ImplTpl - what the impl agent sees.
-- NOTE: Field names match template variables for TH validation.
data ImplTemplateCtx = ImplTemplateCtx
  { typeName       :: Text
  , functions      :: [FunctionSpec]
  , implPath       :: FilePath
  , echoes         :: Maybe EchoChannel
  , hardeningHints :: [HardeningHint]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Context for MutationAdversaryTpl.
data MutationTemplateCtx = MutationTemplateCtx
  { mtcImplPath    :: FilePath
  , mtcTestPath    :: FilePath
  , mtcFunctions   :: [FunctionSpec]
  , mtcScopeLevel  :: ScopeLevel
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Context for TypeAdversaryTpl.
-- NOTE: Field names match template variables for TH validation.
data TypeAdversaryTemplateCtx = TypeAdversaryTemplateCtx
  { types      :: TypesAgentOutput
  , scopeLevel :: ScopeLevel
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Context for TypesFixTpl.
-- NOTE: Field names match template variables for TH validation.
data TypesFixTemplateCtx = TypesFixTemplateCtx
  { originalTypes :: TypesAgentOutput
  , holes         :: [TypeHole]
  , attempt       :: Int
  , priorFixes    :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Context for ConflictResolveTpl.
-- Provides conflict markers and agent context for resolution.
-- NOTE: Field names match template variables for TH validation.
data ConflictResolveTemplateCtx = ConflictResolveTemplateCtx
  { conflictedFiles :: [(FilePath, Text)]  -- (path, content with markers)
  , testsContext    :: Text                -- What tests agent intended
  , implContext     :: Text                -- What impl agent intended
  , mergeWorktree   :: FilePath            -- Where to write resolved files
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Context for FixTpl (WS4 validation loop).
-- Shows test failures and accumulated understanding to the fix agent.
-- NOTE: Field names match template variables for TH validation.
data FixTemplateCtx = FixTemplateCtx
  { failures       :: [StructuredFailure]      -- Current test failures
  , understanding  :: UnderstandingState       -- Accumulated learning
  , fixFunctions   :: [FunctionSpec]           -- Function specs for reference (prefixed to avoid collision)
  , worktreePath   :: FilePath                 -- Where the code lives
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)
