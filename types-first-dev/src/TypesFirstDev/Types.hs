{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FieldSelectors #-}

-- | Schema types for the types-first development workflow.
--
-- These are the structured outputs from Claude Code sessions.
-- Types derive Generic and use StructuredOutput for unified schema+parsing.
-- Field name prefixes (like "td" in "tdTypeName") are automatically stripped.
module TypesFirstDev.Types
  ( -- * Entry Type
    StackSpec(..)

    -- * Project Type
  , ProjectType(..)

    -- * Workflow Errors
  , WorkflowError(..)

    -- * Build Targets (for validation wrapper)
  , BuildTarget(..)
  , buildTargetArgs
  , BuildValidationResult(..)
  , WorkflowResult(..)

    -- * Re-exports for session management
  , ResumeStrategy(..)

    -- * Structured Signature Types (v2)
  , FunctionSig(..)
  , TestPriority(..)

    -- * Semantic Descriptions (v3)
  , FunctionExample(..)
  , FunctionSemantics(..)
  , StubsOutput(..)

    -- * Per-Function Rubrics (LLM sensor output - semantic only)
  , FunctionRubric(..)
  , BoundaryNote(..)
  , TestFunctionRubric(..)
  , Blocker(..)

    -- * Progress Tracking (v2)
  , IncrementalProgress(..)
  , WorkStatus(..)

    -- * Types Agent Output
  , TypeDefinitions(..)

    -- * Tests Agent Output
  , TestsResult(..)

    -- * Impl Agent Output
  , ImplResult(..)

    -- * Test Loop State
  , TestLoopState(..)
  , TestResult(..)

    -- * Final Result
  , ImplementationResult(..)

    -- * Skeleton Generation Output
  , SkeletonGenerated(..)

    -- * v3 Stubs Output (for fork)
  , StubsGenerated(..)

    -- * Internal Types
  , ForkInput(..)
  , ParallelResults(..)

    -- * Session State (Memory effect)
  , SessionContext(..)
  , emptySessionContext

    -- * TDD Workflow Types (Sequential)
  , SkeletonState(..)
  , TestsWritten(..)
  , TestsVerified(..)
  , ImplWritten(..)
  , ValidationFailure(..)
  , FixResult(..)
  , TDDResult(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effects.Cabal (TestFailure)
import Tidepool.Effects.Worktree (WorktreePath)
import Tidepool.StructuredOutput (StructuredOutput)



-- ════════════════════════════════════════════════════════════════════════════
-- ENTRY TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | Type of project to generate.
--
-- Determines which templates to use for skeleton generation.
data ProjectType
  = PureLibrary
    -- ^ Pure Haskell library (like Stack example)
  | ServantServer
    -- ^ Servant webserver with HTTP endpoints
  | CLIApp
    -- ^ Command-line application
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Strategy for resuming Claude Code sessions across retries.
data ResumeStrategy
  = SmartResume
    -- ^ Use full conversation history for context
  | FreshStart
    -- ^ Start with fresh context on retry
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Specification for the data type / service to implement.
--
-- This is the entry point to the graph - defines what to build.
data StackSpec = StackSpec
  { ssProjectPath :: FilePath
    -- ^ Path to the project root.
  , ssModuleName :: Text
    -- ^ Module name (e.g., "Data.Stack" or "UrlShortener").
  , ssDescription :: Text
    -- ^ Natural language description of the data structure.
  , ssAcceptanceCriteria :: [Text]
    -- ^ High-level acceptance criteria to inform test priorities.
  , ssProjectType :: ProjectType
    -- ^ Type of project (determines template set).
  , ssResumeStrategy :: ResumeStrategy
    -- ^ Strategy for resuming Claude Code sessions across retries.
    -- 'SmartResume' is recommended for most workflows.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance StructuredOutput StackSpec


-- ════════════════════════════════════════════════════════════════════════════
-- WORKFLOW ERRORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during workflow execution.
--
-- These are modeled as data rather than exceptions so the effect system
-- can handle them properly.
data WorkflowError
  = SkeletonCompileFailed Text
    -- ^ Skeleton generation produced code that doesn't compile
  | WorktreeCreationFailed Text
    -- ^ Failed to create git worktree
  | AgentFailed Text Text
    -- ^ Agent name and error message
  | AgentNoOutput Text
    -- ^ Agent returned no structured output
  | AgentParseFailed Text Text
    -- ^ Agent name and parse error
  | MergeFailed Text
    -- ^ Merge operation failed
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- BUILD TARGETS
-- ════════════════════════════════════════════════════════════════════════════

-- | Build target for validation wrapper.
--
-- Specifies what to validate after an agent completes. This is a generic
-- abstraction that can be shared across different nodes to ensure consistent
-- validation behavior.
--
-- Design principle: Each agent declares what it should be validated against.
-- - Tests agent: BuildAll (must compile library AND test suite)
-- - Impl agent: BuildLib (just the library)
-- - Fix agent: BuildAll (full validation after fix)
data BuildTarget
  = BuildLib
    -- ^ Just the library (cabal build)
  | BuildAll
    -- ^ Everything (cabal build all) - library + tests + benchmarks
  | BuildTests
    -- ^ Build test suite specifically
  | BuildAndTest
    -- ^ Build all and run tests (cabal test)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Get the cabal command args for a build target.
buildTargetArgs :: BuildTarget -> [String]
buildTargetArgs = \case
  BuildLib -> ["build", "-v0"]
  BuildAll -> ["build", "-v0", "all"]
  BuildTests -> ["build", "-v0", "all"]  -- Build all to include tests
  BuildAndTest -> ["test"]

-- | Result of running an agent with build validation.
--
-- Build failures don't error out - they return BuildNeedsFix so the graph
-- can route to a fix agent. This ensures ALL build failures are handled
-- by the LLM rather than terminating the workflow.
data BuildValidationResult a
  = BuildSuccess a
    -- ^ Build passed, here's the agent result
  | BuildNeedsFix Text Int a
    -- ^ Build failed: error message, attempt count, last agent result
    -- Route this to a fix agent or back to the same agent with error context
  | BuildFatalError Text
    -- ^ Non-recoverable error (e.g., Claude Code crashed, network error)
    -- This is the only case that should terminate the workflow
  deriving stock (Show, Eq, Generic, Functor)
  deriving anyclass (FromJSON, ToJSON)


-- | Result of the post-fork validation/merge phase.
--
-- Design principle: Almost nothing should fail. Instead, route back to agents
-- to fix issues. Only truly unrecoverable errors (network failure, etc.) should
-- be WorkflowFatal.
data WorkflowResult
  = WorkflowSuccess
    -- ^ Tests passed, implementation complete
  | NeedsImplFix [(FilePath, Int, Text)] Int
    -- ^ Impl has undefined: [(file, line, content)], attempt count
    -- Route back to impl agent with these locations
  | NeedsTestsFix Text Int
    -- ^ Tests agent issue: reason, attempt count
    -- Route back to tests agent
  | NeedsMergeResolution [FilePath] Int
    -- ^ Git merge conflict: conflicted files, attempt count
    -- Route to conflict resolution agent
  | NeedsPostMergeFix Text Int
    -- ^ Post-merge tests failed: test output, attempt count
    -- Route to fix agent
  | WorkflowFatal Text
    -- ^ Truly unrecoverable error (should be rare!)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- STRUCTURED SIGNATURE TYPES (v2)
-- ════════════════════════════════════════════════════════════════════════════

-- | Structured function signature with metadata.
--
-- Replaces plain text signatures for richer LLM context.
data FunctionSig = FunctionSig
  { fsName :: Text
    -- ^ Function name (e.g., "push").
  , fsSignature :: Text
    -- ^ Full type signature (e.g., "a -> Stack a -> Stack a").
  , fsDescription :: Text
    -- ^ Brief description of what the function does.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance StructuredOutput FunctionSig

-- | Test priority for incremental development.
--
-- Guides which functions to test first based on dependency order.
data TestPriority = TestPriority
  { tpName :: Text
    -- ^ Property name (e.g., "lifo_order", "push_pop_inverse").
  , tpDescription :: Text
    -- ^ What to test (e.g., "Stack maintains LIFO order").
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance StructuredOutput TestPriority


-- ════════════════════════════════════════════════════════════════════════════
-- SEMANTIC DESCRIPTIONS (v3 - stubs-driven workflow)
-- ════════════════════════════════════════════════════════════════════════════

-- | Example input/output pair demonstrating function behavior.
--
-- Used by stubs agent to describe what each function should do.
-- The tests agent uses these to write property tests.
data FunctionExample = FunctionExample
  { feInput :: Text
    -- ^ Description of input (e.g., "push 1 (push 2 empty)")
  , feExpected :: Text
    -- ^ Expected output description (e.g., "Stack with 1 on top")
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Semantic description of a function's behavior.
--
-- The contract between stubs and tests agents. Stubs agent describes
-- what each function should do; tests agent writes tests to verify it.
data FunctionSemantics = FunctionSemantics
  { fsmName :: Text
    -- ^ Function name (e.g., "push")
  , fsmSignature :: Text
    -- ^ Type signature (e.g., "a -> Stack a -> Stack a")
  , fsmBehavior :: Text
    -- ^ Natural language description of what the function does.
    -- e.g., "Adds an element to the top of the stack"
  , fsmExamples :: [FunctionExample]
    -- ^ Example inputs/outputs demonstrating the behavior.
    -- Used to derive concrete test cases.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Output from the stubs agent (v3 workflow).
--
-- The stubs agent:
-- 1. Writes actual .hs files with `undefined` implementations
-- 2. Returns semantic descriptions for each function
-- 3. cabal build must pass before returning
data StubsOutput = StubsOutput
  { soModuleName :: Text
    -- ^ Module name (e.g., "Data.Stack", "UrlShortener")
  , soDataType :: Text
    -- ^ The data type definition that was written
  , soFunctions :: [FunctionSemantics]
    -- ^ Semantic descriptions for each function (drives test generation)
  , soImports :: [Text]
    -- ^ Additional imports used in the stubs
  , soCommitMessage :: Text
    -- ^ Git commit message for the stubs
  , soBlocker :: Maybe Text
    -- ^ If blocked, explain why. Null if successful.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)


-- ════════════════════════════════════════════════════════════════════════════
-- PER-FUNCTION RUBRICS (LLM sensor output - semantic only)
-- ════════════════════════════════════════════════════════════════════════════

-- | Per-function rubric from implementation agent.
--
-- Design principle: LLM is sensor, code is controller.
-- We ask for SEMANTIC information we can't mechanically derive.
-- Handler computes mechanical checks (build, undefined, tests) separately.
--
-- Key insight: Ask for useful information without hinting at consequences.
data FunctionRubric = FunctionRubric
  { frFunctionName :: Text
    -- ^ Which function this rubric evaluates

  , frApproach :: Text
    -- ^ How did you implement it?
    -- "recursion" | "fold" | "unfold" | "pattern-match" | "library" | "lens" | "state-monad" | "other"
    -- Category only - no value ordering (recursion isn't "better" than fold)

  , frOpenQuestions :: [Text]
    -- ^ What are you unsure about?
    -- Empty = confident; non-empty = specific questions for review
    -- REPLACES confidence score - list length is derived metric

  , frUnhandledCases :: [Text]
    -- ^ What inputs/scenarios does this NOT handle?
    -- Empty = handles everything you're aware of
    -- REPLACES edge case coverage score - content is actionable

  , frBoundaryReasoning :: [BoundaryNote]
    -- ^ Design intent for boundary cases
    -- The "what happens" is testable; the "why" requires understanding
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Boundary case reasoning from implementation.
--
-- Captures design intent for edge cases - semantic info we can't grep for.
data BoundaryNote = BoundaryNote
  { bnCase :: Text
    -- ^ Which boundary: "empty-input" | "single-element" | "negative" | "overflow" | etc.
  , bnIntendedBehavior :: Text
    -- ^ What SHOULD happen and why (design intent, not just observed)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Per-function rubric from tests agent.
--
-- Semantic information about test coverage - things we can't mechanically derive.
data TestFunctionRubric = TestFunctionRubric
  { tfrFunctionName :: Text
    -- ^ Which function these tests cover

  , tfrPropertiesWritten :: [Text]
    -- ^ Descriptions of what each property tests
    -- List content is semantic; length is derived metric

  , tfrApproach :: Text
    -- ^ Test approach: "property-based" | "example-based" | "integration" | "golden" | "mixed"

  , tfrScenariosNotTested :: [Text]
    -- ^ What scenarios did you consciously NOT test?
    -- Empty = comprehensive; non-empty = roadmap for more tests

  , tfrAssumptionsMade :: [Text]
    -- ^ What did you assume about the implementation?
    -- Enables probing: generate counter-assumption tests
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Structured blocker information.
--
-- Semantic info about what's blocking and what would unblock.
-- No severity field - controller infers from content.
data Blocker = Blocker
  { blCategory :: Text
    -- ^ What kind: "dependency" | "tooling" | "spec-unclear" | "type-system" | "environment" | "other"

  , blDescription :: Text
    -- ^ What's the blocker?

  , blAttemptedSolutions :: [Text]
    -- ^ What did you try? (semantic - can't mechanically know)

  , blWouldUnblock :: Text
    -- ^ What would resolve this? (requires reasoning)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)


-- ════════════════════════════════════════════════════════════════════════════
-- PROGRESS TRACKING (v2)
-- ════════════════════════════════════════════════════════════════════════════

-- | Incremental progress tracking for multi-function implementations.
--
-- Tracks which functions have passing tests vs need work.
data IncrementalProgress = IncrementalProgress
  { ipCompletedFunctions :: [Text]
    -- ^ Functions with passing implementations.
  , ipFailingFunctions :: [Text]
    -- ^ Functions with failing tests.
  , ipPendingFunctions :: [Text]
    -- ^ Functions not yet attempted.
  }
  deriving stock (Show, Eq, Generic)

instance StructuredOutput IncrementalProgress

-- | Work status for coordination between agents.
--
-- Uses bool + text to avoid sum type oneOf schema issues.
data WorkStatus = WorkStatus
  { wsIsBlocked :: Bool
    -- ^ True if work is blocked and cannot proceed.
  , wsDetail :: Text
    -- ^ Status detail: current focus if not blocked, reason if blocked.
  }
  deriving stock (Show, Eq, Generic)

instance StructuredOutput WorkStatus


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES AGENT OUTPUT
-- ════════════════════════════════════════════════════════════════════════════

-- | Output from the Types agent - type signatures only.
--
-- The types agent writes only type signatures and data definitions,
-- no implementations. This constrains the search space for impl.
-- Module header is derived by templates from the signatures.
data TypeDefinitions = TypeDefinitions
  { tdTypeName :: Text
    -- ^ Type constructor name (e.g., "Stack").
  , tdDataType :: Text
    -- ^ The data type definition (e.g., "data Stack a = Empty | Push a (Stack a)")
  , tdSignatures :: [FunctionSig]
    -- ^ Structured function signatures with metadata.
  , tdTestPriorities :: [TestPriority]
    -- ^ Test priorities for incremental development.
  , tdImports :: [Text]
    -- ^ Additional imports needed by the data types.
    -- Each entry is a full import statement like "import Data.ByteString (ByteString)"
    -- The skeleton generator will include these in the generated module.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance StructuredOutput TypeDefinitions


-- ════════════════════════════════════════════════════════════════════════════
-- SKELETON GENERATION OUTPUT
-- ════════════════════════════════════════════════════════════════════════════

-- | Output from skeleton generation - paths to generated stub files.
--
-- After the types node produces TypeDefinitions, skeleton generation:
-- 1. Renders impl-skeleton.jinja → src/<ModulePath>.hs (with undefined stubs)
-- 2. Renders test-skeleton.jinja → test/Main.hs (with QuickCheck scaffolding)
-- 3. Runs `cabal build` to verify skeletons compile
--
-- This ensures the parallel impl/test agents start from a compiling baseline.
-- Session ID is tracked via Memory effect (SessionContext), not passed explicitly.
data SkeletonGenerated = SkeletonGenerated
  { sgImplPath :: FilePath
    -- ^ Path to generated implementation skeleton (e.g., "src/Data/Stack.hs").
  , sgTestPath :: FilePath
    -- ^ Path to generated test skeleton (e.g., "test/Main.hs").
  , sgTypeDefs :: TypeDefinitions
    -- ^ Type definitions from the types agent.
  , sgProjectPath :: FilePath
    -- ^ Project root path.
  , sgModuleName :: Text
    -- ^ Module name (e.g., "Data.Stack").
  }
  deriving stock (Show, Eq, Generic)

instance StructuredOutput SkeletonGenerated


-- ════════════════════════════════════════════════════════════════════════════
-- v3 STUBS OUTPUT (for fork)
-- ════════════════════════════════════════════════════════════════════════════

-- | Output from stubs agent for the fork handler (v3 workflow).
--
-- The stubs agent writes actual .hs files and returns semantic descriptions.
-- This replaces SkeletonGenerated in the v3 flow.
data StubsGenerated = StubsGenerated
  { stgImplPath :: FilePath
    -- ^ Path to impl file written by stubs agent (e.g., "src/UrlShortener.hs").
  , stgTestPath :: FilePath
    -- ^ Path to test file (test/Main.hs - may be template-generated or empty).
  , stgSemantics :: [FunctionSemantics]
    -- ^ Semantic descriptions from stubs agent (drives test generation).
  , stgDataType :: Text
    -- ^ Data type definitions (for reference in tests).
  , stgProjectPath :: FilePath
    -- ^ Project root path.
  , stgModuleName :: Text
    -- ^ Module name (e.g., "UrlShortener").
  , stgSessionId :: Text
    -- ^ Session ID from stubs agent (for forking parallel agents).
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- TESTS AGENT OUTPUT
-- ════════════════════════════════════════════════════════════════════════════

-- | Result from the tests agent.
--
-- SEMANTIC output only - handler computes mechanical checks (build status).
--
-- Design: LLM is sensor, code is controller.
-- You report semantic rubrics; handler verifies build and decides routing.
data TestsResult = TestsResult
  { trFunctionRubrics :: [TestFunctionRubric]
    -- ^ Per-function test rubrics (semantic info).
    -- One entry for each function you wrote tests for.

  , trCommitMessage :: Text
    -- ^ Git commit message (50 chars max, imperative mood).
    -- Example: "Add QuickCheck properties for Stack LIFO behavior"

  , trTestingStrategy :: Text
    -- ^ Explain your testing approach. What properties did you focus on?

  , trBlocker :: Maybe Blocker
    -- ^ If blocked, provide structured blocker info. Null if successful.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance StructuredOutput TestsResult


-- ════════════════════════════════════════════════════════════════════════════
-- IMPL AGENT OUTPUT
-- ════════════════════════════════════════════════════════════════════════════

-- | Result from the implementation agent.
--
-- SEMANTIC output only - handler computes mechanical checks (build, undefined).
--
-- Design: LLM is sensor, code is controller.
-- You report semantic rubrics; handler verifies build and decides routing.
data ImplResult = ImplResult
  { irFunctionRubrics :: [FunctionRubric]
    -- ^ Per-function implementation rubrics (semantic info).
    -- One entry for each function you implemented.

  , irCommitMessage :: Text
    -- ^ Git commit message (50 chars max, imperative mood).
    -- Example: "Implement Stack with recursive data type"

  , irDesignNotes :: Text
    -- ^ Explain key design decisions. What data representation did you choose?

  , irBlocker :: Maybe Blocker
    -- ^ If blocked, provide structured blocker info. Null if successful.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance StructuredOutput ImplResult


-- ════════════════════════════════════════════════════════════════════════════
-- TEST LOOP STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | State for the test loop that retries on failure.
data TestLoopState = TestLoopState
  { tlsRetryCount :: Int
    -- ^ Current retry count.
  , tlsMaxRetries :: Int
    -- ^ Maximum retries before giving up.
  , tlsLastError :: Maybe Text
    -- ^ Last error message (if any).
  , tlsTestsFile :: FilePath
    -- ^ Path to test file.
  , tlsImplFile :: FilePath
    -- ^ Path to implementation file.
  }
  deriving stock (Show, Eq, Generic)

instance StructuredOutput TestLoopState

-- | Result of running tests.
data TestResult
  = TestsPassed Int
    -- ^ All tests passed (number of tests).
  | TestsFailed Text
    -- ^ Tests failed with error message.
  deriving stock (Show, Eq, Generic)

instance StructuredOutput TestResult


-- ════════════════════════════════════════════════════════════════════════════
-- FINAL RESULT
-- ════════════════════════════════════════════════════════════════════════════

-- | Final result of the types-first workflow.
data ImplementationResult = ImplementationResult
  { irSuccess :: Bool
    -- ^ Whether implementation succeeded.
  , irTestsPassed :: Int
    -- ^ Number of tests that passed.
  , irSummary :: Text
    -- ^ Summary of what was accomplished.
  }
  deriving stock (Show, Eq, Generic)

instance StructuredOutput ImplementationResult


-- ════════════════════════════════════════════════════════════════════════════
-- INTERNAL TYPES (for graph transitions)
-- ════════════════════════════════════════════════════════════════════════════

-- | Input to the fork handler.
--
-- Contains type definitions and project info. Session ID is now tracked
-- via the Memory effect (SessionContext) rather than passed explicitly.
data ForkInput = ForkInput
  { fiTypeDefs :: TypeDefinitions
    -- ^ Type definitions from types agent.
  , fiProjectPath :: FilePath
    -- ^ Project root path.
  , fiModuleName :: Text
    -- ^ Module name for generated files.
  }
  deriving stock (Show, Eq, Generic)

instance StructuredOutput ForkInput

-- | Results from parallel agents.
--
-- Combined output from tests and impl agents for merging.
-- Includes session IDs and costs for tracking and aggregation.
data ParallelResults = ParallelResults
  { prTestsWorktree :: WorktreePath
    -- ^ Path to tests worktree.
  , prImplWorktree :: WorktreePath
    -- ^ Path to impl worktree.
  , prTestsResult :: TestsResult
    -- ^ Result metadata from tests agent.
  , prImplResult :: ImplResult
    -- ^ Result metadata from impl agent.
  , prTestsSessionId :: Text
    -- ^ Session ID from tests agent.
  , prImplSessionId :: Text
    -- ^ Session ID from impl agent.
  , prTestsCost :: Double
    -- ^ Cost (USD) from tests agent.
  , prImplCost :: Double
    -- ^ Cost (USD) from impl agent.
  , prParentSessionId :: Text
    -- ^ Parent session ID (from types/stubs agent) for lineage tracking.
  }
  deriving stock (Show, Eq, Generic)

instance StructuredOutput ParallelResults


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION STATE (Memory Effect)
-- ════════════════════════════════════════════════════════════════════════════

-- | Session context tracked via Memory effect.
--
-- Replaces explicit session ID threading through intermediate types
-- (ForkInput.fiSessionId, SkeletonGenerated.sgSessionId, etc.).
--
-- The Memory effect provides typed persistent state that handlers can
-- read/write without explicit parameter passing.
--
-- @
-- -- Store after types agent completes
-- updateMem \@SessionContext (\\s -> s { scSessionId = result.sessionId })
--
-- -- Read in later handlers
-- ctx <- getMem \@SessionContext
-- let sessionId = ctx.scSessionId
-- @
data SessionContext = SessionContext
  { scSessionId :: Text
    -- ^ Current parent session ID (from types/stubs agent).
    -- Used by fork handler to spawn parallel agents that share context.
  , scProjectPath :: FilePath
    -- ^ Project root path (set at entry, used throughout).
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Empty session context (initial state).
emptySessionContext :: SessionContext
emptySessionContext = SessionContext
  { scSessionId = ""
  , scProjectPath = ""
  }


-- ════════════════════════════════════════════════════════════════════════════
-- TDD WORKFLOW TYPES (Sequential - tests before impl)
-- ════════════════════════════════════════════════════════════════════════════

-- | State after skeleton generation in TDD workflow.
--
-- Contains paths to generated files and type definitions for downstream use.
data SkeletonState = SkeletonState
  { ssTypeDefs :: TypeDefinitions
    -- ^ Type definitions from the types agent.
  , ssImplPath :: FilePath
    -- ^ Path to generated implementation skeleton.
  , ssTestPath :: FilePath
    -- ^ Path to generated test skeleton.
  , ssProjectPath :: FilePath
    -- ^ Project root path.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance StructuredOutput SkeletonState

-- | State after tests are written (TDD step 1).
--
-- Tests have been written but not yet verified to fail.
data TestsWritten = TestsWritten
  { twSkeletonState :: SkeletonState
    -- ^ State from skeleton generation.
  , twTestsResult :: TestsResult
    -- ^ Result metadata from tests agent.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance StructuredOutput TestsWritten

-- | State after verifying tests fail as expected (TDD step 2).
--
-- Tests have been verified to fail before implementation.
-- This proves the tests are meaningful (not trivially passing).
data TestsVerified = TestsVerified
  { tvTestsWritten :: TestsWritten
    -- ^ State from tests writing.
  , tvFailingTests :: [TestFailure]
    -- ^ Expected failures before implementation.
    -- Used to track which tests should eventually pass.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance StructuredOutput TestsVerified

-- | State after implementation is written (TDD step 3).
--
-- Implementation has been written and compiles, but not yet validated.
data ImplWritten = ImplWritten
  { iwTestsVerified :: TestsVerified
    -- ^ State from test verification.
  , iwImplResult :: ImplResult
    -- ^ Result metadata from impl agent.
  , iwAttempt :: Int
    -- ^ Current attempt number (for retry tracking).
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance StructuredOutput ImplWritten

-- | Validation failure with structured test output.
--
-- When tests fail after implementation, this captures the failures
-- for the fix agent to address.
data ValidationFailure = ValidationFailure
  { vfImplWritten :: ImplWritten
    -- ^ State from implementation.
  , vfFailures :: [TestFailure]
    -- ^ Structured test failures for LLM consumption.
  , vfAttempt :: Int
    -- ^ Current fix attempt number.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance StructuredOutput ValidationFailure

-- | Result from the fix agent.
--
-- SEMANTIC output only - handler computes mechanical checks.
--
-- Design: LLM is sensor, code is controller.
-- You report semantic rubrics; handler verifies build and decides routing.
--
-- Note: Uses "fix" prefix (not "fr") to avoid confusion with FunctionRubric
-- which also uses "fr" prefix. ImplResult uses "ir", TestsResult uses "tr".
data FixResult = FixResult
  { fixFunctionRubrics :: [FunctionRubric]
    -- ^ Per-function rubrics for functions you fixed (semantic info).
    -- Controller compares to previous rubrics to detect progress.
  , fixChangesMade :: [Text]
    -- ^ Summary of changes made (for logging).
  , fixCommitMessage :: Text
    -- ^ Git commit message (50 chars max).
  , fixBlocker :: Maybe Blocker
    -- ^ If blocked, provide structured blocker info. Null if successful.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance StructuredOutput FixResult

-- | Final result of the TDD workflow.
--
-- Indicates whether the workflow succeeded and how many attempts it took.
data TDDResult = TDDResult
  { tdrSuccess :: Bool
    -- ^ Whether all tests pass.
  , tdrAttempts :: Int
    -- ^ Number of fix attempts needed (1 = first try succeeded).
  , tdrTypeDefs :: TypeDefinitions
    -- ^ Final type definitions.
  , tdrTestsResult :: TestsResult
    -- ^ Final tests result.
  , tdrImplResult :: ImplResult
    -- ^ Final impl result.
  , tdrFinalTestOutput :: Text
    -- ^ Final test output (for logging).
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance StructuredOutput TDDResult
