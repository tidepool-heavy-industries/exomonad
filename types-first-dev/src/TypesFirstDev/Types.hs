{-# LANGUAGE FieldSelectors #-}

-- | Schema types for the types-first development workflow.
--
-- These are the structured outputs from Claude Code sessions.
-- Each type has FromJSON/ToJSON for parsing LLM output and
-- HasJSONSchema for passing to the LLM as a schema constraint.
module TypesFirstDev.Types
  ( -- * Entry Type
    StackSpec(..)

    -- * Structured Signature Types (v2)
  , FunctionSig(..)
  , TestPriority(..)

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

    -- * Internal Types
  , ForkInput(..)
  , ParallelResults(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effects.Worktree (WorktreePath)
import Tidepool.Schema (HasJSONSchema)



-- ════════════════════════════════════════════════════════════════════════════
-- ENTRY TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | Specification for the Stack data type to implement.
--
-- This is the entry point to the graph - defines what to build.
data StackSpec = StackSpec
  { ssProjectPath :: FilePath
    -- ^ Path to the project root.
  , ssModuleName :: Text
    -- ^ Module name (e.g., "Data.Stack").
  , ssDescription :: Text
    -- ^ Natural language description of the data structure.
  , ssAcceptanceCriteria :: [Text]
    -- ^ High-level acceptance criteria to inform test priorities.
  }
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
  -- HasJSONSchema derived via TH in Schema.hs

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
  -- HasJSONSchema derived via TH in Schema.hs


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
  deriving anyclass (FromJSON, ToJSON)

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
  deriving anyclass (FromJSON, ToJSON)


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
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
  -- HasJSONSchema derived in TypesFirstDev.Schema via TH


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
  deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- TESTS AGENT OUTPUT
-- ════════════════════════════════════════════════════════════════════════════

-- | Result from the tests agent.
--
-- You edited the test skeleton to add QuickCheck property tests.
-- Report your results here so the handler can commit and merge your work.
data TestsResult = TestsResult
  { trBuildPassed :: Bool
    -- ^ Did `cabal build test-repo-test` succeed after your edits?
    -- This compiles the test suite. You MUST verify this before returning.

  , trAllPropertiesWritten :: Bool
    -- ^ Did you write properties for ALL functions AND acceptance criteria?
    -- Set False if any required property is missing or placeholder.

  , trCommitMessage :: Text
    -- ^ Git commit message (50 chars max, imperative mood).
    -- Example: "Add QuickCheck properties for Stack LIFO behavior"

  , trTestingStrategy :: Text
    -- ^ Explain your testing approach. What properties did you focus on?
    -- Any assumptions about the implementation? Edge cases covered?

  , trBlocker :: Maybe Text
    -- ^ If blocked or incomplete, explain why. Missing Arbitrary instances?
    -- Type issues? Null if work completed successfully.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
  -- HasJSONSchema derived via TH in Schema.hs


-- ════════════════════════════════════════════════════════════════════════════
-- IMPL AGENT OUTPUT
-- ════════════════════════════════════════════════════════════════════════════

-- | Result from the implementation agent.
--
-- You edited the skeleton file to replace `undefined` stubs with working code.
-- Report your results here so the handler can commit and merge your work.
data ImplResult = ImplResult
  { irBuildPassed :: Bool
    -- ^ Did `cabal build test-repo` succeed after your edits?
    -- You MUST run this command before returning. Set False if build failed.

  , irAllFunctionsImplemented :: Bool
    -- ^ Did you implement ALL functions listed in the task?
    -- Set False if any function still has `undefined` or is incomplete.

  , irCommitMessage :: Text
    -- ^ Git commit message (50 chars max, imperative mood).
    -- Example: "Implement Stack with recursive data type"
    -- Be specific about WHAT you built, not HOW.

  , irDesignNotes :: Text
    -- ^ Explain key design decisions. What data representation did you choose?
    -- Any tradeoffs? Edge cases handled? This helps the test agent understand.

  , irBlocker :: Maybe Text
    -- ^ If blocked or incomplete, explain why. What would unblock you?
    -- Null if work completed successfully. Used for retry/escalation.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
  -- HasJSONSchema derived via TH in Schema.hs


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
  deriving anyclass (FromJSON, ToJSON)

-- | Result of running tests.
data TestResult
  = TestsPassed Int
    -- ^ All tests passed (number of tests).
  | TestsFailed Text
    -- ^ Tests failed with error message.
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


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
  deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- INTERNAL TYPES (for graph transitions)
-- ════════════════════════════════════════════════════════════════════════════

-- | Input to the fork handler.
--
-- Contains the session ID from the types agent and the type definitions.
data ForkInput = ForkInput
  { fiSessionId :: Text
    -- ^ Session ID from types agent (for forking).
  , fiTypeDefs :: TypeDefinitions
    -- ^ Type definitions from types agent.
  , fiProjectPath :: FilePath
    -- ^ Project root path.
  , fiModuleName :: Text
    -- ^ Module name for generated files.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Results from parallel agents.
--
-- Combined output from tests and impl agents for merging.
data ParallelResults = ParallelResults
  { prTestsWorktree :: WorktreePath
    -- ^ Path to tests worktree.
  , prImplWorktree :: WorktreePath
    -- ^ Path to impl worktree.
  , prTestsResult :: TestsResult
    -- ^ Result metadata from tests agent.
  , prImplResult :: ImplResult
    -- ^ Result metadata from impl agent.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
