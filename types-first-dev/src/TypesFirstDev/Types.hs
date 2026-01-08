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
  , TestDefinitions(..)

    -- * Impl Agent Output
  , ImplementationCode(..)

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
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)

-- | Test priority for incremental development.
--
-- Guides which functions to test first based on dependency order.
data TestPriority = TestPriority
  { tpFunction :: Text
    -- ^ Function name this test targets.
  , tpPriority :: Int
    -- ^ Priority (1 = most critical, higher = less critical).
  , tpRationale :: Text
    -- ^ Why this priority ranking.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)


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
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)

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
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES AGENT OUTPUT
-- ════════════════════════════════════════════════════════════════════════════

-- | Output from the Types agent - type signatures only.
--
-- The types agent writes only type signatures and data definitions,
-- no implementations. This constrains the search space for impl.
-- Module header is derived by templates from the signatures.
data TypeDefinitions = TypeDefinitions
  { tdDataType :: Text
    -- ^ The data type definition (e.g., "data Stack a = Empty | Push a (Stack a)")
  , tdSignatures :: [FunctionSig]
    -- ^ Structured function signatures with metadata.
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

-- | Output from the Tests agent - QuickCheck property tests.
--
-- Written in parallel with implementation, both working from type signatures.
data TestDefinitions = TestDefinitions
  { testModuleCode :: Text
    -- ^ Complete test module code with properties.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- IMPL AGENT OUTPUT
-- ════════════════════════════════════════════════════════════════════════════

-- | Output from the Impl agent - function implementations.
--
-- Must satisfy the type signatures from TypeDefinitions.
data ImplementationCode = ImplementationCode
  { implModuleCode :: Text
    -- ^ Complete implementation module code.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


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
  , prTestDefs :: TestDefinitions
    -- ^ Test definitions from tests agent.
  , prImplCode :: ImplementationCode
    -- ^ Implementation code from impl agent.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
