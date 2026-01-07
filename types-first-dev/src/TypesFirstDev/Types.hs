-- | Schema types for the types-first development workflow.
--
-- These are the structured outputs from Claude Code sessions.
-- Each type has FromJSON/ToJSON for parsing LLM output and
-- HasJSONSchema for passing to the LLM as a schema constraint.
module TypesFirstDev.Types
  ( -- * Entry Type
    StackSpec(..)

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

    -- * Internal Types
  , ForkInput(..)
  , ParallelResults(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)

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
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES AGENT OUTPUT
-- ════════════════════════════════════════════════════════════════════════════

-- | Output from the Types agent - type signatures only.
--
-- The types agent writes only type signatures and data definitions,
-- no implementations. This constrains the search space for impl.
data TypeDefinitions = TypeDefinitions
  { tdDataType :: Text
    -- ^ The data type definition (e.g., "data Stack a = Empty | Push a (Stack a)")
  , tdSignatures :: [Text]
    -- ^ Type signatures for functions (e.g., ["push :: a -> Stack a -> Stack a"])
  , tdModuleHeader :: Text
    -- ^ Module header with exports
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)


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
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)


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
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)


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
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)

-- | Result of running tests.
data TestResult
  = TestsPassed Int
    -- ^ All tests passed (number of tests).
  | TestsFailed Text
    -- ^ Tests failed with error message.
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)


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
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)


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
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)

-- | Results from parallel agents.
--
-- Combined output from tests and impl agents for merging.
data ParallelResults = ParallelResults
  { prTestsWorktree :: FilePath
    -- ^ Path to tests worktree.
  , prImplWorktree :: FilePath
    -- ^ Path to impl worktree.
  , prTestDefs :: TestDefinitions
    -- ^ Test definitions from tests agent.
  , prImplCode :: ImplementationCode
    -- ^ Implementation code from impl agent.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)
