-- | Cabal build/test effect for TDD workflows.
--
-- Effect type only - interpreter lives in tidepool-native-gui/cabal-interpreter.
-- Enables graphs to run build/test commands and receive structured feedback.
--
-- = Example Usage
--
-- @
-- import Tidepool.Effects.Cabal
--
-- validateHandler :: Member Cabal effs => Eff effs CabalResult
-- validateHandler path = do
--   buildResult <- cabalBuild path
--   case buildResult of
--     CabalSuccess -> cabalTest path
--     failure -> pure failure
-- @
--
-- = Design Notes
--
-- The Cabal effect enables TDD workflows where:
-- - Tests are written before implementation
-- - Tests are verified to fail (proves they're meaningful)
-- - Implementation is written
-- - Tests are verified to pass
-- - If tests fail, structured feedback drives iteration
--
-- = Test Failure Parsing
--
-- QuickCheck and HSpec output is parsed into structured `TestFailure` records
-- containing property names, counterexamples, and seeds. This enables LLMs
-- to receive actionable feedback for fixing implementations.
module Tidepool.Effects.Cabal
  ( -- * Effect
    Cabal(..)
  , cabalBuild
  , cabalTest
  , cabalClean

    -- * Result Types
  , CabalResult(..)
  , TestFailure(..)
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.StructuredOutput (StructuredOutput)


-- ════════════════════════════════════════════════════════════════════════════
-- TEST FAILURE
-- ════════════════════════════════════════════════════════════════════════════

-- | Structured test failure for LLM consumption.
--
-- Parsed from QuickCheck/HSpec output to provide actionable feedback.
--
-- @
-- TestFailure
--   { tfPropertyName = "prop_lifo_order"
--   , tfMessage = "Falsified (after 5 tests)"
--   , tfCounterexample = Just "[1,2,3]"
--   , tfSeed = Just 12345
--   , tfLocation = Just "test/Main.hs:42"
--   }
-- @
data TestFailure = TestFailure
  { tfPropertyName :: Text
    -- ^ Property or test name that failed (e.g., "prop_pushPop")
  , tfMessage :: Text
    -- ^ Failure message from the test framework
  , tfCounterexample :: Maybe Text
    -- ^ QuickCheck counterexample if available
  , tfSeed :: Maybe Int
    -- ^ QuickCheck seed for reproduction
  , tfLocation :: Maybe Text
    -- ^ Source location (file:line) if available
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance StructuredOutput TestFailure


-- ════════════════════════════════════════════════════════════════════════════
-- RESULT TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of a cabal operation.
--
-- Use pattern matching to handle different outcomes:
--
-- @
-- case result of
--   CabalSuccess -> proceed
--   CabalBuildFailure code stderr stdout -> handleBuildError stderr
--   CabalTestFailure failures raw -> handleTestFailures failures
--   CabalTestSuccess output -> handleSuccess output
-- @
data CabalResult
  = CabalSuccess
    -- ^ Operation succeeded (for clean, etc.)
  | CabalBuildFailure
      { cbfExitCode :: Int
        -- ^ Exit code from cabal
      , cbfStderr :: Text
        -- ^ Stderr output (usually contains errors)
      , cbfStdout :: Text
        -- ^ Stdout output
      }
    -- ^ Build failed with compiler errors
  | CabalTestFailure
      { ctfParsedFailures :: [TestFailure]
        -- ^ Structured test failures parsed from output
      , ctfRawOutput :: Text
        -- ^ Raw test output for debugging
      }
    -- ^ Tests ran but some failed
  | CabalTestSuccess
      { ctsOutput :: Text
        -- ^ Test output (for logging)
      }
    -- ^ All tests passed
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Cabal effect for build/test operations.
--
-- All operations take a project path and return 'CabalResult'.
-- The interpreter handles subprocess management and output parsing.
data Cabal r where
  -- | Build the project.
  --
  -- Equivalent to @cabal build all@.
  -- Returns 'CabalSuccess' or 'CabalBuildFailure'.
  CabalBuild
    :: FilePath          -- ^ Project directory
    -> Cabal CabalResult

  -- | Run tests.
  --
  -- Equivalent to @cabal test --test-show-details=always@.
  -- Returns 'CabalTestSuccess' or 'CabalTestFailure' with parsed failures.
  -- Returns 'CabalBuildFailure' if build fails.
  CabalTest
    :: FilePath          -- ^ Project directory
    -> Cabal CabalResult

  -- | Clean build artifacts.
  --
  -- Equivalent to @cabal clean@.
  -- Returns 'CabalSuccess' or 'CabalBuildFailure'.
  CabalClean
    :: FilePath          -- ^ Project directory
    -> Cabal CabalResult


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Build the project.
--
-- @
-- result <- cabalBuild "/path/to/project"
-- case result of
--   CabalSuccess -> proceed
--   CabalBuildFailure _ stderr _ -> handleError stderr
-- @
cabalBuild
  :: Member Cabal effs
  => FilePath
  -> Eff effs CabalResult
cabalBuild = send . CabalBuild

-- | Run tests with detailed output.
--
-- @
-- result <- cabalTest "/path/to/project"
-- case result of
--   CabalTestSuccess output -> logSuccess output
--   CabalTestFailure failures _ -> fixImplementation failures
--   CabalBuildFailure _ stderr _ -> fixCompilation stderr
-- @
cabalTest
  :: Member Cabal effs
  => FilePath
  -> Eff effs CabalResult
cabalTest = send . CabalTest

-- | Clean build artifacts.
--
-- Useful before a fresh build or to reclaim disk space.
cabalClean
  :: Member Cabal effs
  => FilePath
  -> Eff effs CabalResult
cabalClean = send . CabalClean
