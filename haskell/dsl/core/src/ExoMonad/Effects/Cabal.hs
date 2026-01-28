-- | Cabal build/test effect for TDD workflows.
--
-- Effect type only - interpreter lives in exomonad-native-gui/cabal-interpreter.
-- Enables graphs to run build/test commands and receive structured feedback.
--
-- = Example Usage
--
-- @
-- import ExoMonad.Effects.Cabal
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
-- - If tests fail, raw output drives iteration
module ExoMonad.Effects.Cabal
  ( -- * Effect
    Cabal(..)
  , cabalBuild
  , cabalTest
  , cabalClean

    -- * Result Types
  , CabalResult(..)
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

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
--   CabalTestFailure raw -> handleTestFailures raw
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
      { ctfRawOutput :: Text
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
  -- Returns 'CabalTestSuccess' or 'CabalTestFailure' with raw output.
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
--   CabalTestFailure raw -> fixImplementation raw
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