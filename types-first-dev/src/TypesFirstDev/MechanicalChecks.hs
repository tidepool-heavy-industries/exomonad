{-# LANGUAGE OverloadedStrings #-}

-- | Mechanical checks computed by the handler, not reported by LLM.
--
-- Design principle: If we can verify something mechanically, we don't ask the LLM.
-- This saves tokens and eliminates gaming opportunities.
module TypesFirstDev.MechanicalChecks
  ( MechanicalChecks(..)
  , runMechanicalChecks
  , checkBuild
  , checkTests
  , checkUndefined
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)


-- | Mechanical checks computed by the handler.
--
-- These are facts we can verify ourselves - no need to ask the LLM.
data MechanicalChecks = MechanicalChecks
  { mcBuildPassed :: Bool
    -- ^ Did `cabal build` succeed?
  , mcTestsPassed :: Bool
    -- ^ Did `cabal test` succeed?
  , mcHasUndefined :: Bool
    -- ^ Does the code contain `undefined` or similar stubs?
  , mcBuildOutput :: Text
    -- ^ Build output for debugging/logging
  , mcTestOutput :: Text
    -- ^ Test output for debugging/logging
  }
  deriving stock (Show, Eq)


-- | Run all mechanical checks for a project.
runMechanicalChecks
  :: FilePath  -- ^ Project path
  -> FilePath  -- ^ Implementation file to check for undefined
  -> IO MechanicalChecks
runMechanicalChecks projectPath implFile = do
  (buildOk, buildOut) <- checkBuild projectPath
  (testsOk, testOut) <- if buildOk
    then checkTests projectPath
    else pure (False, "Skipped - build failed")
  hasUndef <- checkUndefined implFile
  pure MechanicalChecks
    { mcBuildPassed = buildOk
    , mcTestsPassed = testsOk
    , mcHasUndefined = hasUndef
    , mcBuildOutput = buildOut
    , mcTestOutput = testOut
    }


-- | Check if the project builds.
checkBuild :: FilePath -> IO (Bool, Text)
checkBuild projectPath = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode
    "cabal"
    ["build", "--project-dir=" <> projectPath]
    ""
  let output = T.pack stdout <> "\n" <> T.pack stderr
  pure (exitCode == ExitSuccess, output)


-- | Check if tests pass.
checkTests :: FilePath -> IO (Bool, Text)
checkTests projectPath = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode
    "cabal"
    ["test", "--project-dir=" <> projectPath]
    ""
  let output = T.pack stdout <> "\n" <> T.pack stderr
  pure (exitCode == ExitSuccess, output)


-- | Check if file or directory contains `undefined` or stub patterns.
--
-- Uses grep to search recursively if given a directory.
-- Looks for:
-- - `undefined`
-- - `error "TODO"`
-- - `error "not implemented"`
checkUndefined :: FilePath -> IO Bool
checkUndefined path = do
  -- Use grep -r to handle both files and directories
  -- Returns exit code 0 if pattern found, 1 if not found, 2 on error
  (exitCode, _, _) <- readProcessWithExitCode
    "grep"
    [ "-r"                    -- recursive
    , "-q"                    -- quiet (just return exit code)
    , "-E"                    -- extended regex
    , "--include=*.hs"        -- only Haskell files
    , stubPattern
    , path
    ]
    ""
  -- Exit code 0 means pattern was found (has undefined)
  pure (exitCode == ExitSuccess)
  where
    -- Combined pattern for all stub indicators
    -- Use word boundary for 'undefined' to avoid false positives
    stubPattern = "\\bundefined\\b|error \"TODO\"|error \"[Nn]ot implemented\"| = error \""
