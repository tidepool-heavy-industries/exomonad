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
import qualified Data.Text.IO as TIO
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


-- | Check if file contains `undefined` or stub patterns.
--
-- Looks for:
-- - `undefined`
-- - `error "TODO"`
-- - `error "not implemented"`
checkUndefined :: FilePath -> IO Bool
checkUndefined filePath = do
  content <- TIO.readFile filePath
  pure $ any (`T.isInfixOf` content) stubPatterns
  where
    stubPatterns =
      [ "undefined"
      , "error \"TODO\""
      , "error \"Not implemented\""
      , "error \"not implemented\""
      , "= error"  -- common pattern: foo = error "..."
      ]
