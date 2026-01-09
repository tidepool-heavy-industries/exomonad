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
  , UndefinedLocation(..)
  , findUndefined
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


-- | A location where undefined/stub was found.
data UndefinedLocation = UndefinedLocation
  { ulFile :: FilePath
  , ulLine :: Int
  , ulContent :: Text
  }
  deriving stock (Show, Eq)


-- | Find all undefined/stub locations in a file or directory.
--
-- Returns detailed info that can be fed back to the agent for fixing.
findUndefined :: FilePath -> IO [UndefinedLocation]
findUndefined path = do
  -- Use grep -rn to get file:line:content
  (exitCode, stdout, _) <- readProcessWithExitCode
    "grep"
    [ "-rn"                   -- recursive with line numbers
    , "-E"                    -- extended regex
    , "--include=*.hs"        -- only Haskell files
    , stubPattern
    , path
    ]
    ""
  case exitCode of
    ExitSuccess -> pure $ parseGrepOutput stdout
    _ -> pure []  -- No matches or error
  where
    stubPattern = "\\bundefined\\b|error \"TODO\"|error \"[Nn]ot implemented\"| = error \""

    parseGrepOutput :: String -> [UndefinedLocation]
    parseGrepOutput = mapMaybe parseLine . lines

    parseLine :: String -> Maybe UndefinedLocation
    parseLine s = case break (== ':') s of
      (file, ':':rest) -> case break (== ':') rest of
        (lineStr, ':':content) -> case reads lineStr of
          [(lineNum, "")] -> Just UndefinedLocation
            { ulFile = file
            , ulLine = lineNum
            , ulContent = T.pack content
            }
          _ -> Nothing
        _ -> Nothing
      _ -> Nothing

    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe f = foldr (\x acc -> maybe acc (:acc) (f x)) []


-- | Check if file or directory contains `undefined` or stub patterns.
--
-- Uses grep to search recursively if given a directory.
-- Looks for:
-- - `undefined`
-- - `error "TODO"`
-- - `error "not implemented"`
--
-- Note: Prefer `findUndefined` which returns locations for route-back.
checkUndefined :: FilePath -> IO Bool
checkUndefined path = do
  locations <- findUndefined path
  pure $ not (null locations)
