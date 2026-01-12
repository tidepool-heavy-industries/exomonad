{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rubric-based controller logic for TDD workflow.
--
-- Design principle: LLM is sensor, code is controller.
--
-- The handler receives two input streams:
-- 1. Mechanical checks (computed by handler) - build status, undefined presence, test results
-- 2. Semantic rubrics (from LLM) - critiques, coverage gaps, blockers
--
-- This module provides pure functions that combine both streams
-- and make routing decisions (approve vs more-tests vs retry vs escalate).
--
-- Key insight: Ask for useful information without hinting at consequences.
-- LLM doesn't know how we interpret the rubrics, so it has no incentive to game.
module TypesFirstDev.Policy
  ( -- * Mechanical Checks
    MechanicalChecks(..)
  , runMechanicalChecks
  , checkBuild
  , checkTests
  , findUndefined
  , UndefinedLocation(..)

    -- * V3 Assessment
  , TDDAssessment(..)

    -- * Derived Metrics
  , critiqueCount
  , coverageGapCount
  , totalCritiques

    -- * Routing Decisions (Impl)
  , ImplDecision(..)
  , RetryDecision(..)
  , RetryContext(..)
  , decideImplRetry

    -- * Progress Tracking
  , progressImproved
  , recurringBlockers
  ) where

import Data.List (intersect)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import TypesFirstDev.Types.Shared (Critique(..), CoverageReport(..))


-- ════════════════════════════════════════════════════════════════════════════
-- MECHANICAL CHECKS (computed by handler, not LLM)
-- ════════════════════════════════════════════════════════════════════════════

-- | Mechanical checks computed by the handler.
--
-- These are facts we can verify ourselves - no need to ask the LLM.
data MechanicalChecks = MechanicalChecks
  { mcBuildPassed  :: Bool   -- ^ Did `cabal build` succeed?
  , mcTestsPassed  :: Bool   -- ^ Did `cabal test` succeed?
  , mcHasUndefined :: Bool   -- ^ Does the code contain `undefined` or stubs?
  , mcBuildOutput  :: Text   -- ^ Build output for debugging
  , mcTestOutput   :: Text   -- ^ Test output for debugging
  , mcTestCount    :: Int    -- ^ Number of tests run
  , mcFailedTests  :: [Text] -- ^ Names of failed tests
  }
  deriving stock (Show, Eq)


-- | Run all mechanical checks for a project.
runMechanicalChecks
  :: FilePath  -- ^ Project path
  -> FilePath  -- ^ Implementation file to check for undefined
  -> IO MechanicalChecks
runMechanicalChecks projectPath implFile = do
  (buildOk, buildOut) <- checkBuild projectPath
  (testsOk, testOut, testCount, failed) <- if buildOk
    then checkTests projectPath
    else pure (False, "Skipped - build failed", 0, [])
  hasUndef <- not . null <$> findUndefined implFile
  pure MechanicalChecks
    { mcBuildPassed = buildOk
    , mcTestsPassed = testsOk
    , mcHasUndefined = hasUndef
    , mcBuildOutput = buildOut
    , mcTestOutput = testOut
    , mcTestCount = testCount
    , mcFailedTests = failed
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


-- | Check if tests pass, returning count and failed test names.
checkTests :: FilePath -> IO (Bool, Text, Int, [Text])
checkTests projectPath = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode
    "cabal"
    ["test", "--project-dir=" <> projectPath, "--test-show-details=always"]
    ""
  let output = T.pack stdout <> "\n" <> T.pack stderr
      (testCount, failedNames) = parseTestOutput output
  pure (exitCode == ExitSuccess, output, testCount, failedNames)

-- | Parse cabal test output to extract test count and failed test names.
--
-- Looks for:
-- - "X of Y test suites (A of B test cases) passed." → extracts B (total test cases)
-- - Lines containing ": FAIL" → extracts test names
parseTestOutput :: Text -> (Int, [Text])
parseTestOutput output =
  let outputLines = T.lines output
      -- Parse test count from summary line: "X of Y test suites (A of B test cases) passed."
      testCount = case filter (T.isInfixOf "test cases) passed") outputLines of
        (summaryLine:_) -> parseTestCount summaryLine
        [] -> 0
      -- Find failed test names from lines like "    testName: FAIL"
      failedNames = concatMap extractFailedTest outputLines
  in (testCount, failedNames)
  where
    -- Extract total test count from "... (A of B test cases) passed."
    parseTestCount :: Text -> Int
    parseTestCount line =
      case T.breakOn "of " (snd $ T.breakOn "(" line) of
        (_, rest) ->
          case T.words (T.drop 3 rest) of  -- Skip "of "
            (numText:_) -> either (const 0) fst (TR.decimal numText)
            _ -> 0

    -- Extract test name if line indicates failure
    extractFailedTest :: Text -> [Text]
    extractFailedTest line
      | ": FAIL" `T.isInfixOf` line =
          -- Line format: "    testName: FAIL" or "    testName: FAIL (reason)"
          let trimmed = T.strip line
              testName = T.strip $ fst $ T.breakOn ":" trimmed
          in if T.null testName then [] else [testName]
      | otherwise = []


-- | A location where undefined/stub was found.
data UndefinedLocation = UndefinedLocation
  { ulFile    :: FilePath
  , ulLine    :: Int
  , ulContent :: Text
  }
  deriving stock (Show, Eq)


-- | Find all undefined/stub locations in a file or directory.
findUndefined :: FilePath -> IO [UndefinedLocation]
findUndefined path = do
  (exitCode, stdout, _) <- readProcessWithExitCode
    "grep"
    [ "-rn", "-E", "--include=*.hs"
    , stubPattern
    , path
    ]
    ""
  case exitCode of
    ExitSuccess -> pure $ parseGrepOutput stdout
    _ -> pure []
  where
    stubPattern = "\\bundefined\\b|error \"TODO\"|error \"[Nn]ot implemented\""

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


-- ════════════════════════════════════════════════════════════════════════════
-- V3 ASSESSMENT (combining mechanical + semantic)
-- ════════════════════════════════════════════════════════════════════════════

-- | V3 assessment combining mechanical checks and TDD review rubrics.
data TDDAssessment = TDDAssessment
  { v3Mechanical :: MechanicalChecks
    -- ^ Mechanical checks computed by handler
  , v3Critiques  :: [Critique]
    -- ^ TDD's critiques of the implementation
  , v3Coverage   :: CoverageReport
    -- ^ Coverage analysis from TDD
  , v3Blocker    :: Maybe Text
    -- ^ Blocker description if stuck
  }
  deriving stock (Show, Eq)


-- ════════════════════════════════════════════════════════════════════════════
-- DERIVED METRICS
-- ════════════════════════════════════════════════════════════════════════════

-- | Count of critiques (issue metric).
critiqueCount :: TDDAssessment -> Int
critiqueCount = length . v3Critiques

-- | Count of criteria without tests (coverage gap metric).
coverageGapCount :: TDDAssessment -> Int
coverageGapCount = length . crCriteriaMissing . v3Coverage

-- | Total critique count across assessments.
totalCritiques :: [TDDAssessment] -> Int
totalCritiques = sum . map critiqueCount


-- ════════════════════════════════════════════════════════════════════════════
-- IMPL RETRY DECISIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Impl node decision after test failure.
data ImplDecision
  = RetryImpl RetryContext
    -- ^ Try again with context from failure
  | RequestHelp Text
    -- ^ Ask for clarification
  | GiveUp Text
    -- ^ Stuck, escalate
  deriving (Show, Eq)


-- | Retry decision result.
data RetryDecision
  = Retry RetryContext
  | Escalate Text
  | AskHuman [Text]
  deriving (Show, Eq)


-- | Context for retry attempts.
data RetryContext = RetryContext
  { rcFailedTests   :: [Text]  -- ^ Which tests are still failing
  , rcAttemptNumber :: Int     -- ^ Current attempt (for max-retry check)
  , rcPreviousFixes :: [Text]  -- ^ What was tried before
  }
  deriving (Show, Eq)


-- | Decide whether Impl should retry or escalate.
--
-- Uses trajectory analysis (are we making progress?).
-- Caller passes previous fixes from Memory for accumulation.
decideImplRetry
  :: Int          -- ^ Current attempt number
  -> Int          -- ^ Max attempts allowed
  -> [Text]       -- ^ Previously failing tests
  -> [Text]       -- ^ Currently failing tests
  -> [Text]       -- ^ Previous fix attempts (from Memory, passed by caller)
  -> Maybe Text   -- ^ Blocker if any
  -> ImplDecision
decideImplRetry attempt maxAttempts prevFailed currFailed prevFixes mBlocker
  -- Blocker → ask for help
  | Just blocker <- mBlocker = RequestHelp blocker

  -- Max attempts reached → give up
  | attempt >= maxAttempts = GiveUp $ "No progress after " <> T.pack (show maxAttempts) <> " attempts"

  -- Progress being made → retry
  | progressImproved prevFailed currFailed =
      RetryImpl RetryContext
        { rcFailedTests = currFailed
        , rcAttemptNumber = attempt + 1
        , rcPreviousFixes = prevFixes
        }

  -- No progress but under limit → one more try
  | otherwise =
      RetryImpl RetryContext
        { rcFailedTests = currFailed
        , rcAttemptNumber = attempt + 1
        , rcPreviousFixes = prevFixes
        }


-- ════════════════════════════════════════════════════════════════════════════
-- PROGRESS TRACKING
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if progress is being made (fewer failures).
progressImproved :: [Text] -> [Text] -> Bool
progressImproved prev curr
  | null prev = True   -- First attempt, any state is progress
  | null curr = True   -- All tests pass!
  | otherwise = length curr < length prev


-- | Detect recurring blockers across attempts.
--
-- If the same blockers keep appearing, the LLM is stuck.
recurringBlockers :: [Text] -> [Text] -> [Text]
recurringBlockers prev curr = intersect prev curr
