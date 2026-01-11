{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rubric-based controller logic for V3 TDD workflow.
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
module TypesFirstDev.V3.Policy
  ( -- * Mechanical Checks
    MechanicalChecks(..)
  , runMechanicalChecks
  , checkBuild
  , checkTests
  , findUndefined
  , UndefinedLocation(..)

    -- * V3 Assessment
  , V3Assessment(..)

    -- * Derived Metrics
  , critiqueCount
  , coverageGapCount
  , totalCritiques

    -- * Routing Decisions (V3 TDD)
  , TDDDecision(..)
  , ImplDecision(..)
  , RetryDecision(..)
  , RetryContext(..)
  , decideTDDReview
  , decideImplRetry

    -- * Progress Tracking
  , progressImproved
  , recurringBlockers
  ) where

import Data.List (intersect)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import TypesFirstDev.V3.Types.Shared (Critique(..), CoverageReport(..))
import TypesFirstDev.V3.Types.Payloads (ImplResult(..))


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
      -- TODO: Parse test count and failed names from output
      testCount = 0
      failedNames = []
  pure (exitCode == ExitSuccess, output, testCount, failedNames)


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
data V3Assessment = V3Assessment
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
critiqueCount :: V3Assessment -> Int
critiqueCount = length . v3Critiques

-- | Count of criteria without tests (coverage gap metric).
coverageGapCount :: V3Assessment -> Int
coverageGapCount = length . crCriteriaMissing . v3Coverage

-- | Total critique count across assessments.
totalCritiques :: [V3Assessment] -> Int
totalCritiques = sum . map critiqueCount


-- ════════════════════════════════════════════════════════════════════════════
-- TDD REVIEW DECISIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | TDD review decision after evaluating impl.
data TDDDecision
  = Approved
    -- ^ All criteria satisfied, approve for merge
  | MoreTests [Text]
    -- ^ Need more tests for these criteria
  | Reject [Critique]
    -- ^ Impl has issues, send back to fix
  deriving (Show, Eq)


-- | Decide TDD review outcome based on assessment.
--
-- Priority:
-- 1. Mechanical failures (hard stops)
-- 2. Critiques (impl issues)
-- 3. Coverage gaps (missing tests)
-- 4. All clear → approve
decideTDDReview :: V3Assessment -> TDDDecision
decideTDDReview V3Assessment{..}
  -- FIRST: Mechanical failures
  | not (mcBuildPassed v3Mechanical) = Reject [mechanicalCritique "Build failed"]
  | mcHasUndefined v3Mechanical = Reject [mechanicalCritique "Contains undefined stubs"]
  | not (mcTestsPassed v3Mechanical) = Reject [mechanicalCritique "Tests failing"]

  -- THEN: Semantic critiques
  | not (null v3Critiques) = Reject v3Critiques

  -- THEN: Coverage gaps → need more tests
  | not (null (crCriteriaMissing v3Coverage)) = MoreTests (crCriteriaMissing v3Coverage)

  -- All clear
  | otherwise = Approved
  where
    mechanicalCritique msg = Critique
      { cqFile = ""
      , cqLine = 0
      , cqIssue = msg
      , cqRequiredFix = "Fix mechanical failure"
      }


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
decideImplRetry
  :: Int          -- ^ Current attempt number
  -> Int          -- ^ Max attempts allowed
  -> [Text]       -- ^ Previously failing tests
  -> [Text]       -- ^ Currently failing tests
  -> Maybe Text   -- ^ Blocker if any
  -> ImplDecision
decideImplRetry attempt maxAttempts prevFailed currFailed mBlocker
  -- Blocker → ask for help
  | Just blocker <- mBlocker = RequestHelp blocker

  -- Max attempts reached → give up
  | attempt >= maxAttempts = GiveUp $ "No progress after " <> T.pack (show maxAttempts) <> " attempts"

  -- Progress being made → retry
  | progressImproved prevFailed currFailed =
      RetryImpl RetryContext
        { rcFailedTests = currFailed
        , rcAttemptNumber = attempt + 1
        , rcPreviousFixes = []  -- TODO: accumulate from Memory
        }

  -- No progress but under limit → one more try
  | otherwise =
      RetryImpl RetryContext
        { rcFailedTests = currFailed
        , rcAttemptNumber = attempt + 1
        , rcPreviousFixes = []
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
