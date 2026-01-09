{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rubric-based controller logic for types-first development.
--
-- Design principle: LLM is sensor, code is controller.
--
-- The handler receives two input streams:
-- 1. Mechanical checks (computed by handler) - build status, undefined presence, etc.
-- 2. Semantic rubrics (from LLM) - open questions, unhandled cases, approach taken
--
-- This module provides pure functions that combine both streams
-- and make routing decisions (merge vs retry vs escalate vs ask human).
--
-- Key insight: Ask for useful information without hinting at consequences.
-- LLM doesn't know how we interpret the rubrics, so it has no incentive to game.
module TypesFirstDev.RubricPolicy
  ( -- * Full Assessment (both streams)
    FullAssessment(..)

    -- * Derived Metrics
  , uncertaintyCount
  , gapCount
  , testGapCount
  , assumptionCount
  , totalUncertainty
  , totalGaps

    -- * Routing Decisions
  , MergeDecision(..)
  , WorkItem(..)
  , WorkReason(..)
  , ReviewItem(..)
  , RetryDecision(..)
  , RetryContext(..)
  , decideMerge
  , decideRetry

    -- * Helpers
  , recurringQuestions
  , gapsImproved
  , extractReviewItems
  ) where

import Data.List (intersect)
import Data.Text (Text)
import qualified Data.Text as T

import TypesFirstDev.Types
  ( FunctionRubric(..)
  , TestFunctionRubric(..)
  , Blocker(..)
  , BoundaryNote(..)
  )
import TypesFirstDev.MechanicalChecks (MechanicalChecks(..))


-- ════════════════════════════════════════════════════════════════════════════
-- FULL ASSESSMENT (combining both streams)
-- ════════════════════════════════════════════════════════════════════════════

-- | Complete assessment combining mechanical checks and semantic rubrics.
--
-- Handler computes mechanical checks, receives semantic rubrics from LLM,
-- then combines them here for routing decisions.
data FullAssessment = FullAssessment
  { faMechanical :: MechanicalChecks
    -- ^ Mechanical checks computed by handler (build, test, undefined)
  , faImplRubrics :: [FunctionRubric]
    -- ^ Semantic rubrics from implementation agent
  , faTestRubrics :: [TestFunctionRubric]
    -- ^ Semantic rubrics from tests agent
  , faBlocker :: Maybe Blocker
    -- ^ Blocker info from agent (if any)
  }
  deriving stock (Show, Eq)


-- ════════════════════════════════════════════════════════════════════════════
-- DERIVED METRICS (from semantic rubrics)
-- ════════════════════════════════════════════════════════════════════════════

-- | Count of open questions (uncertainty metric).
--
-- List length is the derived metric; empty = confident (bold claim).
uncertaintyCount :: FunctionRubric -> Int
uncertaintyCount = length . frOpenQuestions

-- | Count of unhandled cases (gap metric).
gapCount :: FunctionRubric -> Int
gapCount = length . frUnhandledCases

-- | Count of untested scenarios (test gap metric).
testGapCount :: TestFunctionRubric -> Int
testGapCount = length . tfrScenariosNotTested

-- | Count of assumptions made (probe-able metric).
assumptionCount :: TestFunctionRubric -> Int
assumptionCount = length . tfrAssumptionsMade

-- | Total uncertainty across all impl rubrics.
totalUncertainty :: [FunctionRubric] -> Int
totalUncertainty = sum . map uncertaintyCount

-- | Total gaps across all impl rubrics.
totalGaps :: [FunctionRubric] -> Int
totalGaps = sum . map gapCount


-- ════════════════════════════════════════════════════════════════════════════
-- ROUTING DECISIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Merge decision - should we merge the agent's work?
data MergeDecision
  = MergeOk
    -- ^ All checks passed, merge the work
  | NeedsWork [WorkItem]
    -- ^ Needs more work, with specific items
  | NeedsReview [ReviewItem]
    -- ^ Needs human review (for uncertainties)
  | Blocked Text
    -- ^ Cannot proceed, blocked by something
  deriving (Show, Eq)

-- | Specific work item identified from rubrics.
data WorkItem = WorkItem
  { wiFunction :: Text
    -- ^ Which function needs work (empty for general)
  , wiReason :: WorkReason
    -- ^ Why it needs work
  , wiDetails :: [Text]
    -- ^ Semantic details from rubric (e.g., specific gaps)
  }
  deriving (Show, Eq)

-- | Reason why a function needs work.
data WorkReason
  = MechanicalFailure Text
    -- ^ Build/test/undefined failure
  | HasUnhandledCases
    -- ^ Disclosed gaps in implementation
  | UntestedScenarios
    -- ^ Disclosed test gaps
  | AssumptionsToProbe
    -- ^ Assumptions that should be tested
  deriving (Show, Eq)

-- | Review item for human attention.
data ReviewItem = ReviewItem
  { riFunction :: Text
    -- ^ Which function has questions
  , riQuestions :: [Text]
    -- ^ Specific open questions
  , riApproach :: Text
    -- ^ What approach was used (context)
  }
  deriving (Show, Eq)

-- | Retry decision after failed attempt.
data RetryDecision
  = Retry RetryContext
    -- ^ Try again with context
  | Escalate Text
    -- ^ Stop retrying, escalate
  | AskHuman [Text]
    -- ^ Ask human specific questions
  deriving (Show, Eq)

-- | Context for retry attempts.
data RetryContext = RetryContext
  { rcFocusFunctions :: [Text]
    -- ^ Which functions to focus on
  , rcApproachHints :: [Text]
    -- ^ Hints based on approaches used
  , rcBoundariesToConsider :: [Text]
    -- ^ Boundaries mentioned in rubrics
  }
  deriving (Show, Eq)


-- | Decide whether to merge based on full assessment.
--
-- Priority:
-- 1. Mechanical failures (hard stops)
-- 2. Semantic gaps (actionable work)
-- 3. Uncertainties (route to review)
-- 4. Test gaps (spawn follow-up)
decideMerge :: FullAssessment -> MergeDecision
decideMerge fa
  -- FIRST: Mechanical failures are hard stops (we checked, not LLM)
  | not (mcBuildPassed mech) =
      NeedsWork [WorkItem "" (MechanicalFailure "build failed") [mcBuildOutput mech]]

  | mcHasUndefined mech =
      NeedsWork [WorkItem "" (MechanicalFailure "has undefined") []]

  | not (mcTestsPassed mech) =
      NeedsWork [WorkItem "" (MechanicalFailure "tests failed") [mcTestOutput mech]]

  -- THEN: Semantic gaps are actionable (LLM disclosed them)
  | not (null gapFns) =
      NeedsWork $ map toGapItem gapFns

  -- THEN: Uncertainties route to human review (not blind retry)
  | not (null uncertainFns) =
      NeedsReview $ map toReviewItem uncertainFns

  -- THEN: Test gaps can spawn follow-up work
  | not (null testGapFns) =
      NeedsWork $ map toTestGapItem testGapFns

  -- All clear
  | otherwise = MergeOk
  where
    mech = faMechanical fa

    gapFns = filter (not . null . frUnhandledCases) (faImplRubrics fa)
    uncertainFns = filter (not . null . frOpenQuestions) (faImplRubrics fa)
    testGapFns = filter (not . null . tfrScenariosNotTested) (faTestRubrics fa)

    toGapItem r = WorkItem
      { wiFunction = frFunctionName r
      , wiReason = HasUnhandledCases
      , wiDetails = frUnhandledCases r
      }

    toReviewItem r = ReviewItem
      { riFunction = frFunctionName r
      , riQuestions = frOpenQuestions r
      , riApproach = frApproach r
      }

    toTestGapItem r = WorkItem
      { wiFunction = tfrFunctionName r
      , wiReason = UntestedScenarios
      , wiDetails = tfrScenariosNotTested r
      }


-- | Decide whether to retry or escalate.
--
-- Uses trajectory analysis (comparing previous vs current rubrics).
decideRetry
  :: [FunctionRubric]  -- ^ Previous attempt rubrics
  -> [FunctionRubric]  -- ^ Current attempt rubrics
  -> Int               -- ^ Current attempt number
  -> Maybe Blocker     -- ^ Current blocker (if any)
  -> RetryDecision
decideRetry prev curr attempt mBlocker
  -- Spec unclear → ask human (from blocker category)
  | Just b <- mBlocker, blCategory b == "spec-unclear" =
      AskHuman [blDescription b]

  -- Same open questions recurring → need human input
  | not (null recurring) =
      AskHuman recurring

  -- Progress being made (gaps shrinking) → retry with focus
  | gapsImproved prev curr =
      Retry (contextFromProgress prev curr)

  -- No progress after N attempts → escalate
  | attempt >= 3, not (gapsImproved prev curr) =
      Escalate "No progress after 3 attempts"

  -- Default: retry with context from current rubrics
  | otherwise =
      Retry (contextFromCurrent curr)
  where
    recurring = recurringQuestions prev curr


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Detect recurring questions (same uncertainty across attempts).
--
-- If the same questions keep appearing, the LLM is stuck and needs human input.
recurringQuestions :: [FunctionRubric] -> [FunctionRubric] -> [Text]
recurringQuestions prev curr =
  let prevQs = concatMap frOpenQuestions prev
      currQs = concatMap frOpenQuestions curr
  in intersect prevQs currQs

-- | Check if gaps are shrinking (progress metric).
gapsImproved :: [FunctionRubric] -> [FunctionRubric] -> Bool
gapsImproved prev curr
  | null prev = True   -- No previous, any progress is good
  | null curr = False  -- No current rubrics is bad
  | otherwise = totalGaps curr < totalGaps prev

-- | Extract retry context from progress.
contextFromProgress :: [FunctionRubric] -> [FunctionRubric] -> RetryContext
contextFromProgress _prev curr = RetryContext
  { rcFocusFunctions = stillHasGaps
  , rcApproachHints = approachesUsed
  , rcBoundariesToConsider = mentionedBoundaries
  }
  where
    stillHasGaps = map frFunctionName $ filter (not . null . frUnhandledCases) curr
    approachesUsed = map frApproach curr
    mentionedBoundaries = concatMap (map bnCase . frBoundaryReasoning) curr

-- | Extract retry context from current rubrics only.
contextFromCurrent :: [FunctionRubric] -> RetryContext
contextFromCurrent = contextFromProgress []

-- | Extract review items from uncertainties.
--
-- Routes specific questions to human, not whole task.
extractReviewItems :: [FunctionRubric] -> [ReviewItem]
extractReviewItems = map $ \r -> ReviewItem
  { riFunction = frFunctionName r
  , riQuestions = frOpenQuestions r
  , riApproach = frApproach r
  }
