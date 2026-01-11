{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

-- | Evaluation rubrics and narrative capture for V3 TDD runs.
--
-- Provides structured evaluation of runs for sleeptime agent evolution.
-- Combines automated checks with LLM-judged quality scores.
--
-- Design: Two-pass evaluation
-- 1. Automated pass (during run) - captures mechanical metrics
-- 2. Quality pass (post-run) - LLM judge scores quality dimensions
module TypesFirstDev.V3.Evaluation
  ( -- * Run Evaluation
    RunEvaluation(..)
  , AutomatedScores(..)
  , QualityScores(..)
  , RubricScore(..)

    -- * Narrative Capture
  , RunNarrative(..)
  , NodeNarrative(..)

    -- * Evaluation Helpers
  , evaluateAutomated
  , emptyQualityScores

    -- * Reporting
  , renderEvaluationReport
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import TypesFirstDev.V3.Types.Core (Spec(..), Criterion(..))
import TypesFirstDev.V3.Policy (MechanicalChecks(..))


-- ════════════════════════════════════════════════════════════════════════════
-- RUN EVALUATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Complete evaluation of a V3 TDD run.
--
-- Prefix: re
data RunEvaluation = RunEvaluation
  { reRunId     :: Text           -- ^ Unique run identifier
  , reSpec      :: Spec           -- ^ Input specification
  , reAutomated :: AutomatedScores -- ^ Automatically computed scores
  , reQuality   :: QualityScores  -- ^ LLM-judged quality (filled post-run)
  , reNarrative :: RunNarrative   -- ^ Structured narrative of what happened
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


-- | Automated scores computed directly from run metrics.
--
-- Prefix: as
data AutomatedScores = AutomatedScores
  { asCompiles        :: Bool    -- ^ Did final code compile?
  , asTestsPass       :: Bool    -- ^ Did tests pass?
  , asHasUndefined    :: Bool    -- ^ Any undefined stubs remaining?
  , asCriteriaCount   :: Int     -- ^ Number of acceptance criteria
  , asCriteriaCovered :: Int     -- ^ Criteria with passing tests
  , asScaffoldRetries :: Int     -- ^ Retries in scaffold phase
  , asTddRetries      :: Int     -- ^ Retries in TDD phase (test writing)
  , asImplRetries     :: Int     -- ^ Retries in impl phase
  , asReviewRetries   :: Int     -- ^ Retries in TDD review phase
  , asTotalTokens     :: Int     -- ^ Total tokens consumed
  , asDurationSeconds :: Double  -- ^ Wall clock time
  , asChildCount      :: Int     -- ^ Number of child graphs spawned
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


-- | LLM-judged quality scores.
--
-- Each dimension scored 1-5 with written rationale.
-- Filled by separate evaluation pass (human or LLM judge).
--
-- Prefix: qs
data QualityScores = QualityScores
  { qsSpecFidelity    :: Maybe RubricScore  -- ^ Does code match spec?
  , qsInterfaceDesign :: Maybe RubricScore  -- ^ Quality of interface (types, exports)
  , qsTestQuality     :: Maybe RubricScore  -- ^ Coverage, property-based testing
  , qsImplQuality     :: Maybe RubricScore  -- ^ Clean impl, handles edge cases
  , qsDecomposition   :: Maybe RubricScore  -- ^ Appropriate use of children
  , qsIdiomaticity    :: Maybe RubricScore  -- ^ Haskell best practices
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


-- | A single rubric score with rationale.
--
-- Prefix: rs
data RubricScore = RubricScore
  { rsScore     :: Int   -- ^ 1-5 scale (1=poor, 3=adequate, 5=excellent)
  , rsRationale :: Text  -- ^ Explanation for the score
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- NARRATIVE CAPTURE
-- ════════════════════════════════════════════════════════════════════════════

-- | Structured narrative of a V3 TDD run.
--
-- Captures what happened at each phase for sleeptime analysis.
--
-- Prefix: rn
data RunNarrative = RunNarrative
  { rnScaffold       :: NodeNarrative  -- ^ Scaffold phase
  , rnTddWriteTests  :: NodeNarrative  -- ^ TDD test writing
  , rnImpl           :: NodeNarrative  -- ^ Implementation phase
  , rnTddReview      :: NodeNarrative  -- ^ TDD review phase
  , rnMerger         :: NodeNarrative  -- ^ Merge phase
  , rnChildSummaries :: [Text]         -- ^ Brief summaries of child runs
  , rnOverallSummary :: Text           -- ^ One-paragraph run summary
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


-- | Narrative for a single node execution.
--
-- Prefix: nn
data NodeNarrative = NodeNarrative
  { nnNodeName     :: Text    -- ^ Which node (e.g., "v3Impl")
  , nnDescription  :: Text    -- ^ What this execution did
  , nnKeyDecisions :: [Text]  -- ^ Notable decisions made
  , nnChallenges   :: [Text]  -- ^ Problems encountered
  , nnArtifacts    :: [Text]  -- ^ What was produced (files, commits)
  , nnOutcome      :: Text    -- ^ Final result summary
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- EVALUATION HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Compute automated scores from mechanical checks and run metrics.
evaluateAutomated
  :: Double           -- ^ Duration in seconds
  -> Int              -- ^ Total tokens
  -> Int              -- ^ Criteria count
  -> Int              -- ^ Criteria covered
  -> Int              -- ^ Child count
  -> (Int, Int, Int, Int)  -- ^ (scaffold, tdd, impl, review) retries
  -> MechanicalChecks
  -> AutomatedScores
evaluateAutomated duration tokens criteriaCount criteriaCovered childCount
    (scaffoldRetries, tddRetries, implRetries, reviewRetries) mech =
  AutomatedScores
    { asCompiles = mcBuildPassed mech
    , asTestsPass = mcTestsPassed mech
    , asHasUndefined = mcHasUndefined mech
    , asCriteriaCount = criteriaCount
    , asCriteriaCovered = criteriaCovered
    , asScaffoldRetries = scaffoldRetries
    , asTddRetries = tddRetries
    , asImplRetries = implRetries
    , asReviewRetries = reviewRetries
    , asTotalTokens = tokens
    , asDurationSeconds = duration
    , asChildCount = childCount
    }


-- | Empty quality scores (filled by post-run evaluation).
emptyQualityScores :: QualityScores
emptyQualityScores = QualityScores
  { qsSpecFidelity = Nothing
  , qsInterfaceDesign = Nothing
  , qsTestQuality = Nothing
  , qsImplQuality = Nothing
  , qsDecomposition = Nothing
  , qsIdiomaticity = Nothing
  }


-- ════════════════════════════════════════════════════════════════════════════
-- REPORTING
-- ════════════════════════════════════════════════════════════════════════════

-- | Render evaluation report as Markdown.
renderEvaluationReport :: RunEvaluation -> Text
renderEvaluationReport RunEvaluation{..} = T.unlines $
  [ "# V3 TDD Run Evaluation"
  , ""
  , "**Run ID:** " <> reRunId
  , "**Description:** " <> reSpec.sDescription
  , ""
  , "---"
  , ""
  , "## Acceptance Criteria"
  , ""
  ] ++
  zipWith renderCriterion [1..] (map (.cText) reSpec.sAcceptanceCriteria) ++
  [ ""
  , "---"
  , ""
  , "## Automated Scores"
  , ""
  , "| Metric | Value |"
  , "|--------|-------|"
  , "| Compiles | " <> showBool reAutomated.asCompiles <> " |"
  , "| Tests Pass | " <> showBool reAutomated.asTestsPass <> " |"
  , "| Has Undefined | " <> showBool reAutomated.asHasUndefined <> " |"
  , "| Criteria | " <> T.pack (show reAutomated.asCriteriaCovered)
      <> "/" <> T.pack (show reAutomated.asCriteriaCount) <> " |"
  , "| Children | " <> T.pack (show reAutomated.asChildCount) <> " |"
  , "| Duration | " <> formatDuration reAutomated.asDurationSeconds <> " |"
  , "| Tokens | " <> T.pack (show reAutomated.asTotalTokens) <> " |"
  , "| Retries (scaffold/tdd/impl/review) | "
      <> T.pack (show reAutomated.asScaffoldRetries) <> "/"
      <> T.pack (show reAutomated.asTddRetries) <> "/"
      <> T.pack (show reAutomated.asImplRetries) <> "/"
      <> T.pack (show reAutomated.asReviewRetries) <> " |"
  , ""
  , "---"
  , ""
  , "## Phase Narratives"
  , ""
  ] ++
  renderNodeNarrative "Scaffold" reNarrative.rnScaffold ++
  renderNodeNarrative "TDD WriteTests" reNarrative.rnTddWriteTests ++
  renderNodeNarrative "Implementation" reNarrative.rnImpl ++
  renderNodeNarrative "TDD Review" reNarrative.rnTddReview ++
  renderNodeNarrative "Merger" reNarrative.rnMerger ++
  [ ""
  , "---"
  , ""
  , "## Quality Scores"
  , ""
  ] ++
  renderQualityScores reQuality ++
  [ ""
  , "---"
  , ""
  , "## Summary"
  , ""
  , reNarrative.rnOverallSummary
  ]
  where
    renderCriterion :: Int -> Text -> Text
    renderCriterion n t = T.pack (show n) <> ". " <> t


renderNodeNarrative :: Text -> NodeNarrative -> [Text]
renderNodeNarrative phase NodeNarrative{..} =
  [ "### " <> phase
  , ""
  , nnDescription
  , ""
  ] ++
  (if null nnKeyDecisions then [] else
    ["**Key decisions:**", ""] ++ map ("- " <>) nnKeyDecisions ++ [""]) ++
  (if null nnChallenges then [] else
    ["**Challenges:**", ""] ++ map ("- " <>) nnChallenges ++ [""]) ++
  ["**Outcome:** " <> nnOutcome, ""]


renderQualityScores :: QualityScores -> [Text]
renderQualityScores QualityScores{..} =
  [ "| Dimension | Score | Rationale |"
  , "|-----------|-------|-----------|"
  , renderScoreRow "Spec Fidelity" qsSpecFidelity
  , renderScoreRow "Interface Design" qsInterfaceDesign
  , renderScoreRow "Test Quality" qsTestQuality
  , renderScoreRow "Impl Quality" qsImplQuality
  , renderScoreRow "Decomposition" qsDecomposition
  , renderScoreRow "Idiomaticity" qsIdiomaticity
  ]


renderScoreRow :: Text -> Maybe RubricScore -> Text
renderScoreRow name Nothing =
  "| " <> name <> " | - | _Not yet evaluated_ |"
renderScoreRow name (Just RubricScore{..}) =
  "| " <> name <> " | " <> T.pack (show rsScore) <> "/5 | " <> rsRationale <> " |"


showBool :: Bool -> Text
showBool True = "Yes"
showBool False = "**No**"


formatDuration :: Double -> Text
formatDuration secs
  | secs < 60 = T.pack (show (round secs :: Int)) <> "s"
  | otherwise =
      let mins = floor (secs / 60) :: Int
          s = round (secs - fromIntegral mins * 60) :: Int
      in T.pack (show mins) <> "m " <> T.pack (show s) <> "s"
