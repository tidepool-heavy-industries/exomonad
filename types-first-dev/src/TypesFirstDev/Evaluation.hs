{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- | Evaluation rubrics and narrative capture for baseline comparison.
--
-- Provides structured evaluation of generated code across multiple dimensions,
-- combining automated checks with LLM-judged quality scores.
module TypesFirstDev.Evaluation
  ( -- * Rubric Types
    RunEvaluation(..)
  , AutomatedScores(..)
  , QualityScores(..)
  , RubricScore(..)

    -- * Narrative
  , RunNarrative(..)
  , PhaseNarrative(..)
  , AgentNarrative(..)

    -- * Evaluation
  , evaluateAutomated
  , emptyQualityScores

    -- * Reporting
  , renderEvaluationReport
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import TypesFirstDev.Types (StackSpec(..), ParallelResults(..), TestsResult(..), ImplResult(..))


-- ════════════════════════════════════════════════════════════════════════════
-- RUBRIC TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Complete evaluation of a single run.
data RunEvaluation = RunEvaluation
  { reRunId :: Text
    -- ^ Unique run identifier
  , reSpec :: StackSpec
    -- ^ Input specification
  , reAutomated :: AutomatedScores
    -- ^ Automatically computed scores
  , reQuality :: QualityScores
    -- ^ LLM-judged quality scores (may be empty initially)
  , reNarrative :: RunNarrative
    -- ^ Structured narrative of what happened
  }
  deriving (Generic, ToJSON, FromJSON, Show)


-- | Automated scores - computed directly from run results.
data AutomatedScores = AutomatedScores
  { asCompiles :: Bool
    -- ^ Did the final code compile?
  , asTestsPass :: Bool
    -- ^ Did tests pass post-merge?
  , asEndpointCount :: Int
    -- ^ Number of endpoints detected (target: 3 for url-shortener)
  , asTypesRetries :: Int
    -- ^ Retries needed in types phase
  , asImplRetries :: Int
    -- ^ Retries needed in impl phase
  , asTestsRetries :: Int
    -- ^ Retries needed in tests phase
  , asTotalTokens :: Int
    -- ^ Total tokens consumed across all agents
  , asDurationSeconds :: Double
    -- ^ Wall clock time for entire run
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)


-- | LLM-judged quality scores.
--
-- Each dimension is scored 1-5 with a written rationale.
-- These are filled in by a separate evaluation pass (human or LLM judge).
data QualityScores = QualityScores
  { qsSpecFidelity :: Maybe RubricScore
    -- ^ Does code match the spec description and acceptance criteria?
  , qsTypeDesign :: Maybe RubricScore
    -- ^ Quality of data types (newtypes, records, sum types)
  , qsTestQuality :: Maybe RubricScore
    -- ^ Coverage of acceptance criteria, property-based testing
  , qsImplQuality :: Maybe RubricScore
    -- ^ Clean implementation, no stubs, handles edge cases
  , qsCoherence :: Maybe RubricScore
    -- ^ Do types/tests/impl work together as unified design?
  , qsIdiomaticity :: Maybe RubricScore
    -- ^ Haskell best practices (applicative, proper errors, etc.)
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)


-- | A single rubric score with rationale.
data RubricScore = RubricScore
  { rsScore :: Int
    -- ^ 1-5 scale (1=poor, 3=adequate, 5=excellent)
  , rsRationale :: Text
    -- ^ Explanation for the score
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)


-- ════════════════════════════════════════════════════════════════════════════
-- NARRATIVE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Structured narrative capturing key decisions and outcomes.
data RunNarrative = RunNarrative
  { rnTypesPhase :: PhaseNarrative
    -- ^ What happened in types phase
  , rnImplAgent :: AgentNarrative
    -- ^ What the impl agent did
  , rnTestsAgent :: AgentNarrative
    -- ^ What the tests agent did
  , rnMergeOutcome :: Text
    -- ^ Summary of merge phase
  , rnOverallSummary :: Text
    -- ^ One-paragraph summary of the run
  }
  deriving (Generic, ToJSON, FromJSON, Show)


-- | Narrative for a workflow phase.
data PhaseNarrative = PhaseNarrative
  { pnDescription :: Text
    -- ^ What this phase did
  , pnKeyDecisions :: [Text]
    -- ^ Notable decisions made
  , pnArtifacts :: [Text]
    -- ^ What was produced (types, files, etc.)
  }
  deriving (Generic, ToJSON, FromJSON, Show)


-- | Narrative for an individual agent's work.
data AgentNarrative = AgentNarrative
  { anStrategy :: Text
    -- ^ High-level approach taken
  , anKeyDecisions :: [Text]
    -- ^ Notable implementation choices
  , anChallenges :: [Text]
    -- ^ Problems encountered (retries, errors)
  , anOutcome :: Text
    -- ^ Final result summary
  }
  deriving (Generic, ToJSON, FromJSON, Show)


-- ════════════════════════════════════════════════════════════════════════════
-- EVALUATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Compute automated scores from parallel results.
evaluateAutomated
  :: Double          -- ^ Duration in seconds
  -> Int             -- ^ Total tokens
  -> Int             -- ^ Endpoint count (from code analysis)
  -> ParallelResults
  -> AutomatedScores
evaluateAutomated duration tokens endpoints ParallelResults{..} = AutomatedScores
  { asCompiles = prImplResult.irBuildPassed && prTestsResult.trBuildPassed
  , asTestsPass = prTestsResult.trAllPropertiesWritten  -- Proxy for tests passing
  , asEndpointCount = endpoints
  , asTypesRetries = 0  -- TODO: capture from workflow
  , asImplRetries = 0   -- TODO: capture from workflow
  , asTestsRetries = 0  -- TODO: capture from workflow
  , asTotalTokens = tokens
  , asDurationSeconds = duration
  }


-- | Empty quality scores (to be filled by evaluation pass).
emptyQualityScores :: QualityScores
emptyQualityScores = QualityScores
  { qsSpecFidelity = Nothing
  , qsTypeDesign = Nothing
  , qsTestQuality = Nothing
  , qsImplQuality = Nothing
  , qsCoherence = Nothing
  , qsIdiomaticity = Nothing
  }


-- ════════════════════════════════════════════════════════════════════════════
-- REPORTING
-- ════════════════════════════════════════════════════════════════════════════

-- | Render a complete evaluation report as Markdown.
renderEvaluationReport :: RunEvaluation -> Text
renderEvaluationReport RunEvaluation{..} = T.unlines $
  [ "# Baseline Evaluation Report"
  , ""
  , "**Run ID:** " <> reRunId
  , ""
  , "**Module:** " <> reSpec.ssModuleName
  , ""
  , "---"
  , ""
  , "## Specification"
  , ""
  , reSpec.ssDescription
  , ""
  , "### Acceptance Criteria"
  , ""
  ] ++
  map ("- " <>) reSpec.ssAcceptanceCriteria ++
  [ ""
  , "---"
  , ""
  , "## Automated Scores"
  , ""
  , "| Metric | Value |"
  , "|--------|-------|"
  , "| Compiles | " <> showBool reAutomated.asCompiles <> " |"
  , "| Tests Pass | " <> showBool reAutomated.asTestsPass <> " |"
  , "| Endpoints | " <> T.pack (show reAutomated.asEndpointCount) <> "/3 |"
  , "| Duration | " <> formatDuration reAutomated.asDurationSeconds <> " |"
  , "| Tokens | " <> T.pack (show reAutomated.asTotalTokens) <> " |"
  , "| Retries (types/impl/tests) | "
      <> T.pack (show reAutomated.asTypesRetries) <> "/"
      <> T.pack (show reAutomated.asImplRetries) <> "/"
      <> T.pack (show reAutomated.asTestsRetries) <> " |"
  , ""
  , "---"
  , ""
  , "## Narrative"
  , ""
  , "### Types Phase"
  , ""
  , reNarrative.rnTypesPhase.pnDescription
  , ""
  , "**Key decisions:**"
  , ""
  ] ++
  map ("- " <>) reNarrative.rnTypesPhase.pnKeyDecisions ++
  [ ""
  , "### Implementation Agent"
  , ""
  , "**Strategy:** " <> reNarrative.rnImplAgent.anStrategy
  , ""
  , "**Key decisions:**"
  , ""
  ] ++
  map ("- " <>) reNarrative.rnImplAgent.anKeyDecisions ++
  [ ""
  , "**Outcome:** " <> reNarrative.rnImplAgent.anOutcome
  , ""
  , "### Tests Agent"
  , ""
  , "**Strategy:** " <> reNarrative.rnTestsAgent.anStrategy
  , ""
  , "**Key decisions:**"
  , ""
  ] ++
  map ("- " <>) reNarrative.rnTestsAgent.anKeyDecisions ++
  [ ""
  , "**Outcome:** " <> reNarrative.rnTestsAgent.anOutcome
  , ""
  , "### Merge Outcome"
  , ""
  , reNarrative.rnMergeOutcome
  , ""
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


-- | Render quality scores table.
renderQualityScores :: QualityScores -> [Text]
renderQualityScores QualityScores{..} =
  [ "| Dimension | Score | Rationale |"
  , "|-----------|-------|-----------|"
  , renderScoreRow "Spec Fidelity" qsSpecFidelity
  , renderScoreRow "Type Design" qsTypeDesign
  , renderScoreRow "Test Quality" qsTestQuality
  , renderScoreRow "Impl Quality" qsImplQuality
  , renderScoreRow "Coherence" qsCoherence
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
