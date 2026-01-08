{-# LANGUAGE RecordWildCards #-}

-- | Statistics capture from ClaudeCodeResult.
--
-- Extracts per-node execution statistics for run analysis.
module TypesFirstDev.Stats
  ( -- * Stats Types
    AgentStats(..)
  , RunMetadata(..)
  , ExperimentParams(..)

    -- * Stats Extraction
  , captureAgentStats
  , aggregateModelUsage

    -- * Time Utilities
  , formatTimestamp
  , generateRunId
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), (.:?), (.!=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import GHC.Generics (Generic)

import Tidepool.ClaudeCode.Types (ClaudeCodeResult(..), ModelUsage(..))
import TypesFirstDev.Types (StackSpec)


-- | Statistics for a single agent node execution.
data AgentStats = AgentStats
  { asNodeName :: Text
    -- ^ Node name (e.g., "types", "tests", "impl")
  , asDurationSeconds :: Double
    -- ^ Wall clock execution time (placeholder - not in ClaudeCodeResult)
  , asInputTokens :: Int
    -- ^ Total input tokens across all models
  , asOutputTokens :: Int
    -- ^ Total output tokens across all models
  , asCost :: Double
    -- ^ Total cost in USD
  , asRetries :: Int
    -- ^ Number of retry attempts
  , asSuccess :: Bool
    -- ^ Whether the node completed successfully
  , asErrorMessage :: Maybe Text
    -- ^ Error message if failed
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AgentStats where
  toJSON AgentStats{..} = object
    [ "nodeName" .= asNodeName
    , "durationSeconds" .= asDurationSeconds
    , "inputTokens" .= asInputTokens
    , "outputTokens" .= asOutputTokens
    , "cost" .= asCost
    , "retries" .= asRetries
    , "success" .= asSuccess
    , "errorMessage" .= asErrorMessage
    ]

instance FromJSON AgentStats where
  parseJSON = withObject "AgentStats" $ \o -> AgentStats
    <$> o .: "nodeName"
    <*> o .: "durationSeconds"
    <*> o .: "inputTokens"
    <*> o .: "outputTokens"
    <*> o .: "cost"
    <*> o .: "retries"
    <*> o .: "success"
    <*> o .:? "errorMessage"


-- | Experiment parameters for non-baseline runs.
data ExperimentParams = ExperimentParams
  { epName :: Text
    -- ^ Experiment name (e.g., "session-threading", "hooks")
  , epDescription :: Text
    -- ^ Human-readable description of what changed
  , epBranch :: Text
    -- ^ Git branch name for the experiment
  , epChanges :: [Text]
    -- ^ List of specific modifications made
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ExperimentParams where
  toJSON ExperimentParams{..} = object
    [ "name" .= epName
    , "description" .= epDescription
    , "branch" .= epBranch
    , "changes" .= epChanges
    ]

instance FromJSON ExperimentParams where
  parseJSON = withObject "ExperimentParams" $ \o -> ExperimentParams
    <$> o .: "name"
    <*> o .: "description"
    <*> o .: "branch"
    <*> o .: "changes"


-- | Complete metadata for a workflow run.
data RunMetadata = RunMetadata
  { rmRunId :: Text
    -- ^ Unique run identifier (timestamp-based)
  , rmTimestamp :: UTCTime
    -- ^ When the run started
  , rmDurationSeconds :: Double
    -- ^ Total wall clock time
  , rmSuccess :: Bool
    -- ^ Whether the workflow completed successfully
  , rmSpec :: StackSpec
    -- ^ Input specification
  , rmAgentStats :: Map Text AgentStats
    -- ^ Per-node statistics keyed by node name
  , rmTotalCost :: Double
    -- ^ Sum of all API costs
  , rmTotalTokens :: Int
    -- ^ Sum of all input + output tokens
  , rmExperiment :: Maybe ExperimentParams
    -- ^ Null for baseline runs
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RunMetadata where
  toJSON RunMetadata{..} = object
    [ "runId" .= rmRunId
    , "timestamp" .= rmTimestamp
    , "durationSeconds" .= rmDurationSeconds
    , "success" .= rmSuccess
    , "spec" .= rmSpec
    , "agentStats" .= rmAgentStats
    , "totalCost" .= rmTotalCost
    , "totalTokens" .= rmTotalTokens
    , "experiment" .= rmExperiment
    ]

instance FromJSON RunMetadata where
  parseJSON = withObject "RunMetadata" $ \o -> RunMetadata
    <$> o .: "runId"
    <*> o .: "timestamp"
    <*> o .: "durationSeconds"
    <*> o .: "success"
    <*> o .: "spec"
    <*> o .:? "agentStats" .!= Map.empty
    <*> o .: "totalCost"
    <*> o .: "totalTokens"
    <*> o .:? "experiment"


-- | Capture statistics from a ClaudeCodeResult.
--
-- Extracts token counts, cost, and error status from the result.
captureAgentStats
  :: Text              -- ^ Node name
  -> Double            -- ^ Duration in seconds (measured externally)
  -> Int               -- ^ Retry count
  -> ClaudeCodeResult  -- ^ Result from Claude Code execution
  -> AgentStats
captureAgentStats nodeName duration retries result =
  let (inputToks, outputToks) = aggregateModelUsage result.ccrModelUsage
      errMsg = if result.ccrIsError
               then result.ccrResult
               else Nothing
  in AgentStats
    { asNodeName = nodeName
    , asDurationSeconds = duration
    , asInputTokens = inputToks
    , asOutputTokens = outputToks
    , asCost = result.ccrTotalCostUsd
    , asRetries = retries
    , asSuccess = not result.ccrIsError
    , asErrorMessage = errMsg
    }


-- | Aggregate token usage across all models.
--
-- Returns (totalInputTokens, totalOutputTokens).
aggregateModelUsage :: Map Text ModelUsage -> (Int, Int)
aggregateModelUsage usageMap =
  let usages = Map.elems usageMap
      inputToks = sum $ map muInputTokens usages
      outputToks = sum $ map muOutputTokens usages
  in (inputToks, outputToks)


-- | Format a timestamp for display/file naming.
formatTimestamp :: UTCTime -> String
formatTimestamp = formatTime defaultTimeLocale "%Y%m%dT%H%M%S"


-- | Generate a unique run ID from a timestamp.
generateRunId :: UTCTime -> Text
generateRunId t = "run-" <> T.pack (formatTimestamp t)
