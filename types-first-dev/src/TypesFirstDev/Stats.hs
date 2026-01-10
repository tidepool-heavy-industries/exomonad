{-# LANGUAGE RecordWildCards #-}

-- | Statistics capture from ClaudeCodeResult.
--
-- Extracts per-node execution statistics for run analysis.
module TypesFirstDev.Stats
  ( -- * Stats Types
    AgentStats(..)
  , RunMetadata(..)
  , ExperimentParams(..)
  , AggregateStats(..)

    -- * Stats Extraction
  , captureAgentStats
  , aggregateModelUsage
  , aggregateCosts

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

-- ClaudeCodeResult imported from Tidepool.Graph.Goto below
-- ModelUsage defined locally below since tidepool-core doesn't provide detailed stats
import Tidepool.Graph.Goto (ClaudeCodeResult(..))
import TypesFirstDev.Types (StackSpec)


-- | Model usage statistics (tokens, costs).
-- Locally defined since tidepool-core ClaudeCodeResult doesn't include metrics.
data ModelUsage = ModelUsage
  { muInputTokens :: Int
    -- ^ Input tokens consumed
  , muOutputTokens :: Int
    -- ^ Output tokens generated
  , muCost :: Double
    -- ^ Cost in USD
  }
  deriving stock (Show, Eq, Generic)


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
-- MVP: Placeholder implementation since tidepool-core ClaudeCodeResult
-- doesn't include detailed metrics yet. Returns default stats with nodeName.
captureAgentStats
  :: Text              -- ^ Node name
  -> Double            -- ^ Duration in seconds (measured externally)
  -> Int               -- ^ Retry count
  -> ClaudeCodeResult schema  -- ^ Result from Claude Code execution
  -> AgentStats
captureAgentStats nodeName duration retries _result =
  AgentStats
    { asNodeName = nodeName
    , asDurationSeconds = duration
    , asInputTokens = 0      -- TODO: Extract from ClaudeCodeResult when available
    , asOutputTokens = 0     -- TODO: Extract from ClaudeCodeResult when available
    , asCost = 0.0           -- TODO: Extract from ClaudeCodeResult when available
    , asRetries = retries
    , asSuccess = True       -- TODO: Track success status in ClaudeCodeResult
    , asErrorMessage = Nothing
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


-- ════════════════════════════════════════════════════════════════════════════
-- AGGREGATE STATS (for session chains)
-- ════════════════════════════════════════════════════════════════════════════

-- | Aggregate statistics from a session chain.
--
-- When using fork/resume, multiple sessions contribute to one logical operation.
-- This aggregates costs across all sessions in a chain.
data AggregateStats = AggregateStats
  { asTotalCost :: Double
    -- ^ Sum of costs from all sessions
  , asTotalInputTokens :: Int
    -- ^ Sum of input tokens from all sessions
  , asTotalOutputTokens :: Int
    -- ^ Sum of output tokens from all sessions
  , asSessionCount :: Int
    -- ^ Number of sessions in the chain
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AggregateStats where
  toJSON AggregateStats{..} = object
    [ "totalCost" .= asTotalCost
    , "totalInputTokens" .= asTotalInputTokens
    , "totalOutputTokens" .= asTotalOutputTokens
    , "sessionCount" .= asSessionCount
    ]

instance FromJSON AggregateStats where
  parseJSON = withObject "AggregateStats" $ \o -> AggregateStats
    <$> o .: "totalCost"
    <*> o .: "totalInputTokens"
    <*> o .: "totalOutputTokens"
    <*> o .: "sessionCount"


-- | Aggregate costs from multiple ClaudeCodeResults.
--
-- MVP: Placeholder since ClaudeCodeResult doesn't include cost metrics yet.
aggregateCosts :: [ClaudeCodeResult schema] -> AggregateStats
aggregateCosts results =
  AggregateStats
    { asTotalCost = 0.0      -- TODO: Extract from ClaudeCodeResult when available
    , asTotalInputTokens = 0 -- TODO: Extract from ClaudeCodeResult when available
    , asTotalOutputTokens = 0 -- TODO: Extract from ClaudeCodeResult when available
    , asSessionCount = length results
    }
