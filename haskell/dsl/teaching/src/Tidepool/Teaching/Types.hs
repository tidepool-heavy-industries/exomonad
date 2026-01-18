{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tidepool.Teaching.Types
  ( -- * Training Data
    TrainingExample(..)
  , RecordingHandles(..)
  , AnthropicApiKey(..)

    -- * Session Configuration
  , TeachingConfig(..)

    -- * Runtime Environment
  , TeachingEnv(..)

    -- * Captured Turn Data
  , TeachingTurn(..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import System.IO (Handle)

-- | A training example pair: raw Anthropic response + converted FunctionGemma format
--
-- This is the core unit of teaching data. Each example contains:
-- - The raw Anthropic Messages API response (for offline iteration)
-- - The converted FunctionGemma JSONL line (ready for training)
-- - Optional teacher guidance that was provided to Haiku
-- - Metadata for tracking and debugging
data TrainingExample = TrainingExample
  { teAnthropicRaw :: Value
    -- ^ Raw Anthropic Messages API response JSON
  , teFunctionGemmaFormatted :: Text
    -- ^ Converted FunctionGemma JSONL line (complete conversation turn)
  , teTeacherGuidance :: Maybe Text
    -- ^ Domain-specific guidance that was provided to Haiku
  , teTimestamp :: UTCTime
    -- ^ When this example was recorded
  , teToolName :: Text
    -- ^ Name of the tool that was invoked
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Anthropic API key (type-safe wrapper)
newtype AnthropicApiKey = AnthropicApiKey { unAnthropicApiKey :: Text }
  deriving newtype (Show, Eq, IsString)

-- | File handles for dual-output recording
--
-- Each teaching session writes to two JSONL files:
-- - anthropic.jsonl: Raw Anthropic Messages API responses
-- - gemma.jsonl: Converted FunctionGemma training data
data RecordingHandles = RecordingHandles
  { rhRawHandle :: Handle
    -- ^ Handle to anthropic.jsonl (raw responses)
  , rhGemmaHandle :: Handle
    -- ^ Handle to gemma.jsonl (converted training data)
  , rhSessionDir :: FilePath
    -- ^ Session directory path (.tidepool/training/session-{uuid}/)
  }


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Global teaching session configuration.
--
-- Controls whether teaching mode is active and where to write training data.
-- Set at server startup, not per-request.
data TeachingConfig = TeachingConfig
  { tcEnabled :: Bool
    -- ^ Master switch: True = call Haiku and record, False = normal production
  , tcOutputDir :: FilePath
    -- ^ Base directory for training data (.tidepool/training/)
  , tcSessionId :: UUID
    -- ^ Unique identifier for this teaching session
  , tcAnthropicKey :: Text
    -- ^ Anthropic API key for Haiku calls
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- RUNTIME ENVIRONMENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Runtime environment for teaching mode.
--
-- Created at session start, threaded through teaching interpreter.
-- Contains everything needed to call Haiku and record training data.
data TeachingEnv = TeachingEnv
  { teConfig :: TeachingConfig
    -- ^ Session configuration
  , teHandles :: RecordingHandles
    -- ^ Open file handles for recording
  , teGuidance :: Text
    -- ^ Teacher guidance to prepend to system prompts
  }


-- ════════════════════════════════════════════════════════════════════════════
-- CAPTURED TURN DATA
-- ════════════════════════════════════════════════════════════════════════════

-- | A captured LLM turn for training data generation.
--
-- Contains everything needed to generate training examples:
-- - Node context (which node in which graph)
-- - Full request (system prompt, user content, tools)
-- - Full response (reasoning, tool calls, structured output)
--
-- This is captured at the LLM effect level, not individual tool level.
data TeachingTurn = TeachingTurn
  { ttNodeName :: Text
    -- ^ Name of the node (e.g., "gClassify")
  , ttGraphName :: Text
    -- ^ Name of the containing graph (e.g., "SupportGraph")
  , ttSystemPrompt :: Text
    -- ^ System prompt sent to Haiku
  , ttUserContent :: Value
    -- ^ User content blocks (as JSON)
  , ttOutputSchema :: Value
    -- ^ Structured output schema
  , ttToolDefs :: [Value]
    -- ^ Tool definitions provided
  , ttResponse :: Value
    -- ^ Raw Haiku response (full JSON)
  , ttTimestamp :: UTCTime
    -- ^ When this turn was executed
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
