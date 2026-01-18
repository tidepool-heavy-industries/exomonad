{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tidepool.Teaching.Types
  ( TrainingExample(..)
  , TeachingConfig(..)
  , RecordingHandles(..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
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

-- | Global teaching configuration
--
-- Threading this through the system enables all-or-nothing teaching mode:
-- when tcEnabled=True, all ToolDef executions go through Haiku and are recorded.
data TeachingConfig = TeachingConfig
  { tcEnabled :: Bool
    -- ^ Master switch: True = teaching mode (Haiku), False = production (FunctionGemma)
  , tcOutputDir :: FilePath
    -- ^ Base directory for recording sessions (e.g., ".tidepool/training")
  , tcSessionId :: UUID
    -- ^ Unique identifier for this teaching session
  , tcAnthropicKey :: Text
    -- ^ Anthropic API key for Haiku calls
  } deriving (Show, Eq)

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
