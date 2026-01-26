{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ExoMonad.Teaching.Types
  ( -- * Recording Infrastructure
    RecordingHandles(..)

    -- * Session Configuration
  , TeachingConfig(..)

    -- * Runtime Environment
  , TeachingEnv(..)

    -- * Captured Turn Data
  , TeachingTurn(..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import System.IO (Handle)


-- | File handles for dual-output recording
--
-- Each teaching session writes to two JSONL files:
-- - anthropic.jsonl: Full TeachingTurn records with node metadata
-- - gemma.jsonl: Reserved for FunctionGemma format conversion (not yet implemented)
data RecordingHandles = RecordingHandles
  { rhRawHandle :: Handle
    -- ^ Handle to anthropic.jsonl (full turn records)
  , rhGemmaHandle :: Handle
    -- ^ Handle to gemma.jsonl (reserved for FunctionGemma format)
  , rhSessionDir :: FilePath
    -- ^ Session directory path (.exomonad/training/session-{uuid}/)
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
    -- ^ Base directory for training data (.exomonad/training/)
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
