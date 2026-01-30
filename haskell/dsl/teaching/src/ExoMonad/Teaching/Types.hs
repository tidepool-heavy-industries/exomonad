{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ExoMonad.Teaching.Types
  ( -- * Recording Infrastructure
    RecordingHandles (..),

    -- * Session Configuration
    TeachingConfig (..),

    -- * Runtime Environment
    TeachingEnv (..),

    -- * Captured Turn Data
    TeachingTurn (..),
  )
where

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
  { -- | Handle to anthropic.jsonl (full turn records)
    rhRawHandle :: Handle,
    -- | Handle to gemma.jsonl (reserved for FunctionGemma format)
    rhGemmaHandle :: Handle,
    -- | Session directory path (.exomonad/training/session-{uuid}/)
    rhSessionDir :: FilePath
  }

-- ════════════════════════════════════════════════════════════════════════════
-- SESSION CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Global teaching session configuration.
--
-- Controls whether teaching mode is active and where to write training data.
-- Set at server startup, not per-request.
data TeachingConfig = TeachingConfig
  { -- | Master switch: True = call Haiku and record, False = normal production
    tcEnabled :: Bool,
    -- | Base directory for training data (.exomonad/training/)
    tcOutputDir :: FilePath,
    -- | Unique identifier for this teaching session
    tcSessionId :: UUID,
    -- | Anthropic API key for Haiku calls
    tcAnthropicKey :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- RUNTIME ENVIRONMENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Runtime environment for teaching mode.
--
-- Created at session start, threaded through teaching interpreter.
-- Contains everything needed to call Haiku and record training data.
data TeachingEnv = TeachingEnv
  { -- | Session configuration
    teConfig :: TeachingConfig,
    -- | Open file handles for recording
    teHandles :: RecordingHandles,
    -- | Teacher guidance to prepend to system prompts
    teGuidance :: Text
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
  { -- | Name of the node (e.g., "gClassify")
    ttNodeName :: Text,
    -- | Name of the containing graph (e.g., "SupportGraph")
    ttGraphName :: Text,
    -- | System prompt sent to Haiku
    ttSystemPrompt :: Text,
    -- | User content blocks (as JSON)
    ttUserContent :: Value,
    -- | Structured output schema
    ttOutputSchema :: Value,
    -- | Tool definitions provided
    ttToolDefs :: [Value],
    -- | Raw Haiku response (full JSON)
    ttResponse :: Value,
    -- | When this turn was executed
    ttTimestamp :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
