{-# LANGUAGE RecordWildCards #-}

-- | Types for ClaudeCode execution results.
--
-- These types match the JSON output format from zellij-cc.
module Tidepool.ClaudeCode.Types
  ( -- * Result Types
    ClaudeCodeResult(..)

    -- * Errors
  , ClaudeCodeError(..)
  ) where

import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import Data.Text (Text)
import Data.Aeson (Value)
import GHC.Generics (Generic)


-- | Result from zellij-cc run command.
--
-- This matches the JSON output format from zellij-cc:
--
-- @
-- {
--   "exit_code": 0,
--   "output_file": "/tmp/cc-abc123.json",
--   "stderr_file": "/tmp/cc-abc123.stderr",
--   "is_error": false,
--   "result": "Analysis complete...",
--   "structured_output": { ... },
--   "cost_usd": 0.0042,
--   "num_turns": 3
-- }
-- @
data ClaudeCodeResult = ClaudeCodeResult
  { ccrExitCode :: Int
    -- ^ Exit code from zellij-cc (always 0, check is_error for actual status)
  , ccrOutputFile :: FilePath
    -- ^ Path to JSON output file
  , ccrStderrFile :: FilePath
    -- ^ Path to stderr file
  , ccrIsError :: Bool
    -- ^ Whether Claude Code reported an error
  , ccrResult :: Maybe Text
    -- ^ Prose result from Claude Code
  , ccrStructuredOutput :: Maybe Value
    -- ^ Structured output (when --json-schema was provided)
  , ccrCostUsd :: Maybe Double
    -- ^ Cost in USD
  , ccrNumTurns :: Maybe Int
    -- ^ Number of turns (tool use iterations)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ClaudeCodeResult where
  parseJSON = withObject "ClaudeCodeResult" $ \o -> do
    ccrExitCode <- o .: "exit_code"
    ccrOutputFile <- o .: "output_file"
    ccrStderrFile <- o .: "stderr_file"
    ccrIsError <- o .: "is_error"
    ccrResult <- o .:? "result"
    ccrStructuredOutput <- o .:? "structured_output"
    ccrCostUsd <- o .:? "cost_usd"
    ccrNumTurns <- o .:? "num_turns"
    pure ClaudeCodeResult{..}


-- | Errors that can occur during ClaudeCode execution.
data ClaudeCodeError
  = ClaudeCodeProcessError Text
    -- ^ zellij-cc process failed to start or exited with error
  | ClaudeCodeParseError Text
    -- ^ Failed to parse zellij-cc JSON output
  | ClaudeCodeExecutionError Text
    -- ^ Claude Code reported an error (is_error = true)
  | ClaudeCodeNoOutput
    -- ^ No structured output despite expecting one
  deriving stock (Show, Eq)
