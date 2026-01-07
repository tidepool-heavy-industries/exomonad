{-# LANGUAGE RecordWildCards #-}

-- | Types for ClaudeCode execution results.
--
-- These types match the JSON output format from zellij-cc.
module Tidepool.ClaudeCode.Types
  ( -- * Result Types
    ClaudeCodeResult(..)

    -- * Stream Event Types
  , StreamEvent(..)
  , SystemEvent(..)
  , AssistantEvent(..)
  , AssistantMessage(..)
  , ContentBlock(..)
  , UserEvent(..)
  , UserMessage(..)
  , ResultEvent(..)
  , PermissionDenial(..)
  , ModelUsage(..)

    -- * Errors
  , ClaudeCodeError(..)
  ) where

import Data.Aeson
  ( FromJSON(..)
  , Object
  , Value(..)
  , withObject
  , (.:)
  , (.:?)
  , (.!=)
  )
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)


-- ============================================================================
-- Result Type (what zellij-cc returns)
-- ============================================================================

-- | Result from zellij-cc run command.
--
-- This matches the JSON output format from zellij-cc with stream-json support:
--
-- @
-- {
--   "exit_code": 0,
--   "is_error": false,
--   "result": "Analysis complete...",
--   "structured_output": { ... },
--   "session_id": "abc-123",
--   "total_cost_usd": 0.0042,
--   "num_turns": 3,
--   "events": [...],
--   "permission_denials": [...],
--   "model_usage": { "claude-sonnet-4": { ... } }
-- }
-- @
data ClaudeCodeResult = ClaudeCodeResult
  { ccrExitCode :: Int
    -- ^ Exit code from zellij-cc (always 0, check is_error for actual status)
  , ccrIsError :: Bool
    -- ^ Whether Claude Code reported an error
  , ccrResult :: Maybe Text
    -- ^ Prose result from Claude Code
  , ccrStructuredOutput :: Maybe Value
    -- ^ Structured output (when --json-schema was provided)
  , ccrSessionId :: Text
    -- ^ Session ID (always present, available from init event)
  , ccrTotalCostUsd :: Double
    -- ^ Total cost in USD
  , ccrNumTurns :: Int
    -- ^ Number of turns (tool use iterations)
  , ccrEvents :: [StreamEvent]
    -- ^ Full event stream for debugging/replay
  , ccrPermissionDenials :: [PermissionDenial]
    -- ^ Permission denials that occurred
  , ccrModelUsage :: Map Text ModelUsage
    -- ^ Per-model usage breakdown
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ClaudeCodeResult where
  parseJSON = withObject "ClaudeCodeResult" $ \o -> do
    ccrExitCode <- o .: "exit_code"
    ccrIsError <- o .: "is_error"
    ccrResult <- o .:? "result"
    ccrStructuredOutput <- o .:? "structured_output"
    ccrSessionId <- o .: "session_id"
    ccrTotalCostUsd <- o .: "total_cost_usd"
    ccrNumTurns <- o .: "num_turns"
    ccrEvents <- o .:? "events" .!= []
    ccrPermissionDenials <- o .:? "permission_denials" .!= []
    ccrModelUsage <- o .:? "model_usage" .!= mempty
    pure ClaudeCodeResult{..}


-- ============================================================================
-- Stream Event Types
-- ============================================================================

-- | A single event from Claude Code's stream-json output.
data StreamEvent
  = StreamSystem SystemEvent
  | StreamAssistant AssistantEvent
  | StreamUser UserEvent
  | StreamResult ResultEvent
  deriving stock (Show, Eq, Generic)

instance FromJSON StreamEvent where
  parseJSON = withObject "StreamEvent" $ \o -> do
    eventType <- o .: "type"
    case (eventType :: Text) of
      "system" -> StreamSystem <$> parseSystemEvent o
      "assistant" -> StreamAssistant <$> parseAssistantEvent o
      "user" -> StreamUser <$> parseUserEvent o
      "result" -> StreamResult <$> parseResultEvent o
      _ -> fail $ "Unknown stream event type: " <> show eventType

parseSystemEvent :: Object -> Parser SystemEvent
parseSystemEvent o = SystemEvent
  <$> o .: "subtype"
  <*> o .: "session_id"
  <*> o .:? "tools" .!= []
  <*> o .: "model"

parseAssistantEvent :: Object -> Parser AssistantEvent
parseAssistantEvent o = AssistantEvent <$> o .: "message"

parseUserEvent :: Object -> Parser UserEvent
parseUserEvent o = UserEvent
  <$> o .:? "tool_use_result"
  <*> o .:? "message"

parseResultEvent :: Object -> Parser ResultEvent
parseResultEvent o = ResultEvent
  <$> o .: "subtype"
  <*> o .: "is_error"
  <*> o .:? "result"
  <*> o .:? "session_id"
  <*> o .:? "total_cost_usd"
  <*> o .:? "num_turns"
  <*> o .:? "structured_output"
  <*> o .:? "permission_denials" .!= []
  <*> o .:? "model_usage" .!= mempty

data SystemEvent = SystemEvent
  { seSubtype :: Text
  , seSessionId :: Text
  , seTools :: [Text]
  , seModel :: Text
  }
  deriving stock (Show, Eq, Generic)

data AssistantEvent = AssistantEvent
  { aeMessage :: AssistantMessage
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON AssistantEvent where
  parseJSON = withObject "AssistantEvent" $ \o ->
    AssistantEvent <$> o .: "message"

data AssistantMessage = AssistantMessage
  { amContent :: [ContentBlock]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON AssistantMessage where
  parseJSON = withObject "AssistantMessage" $ \o ->
    AssistantMessage <$> o .:? "content" .!= []

data ContentBlock
  = TextBlock Text
  | ToolUseBlock
      { tubName :: Text
      , tubId :: Text
      , tubInput :: Value
      }
  | ToolResultBlock
      { trbToolUseId :: Text
      , trbContent :: Text
      , trbIsError :: Bool
      }
  deriving stock (Show, Eq, Generic)

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \o -> do
    blockType <- o .: "type"
    case (blockType :: Text) of
      "text" -> TextBlock <$> o .: "text"
      "tool_use" -> ToolUseBlock
        <$> o .: "name"
        <*> o .: "id"
        <*> o .: "input"
      "tool_result" -> ToolResultBlock
        <$> o .: "tool_use_id"
        <*> o .: "content"
        <*> o .:? "is_error" .!= False
      _ -> fail $ "Unknown content block type: " <> show blockType

data UserEvent = UserEvent
  { ueToolUseResult :: Maybe Text
  , ueMessage :: Maybe UserMessage
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON UserEvent where
  parseJSON = withObject "UserEvent" $ \o ->
    UserEvent
      <$> o .:? "tool_use_result"
      <*> o .:? "message"

data UserMessage = UserMessage
  { umContent :: [ContentBlock]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON UserMessage where
  parseJSON = withObject "UserMessage" $ \o ->
    UserMessage <$> o .:? "content" .!= []

data ResultEvent = ResultEvent
  { reSubtype :: Text
  , reIsError :: Bool
  , reResult :: Maybe Text
  , reSessionId :: Maybe Text
  , reTotalCostUsd :: Maybe Double
  , reNumTurns :: Maybe Int
  , reStructuredOutput :: Maybe Value
  , rePermissionDenials :: [PermissionDenial]
  , reModelUsage :: Map Text ModelUsage
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ResultEvent where
  parseJSON = withObject "ResultEvent" $ \o ->
    ResultEvent
      <$> o .: "subtype"
      <*> o .: "is_error"
      <*> o .:? "result"
      <*> o .:? "session_id"
      <*> o .:? "total_cost_usd"
      <*> o .:? "num_turns"
      <*> o .:? "structured_output"
      <*> o .:? "permission_denials" .!= []
      <*> o .:? "model_usage" .!= mempty

data PermissionDenial = PermissionDenial
  { pdToolName :: Text
  , pdToolUseId :: Text
  , pdToolInput :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PermissionDenial where
  parseJSON = withObject "PermissionDenial" $ \o -> PermissionDenial
    <$> o .: "tool_name"
    <*> o .: "tool_use_id"
    <*> o .:? "tool_input" .!= Null

data ModelUsage = ModelUsage
  { muInputTokens :: Int
  , muOutputTokens :: Int
  , muCacheReadInputTokens :: Int
  , muCacheCreationInputTokens :: Int
  , muCostUsd :: Double
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ModelUsage where
  parseJSON = withObject "ModelUsage" $ \o -> ModelUsage
    <$> o .:? "inputTokens" .!= 0
    <*> o .:? "outputTokens" .!= 0
    <*> o .:? "cacheReadInputTokens" .!= 0
    <*> o .:? "cacheCreationInputTokens" .!= 0
    <*> o .:? "costUSD" .!= 0


-- ============================================================================
-- Errors
-- ============================================================================

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
