{-# LANGUAGE RecordWildCards #-}

-- | Types for ClaudeCode execution results and control envelope.
--
-- These types match the JSON output format from zellij-cc.
module Tidepool.ClaudeCode.Types
  ( -- * Result Types
    ClaudeCodeResult(..)

    -- * Interrupt Signals
  , InterruptSignal(..)

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

    -- * Control Envelope Types
  , ControlMessage(..)
  , ControlResponse(..)
  , HookInput(..)
  , HookOutput(..)
  , HookSpecificOutput(..)
  , PermissionDecision(..)
  , HookDecision(..)
  , defaultHookOutput
  , preToolUseAllow
  , preToolUseDeny
  , postToolUseOutput
  , hookDecisionToOutput

    -- * Errors
  , ClaudeCodeError(..)
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Object
  , Value(..)
  , withObject
  , object
  , (.:)
  , (.:?)
  , (.!=)
  , (.=)
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
    -- ^ Exit code from Claude process (reflects actual process exit status; see ccrIsError for Claude Code-level errors)
  , ccrIsError :: Bool
    -- ^ Whether Claude Code reported an error
  , ccrResult :: Maybe Text
    -- ^ Prose result from Claude Code
  , ccrStructuredOutput :: Maybe Value
    -- ^ Structured output (when --json-schema was provided)
  , ccrSessionId :: Text
    -- ^ Session ID (from init event; may be "unknown" if parsing failed)
  , ccrSessionTag :: Maybe Text
    -- ^ Tag for correlating with orchestrator state (e.g., worktree name)
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
  , ccrInterrupts :: [InterruptSignal]
    -- ^ Interrupt signals received during execution
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ClaudeCodeResult where
  parseJSON = withObject "ClaudeCodeResult" $ \o -> do
    ccrExitCode <- o .: "exit_code"
    ccrIsError <- o .: "is_error"
    ccrResult <- o .:? "result"
    ccrStructuredOutput <- o .:? "structured_output"
    ccrSessionId <- o .: "session_id"
    ccrSessionTag <- o .:? "session_tag"
    ccrTotalCostUsd <- o .: "total_cost_usd"
    ccrNumTurns <- o .: "num_turns"
    ccrEvents <- o .:? "events" .!= []
    ccrPermissionDenials <- o .:? "permission_denials" .!= []
    ccrModelUsage <- o .:? "model_usage" .!= mempty
    ccrInterrupts <- o .:? "interrupts" .!= []
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
-- Interrupt Signals
-- ============================================================================

-- | An interrupt signal sent by Claude via @zellij-cc signal@.
--
-- These allow Claude to signal non-happy-path transitions in the workflow,
-- such as requesting more context or escalating to a human.
data InterruptSignal = InterruptSignal
  { isSignalType :: Text
    -- ^ Signal type: "transition", "escalate", "request_review", etc.
  , isState :: Maybe Text
    -- ^ Target state for transitions (e.g., "need_more_types")
  , isReason :: Maybe Text
    -- ^ Human-readable reason for the signal
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON InterruptSignal where
  parseJSON = withObject "InterruptSignal" $ \o -> InterruptSignal
    <$> o .: "signal_type"
    <*> o .:? "state"
    <*> o .:? "reason"


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


-- ============================================================================
-- Control Envelope Types
-- ============================================================================

-- | Message received over the control socket from zellij-cc.
data ControlMessage
  = HookEvent { cmInput :: HookInput }
  | McpToolCall
      { cmId :: Text
      , cmToolName :: Text
      , cmArguments :: Value
      }
  deriving stock (Show, Eq, Generic)

instance FromJSON ControlMessage where
  parseJSON = withObject "ControlMessage" $ \o -> do
    msgType <- o .: "type"
    case (msgType :: Text) of
      "HookEvent" -> HookEvent <$> o .: "input"
      "MCPToolCall" -> McpToolCall
        <$> o .: "id"
        <*> o .: "tool_name"
        <*> o .: "arguments"
      _ -> fail $ "Unknown control message type: " <> show msgType

-- | Response to send back over the control socket.
data ControlResponse
  = HookResponse
      { crOutput :: HookOutput
      , crExitCode :: Int
      }
  | McpToolResponse
      { crId :: Text
      , crResult :: Maybe Value
      , crError :: Maybe Text
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON ControlResponse where
  toJSON (HookResponse output exitCode) = object
    [ "type" .= ("HookResponse" :: Text)
    , "output" .= output
    , "exit_code" .= exitCode
    ]
  toJSON (McpToolResponse rid result err) = object
    [ "type" .= ("MCPToolResponse" :: Text)
    , "id" .= rid
    , "result" .= result
    , "error" .= err
    ]

-- | Hook input from Claude Code (matches CC's hook stdin JSON).
data HookInput = HookInput
  { hiSessionId :: Text
  , hiTranscriptPath :: Text
  , hiCwd :: Text
  , hiPermissionMode :: Text
  , hiHookEventName :: Text
  -- Tool-related fields
  , hiToolName :: Maybe Text
  , hiToolInput :: Maybe Value
  , hiToolUseId :: Maybe Text
  , hiToolResponse :: Maybe Value
  -- Other hook-specific fields
  , hiPrompt :: Maybe Text
  , hiMessage :: Maybe Text
  , hiNotificationType :: Maybe Text
  , hiStopHookActive :: Maybe Bool
  , hiTrigger :: Maybe Text
  , hiSource :: Maybe Text
  , hiReason :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON HookInput where
  parseJSON = withObject "HookInput" $ \o -> HookInput
    <$> o .: "session_id"
    <*> o .:? "transcript_path" .!= ""
    <*> o .:? "cwd" .!= ""
    <*> o .:? "permission_mode" .!= "default"
    <*> o .: "hook_event_name"
    <*> o .:? "tool_name"
    <*> o .:? "tool_input"
    <*> o .:? "tool_use_id"
    <*> o .:? "tool_response"
    <*> o .:? "prompt"
    <*> o .:? "message"
    <*> o .:? "notification_type"
    <*> o .:? "stop_hook_active"
    <*> o .:? "trigger"
    <*> o .:? "source"
    <*> o .:? "reason"

-- | Hook output to Claude Code (matches CC's expected stdout JSON).
data HookOutput = HookOutput
  { hoContinue :: Bool
  , hoStopReason :: Maybe Text
  , hoSuppressOutput :: Maybe Bool
  , hoSystemMessage :: Maybe Text
  , hoHookSpecificOutput :: Maybe HookSpecificOutput
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON HookOutput where
  toJSON ho = object $
    [ "continue" .= hoContinue ho
    ]
    ++ maybe [] (\v -> ["stopReason" .= v]) (hoStopReason ho)
    ++ maybe [] (\v -> ["suppressOutput" .= v]) (hoSuppressOutput ho)
    ++ maybe [] (\v -> ["systemMessage" .= v]) (hoSystemMessage ho)
    ++ maybe [] (\v -> ["hookSpecificOutput" .= v]) (hoHookSpecificOutput ho)

-- | Hook-specific output fields.
data HookSpecificOutput
  = PreToolUseOutput
      { ptuPermissionDecision :: Text  -- "allow" | "deny" | "ask"
      , ptuPermissionDecisionReason :: Maybe Text
      , ptuUpdatedInput :: Maybe Value
      }
  | PostToolUseOutput
      { ptoAdditionalContext :: Maybe Text
      }
  | PermissionRequestOutput
      { proDecision :: PermissionDecision
      }
  | OtherHookOutput
      { ohoEventName :: Text
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON HookSpecificOutput where
  toJSON (PreToolUseOutput decision reason updated) = object $
    [ "hookEventName" .= ("PreToolUse" :: Text)
    , "permissionDecision" .= decision
    ]
    ++ maybe [] (\v -> ["permissionDecisionReason" .= v]) reason
    ++ maybe [] (\v -> ["updatedInput" .= v]) updated
  toJSON (PostToolUseOutput ctx) = object $
    [ "hookEventName" .= ("PostToolUse" :: Text)
    ]
    ++ maybe [] (\v -> ["additionalContext" .= v]) ctx
  toJSON (PermissionRequestOutput decision) = object
    [ "hookEventName" .= ("PermissionRequest" :: Text)
    , "decision" .= decision
    ]
  toJSON (OtherHookOutput name) = object
    [ "hookEventName" .= name
    ]

-- | Permission decision for PermissionRequest hook.
data PermissionDecision
  = PermissionAllow { paUpdatedInput :: Maybe Value }
  | PermissionDeny { pdMessage :: Text, pdInterrupt :: Bool }
  deriving stock (Show, Eq, Generic)

instance ToJSON PermissionDecision where
  toJSON (PermissionAllow updated) = object $
    [ "behavior" .= ("allow" :: Text)
    ]
    ++ maybe [] (\v -> ["updatedInput" .= v]) updated
  toJSON (PermissionDeny msg interrupt) = object
    [ "behavior" .= ("deny" :: Text)
    , "message" .= msg
    , "interrupt" .= interrupt
    ]

-- | Hook decision returned by callbacks.
--
-- This is what Haskell handlers return - converted to HookOutput for CC.
data HookDecision
  = HookAllow
    -- ^ Allow the hook to proceed
  | HookDeny Text
    -- ^ Deny with reason (for PreToolUse/PermissionRequest)
  | HookModify Value
    -- ^ Modify tool input (for PreToolUse/PermissionRequest)
  | HookBlock Text
    -- ^ Block and stop processing
  deriving stock (Show, Eq)

-- | Default "allow" HookOutput.
defaultHookOutput :: HookOutput
defaultHookOutput = HookOutput
  { hoContinue = True
  , hoStopReason = Nothing
  , hoSuppressOutput = Nothing
  , hoSystemMessage = Nothing
  , hoHookSpecificOutput = Nothing
  }

-- | Create HookOutput for PreToolUse allow.
preToolUseAllow :: Maybe Text -> Maybe Value -> HookOutput
preToolUseAllow reason modified = defaultHookOutput
  { hoHookSpecificOutput = Just $ PreToolUseOutput "allow" reason modified
  }

-- | Create HookOutput for PreToolUse deny.
preToolUseDeny :: Text -> HookOutput
preToolUseDeny reason = defaultHookOutput
  { hoHookSpecificOutput = Just $ PreToolUseOutput "deny" (Just reason) Nothing
  }

-- | Create HookOutput for PostToolUse.
postToolUseOutput :: Maybe Text -> HookOutput
postToolUseOutput ctx = defaultHookOutput
  { hoHookSpecificOutput = Just $ PostToolUseOutput ctx
  }

-- | Convert HookDecision to HookOutput for a given event.
hookDecisionToOutput :: Text -> HookDecision -> HookOutput
hookDecisionToOutput eventName decision = case decision of
  HookAllow -> case eventName of
    "PreToolUse" -> preToolUseAllow Nothing Nothing
    "PostToolUse" -> postToolUseOutput Nothing
    "PermissionRequest" -> defaultHookOutput
      { hoHookSpecificOutput = Just $ PermissionRequestOutput $ PermissionAllow Nothing }
    _ -> defaultHookOutput
  HookDeny reason -> case eventName of
    "PreToolUse" -> preToolUseDeny reason
    "PermissionRequest" -> defaultHookOutput
      { hoHookSpecificOutput = Just $ PermissionRequestOutput $ PermissionDeny reason False }
    _ -> defaultHookOutput { hoContinue = False, hoStopReason = Just reason }
  HookModify newInput -> case eventName of
    "PreToolUse" -> preToolUseAllow Nothing (Just newInput)
    "PermissionRequest" -> defaultHookOutput
      { hoHookSpecificOutput = Just $ PermissionRequestOutput $ PermissionAllow (Just newInput) }
    _ -> defaultHookOutput  -- Modify doesn't apply to other hooks
  HookBlock reason -> defaultHookOutput
    { hoContinue = False
    , hoStopReason = Just reason
    }
