-- | Shared types for MCP and hook handling.
module ExoMonad.Guest.Types
  ( -- * MCP Call types
    MCPCallInput (..),

    -- * Hook types
    HookInput (..),
    HookOutput (..),
    HookSpecificOutput (..),
    HookEventType (..),
    StopHookOutput (..),
    StopDecision (..),
    allowResponse,
    denyResponse,
    allowStopResponse,
    blockStopResponse,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value, object, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- ============================================================================
-- MCP Call Types
-- ============================================================================

-- | Input for an MCP tool call.
data MCPCallInput = MCPCallInput
  { toolName :: Text,
    toolArgs :: Value
  }
  deriving (Show, Generic)

instance FromJSON MCPCallInput where
  parseJSON = Aeson.withObject "MCPCallInput" $ \v ->
    MCPCallInput
      <$> v .: "toolName"
      <*> v .: "toolArgs"

-- ============================================================================
-- Hook Types (matches Rust protocol.rs)
-- ============================================================================

-- | Hook event type (internal abstractions only).
-- Rust normalizes CLI-specific types (Claude Stop, Gemini AfterAgent) to these.
data HookEventType = SessionEnd | Stop | SubagentStop | PreToolUse
  deriving (Show, Eq, Generic)

instance ToJSON HookEventType where
  toJSON SessionEnd = Aeson.String "SessionEnd"
  toJSON Stop = Aeson.String "Stop"
  toJSON SubagentStop = Aeson.String "SubagentStop"
  toJSON PreToolUse = Aeson.String "PreToolUse"

instance FromJSON HookEventType where
  parseJSON = Aeson.withText "HookEventType" $ \case
    "SessionEnd" -> pure SessionEnd
    "Stop" -> pure Stop
    "SubagentStop" -> pure SubagentStop
    "PreToolUse" -> pure PreToolUse
    other -> fail $ "Unknown hook event type: " <> T.unpack other

-- | Input for a hook event.
-- Ref: https://geminicli.com/docs/hooks/reference/#afteragent
data HookInput = HookInput
  { hiSessionId :: Text,
    hiHookEventName :: HookEventType,
    hiToolName :: Maybe Text,
    hiToolInput :: Maybe Value,
    hiStopHookActive :: Maybe Bool,
    hiPrompt :: Maybe Text,
    hiPromptResponse :: Maybe Text,
    hiTimestamp :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON HookInput where
  parseJSON = Aeson.withObject "HookInput" $ \v ->
    HookInput
      <$> v .: "session_id"
      <*> v .: "hook_event_name"
      <*> v .:? "tool_name"
      <*> v .:? "tool_input"
      <*> v .:? "stop_hook_active"
      <*> v .:? "prompt"
      <*> v .:? "prompt_response"
      <*> v .:? "timestamp"

-- | Output from a hook handler.
data HookOutput = HookOutput
  { continue_ :: Bool,
    stopReason :: Maybe Text,
    suppressOutput :: Maybe Bool,
    systemMessage :: Maybe Text,
    hookSpecificOutput :: Maybe HookSpecificOutput
  }
  deriving (Show, Generic)

instance ToJSON HookOutput where
  toJSON h =
    Aeson.object $
      filter
        ((/= Aeson.Null) . snd)
        [ "continue" .= continue_ h,
          "stopReason" .= stopReason h,
          "suppressOutput" .= suppressOutput h,
          "systemMessage" .= systemMessage h,
          "hookSpecificOutput" .= hookSpecificOutput h
        ]

-- | Hook-specific output (varies by hook type).
data HookSpecificOutput
  = PreToolUseOutput
  { permissionDecision :: Text,
    permissionDecisionReason :: Maybe Text,
    updatedInput :: Maybe Value
  }
  deriving (Show, Generic)

instance ToJSON HookSpecificOutput where
  toJSON (PreToolUseOutput decision reason updated) =
    Aeson.object $
      filter
        ((/= Aeson.Null) . snd)
        [ "hookEventName" .= ("PreToolUse" :: Text),
          "permissionDecision" .= decision,
          "permissionDecisionReason" .= reason,
          "updatedInput" .= updated
        ]

-- | Create an "allow" response for PreToolUse hooks.
allowResponse :: Maybe Text -> HookOutput
allowResponse reason =
  HookOutput
    { continue_ = True,
      stopReason = Nothing,
      suppressOutput = Nothing,
      systemMessage = Nothing,
      hookSpecificOutput =
        Just $
          PreToolUseOutput
            { permissionDecision = "allow",
              permissionDecisionReason = reason,
              updatedInput = Nothing
            }
    }

-- | Create a "deny" response for PreToolUse hooks.
denyResponse :: Text -> HookOutput
denyResponse msg =
  HookOutput
    { continue_ = False,
      stopReason = Just msg,
      suppressOutput = Nothing,
      systemMessage = Nothing,
      hookSpecificOutput =
        Just $
          PreToolUseOutput
            { permissionDecision = "deny",
              permissionDecisionReason = Just msg,
              updatedInput = Nothing
            }
    }

-- ============================================================================
-- Stop Hook Types (SessionEnd, SubagentStop)
-- ============================================================================

-- | Stop hook decision (strongly typed).
-- Serialized to "allow" or "block" at the JSON boundary.
data StopDecision = Allow | Block
  deriving (Show, Eq, Generic)

instance ToJSON StopDecision where
  toJSON Allow = Aeson.String "allow"
  toJSON Block = Aeson.String "block"

-- | Output for Stop hooks (SessionEnd, SubagentStop).
-- Uses Claude Code's HookOutput format: {"continue": bool, "stopReason": "..."}
data StopHookOutput = StopHookOutput
  { decision :: StopDecision,
    reason :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON StopHookOutput where
  toJSON s =
    -- Output internal domain format. Rust edge translates to Claude/Gemini format.
    -- Domain format: {"decision": "allow"/"block", "reason": "..."}
    let decisionVal = case decision s of
          Allow -> "allow" :: Text
          Block -> "block"
     in case reason s of
          Nothing -> object ["decision" .= decisionVal]
          Just r -> object ["decision" .= decisionVal, "reason" .= r]

-- | Create an "allow" response for Stop hooks.
allowStopResponse :: StopHookOutput
allowStopResponse =
  StopHookOutput
    { decision = Allow,
      reason = Nothing
    }

-- | Create a "block" response for Stop hooks with additional prompting.
blockStopResponse :: Text -> StopHookOutput
blockStopResponse msg =
  StopHookOutput
    { decision = Block,
      reason = Just msg
    }
