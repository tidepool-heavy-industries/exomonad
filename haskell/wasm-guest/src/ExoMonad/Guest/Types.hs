-- | Shared types for MCP and hook handling.
module ExoMonad.Guest.Types
  ( -- * MCP Call types
    MCPCallInput (..),

    -- * Hook types
    HookInput (..),
    HookOutput (..),
    HookSpecificOutput (..),
    allowResponse,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value, object, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
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

-- | Input for a hook event.
data HookInput = HookInput
  { hiSessionId :: Text,
    hiHookEventName :: Text,
    hiToolName :: Maybe Text,
    hiToolInput :: Maybe Value
  }
  deriving (Show, Generic)

instance FromJSON HookInput where
  parseJSON = Aeson.withObject "HookInput" $ \v ->
    HookInput
      <$> v .: "session_id"
      <*> v .: "hook_event_name"
      <*> v .:? "tool_name"
      <*> v .:? "tool_input"

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
