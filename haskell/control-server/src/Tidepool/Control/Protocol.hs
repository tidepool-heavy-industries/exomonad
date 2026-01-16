{-# LANGUAGE DuplicateRecordFields #-}
-- | Protocol types matching rust/mantle-shared/src/protocol.rs
--
-- These types must serialize to JSON identically to the Rust types
-- for the NDJSON protocol to work correctly.
module Tidepool.Control.Protocol
  ( -- * Control Messages (TCP envelope)
    ControlMessage(..)
  , ControlResponse(..)
  , McpError(..)

    -- * Hook Types
  , HookInput(..)
  , HookOutput(..)
  , HookSpecificOutput(..)
  , PermissionDecision(..)

    -- * Builder helpers
  , allowPreToolUse
  , denyPreToolUse
  , allowPostToolUse
  , blockHook
  , hookSuccess
  , hookError
  , mcpToolError
  , mcpToolSuccess
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)

-- ============================================================================
-- Hook Input (from Claude Code via mantle-agent)
-- ============================================================================

-- | Hook event payload. Matches Rust HookInput exactly.
data HookInput = HookInput
  { sessionId :: Text
  , transcriptPath :: Text
  , cwd :: Text
  , permissionMode :: Text
  , hookEventName :: Text
  -- Tool-related fields
  , toolName :: Maybe Text
  , toolInput :: Maybe Value
  , toolUseId :: Maybe Text
  , toolResponse :: Maybe Value
  -- Other hook-specific fields
  , prompt :: Maybe Text
  , message :: Maybe Text
  , notificationType :: Maybe Text
  , stopHookActive :: Maybe Bool
  , trigger :: Maybe Text
  , customInstructions :: Maybe Text
  , source :: Maybe Text
  , reason :: Maybe Text
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
    <*> o .:? "custom_instructions"
    <*> o .:? "source"
    <*> o .:? "reason"

instance ToJSON HookInput where
  toJSON i = object $ filter notNull
    [ "session_id" .= i.sessionId
    , "transcript_path" .= i.transcriptPath
    , "cwd" .= i.cwd
    , "permission_mode" .= i.permissionMode
    , "hook_event_name" .= i.hookEventName
    , "tool_name" .= i.toolName
    , "tool_input" .= i.toolInput
    , "tool_use_id" .= i.toolUseId
    , "tool_response" .= i.toolResponse
    , "prompt" .= i.prompt
    , "message" .= i.message
    , "notification_type" .= i.notificationType
    , "stop_hook_active" .= i.stopHookActive
    , "trigger" .= i.trigger
    , "custom_instructions" .= i.customInstructions
    , "source" .= i.source
    , "reason" .= i.reason
    ]
    where
      notNull (_, Null) = False
      notNull _ = True

-- ============================================================================
-- Hook Output (to Claude Code via mantle-agent)
-- ============================================================================

-- | Hook output. Matches Rust HookOutput.
data HookOutput = HookOutput
  { continue_ :: Bool
  , stopReason :: Maybe Text
  , suppressOutput :: Maybe Bool
  , systemMessage :: Maybe Text
  , hookSpecificOutput :: Maybe HookSpecificOutput
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON HookOutput where
  toJSON o = object $ filter notNull
    [ "continue" .= o.continue_
    , "stopReason" .= o.stopReason
    , "suppressOutput" .= o.suppressOutput
    , "systemMessage" .= o.systemMessage
    , "hookSpecificOutput" .= o.hookSpecificOutput
    ]
    where
      notNull (_, Null) = False
      notNull _ = True

instance FromJSON HookOutput where
  parseJSON = withObject "HookOutput" $ \o -> HookOutput
    <$> o .:? "continue" .!= True
    <*> o .:? "stopReason"
    <*> o .:? "suppressOutput"
    <*> o .:? "systemMessage"
    <*> o .:? "hookSpecificOutput"

-- | Hook-specific output fields. Tagged by hookEventName.
data HookSpecificOutput
  = PreToolUseOutput
      { permissionDecision :: Text  -- "allow", "deny", "ask"
      , permissionDecisionReason :: Maybe Text
      , updatedInput :: Maybe Value
      }
  | PostToolUseOutput
      { additionalContext :: Maybe Text
      }
  | UserPromptSubmitOutput
      { additionalContext :: Maybe Text
      }
  | SessionStartOutput
      { additionalContext :: Maybe Text
      }
  | PermissionRequestOutput
      { decision :: PermissionDecision
      }
  | StopOutput
  | SubagentStopOutput
  | NotificationOutput
  | PreCompactOutput
  | SessionEndOutput
  deriving stock (Show, Eq, Generic)

instance ToJSON HookSpecificOutput where
  toJSON = \case
    PreToolUseOutput pd pdr ui -> object $ filter notNull
      [ "hookEventName" .= ("PreToolUse" :: Text)
      , "permissionDecision" .= pd
      , "permissionDecisionReason" .= pdr
      , "updatedInput" .= ui
      ]
    PostToolUseOutput ctx -> object $ filter notNull
      [ "hookEventName" .= ("PostToolUse" :: Text)
      , "additionalContext" .= ctx
      ]
    UserPromptSubmitOutput ctx -> object $ filter notNull
      [ "hookEventName" .= ("UserPromptSubmit" :: Text)
      , "additionalContext" .= ctx
      ]
    SessionStartOutput ctx -> object $ filter notNull
      [ "hookEventName" .= ("SessionStart" :: Text)
      , "additionalContext" .= ctx
      ]
    PermissionRequestOutput dec -> object
      [ "hookEventName" .= ("PermissionRequest" :: Text)
      , "decision" .= dec
      ]
    StopOutput -> object ["hookEventName" .= ("Stop" :: Text)]
    SubagentStopOutput -> object ["hookEventName" .= ("SubagentStop" :: Text)]
    NotificationOutput -> object ["hookEventName" .= ("Notification" :: Text)]
    PreCompactOutput -> object ["hookEventName" .= ("PreCompact" :: Text)]
    SessionEndOutput -> object ["hookEventName" .= ("SessionEnd" :: Text)]
    where
      notNull (_, Null) = False
      notNull _ = True

instance FromJSON HookSpecificOutput where
  parseJSON = withObject "HookSpecificOutput" $ \o -> do
    eventName <- o .: "hookEventName" :: Parser Text
    case eventName of
      "PreToolUse" -> PreToolUseOutput
        <$> o .: "permissionDecision"
        <*> o .:? "permissionDecisionReason"
        <*> o .:? "updatedInput"
      "PostToolUse" -> PostToolUseOutput <$> o .:? "additionalContext"
      "UserPromptSubmit" -> UserPromptSubmitOutput <$> o .:? "additionalContext"
      "SessionStart" -> SessionStartOutput <$> o .:? "additionalContext"
      "PermissionRequest" -> PermissionRequestOutput <$> o .: "decision"
      "Stop" -> pure StopOutput
      "SubagentStop" -> pure SubagentStopOutput
      "Notification" -> pure NotificationOutput
      "PreCompact" -> pure PreCompactOutput
      "SessionEnd" -> pure SessionEndOutput
      _ -> fail $ "Unknown hookEventName: " <> show eventName

-- | Permission decision for PermissionRequest hook.
data PermissionDecision
  = Allow { updatedInput :: Maybe Value }
  | Deny { denyMessage :: Text, interrupt :: Bool }
  deriving stock (Show, Eq, Generic)

instance ToJSON PermissionDecision where
  toJSON (Allow ui) = object $ filter notNull
    [ "behavior" .= ("allow" :: Text)
    , "updatedInput" .= ui
    ]
    where
      notNull (_, Null) = False
      notNull _ = True
  toJSON (Deny msg int) = object
    [ "behavior" .= ("deny" :: Text)
    , "message" .= msg
    , "interrupt" .= int
    ]

instance FromJSON PermissionDecision where
  parseJSON = withObject "PermissionDecision" $ \o -> do
    behavior <- o .: "behavior" :: Parser Text
    case behavior of
      "allow" -> Allow <$> o .:? "updatedInput"
      "deny" -> Deny <$> o .: "message" <*> o .:? "interrupt" .!= False
      _ -> fail $ "Unknown behavior: " <> show behavior

-- ============================================================================
-- Control Socket Protocol
-- ============================================================================

-- | Message sent over TCP from mantle-agent. Tagged by "type".
data ControlMessage
  = HookEvent { input :: HookInput }
  | McpToolCall { mcpId :: Text, toolName :: Text, arguments :: Value }
  deriving stock (Show, Eq, Generic)

instance FromJSON ControlMessage where
  parseJSON = withObject "ControlMessage" $ \o -> do
    msgType <- o .: "type" :: Parser Text
    case msgType of
      "HookEvent" -> HookEvent <$> o .: "input"
      "MCPToolCall" -> McpToolCall
        <$> o .: "id"
        <*> o .: "tool_name"
        <*> o .: "arguments"
      _ -> fail $ "Unknown message type: " <> show msgType

instance ToJSON ControlMessage where
  toJSON (HookEvent i) = object
    [ "type" .= ("HookEvent" :: Text)
    , "input" .= i
    ]
  toJSON (McpToolCall mid tn args) = object
    [ "type" .= ("MCPToolCall" :: Text)
    , "id" .= mid
    , "tool_name" .= tn
    , "arguments" .= args
    ]

-- | Response sent over TCP to mantle-agent. Tagged by "type".
data ControlResponse
  = HookResponse { output :: HookOutput, exitCode :: Int }
  | McpToolResponse { mcpId :: Text, result :: Maybe Value, mcpError :: Maybe McpError }
  deriving stock (Show, Eq, Generic)

instance ToJSON ControlResponse where
  toJSON (HookResponse o ec) = object
    [ "type" .= ("HookResponse" :: Text)
    , "output" .= o
    , "exit_code" .= ec
    ]
  toJSON (McpToolResponse mid res err) = object
    [ "type" .= ("MCPToolResponse" :: Text)
    , "id" .= mid
    , "result" .= res
    , "error" .= err
    ]

instance FromJSON ControlResponse where
  parseJSON = withObject "ControlResponse" $ \o -> do
    msgType <- o .: "type" :: Parser Text
    case msgType of
      "HookResponse" -> HookResponse <$> o .: "output" <*> o .: "exit_code"
      "MCPToolResponse" -> McpToolResponse
        <$> o .: "id"
        <*> o .:? "result"
        <*> o .:? "error"
      _ -> fail $ "Unknown response type: " <> show msgType

-- | MCP error response.
data McpError = McpError
  { code :: Int
  , errorMessage :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON McpError where
  toJSON e = object
    [ "code" .= e.code
    , "message" .= e.errorMessage
    ]

instance FromJSON McpError where
  parseJSON = withObject "McpError" $ \o -> McpError
    <$> o .: "code"
    <*> o .: "message"

-- ============================================================================
-- Builder helpers
-- ============================================================================

-- | Create default HookOutput (continue, no specific output)
defaultOutput :: HookOutput
defaultOutput = HookOutput
  { continue_ = True
  , stopReason = Nothing
  , suppressOutput = Nothing
  , systemMessage = Nothing
  , hookSpecificOutput = Nothing
  }

-- | Allow a PreToolUse hook with optional reason and modified input.
allowPreToolUse :: Maybe Text -> Maybe Value -> HookOutput
allowPreToolUse reason modifiedInput = defaultOutput
  { hookSpecificOutput = Just $ PreToolUseOutput "allow" reason modifiedInput
  }

-- | Deny a PreToolUse hook with reason.
denyPreToolUse :: Text -> HookOutput
denyPreToolUse reason = defaultOutput
  { hookSpecificOutput = Just $ PreToolUseOutput "deny" (Just reason) Nothing
  }

-- | Allow a PostToolUse hook with optional additional context.
allowPostToolUse :: Maybe Text -> HookOutput
allowPostToolUse ctx = defaultOutput
  { hookSpecificOutput = Just $ PostToolUseOutput ctx
  }

-- | Block processing with a reason.
blockHook :: Text -> HookOutput
blockHook reason = defaultOutput
  { continue_ = False
  , stopReason = Just reason
  }

-- | Create a successful hook response.
hookSuccess :: HookOutput -> ControlResponse
hookSuccess o = HookResponse o 0

-- | Create an error hook response (exit code 2).
hookError :: Text -> ControlResponse
hookError msg = HookResponse (blockHook msg) 2

-- | Create an MCP tool error response.
mcpToolError :: Text -> Text -> ControlResponse
mcpToolError reqId msg = McpToolResponse reqId Nothing (Just $ McpError (-32603) msg)

-- | Create a successful MCP tool response.
mcpToolSuccess :: Text -> Value -> ControlResponse
mcpToolSuccess reqId result = McpToolResponse reqId (Just result) Nothing
