{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Protocol types matching rust/exomonad-shared/src/protocol.rs
--
-- These types must serialize to JSON identically to the Rust types
-- for the NDJSON protocol to work correctly.
module ExoMonad.Control.Protocol
  ( -- * Control Messages (envelope)
    ControlMessage (..),
    ControlResponse (..),
    McpError (..),
    ErrorCode (..),
    ToolDefinition (..),
    Runtime (..),
    Role (..),

    -- * Hook Types
    HookInput (..),
    HookOutput (..),
    HookSpecificOutput (..),
    PermissionDecision (..),

    -- * Builder helpers
    allowPreToolUse,
    denyPreToolUse,
    allowPostToolUse,
    blockHook,
    hookSuccess,
    hookError,
    mcpToolError,
    mcpToolSuccess,
    mcpToolErrorWithDetails,

    -- * Dashboard Types
    AgentStatus (..),
    AgentsResponse (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, withText, (.!=), (.:), (.:?), (.=))
import Data.Text (Text)
import Deriving.Aeson
import ExoMonad.Control.RoleConfig (Role (..))
import GHC.Generics (Generic)

-- ============================================================================
-- Hook Input (from Claude Code via exomonad)
-- ============================================================================

-- | Hook event payload. Matches Rust HookInput exactly.
data HookInput = HookInput
  { sessionId :: Text,
    transcriptPath :: Text,
    cwd :: Text,
    permissionMode :: Text,
    hookEventName :: Text,
    -- Tool-related fields
    toolName :: Maybe Text,
    toolInput :: Maybe Value,
    toolUseId :: Maybe Text,
    toolResponse :: Maybe Value,
    -- Other hook-specific fields
    prompt :: Maybe Text,
    message :: Maybe Text,
    notificationType :: Maybe Text,
    stopHookActive :: Maybe Bool,
    trigger :: Maybe Text,
    customInstructions :: Maybe Text,
    source :: Maybe Text,
    reason :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[FieldLabelModifier CamelToSnake, OmitNothingFields] HookInput

-- ============================================================================
-- Hook Output (to Claude Code via exomonad)
-- ============================================================================

-- | Hook output. Matches Rust HookOutput.
-- Note: Rust uses camelCase field names (stopReason, hookSpecificOutput, etc.)
-- so we only strip the trailing underscore from continue_, no CamelToSnake.
data HookOutput = HookOutput
  { continue_ :: Bool,
    stopReason :: Maybe Text,
    suppressOutput :: Maybe Bool,
    systemMessage :: Maybe Text,
    hookSpecificOutput :: Maybe HookSpecificOutput,
    decision :: Maybe Text,
    reason :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[FieldLabelModifier '[StripSuffix "_"], OmitNothingFields] HookOutput

-- | Hook-specific output fields. Tagged by hookEventName.
data HookSpecificOutput
  = PreToolUseOutput
      { permissionDecision :: Text, -- "allow", "deny", "ask"
        permissionDecisionReason :: Maybe Text,
        updatedInput :: Maybe Value
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
      { stopDecision :: Maybe Text, -- "block" or Nothing (allow)
        stopReason :: Maybe Text -- Claude-facing guidance when blocked
      }
  | SubagentStopOutput
      { stopDecision :: Maybe Text, -- "block" or Nothing (allow)
        stopReason :: Maybe Text -- Claude-facing guidance when blocked
      }
  | NotificationOutput
  | PreCompactOutput
  | SessionEndOutput
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[SumTaggedObject "hookEventName" "contents", ConstructorTagModifier '[StripSuffix "Output"], OmitNothingFields] HookSpecificOutput

-- | Permission decision for PermissionRequest hook.
data PermissionDecision
  = Allow {updatedInput :: Maybe Value}
  | Deny {message :: Text, interrupt :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[SumTaggedObject "behavior" "contents", ConstructorTagModifier '[CamelToSnake], OmitNothingFields] PermissionDecision

-- ============================================================================
-- Control Socket Protocol
-- ============================================================================

-- | The runtime environment for the agent.
data Runtime = Claude | Gemini
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[ConstructorTagModifier '[CamelToSnake]] Runtime

-- | Tool definition for MCP discovery.
-- Field names use MCP protocol standard: name, description, inputSchema (camelCase)
data ToolDefinition = ToolDefinition
  { name :: Text,
    description :: Text,
    inputSchema :: Value
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[] ToolDefinition

-- | Message sent over Unix socket from exomonad. Tagged by "type".
-- Note: Rust uses snake_case (tool_name, container_id), so we apply CamelToSnake.
data ControlMessage
  = HookEvent {input :: HookInput, runtime :: Runtime, role :: Role, containerId :: Maybe Text}
  | MCPToolCall {id :: Text, toolName :: Text, arguments :: Value, containerId :: Maybe Text}
  | ToolsListRequest
  | Ping
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[SumTaggedObject "type" "contents", FieldLabelModifier CamelToSnake, OmitNothingFields] ControlMessage

-- | Response sent over Unix socket to exomonad. Tagged by "type".
-- Note: No OmitNothingFields here - MCP protocol expects explicit null for result/error.
data ControlResponse
  = HookResponse {output :: HookOutput, exitCode :: Int}
  | MCPToolResponse {id :: Text, result :: Maybe Value, error :: Maybe McpError}
  | ToolsListResponse {tools :: [ToolDefinition]}
  | Pong
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[SumTaggedObject "type" "contents", FieldLabelModifier CamelToSnake] ControlResponse

-- | MCP error codes for structured error responses.
data ErrorCode
  = InvalidRequest -- -32600: Invalid Request
  | NotFound -- -32001: Resource not found
  | InvalidInput -- -32002: Bad arguments (validation failed)
  | ExternalFailure -- -32003: Subprocess or I/O error
  | StateError -- -32004: Invalid state (already exists, blocked)
  | EnvironmentError -- -32005: Missing environment (not in Zellij)
  | PermissionDenied -- -32006: Role-based access denied
  deriving stock (Show, Eq, Generic)

-- | Get numeric code for ErrorCode.
errorCodeValue :: ErrorCode -> Int
errorCodeValue InvalidRequest = -32600
errorCodeValue NotFound = -32001
errorCodeValue InvalidInput = -32002
errorCodeValue ExternalFailure = -32003
errorCodeValue StateError = -32004
errorCodeValue EnvironmentError = -32005
errorCodeValue PermissionDenied = -32006

instance ToJSON ErrorCode where
  toJSON = toJSON . errorCodeValue

instance FromJSON ErrorCode where
  parseJSON v = do
    code <- parseJSON v
    case (code :: Int) of
      -32600 -> pure InvalidRequest
      -32001 -> pure NotFound
      -32002 -> pure InvalidInput
      -32003 -> pure ExternalFailure
      -32004 -> pure StateError
      -32005 -> pure EnvironmentError
      -32006 -> pure PermissionDenied
      _ -> fail $ "Unknown error code: " <> show code

-- | MCP error response with structured details.
data McpError = McpError
  { code :: ErrorCode,
    message :: Text, -- Human readable explanation
    details :: Maybe Value, -- Structured details for debugging (optional)
    suggestion :: Maybe Text -- Actionable guidance to fix the issue (optional)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[OmitNothingFields] McpError

-- ============================================================================
-- Builder helpers
-- ============================================================================

-- | Create default HookOutput (continue, no specific output)
defaultOutput :: HookOutput
defaultOutput =
  HookOutput
    { continue_ = True,
      stopReason = Nothing,
      suppressOutput = Nothing,
      systemMessage = Nothing,
      hookSpecificOutput = Nothing,
      decision = Nothing,
      reason = Nothing
    }

-- | Allow a PreToolUse hook with optional reason and modified input.
allowPreToolUse :: Maybe Text -> Maybe Value -> HookOutput
allowPreToolUse reason modifiedInput =
  defaultOutput
    { hookSpecificOutput = Just $ PreToolUseOutput "allow" reason modifiedInput
    }

-- | Deny a PreToolUse hook with reason.
denyPreToolUse :: Text -> HookOutput
denyPreToolUse reason =
  defaultOutput
    { hookSpecificOutput = Just $ PreToolUseOutput "deny" (Just reason) Nothing
    }

-- | Allow a PostToolUse hook with optional additional context.
allowPostToolUse :: Maybe Text -> HookOutput
allowPostToolUse ctx =
  defaultOutput
    { hookSpecificOutput = Just $ PostToolUseOutput ctx
    }

-- | Block processing with a reason.
blockHook :: Text -> HookOutput
blockHook reason =
  defaultOutput
    { continue_ = False,
      stopReason = Just reason
    }

-- | Create a successful hook response.
hookSuccess :: HookOutput -> ControlResponse
hookSuccess o = HookResponse o 0

-- | Create an error hook response (exit code runtime-aware).
hookError :: Runtime -> Text -> ControlResponse
hookError r msg = HookResponse (blockHook msg) (runtimeExitCode r)

-- | Get exit code for blocking based on runtime.
runtimeExitCode :: Runtime -> Int
runtimeExitCode Claude = 1
runtimeExitCode Gemini = 2

-- | Create an MCP tool error response with minimal details.
mcpToolError :: Text -> ErrorCode -> Text -> ControlResponse
mcpToolError reqId code msg = MCPToolResponse reqId Nothing (Just $ McpError code msg Nothing Nothing)

-- | Create an MCP tool error response with full details.
mcpToolErrorWithDetails :: Text -> ErrorCode -> Text -> Maybe Value -> Maybe Text -> ControlResponse
mcpToolErrorWithDetails reqId code msg details suggestion =
  MCPToolResponse reqId Nothing (Just $ McpError code msg details suggestion)

-- | Create a successful MCP tool response.
mcpToolSuccess :: Text -> Value -> ControlResponse
mcpToolSuccess reqId result = MCPToolResponse reqId (Just result) Nothing

-- ============================================================================
-- Dashboard Types
-- ============================================================================

-- | Agent status information for the dashboard.
data AgentStatus = AgentStatus
  { asId :: Text,
    asContainerId :: Text,
    asIssueNumber :: Maybe Int,
    asStatus :: Text,
    asStartedAt :: Text,
    asLastActivity :: Maybe Text,
    asLastAction :: Maybe Text,
    asBlocker :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "as", CamelToSnake], OmitNothingFields] AgentStatus

-- | Response for /api/agents endpoint.
data AgentsResponse = AgentsResponse
  { agents :: [AgentStatus]
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[OmitNothingFields] AgentsResponse
