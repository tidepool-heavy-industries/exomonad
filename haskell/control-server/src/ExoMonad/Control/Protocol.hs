{-# LANGUAGE DuplicateRecordFields #-}
-- | Protocol types matching rust/exomonad-shared/src/protocol.rs
--
-- These types must serialize to JSON identically to the Rust types
-- for the NDJSON protocol to work correctly.
module ExoMonad.Control.Protocol
  ( -- * Control Messages (envelope)
    ControlMessage(..)
  , ControlResponse(..)
  , McpError(..)
  , ErrorCode(..)
  , ToolDefinition(..)
  , Runtime(..)
  , Role(..)

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
  , mcpToolErrorWithDetails
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.Control.RoleConfig (Role(..))

-- ============================================================================
-- Hook Input (from Claude Code via exomonad)
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
-- Hook Output (to Claude Code via exomonad)
-- ============================================================================

-- | Hook output. Matches Rust HookOutput.
data HookOutput = HookOutput
  { continue_ :: Bool
  , stopReason :: Maybe Text
  , suppressOutput :: Maybe Bool
  , systemMessage :: Maybe Text
  , hookSpecificOutput :: Maybe HookSpecificOutput
  , decision :: Maybe Text
  , reason :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON HookOutput where
  toJSON o = object $ filter notNull
    [ "continue" .= o.continue_
    , "stopReason" .= o.stopReason
    , "suppressOutput" .= o.suppressOutput
    , "systemMessage" .= o.systemMessage
    , "hookSpecificOutput" .= o.hookSpecificOutput
    , "decision" .= o.decision
    , "reason" .= o.reason
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
    <*> o .:? "decision"
    <*> o .:? "reason"

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
      { stopDecision :: Maybe Text  -- "block" or Nothing (allow)
      , stopReason :: Maybe Text    -- Claude-facing guidance when blocked
      }
  | SubagentStopOutput
      { stopDecision :: Maybe Text  -- "block" or Nothing (allow)
      , stopReason :: Maybe Text    -- Claude-facing guidance when blocked
      }
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
    StopOutput dec rsn -> object $ filter notNull
      [ "hookEventName" .= ("Stop" :: Text)
      , "decision" .= dec
      , "reason" .= rsn
      ]
    SubagentStopOutput dec rsn -> object $ filter notNull
      [ "hookEventName" .= ("SubagentStop" :: Text)
      , "decision" .= dec
      , "reason" .= rsn
      ]
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
      "Stop" -> StopOutput <$> o .:? "decision" <*> o .:? "reason"
      "SubagentStop" -> SubagentStopOutput <$> o .:? "decision" <*> o .:? "reason"
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

-- | The runtime environment for the agent.
data Runtime = Claude | Gemini
  deriving stock (Show, Eq, Generic)

instance FromJSON Runtime where
  parseJSON = withText "Runtime" $ \case
    "claude" -> pure Claude
    "gemini" -> pure Gemini
    r -> fail $ "Unknown runtime: " <> show r

instance ToJSON Runtime where
  toJSON Claude = String "claude"
  toJSON Gemini = String "gemini"

-- | Tool definition for MCP discovery.
-- Field names use MCP protocol standard: name, description, inputSchema
data ToolDefinition = ToolDefinition
  { tdName :: Text
  , tdDescription :: Text
  , tdInputSchema :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ToolDefinition where
  parseJSON = withObject "ToolDefinition" $ \o -> ToolDefinition
    <$> o .: "name"
    <*> o .: "description"
    <*> o .: "inputSchema"

-- | Custom ToJSON to use MCP-standard field names (without td prefix)
instance ToJSON ToolDefinition where
  toJSON t = object
    [ "name" .= t.tdName
    , "description" .= t.tdDescription
    , "inputSchema" .= t.tdInputSchema
    ]

-- | Message sent over Unix socket from exomonad. Tagged by "type".
data ControlMessage
  = HookEvent { input :: HookInput, runtime :: Runtime, role :: Role, containerId :: Maybe Text }
  | McpToolCall { mcpId :: Text, toolName :: Text, arguments :: Value }
  | ToolsListRequest
  | Ping
  deriving stock (Show, Eq, Generic)

instance FromJSON ControlMessage where
  parseJSON = withObject "ControlMessage" $ \o -> do
    msgType <- o .: "type" :: Parser Text
    case msgType of
      "HookEvent" -> HookEvent
        <$> o .: "input"
        <*> o .:? "runtime" .!= Claude
        <*> o .:? "role" .!= Dev
        <*> o .:? "container_id"
      "MCPToolCall" -> McpToolCall
        <$> o .: "id"
        <*> o .: "tool_name"
        <*> o .: "arguments"
      "ToolsListRequest" -> pure ToolsListRequest
      "Ping" -> pure Ping
      _ -> fail $ "Unknown message type: " <> show msgType

instance ToJSON ControlMessage where
  toJSON (HookEvent i r rl cid) = object $
    [ "type" .= ("HookEvent" :: Text)
    , "input" .= i
    , "runtime" .= r
    , "role" .= rl
    ] ++ maybe [] (\c -> ["container_id" .= c]) cid
  toJSON (McpToolCall mid tn args) = object
    [ "type" .= ("MCPToolCall" :: Text)
    , "id" .= mid
    , "tool_name" .= tn
    , "arguments" .= args
    ]
  toJSON ToolsListRequest = object
    [ "type" .= ("ToolsListRequest" :: Text)
    ]
  toJSON Ping = object
    [ "type" .= ("Ping" :: Text)
    ]

-- | Response sent over Unix socket to exomonad. Tagged by "type".
data ControlResponse
  = HookResponse { output :: HookOutput, exitCode :: Int }
  | McpToolResponse { mcpId :: Text, result :: Maybe Value, mcpError :: Maybe McpError }
  | ToolsListResponse { tools :: [ToolDefinition] }
  | Pong
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
  toJSON (ToolsListResponse ts) = object
    [ "type" .= ("ToolsListResponse" :: Text)
    , "tools" .= ts
    ]
  toJSON Pong = object
    [ "type" .= ("Pong" :: Text)
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
      "ToolsListResponse" -> ToolsListResponse <$> o .: "tools"
      "Pong" -> pure Pong
      _ -> fail $ "Unknown response type: " <> show msgType

-- | MCP error codes for structured error responses.
--
-- Errors are categorized by failure mode to help Claude understand what went wrong:
--
-- === NotFound (-32001)
-- Resource does not exist or could not be found.
--
-- Example: Issue not found, binary missing, symbol not found in codebase
--
-- === InvalidInput (-32002)
-- Arguments failed validation. Caller should fix the input.
--
-- Example: Invalid Issue ID (contains path separators), empty env vars, malformed JSON
--
-- === ExternalFailure (-32003)
-- External subprocess or I/O operation failed.
--
-- Example: File system error, subprocess exit failure, git command failure
--
-- === StateError (-32004)
-- Operation cannot proceed due to invalid state (not invalid input).
--
-- Example: Issue is blocked, worktree already exists, conflict in state
--
-- === EnvironmentError (-32005)
-- Missing or invalid environment configuration.
--
-- Example: Not in Zellij session, missing API key, incomplete setup
data ErrorCode
  = InvalidRequest        -- -32600: Invalid Request
  | NotFound              -- -32001: Resource not found
  | InvalidInput          -- -32002: Bad arguments (validation failed)
  | ExternalFailure       -- -32003: Subprocess or I/O error
  | StateError            -- -32004: Invalid state (already exists, blocked)
  | EnvironmentError      -- -32005: Missing environment (not in Zellij)
  | PermissionDenied      -- -32006: Role-based access denied
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

-- | MCP error response with structured details.
--
-- All MCP tool errors use this format instead of plain text strings.
--
-- Examples:
--
-- 1. Validation error (InvalidInput):
--    { "code": -32002
--    , "message": "Invalid spawn_agents arguments: issue_id contains path separators"
--    , "suggestion": "Use Issue ID without slashes, e.g. '123' instead of 'gh-123/'"
--    }
--
-- 2. Not found error (NotFound):
--    { "code": -32001
--    , "message": "Issue #123 not found"
--    , "details": { "searched_in": "GitHub API" }
--    , "suggestion": "Use 'gh_issue_list' to see available issues"
--    }
--
-- 3. External failure (ExternalFailure):
--    { "code": -32003
--    , "message": "File system error: permission denied"
--    , "details": { "path": ".exomonad/sockets", "errno": 13 }
--    , "suggestion": "Check that .exomonad/ directory is writable"
--    }
--
-- 4. State error (StateError):
--    { "code": -32004
--    , "message": "Worktree already exists"
--    , "details": { "path": "worktrees/gh-123-..." }
--    , "suggestion": "Delete existing worktree or use Zellij tabs instead"
--    }
--
-- Note: Fields details and suggestion are optional.
-- Claude uses the error code to categorize the error type,
-- message for human-readable explanation,
-- details for debugging/logging,
-- suggestion for actionable guidance.
data McpError = McpError
  { code :: ErrorCode
  , message :: Text              -- Human readable explanation
  , details :: Maybe Value       -- Structured details for debugging (optional)
  , suggestion :: Maybe Text     -- Actionable guidance to fix the issue (optional)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON McpError where
  toJSON e = object $ filter notNull
    [ "code" .= errorCodeValue e.code
    , "message" .= e.message
    , "details" .= e.details
    , "suggestion" .= e.suggestion
    ]
    where
      notNull (_, Null) = False
      notNull _ = True

instance FromJSON McpError where
  parseJSON = withObject "McpError" $ \o -> do
    codeNum <- o .: "code" :: Parser Int
    code <- case codeNum of
      -32001 -> pure NotFound
      -32002 -> pure InvalidInput
      -32003 -> pure ExternalFailure
      -32004 -> pure StateError
      -32005 -> pure EnvironmentError
      -32006 -> pure PermissionDenied
      n -> fail $ "Unknown error code: " <> show n
    McpError code
      <$> o .: "message"
      <*> o .:? "details"
      <*> o .:? "suggestion"

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
  , decision = Nothing
  , reason = Nothing
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

-- | Create an error hook response (exit code runtime-aware).
hookError :: Runtime -> Text -> ControlResponse
hookError r msg = HookResponse (blockHook msg) (runtimeExitCode r)

-- | Get exit code for blocking based on runtime.
runtimeExitCode :: Runtime -> Int
runtimeExitCode Claude = 1
runtimeExitCode Gemini = 2

-- | Create an MCP tool error response with minimal details.
mcpToolError :: Text -> ErrorCode -> Text -> ControlResponse
mcpToolError reqId code msg = McpToolResponse reqId Nothing (Just $ McpError code msg Nothing Nothing)

-- | Create an MCP tool error response with full details.
mcpToolErrorWithDetails :: Text -> ErrorCode -> Text -> Maybe Value -> Maybe Text -> ControlResponse
mcpToolErrorWithDetails reqId code msg details suggestion =
  McpToolResponse reqId Nothing (Just $ McpError code msg details suggestion)

-- | Create a successful MCP tool response.
mcpToolSuccess :: Text -> Value -> ControlResponse
mcpToolSuccess reqId result = McpToolResponse reqId (Just result) Nothing