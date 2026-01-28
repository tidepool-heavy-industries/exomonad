//! Protocol types for the control envelope.
//!
//! Defines message types for hook events and MCP tool calls that flow
//! between exomonad subcommands and the Haskell control socket server.
//!
//! ## Message Flow
//!
//! ```text
//! Claude Code hooks       exomonad hook <event>      Control Socket
//!      (stdin JSON)  -->   (parse & forward)   -->   (Haskell server)
//!                    <--   (response JSON)     <--
//! ```
//!
//! Message formats match Claude Code's hook stdin/stdout schemas exactly
//! to allow passthrough with minimal transformation.

use clap::ValueEnum;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;

// ============================================================================
// Protocol Types
// ============================================================================

/// The runtime environment for the agent.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, ValueEnum, Default, strum::Display)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum Runtime {
    /// Anthropic's Claude Code CLI.
    #[default]
    Claude,
    /// Google's Gemini CLI.
    Gemini,
}

/// The role of the agent (determines hook behavior and context).
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, ValueEnum, Default, strum::Display)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum Role {
    /// Developer/subagent role - focused on implementing tasks.
    #[default]
    Dev,
    /// Tech Lead role - orchestration, oversight, planning.
    Tl,
    /// Product Manager role - triage, prioritization, health.
    Pm,
}

// ============================================================================
// Hook Event Types (from Claude Code stdin)
// ============================================================================

/// Hook event payload received from Claude Code via stdin.
///
/// This matches the JSON schema that Claude Code sends to hook commands.
/// Fields vary by hook type - we capture them all and let the Haskell
/// handler pick what it needs.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HookInput {
    /// Session ID from Claude Code.
    pub session_id: String,

    /// Path to the transcript file.
    #[serde(default)]
    pub transcript_path: String,

    /// Current working directory.
    #[serde(default)]
    pub cwd: String,

    /// Permission mode (default, plan, acceptEdits, dontAsk, bypassPermissions).
    #[serde(default)]
    pub permission_mode: String,

    /// The hook event name (PreToolUse, PostToolUse, etc.).
    pub hook_event_name: String,

    // ----- Tool-related fields (PreToolUse, PostToolUse, PermissionRequest) -----
    /// Tool name for tool-related hooks.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_name: Option<String>,

    /// Tool input arguments for tool-related hooks.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_input: Option<Value>,

    /// Tool use ID for tool-related hooks.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_use_id: Option<String>,

    /// Tool response (PostToolUse only).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_response: Option<Value>,

    // ----- Other hook-specific fields -----
    /// User prompt text (UserPromptSubmit).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub prompt: Option<String>,

    /// Notification message (Notification).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,

    /// Notification type (Notification).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub notification_type: Option<String>,

    /// Whether stop hook is active (Stop, SubagentStop).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stop_hook_active: Option<bool>,

    /// Compact trigger (PreCompact).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trigger: Option<String>,

    /// Custom instructions (PreCompact).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub custom_instructions: Option<String>,

    /// Session start source (SessionStart).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,

    /// Session end reason (SessionEnd).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
}

// ============================================================================
// Hook Output Types (to Claude Code stdout)
// ============================================================================

/// Common fields for all hook output types.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct HookOutput {
    /// Whether to continue processing (default: true).
    #[serde(rename = "continue", default = "default_true")]
    pub continue_: bool,

    /// Reason for stopping (when continue_ = false).
    #[serde(skip_serializing_if = "Option::is_none", rename = "stopReason")]
    pub stop_reason: Option<String>,

    /// Whether to suppress output in Claude Code UI.
    #[serde(
        skip_serializing_if = "Option::is_none",
        rename = "suppressOutput",
        default
    )]
    pub suppress_output: Option<bool>,

    /// System message to show to user.
    #[serde(skip_serializing_if = "Option::is_none", rename = "systemMessage")]
    pub system_message: Option<String>,

    /// Hook-specific output fields.
    #[serde(skip_serializing_if = "Option::is_none", rename = "hookSpecificOutput")]
    pub hook_specific_output: Option<HookSpecificOutput>,
}

fn default_true() -> bool {
    true
}

/// Hook-specific output fields.
///
/// The inner structure varies by hook type.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "hookEventName")]
pub enum HookSpecificOutput {
    /// PreToolUse hook output.
    PreToolUse {
        /// Permission decision: "allow", "deny", or "ask".
        #[serde(rename = "permissionDecision")]
        permission_decision: String,

        /// Reason for the decision.
        #[serde(
            skip_serializing_if = "Option::is_none",
            rename = "permissionDecisionReason"
        )]
        permission_decision_reason: Option<String>,

        /// Modified tool input (only with "allow" decision).
        #[serde(skip_serializing_if = "Option::is_none", rename = "updatedInput")]
        updated_input: Option<Value>,
    },

    /// PostToolUse hook output.
    PostToolUse {
        /// Additional context for Claude about the tool result.
        #[serde(skip_serializing_if = "Option::is_none", rename = "additionalContext")]
        additional_context: Option<String>,
    },

    /// UserPromptSubmit hook output.
    UserPromptSubmit {
        /// Additional context added to conversation.
        #[serde(skip_serializing_if = "Option::is_none", rename = "additionalContext")]
        additional_context: Option<String>,
    },

    /// SessionStart hook output.
    SessionStart {
        /// Context loaded at session start.
        #[serde(skip_serializing_if = "Option::is_none", rename = "additionalContext")]
        additional_context: Option<String>,
    },

    /// PermissionRequest hook output.
    PermissionRequest {
        /// Permission decision.
        decision: PermissionDecision,
    },

    /// Stop hook output.
    Stop {
        /// Decision: "block" to prevent stopping, or None to allow.
        #[serde(skip_serializing_if = "Option::is_none")]
        decision: Option<String>,

        /// Claude-facing guidance when blocked.
        #[serde(skip_serializing_if = "Option::is_none")]
        reason: Option<String>,
    },

    /// SubagentStop hook output.
    SubagentStop {
        /// Decision: "block" to prevent stopping, or None to allow.
        #[serde(skip_serializing_if = "Option::is_none")]
        decision: Option<String>,

        /// Claude-facing guidance when blocked.
        #[serde(skip_serializing_if = "Option::is_none")]
        reason: Option<String>,
    },

    /// Notification hook output.
    Notification,

    /// PreCompact hook output.
    PreCompact,

    /// SessionEnd hook output.
    SessionEnd,
}

/// Permission decision for PermissionRequest hook.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "behavior")]
pub enum PermissionDecision {
    /// Allow the tool call.
    #[serde(rename = "allow")]
    Allow {
        /// Modified tool input.
        #[serde(skip_serializing_if = "Option::is_none", rename = "updatedInput")]
        updated_input: Option<Value>,
    },

    /// Deny the tool call.
    #[serde(rename = "deny")]
    Deny {
        /// Message explaining denial.
        message: String,

        /// Whether to interrupt the current turn.
        #[serde(default)]
        interrupt: bool,
    },
}

// ============================================================================
// Service Protocol Types
// ============================================================================

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(tag = "type")]
pub enum ServiceRequest {
    // LLM
    AnthropicChat {
        model: String,
        messages: Vec<ChatMessage>,
        max_tokens: u32,
        #[serde(skip_serializing_if = "Option::is_none")]
        tools: Option<Vec<Tool>>,
        #[serde(skip_serializing_if = "Option::is_none")]
        system: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        thinking: Option<Value>,
    },
    OllamaGenerate {
        model: String,
        prompt: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        system: Option<String>,
    },

    // GitHub
    GitHubGetIssue {
        owner: String,
        repo: String,
        number: u32,
    },
    GitHubCreateIssue {
        owner: String,
        repo: String,
        title: String,
        body: String,
        labels: Vec<String>,
    },
    GitHubListIssues {
        owner: String,
        repo: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        state: Option<IssueState>,
        labels: Vec<String>,
    },
    GitHubCreatePR {
        owner: String,
        repo: String,
        title: String,
        body: String,
        head: String,
        base: String,
    },
    GitHubGetPR {
        owner: String,
        repo: String,
        number: u32,
    },

    // Observability
    OtelSpan {
        trace_id: String,
        span_id: String,
        name: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        start_ns: Option<u64>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        end_ns: Option<u64>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        attributes: Option<HashMap<String, String>>,
    },
    OtelMetric {
        name: String,
        value: f64,
        labels: HashMap<String, String>,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(tag = "type")]
pub enum ServiceResponse {
    // LLM
    #[serde(rename = "AnthropicChatResponse")]
    AnthropicChat {
        content: Vec<ContentBlock>,
        #[serde(rename = "stop_reason")]
        stop_reason: StopReason,
        usage: Usage,
    },
    #[serde(rename = "OllamaGenerateResponse")]
    OllamaGenerate { response: String, done: bool },

    // GitHub
    #[serde(rename = "GitHubIssueResponse")]
    GitHubIssue {
        number: u32,
        title: String,
        body: String,
        state: String,
        labels: Vec<String>,
        url: String,
    },
    #[serde(rename = "GitHubIssuesResponse")]
    GitHubIssues { issues: Vec<GitHubIssueRef> },
    #[serde(rename = "GitHubPRResponse")]
    GitHubPR {
        number: u32,
        url: String,
        state: String,
    },

    // Observability
    #[serde(rename = "OtelAckResponse")]
    OtelAck,

    // Error
    #[serde(rename = "ErrorResponse")]
    Error { code: i32, message: String },
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ChatMessage {
    pub role: String,
    pub content: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Tool {
    pub name: String,
    pub description: String,
    pub input_schema: Value,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum StopReason {
    EndTurn,
    MaxTokens,
    StopSequence,
    ToolUse,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ContentBlock {
    #[serde(rename = "type")]
    pub block_type: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub input: Option<Value>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Usage {
    pub input_tokens: u32,
    pub output_tokens: u32,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "lowercase")]
pub enum IssueState {
    Open,
    Closed,
    All,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct GitHubIssueRef {
    pub number: u32,
    pub title: String,
    pub state: String,
}

// ============================================================================
// Control Socket Protocol
// ============================================================================

/// Tool definition for MCP discovery (must match Haskell ToolDefinition).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolDefinition {
    pub name: String,
    pub description: String,
    #[serde(rename = "inputSchema")]
    pub input_schema: Value,
}

/// Message sent over the control socket to Haskell.
///
/// Wraps either a hook event, MCP tool call, or tools list request.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum ControlMessage {
    /// Hook event from Claude Code.
    HookEvent {
        /// The raw hook input from CC stdin (boxed to reduce enum size).
        input: Box<HookInput>,
        /// The runtime that emitted the hook.
        #[serde(default)]
        runtime: Runtime,
        /// The role of the agent (dev, tl, pm).
        #[serde(default)]
        role: Role,
        /// Container ID for remote command execution (derived from EXOMONAD_ISSUE_ID).
        #[serde(default, skip_serializing_if = "Option::is_none")]
        container_id: Option<String>,
    },

    /// MCP tool call from Claude Code.
    #[serde(rename = "MCPToolCall")]
    McpToolCall {
        /// JSON-RPC request ID.
        id: String,
        /// Tool name.
        tool_name: String,
        /// Tool arguments.
        arguments: Value,
        /// Optional role for routing (not serialized).
        #[serde(skip)]
        role: Option<Role>,
    },

    /// Request list of available MCP tools from control server.
    ToolsListRequest {
        /// Optional role for routing (not serialized).
        #[serde(skip)]
        role: Option<Role>,
    },

    /// Health check ping.
    Ping,
}

/// Response from Haskell control socket.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum ControlResponse {
    /// Response to hook event.
    HookResponse {
        /// The hook output to send to CC stdout.
        output: HookOutput,

        /// Exit code for the hook command (0 = success, 2 = blocking error).
        exit_code: i32,
    },

    /// Response to MCP tool call.
    #[serde(rename = "MCPToolResponse")]
    McpToolResponse {
        /// JSON-RPC request ID.
        id: String,
        /// Tool result (null on error).
        result: Option<Value>,
        /// Error details (null on success).
        error: Option<McpError>,
    },

    /// Response to tools list request.
    ToolsListResponse {
        /// Available MCP tools.
        tools: Vec<ToolDefinition>,
    },

    /// Health check pong.
    Pong,
}

/// MCP error response.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpError {
    /// Error code.
    pub code: i32,
    /// Error message.
    pub message: String,
    /// Structured details for debugging.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub details: Option<Value>,
    /// Actionable guidance to fix the issue.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub suggestion: Option<String>,
}

// ============================================================================
// Builder helpers
// ============================================================================

impl HookOutput {
    /// Create an "allow" response for PreToolUse.
    pub fn pre_tool_use_allow(reason: Option<String>, modified_input: Option<Value>) -> Self {
        Self {
            continue_: true,
            hook_specific_output: Some(HookSpecificOutput::PreToolUse {
                permission_decision: "allow".to_string(),
                permission_decision_reason: reason,
                updated_input: modified_input,
            }),
            ..Default::default()
        }
    }

    /// Create a "deny" response for PreToolUse.
    pub fn pre_tool_use_deny(reason: String) -> Self {
        Self {
            continue_: true,
            hook_specific_output: Some(HookSpecificOutput::PreToolUse {
                permission_decision: "deny".to_string(),
                permission_decision_reason: Some(reason),
                updated_input: None,
            }),
            ..Default::default()
        }
    }

    /// Create an "allow" response for PostToolUse with optional context.
    pub fn post_tool_use_allow(additional_context: Option<String>) -> Self {
        Self {
            continue_: true,
            hook_specific_output: Some(HookSpecificOutput::PostToolUse { additional_context }),
            ..Default::default()
        }
    }

    /// Create a "block" response that stops processing.
    pub fn block(reason: String) -> Self {
        Self {
            continue_: false,
            stop_reason: Some(reason),
            ..Default::default()
        }
    }
}

impl ControlResponse {
    /// Create a success response for a hook.
    pub fn hook_success(output: HookOutput) -> Self {
        Self::HookResponse {
            output,
            exit_code: 0,
        }
    }

    /// Create a blocking error response for a hook.
    pub fn hook_error(message: String, runtime: Runtime) -> Self {
        let exit_code = match runtime {
            Runtime::Claude => 1,
            Runtime::Gemini => 2,
        };
        Self::HookResponse {
            output: HookOutput {
                continue_: false,
                stop_reason: Some(message),
                ..Default::default()
            },
            exit_code,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hook_input_deserialize() {
        let json = r#"{
            "session_id": "abc123",
            "transcript_path": "/tmp/transcript.jsonl",
            "cwd": "/home/user/project",
            "permission_mode": "default",
            "hook_event_name": "PreToolUse",
            "tool_name": "Write",
            "tool_input": {"file_path": "/tmp/test.txt", "content": "hello"},
            "tool_use_id": "toolu_123"
        }"#;

        let input: HookInput = serde_json::from_str(json).unwrap();
        assert_eq!(input.hook_event_name, "PreToolUse");
        assert_eq!(input.tool_name, Some("Write".to_string()));
    }

    #[test]
    fn test_hook_output_serialize() {
        let output = HookOutput::pre_tool_use_allow(
            Some("Allowed by ExoMonad".to_string()),
            Some(serde_json::json!({"file_path": "/tmp/safe.txt"})),
        );

        let json = serde_json::to_string_pretty(&output).unwrap();
        assert!(json.contains("permissionDecision"));
        assert!(json.contains("allow"));
    }

    #[test]
    fn test_control_message_roundtrip() {
        let msg = ControlMessage::HookEvent {
            input: Box::new(HookInput {
                session_id: "test".to_string(),
                transcript_path: String::new(),
                cwd: String::new(),
                permission_mode: "default".to_string(),
                hook_event_name: "Stop".to_string(),
                tool_name: None,
                tool_input: None,
                tool_use_id: None,
                tool_response: None,
                prompt: None,
                message: None,
                notification_type: None,
                stop_hook_active: Some(true),
                trigger: None,
                custom_instructions: None,
                source: None,
                reason: None,
            }),
            runtime: Runtime::Claude,
            role: Role::Dev,
            container_id: Some("exomonad-agent-123".to_string()),
        };

        let json = serde_json::to_string(&msg).unwrap();
        let parsed: ControlMessage = serde_json::from_str(&json).unwrap();

        match parsed {
            ControlMessage::HookEvent { input, runtime, role, container_id } => {
                assert_eq!(input.session_id, "test");
                assert_eq!(runtime, Runtime::Claude);
                assert_eq!(role, Role::Dev);
                assert_eq!(container_id, Some("exomonad-agent-123".to_string()));
            }
            _ => panic!("Wrong variant"),
        }
    }
}