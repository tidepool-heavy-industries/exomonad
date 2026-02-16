//! Hook event types and builders.
//!
//! Types for Claude Code hook stdin/stdout communication.

use crate::domain::{PermissionMode, SessionId, ToolName, ToolPermission};
use crate::protocol::Runtime;
use serde::{Deserialize, Serialize};
use serde_json::Value;

// ============================================================================
// Hook Event Types (from Claude Code stdin)
// ============================================================================

/// Hook event payload received from Claude Code via stdin.
///
/// This matches the JSON schema that Claude Code sends to hook commands.
/// Fields vary by hook type - we capture them all and let the Haskell
/// handler pick what it needs.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct HookInput {
    /// Session ID from Claude Code.
    pub session_id: SessionId,

    /// Path to the transcript file.
    #[serde(default)]
    pub transcript_path: String,

    /// Current working directory.
    #[serde(default)]
    pub cwd: String,

    /// Permission mode.
    #[serde(default)]
    pub permission_mode: PermissionMode,

    /// The hook event name (PreToolUse, PostToolUse, etc.).
    pub hook_event_name: String,

    /// Runtime environment (claude, gemini). Injected by Rust before WASM call.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub runtime: Option<Runtime>,

    // ----- Tool-related fields (PreToolUse, PostToolUse, PermissionRequest) -----
    /// Tool name for tool-related hooks.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_name: Option<ToolName>,

    /// Tool input arguments for tool-related hooks.
    #[serde(alias = "tool_parameters")]
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

    /// Agent's response text (AfterAgent).
    /// Ref: <https://geminicli.com/docs/hooks/reference/#afteragent>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub prompt_response: Option<String>,

    /// Event timestamp.
    /// Ref: <https://geminicli.com/docs/hooks/reference/#afteragent>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timestamp: Option<String>,
}

// ============================================================================
// Hook Output Types (to Claude Code stdout)
// ============================================================================

/// Common fields for all hook output types.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ClaudePreToolUseOutput {
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

impl Default for ClaudePreToolUseOutput {
    fn default() -> Self {
        Self {
            continue_: true, // Semantic default: allow continuation
            stop_reason: None,
            suppress_output: None,
            system_message: None,
            hook_specific_output: None,
        }
    }
}

fn default_true() -> bool {
    true
}

/// Hook-specific output fields.
///
/// The inner structure varies by hook type.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "hookEventName")]
pub enum HookSpecificOutput {
    /// PreToolUse hook output.
    PreToolUse {
        /// Permission decision: "allow", "deny", or "ask".
        #[serde(rename = "permissionDecision")]
        permission_decision: ToolPermission,

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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
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
// Builder helpers
// ============================================================================

impl ClaudePreToolUseOutput {
    /// Create an "allow" response for PreToolUse.
    pub fn pre_tool_use_allow(reason: Option<String>, modified_input: Option<Value>) -> Self {
        Self {
            continue_: true,
            hook_specific_output: Some(HookSpecificOutput::PreToolUse {
                permission_decision: ToolPermission::Allow,
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
                permission_decision: ToolPermission::Deny,
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

// ============================================================================
// Internal Domain Types (from WASM, translated at edge)
// ============================================================================

/// Stop hook decision from WASM (domain type).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum StopDecision {
    /// Allow the agent to stop
    Allow,
    /// Block stopping, send reason as correction prompt
    Block,
}

/// Internal stop hook output from WASM.
/// This is the domain type that Haskell returns. Rust translates to runtime-specific format.
#[derive(Debug, Clone, Deserialize)]
pub struct InternalStopHookOutput {
    /// Decision: allow or block
    pub decision: StopDecision,
    /// Reason for blocking (sent to agent as correction prompt)
    pub reason: Option<String>,
}

impl InternalStopHookOutput {
    /// Translate to Claude Code format.
    pub fn to_claude(&self) -> ClaudeStopHookOutput {
        match self.decision {
            StopDecision::Allow => ClaudeStopHookOutput {
                continue_: true,
                stop_reason: None,
            },
            StopDecision::Block => ClaudeStopHookOutput {
                continue_: false,
                stop_reason: self.reason.clone(),
            },
        }
    }

    /// Translate to Gemini CLI format.
    /// Ref: <https://geminicli.com/docs/hooks/reference/#afteragent>
    pub fn to_gemini(&self) -> GeminiStopHookOutput {
        match self.decision {
            StopDecision::Allow => GeminiStopHookOutput {
                decision: GeminiStopDecision::Allow,
                reason: None,
                continue_: true,
                clear_context: None,
                system_message: None,
                suppress_output: None,
            },
            StopDecision::Block => GeminiStopHookOutput {
                decision: GeminiStopDecision::Deny, // Gemini uses "deny" for retry
                reason: self.reason.clone(),
                continue_: true,
                clear_context: None,
                system_message: None,
                suppress_output: None,
            },
        }
    }

    /// Translate to runtime-specific format and serialize to JSON.
    pub fn to_runtime_json(&self, runtime: &Runtime) -> String {
        match runtime {
            Runtime::Claude => serde_json::to_string(&self.to_claude())
                .unwrap_or_else(|_| r#"{"continue":true}"#.to_string()),
            Runtime::Gemini => serde_json::to_string(&self.to_gemini())
                .unwrap_or_else(|_| r#"{"decision":"allow"}"#.to_string()),
        }
    }
}

/// Claude Code stop hook output format.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct ClaudeStopHookOutput {
    /// Whether to continue (true = allow stop, false = block)
    #[serde(rename = "continue")]
    pub continue_: bool,
    /// Reason for blocking
    #[serde(skip_serializing_if = "Option::is_none", rename = "stopReason")]
    pub stop_reason: Option<String>,
}

/// Gemini CLI stop hook decision.
/// Ref: <https://geminicli.com/docs/hooks/reference/#afteragent>
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum GeminiStopDecision {
    /// Allow the agent to stop
    Allow,
    /// Deny and trigger retry with reason as correction prompt
    Deny,
}

/// Gemini CLI stop hook output format.
/// Ref: <https://geminicli.com/docs/hooks/reference/#afteragent>
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct GeminiStopHookOutput {
    /// Decision: allow or deny (deny triggers retry with reason as correction prompt)
    pub decision: GeminiStopDecision,
    /// Reason sent to agent as correction prompt (when decision = deny)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
    /// Whether to continue the session (false = exit)
    #[serde(rename = "continue", default = "default_true")]
    pub continue_: bool,
    /// Whether to clear conversation context
    #[serde(skip_serializing_if = "Option::is_none", rename = "clearContext")]
    pub clear_context: Option<bool>,
    /// System message to show to user
    #[serde(skip_serializing_if = "Option::is_none", rename = "systemMessage")]
    pub system_message: Option<String>,
    /// Whether to suppress CLI output
    #[serde(skip_serializing_if = "Option::is_none", rename = "suppressOutput")]
    pub suppress_output: Option<bool>,
}

// ============================================================================
// Hook Envelope (server ↔ CLI wire type)
// ============================================================================

/// Wire type for the `/hook` HTTP endpoint.
/// Server returns this; CLI prints `stdout` and exits with `exit_code`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HookEnvelope {
    /// The JSON string to print to stdout (consumed by Claude Code / Gemini CLI).
    pub stdout: String,
    /// Process exit code (0 = success, 2 = block).
    pub exit_code: i32,
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
        assert_eq!(input.tool_name.as_ref().map(|t| t.as_str()), Some("Write"));
    }

    #[test]
    fn test_hook_output_serialize() {
        let output = ClaudePreToolUseOutput::pre_tool_use_allow(
            Some("Allowed by ExoMonad".to_string()),
            Some(serde_json::json!({"file_path": "/tmp/safe.txt"})),
        );

        let json = serde_json::to_string_pretty(&output).unwrap();
        assert!(json.contains("permissionDecision"));
        assert!(json.contains("allow"));
    }

    // =========================================================================
    // ClaudePreToolUseOutput comprehensive serialization tests
    // =========================================================================

    #[test]
    fn test_hook_output_pre_tool_use_allow_format() {
        let output = ClaudePreToolUseOutput::pre_tool_use_allow(Some("test reason".into()), None);
        let json = serde_json::to_value(&output).unwrap();

        assert_eq!(json["continue"], true);
        assert!(
            json["stopReason"].is_null() || !json.as_object().unwrap().contains_key("stopReason")
        );

        let specific = &json["hookSpecificOutput"];
        assert_eq!(specific["hookEventName"], "PreToolUse");
        assert_eq!(specific["permissionDecision"], "allow");
        assert_eq!(specific["permissionDecisionReason"], "test reason");
    }

    #[test]
    fn test_hook_output_pre_tool_use_deny_format() {
        let output = ClaudePreToolUseOutput::pre_tool_use_deny("not allowed".into());
        let json = serde_json::to_value(&output).unwrap();

        assert_eq!(json["continue"], true); // deny still continues, just blocks this tool
        let specific = &json["hookSpecificOutput"];
        assert_eq!(specific["hookEventName"], "PreToolUse");
        assert_eq!(specific["permissionDecision"], "deny");
        assert_eq!(specific["permissionDecisionReason"], "not allowed");
    }

    #[test]
    fn test_hook_output_pre_tool_use_with_updated_input() {
        let modified = serde_json::json!({"file_path": "/safe/path.txt"});
        let output = ClaudePreToolUseOutput::pre_tool_use_allow(None, Some(modified.clone()));
        let json = serde_json::to_value(&output).unwrap();

        let specific = &json["hookSpecificOutput"];
        assert_eq!(specific["updatedInput"], modified);
    }

    #[test]
    fn test_hook_output_block_format() {
        let output = ClaudePreToolUseOutput::block("session terminated".into());
        let json = serde_json::to_value(&output).unwrap();

        assert_eq!(json["continue"], false);
        assert_eq!(json["stopReason"], "session terminated");
    }

    #[test]
    fn test_hook_output_post_tool_use_format() {
        let output = ClaudePreToolUseOutput::post_tool_use_allow(Some("additional context".into()));
        let json = serde_json::to_value(&output).unwrap();

        assert_eq!(json["continue"], true);
        let specific = &json["hookSpecificOutput"];
        assert_eq!(specific["hookEventName"], "PostToolUse");
        assert_eq!(specific["additionalContext"], "additional context");
    }

    #[test]
    fn test_hook_output_default() {
        let output = ClaudePreToolUseOutput::default();
        let json = serde_json::to_value(&output).unwrap();

        // Default should have continue=true and no hook_specific_output
        assert_eq!(json["continue"], true);
    }

    // =========================================================================
    // HookInput comprehensive parsing tests
    // =========================================================================

    #[test]
    fn test_hook_input_minimal() {
        let json = r#"{"session_id":"s","hook_event_name":"Stop"}"#;
        let input: HookInput = serde_json::from_str(json).unwrap();
        assert_eq!(input.session_id.as_str(), "s");
        assert_eq!(input.hook_event_name, "Stop");
        assert!(input.tool_name.is_none());
    }

    #[test]
    fn test_hook_input_with_tool_parameters_alias() {
        // Gemini CLI uses tool_parameters instead of tool_input
        let json = r#"{"session_id":"s","hook_event_name":"PreToolUse","tool_parameters":{"key":"value"}}"#;
        let input: HookInput = serde_json::from_str(json).unwrap();
        assert!(input.tool_input.is_some());
        assert_eq!(input.tool_input.unwrap()["key"], "value");
    }

    #[test]
    fn test_hook_input_extra_fields_ignored() {
        let json = r#"{"session_id":"s","hook_event_name":"Stop","unknown_field":"ignored","another":123}"#;
        let result: Result<HookInput, _> = serde_json::from_str(json);
        assert!(result.is_ok());
    }

    #[test]
    fn test_hook_input_unknown_permission_mode() {
        let json = r#"{"session_id":"s","hook_event_name":"Stop","permission_mode":"futureMode"}"#;
        let input: HookInput = serde_json::from_str(json).unwrap();
        assert_eq!(input.permission_mode, PermissionMode::Default);
    }

    #[test]
    fn test_hook_input_missing_permission_mode() {
        let json = r#"{"session_id":"s","hook_event_name":"Stop"}"#;
        let input: HookInput = serde_json::from_str(json).unwrap();
        assert_eq!(input.permission_mode, PermissionMode::Default);
    }

    #[test]
    fn test_hook_input_all_fields() {
        let json = r#"{
            "session_id": "sess-123",
            "transcript_path": "/tmp/t.jsonl",
            "cwd": "/home/user",
            "permission_mode": "plan",
            "hook_event_name": "PreToolUse",
            "tool_name": "Write",
            "tool_input": {"file_path": "/x"},
            "tool_use_id": "toolu_abc",
            "prompt": "user prompt",
            "message": "notification",
            "stop_hook_active": true
        }"#;
        let input: HookInput = serde_json::from_str(json).unwrap();
        assert_eq!(input.session_id.as_str(), "sess-123");
        assert_eq!(input.cwd, "/home/user");
        assert_eq!(input.permission_mode, PermissionMode::Plan);
        assert_eq!(input.tool_name.as_ref().map(|t| t.as_str()), Some("Write"));
        assert_eq!(input.prompt, Some("user prompt".into()));
        assert_eq!(input.stop_hook_active, Some(true));
    }
}
