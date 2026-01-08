use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ============================================================================
// Interrupt Signal Types
// ============================================================================

/// An interrupt signal sent by Claude via `zellij-cc signal`
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct InterruptSignal {
    /// Signal type: "transition", "escalate", "request_review", etc.
    pub signal_type: String,
    /// Target state for transitions (e.g., "need_more_types")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub state: Option<String>,
    /// Human-readable reason for the signal
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
}

// ============================================================================
// Stream Event Types (for parsing Claude Code's stream-json output)
// ============================================================================

/// A single event from Claude Code's stream-json output.
/// Each line of output is one of these variants.
#[derive(Deserialize, Serialize, Debug, Clone, PartialEq)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum StreamEvent {
    System(SystemEvent),
    Assistant(AssistantEvent),
    User(UserEvent),
    Result(ResultEvent),
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq, Eq)]
pub struct SystemEvent {
    pub subtype: String,
    pub session_id: String,
    #[serde(default)]
    pub tools: Vec<String>,
    pub model: String,
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq)]
pub struct AssistantEvent {
    pub message: AssistantMessage,
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq)]
pub struct AssistantMessage {
    #[serde(default)]
    pub content: Vec<ContentBlock>,
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum ContentBlock {
    Text { text: String },
    ToolUse { name: String, id: String, input: serde_json::Value },
    ToolResult { tool_use_id: String, content: String, is_error: Option<bool> },
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq)]
pub struct UserEvent {
    #[serde(default)]
    pub tool_use_result: Option<String>,
    #[serde(default)]
    pub message: Option<UserMessage>,
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq)]
pub struct UserMessage {
    #[serde(default)]
    pub content: Vec<ContentBlock>,
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq)]
pub struct ResultEvent {
    pub subtype: String,
    pub is_error: bool,
    pub result: Option<String>,
    pub session_id: Option<String>,
    pub total_cost_usd: Option<f64>,
    pub num_turns: Option<i64>,
    pub structured_output: Option<serde_json::Value>,
    #[serde(default)]
    pub permission_denials: Vec<PermissionDenial>,
    #[serde(default, rename = "modelUsage")]
    pub model_usage: HashMap<String, ModelUsage>,
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq)]
pub struct PermissionDenial {
    pub tool_name: String,
    pub tool_use_id: String,
    #[serde(default)]
    pub tool_input: serde_json::Value,
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ModelUsage {
    #[serde(default)]
    pub input_tokens: i64,
    #[serde(default)]
    pub output_tokens: i64,
    #[serde(default)]
    pub cache_read_input_tokens: i64,
    #[serde(default)]
    pub cache_creation_input_tokens: i64,
    #[serde(default)]
    pub cost_usd: f64,
}

// ============================================================================
// Output Types (what zellij-cc returns to callers)
// ============================================================================

#[derive(Serialize, Deserialize)]
pub struct RunResult {
    /// Exit code from claude process
    pub exit_code: i32,
    /// Whether Claude Code reported an error
    pub is_error: bool,
    /// Prose result from Claude Code
    pub result: Option<String>,
    /// Structured output (when --json-schema was provided)
    pub structured_output: Option<serde_json::Value>,
    /// Session ID (available immediately from init event)
    pub session_id: String,
    /// Tag for correlating with orchestrator state (e.g., worktree name)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_tag: Option<String>,
    /// Cost in USD
    pub total_cost_usd: f64,
    /// Number of turns (tool use iterations)
    pub num_turns: i64,
    /// Full event stream for debugging/replay
    pub events: Vec<StreamEvent>,
    /// Permission denials with details
    pub permission_denials: Vec<PermissionDenial>,
    /// Per-model usage breakdown
    pub model_usage: HashMap<String, ModelUsage>,
    /// Interrupt signals received during execution
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub interrupts: Vec<InterruptSignal>,
}
