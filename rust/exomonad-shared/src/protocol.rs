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
#[derive(
    Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, ValueEnum, Default, strum::Display,
)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum Runtime {
    /// Anthropic's Claude Code CLI.
    #[default]
    Claude,
    /// Google's Gemini CLI.
    Gemini,
}

/// Hook event type for Claude Code hooks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum, strum::Display)]
#[strum(serialize_all = "kebab-case")]
pub enum HookEventType {
    /// Before tool execution (can allow/deny/modify)
    PreToolUse,
    /// After tool completion
    PostToolUse,
    /// When a notification is shown
    Notification,
    /// When Claude Code wants to stop
    Stop,
    /// When a subagent (Task tool) starts
    SubagentStart,
    /// When a subagent (Task tool) finishes
    SubagentStop,
    /// Before a compact operation
    PreCompact,
    /// When a session starts or resumes
    SessionStart,
    /// When a session ends
    SessionEnd,
    /// When permission dialog is shown
    PermissionRequest,
    /// When user submits a prompt
    UserPromptSubmit,
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

    /// Runtime environment (claude, gemini). Injected by Rust before WASM call.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub runtime: Option<Runtime>,

    // ----- Tool-related fields (PreToolUse, PostToolUse, PermissionRequest) -----
    /// Tool name for tool-related hooks.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_name: Option<String>,

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
}

// ============================================================================
// Hook Output Types (to Claude Code stdout)
// ============================================================================

/// Common fields for all hook output types.
#[derive(Debug, Clone, Serialize, Deserialize)]
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

impl Default for HookOutput {
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
        #[serde(default)]
        include_comments: bool,
    },
    GitHubCreateIssue {
        owner: String,
        repo: String,
        title: String,
        body: String,
        labels: Vec<String>,
    },
    GitHubUpdateIssue {
        owner: String,
        repo: String,
        number: u32,
        #[serde(skip_serializing_if = "Option::is_none")]
        title: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        body: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        state: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        labels: Option<Vec<String>>,
        #[serde(skip_serializing_if = "Option::is_none")]
        assignees: Option<Vec<String>>,
    },
    GitHubAddIssueLabel {
        owner: String,
        repo: String,
        number: u32,
        label: String,
    },
    GitHubRemoveIssueLabel {
        owner: String,
        repo: String,
        number: u32,
        label: String,
    },
    GitHubAddIssueAssignee {
        owner: String,
        repo: String,
        number: u32,
        assignee: String,
    },
    GitHubRemoveIssueAssignee {
        owner: String,
        repo: String,
        number: u32,
        assignee: String,
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
        #[serde(default)]
        include_details: bool,
    },
    GitHubListPullRequests {
        owner: String,
        repo: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        state: Option<String>, // open, closed, merged, all
        #[serde(skip_serializing_if = "Option::is_none")]
        limit: Option<u32>,
    },
    GitHubGetPullRequestReviews {
        owner: String,
        repo: String,
        number: u32,
    },
    GitHubGetDiscussion {
        owner: String,
        repo: String,
        number: u32,
    },
    GitHubCheckAuth,

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

    // User Interaction (from Zellij plugin)
    UserInteraction {
        request_id: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        payload: Option<Value>,
        #[serde(default)]
        cancel: bool,
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
        author: String,
        #[serde(default)]
        comments: Vec<GitHubDiscussionComment>,
    },
    #[serde(rename = "GitHubIssuesResponse")]
    GitHubIssues { issues: Vec<GitHubIssueRef> },
    #[serde(rename = "GitHubPRResponse")]
    GitHubPR {
        number: u32,
        title: String,
        body: String,
        author: String,
        url: String,
        state: String,
        head_ref_name: String,
        base_ref_name: String,
        created_at: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        merged_at: Option<String>,
        labels: Vec<String>,
        #[serde(default)]
        comments: Vec<GitHubDiscussionComment>,
        #[serde(default)]
        reviews: Vec<GitHubReviewComment>,
    },
    #[serde(rename = "GitHubPullRequestsResponse")]
    GitHubPullRequests { pull_requests: Vec<GitHubPRRef> },
    #[serde(rename = "GitHubReviewsResponse")]
    GitHubReviews { reviews: Vec<GitHubReviewComment> },
    #[serde(rename = "GitHubDiscussionResponse")]
    GitHubDiscussion {
        number: u32,
        title: String,
        body: String,
        author: String,
        url: String,
        comments: Vec<GitHubDiscussionComment>,
    },
    #[serde(rename = "GitHubAuthResponse")]
    GitHubAuth {
        authenticated: bool,
        user: Option<String>,
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
    pub body: String,
    pub state: String,
    pub url: String,
    pub author: GitHubAuthorRef,
    pub labels: Vec<GitHubLabelRef>,
    #[serde(default)]
    pub comments: Vec<GitHubDiscussionComment>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct GitHubLabelRef {
    pub name: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct GitHubAuthorRef {
    pub login: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct GitHubPRRef {
    pub number: u32,
    pub title: String,
    pub state: String,
    pub url: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct GitHubReviewComment {
    pub author: String,
    pub body: String,
    pub path: String,
    pub line: Option<u32>,
    pub state: String,
    pub created_at: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct GitHubDiscussionComment {
    pub author: String,
    pub body: String,
    pub created_at: String,
    pub replies: Vec<GitHubDiscussionComment>,
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

    // =========================================================================
    // ServiceRequest/ServiceResponse GitHub roundtrip tests
    // =========================================================================

    #[test]
    fn test_github_get_issue_request_roundtrip() {
        let req = ServiceRequest::GitHubGetIssue {
            owner: "octocat".into(),
            repo: "hello-world".into(),
            number: 42,
            include_comments: true,
        };
        let json = serde_json::to_string(&req).unwrap();
        let parsed: ServiceRequest = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceRequest::GitHubGetIssue {
                owner,
                repo,
                number,
                include_comments,
            } => {
                assert_eq!(owner, "octocat");
                assert_eq!(repo, "hello-world");
                assert_eq!(number, 42);
                assert!(include_comments);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_get_issue_request_defaults() {
        // include_comments should default to false when absent
        let json = r#"{"type":"GitHubGetIssue","owner":"o","repo":"r","number":1}"#;
        let parsed: ServiceRequest = serde_json::from_str(json).unwrap();
        match parsed {
            ServiceRequest::GitHubGetIssue {
                include_comments, ..
            } => {
                assert!(!include_comments);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_issue_response_roundtrip() {
        let resp = ServiceResponse::GitHubIssue {
            number: 42,
            title: "Fix the bug".into(),
            body: "It's broken".into(),
            state: "open".into(),
            labels: vec!["bug".into(), "critical".into()],
            url: "https://github.com/octocat/hello-world/issues/42".into(),
            author: "octocat".into(),
            comments: vec![GitHubDiscussionComment {
                author: "reviewer".into(),
                body: "Looks good".into(),
                created_at: "2024-01-15T10:00:00Z".into(),
                replies: vec![],
            }],
        };
        let json = serde_json::to_string(&resp).unwrap();
        let parsed: ServiceResponse = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceResponse::GitHubIssue {
                number,
                title,
                author,
                comments,
                labels,
                ..
            } => {
                assert_eq!(number, 42);
                assert_eq!(title, "Fix the bug");
                assert_eq!(author, "octocat");
                assert_eq!(comments.len(), 1);
                assert_eq!(comments[0].author, "reviewer");
                assert_eq!(labels, vec!["bug", "critical"]);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_issue_response_empty_comments_default() {
        // comments should default to empty vec when absent
        let json = r#"{"type":"GitHubIssueResponse","number":1,"title":"t","body":"b","state":"open","labels":[],"url":"u","author":"a"}"#;
        let parsed: ServiceResponse = serde_json::from_str(json).unwrap();
        match parsed {
            ServiceResponse::GitHubIssue { comments, .. } => {
                assert!(comments.is_empty());
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_get_pr_request_roundtrip() {
        let req = ServiceRequest::GitHubGetPR {
            owner: "octocat".into(),
            repo: "hello-world".into(),
            number: 99,
            include_details: true,
        };
        let json = serde_json::to_string(&req).unwrap();
        let parsed: ServiceRequest = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceRequest::GitHubGetPR {
                owner,
                repo,
                number,
                include_details,
            } => {
                assert_eq!(owner, "octocat");
                assert_eq!(repo, "hello-world");
                assert_eq!(number, 99);
                assert!(include_details);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_get_pr_request_defaults() {
        let json = r#"{"type":"GitHubGetPR","owner":"o","repo":"r","number":1}"#;
        let parsed: ServiceRequest = serde_json::from_str(json).unwrap();
        match parsed {
            ServiceRequest::GitHubGetPR {
                include_details, ..
            } => {
                assert!(!include_details);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_pr_response_roundtrip() {
        let resp = ServiceResponse::GitHubPR {
            number: 99,
            title: "Add feature".into(),
            body: "This adds X".into(),
            author: "octocat".into(),
            url: "https://github.com/octocat/hello-world/pull/99".into(),
            state: "open".into(),
            head_ref_name: "feature-branch".into(),
            base_ref_name: "main".into(),
            created_at: "2024-01-15T10:00:00Z".into(),
            merged_at: Some("2024-01-16T12:00:00Z".into()),
            labels: vec!["enhancement".into()],
            comments: vec![GitHubDiscussionComment {
                author: "reviewer".into(),
                body: "LGTM".into(),
                created_at: "2024-01-15T11:00:00Z".into(),
                replies: vec![],
            }],
            reviews: vec![GitHubReviewComment {
                author: "reviewer".into(),
                body: "Approved".into(),
                path: "src/main.rs".into(),
                line: Some(42),
                state: "APPROVED".into(),
                created_at: "2024-01-15T12:00:00Z".into(),
            }],
        };
        let json = serde_json::to_string(&resp).unwrap();
        let parsed: ServiceResponse = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceResponse::GitHubPR {
                number,
                title,
                author,
                merged_at,
                labels,
                comments,
                reviews,
                ..
            } => {
                assert_eq!(number, 99);
                assert_eq!(title, "Add feature");
                assert_eq!(author, "octocat");
                assert_eq!(merged_at, Some("2024-01-16T12:00:00Z".into()));
                assert_eq!(labels, vec!["enhancement"]);
                assert_eq!(comments.len(), 1);
                assert_eq!(reviews.len(), 1);
                assert_eq!(reviews[0].state, "APPROVED");
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_pr_response_optional_defaults() {
        // merged_at absent, comments/reviews absent should all default
        let json = r#"{
            "type": "GitHubPRResponse",
            "number": 1, "title": "t", "body": "b", "author": "a",
            "url": "u", "state": "open", "head_ref_name": "h",
            "base_ref_name": "main", "created_at": "2024-01-01T00:00:00Z",
            "labels": []
        }"#;
        let parsed: ServiceResponse = serde_json::from_str(json).unwrap();
        match parsed {
            ServiceResponse::GitHubPR {
                merged_at,
                comments,
                reviews,
                ..
            } => {
                assert_eq!(merged_at, None);
                assert!(comments.is_empty());
                assert!(reviews.is_empty());
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_create_issue_request_roundtrip() {
        let req = ServiceRequest::GitHubCreateIssue {
            owner: "octocat".into(),
            repo: "hello-world".into(),
            title: "New bug".into(),
            body: "Details here".into(),
            labels: vec!["bug".into()],
        };
        let json = serde_json::to_string(&req).unwrap();
        let parsed: ServiceRequest = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceRequest::GitHubCreateIssue {
                owner,
                title,
                labels,
                ..
            } => {
                assert_eq!(owner, "octocat");
                assert_eq!(title, "New bug");
                assert_eq!(labels, vec!["bug"]);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_create_pr_request_roundtrip() {
        let req = ServiceRequest::GitHubCreatePR {
            owner: "octocat".into(),
            repo: "hello-world".into(),
            title: "Add feature".into(),
            body: "This adds X".into(),
            head: "feature".into(),
            base: "main".into(),
        };
        let json = serde_json::to_string(&req).unwrap();
        let parsed: ServiceRequest = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceRequest::GitHubCreatePR {
                owner,
                title,
                head,
                base,
                ..
            } => {
                assert_eq!(owner, "octocat");
                assert_eq!(title, "Add feature");
                assert_eq!(head, "feature");
                assert_eq!(base, "main");
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_list_issues_request_roundtrip() {
        let req = ServiceRequest::GitHubListIssues {
            owner: "octocat".into(),
            repo: "hello-world".into(),
            state: Some(IssueState::Open),
            labels: vec!["bug".into()],
        };
        let json = serde_json::to_string(&req).unwrap();
        let parsed: ServiceRequest = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceRequest::GitHubListIssues { labels, .. } => {
                assert_eq!(labels, vec!["bug"]);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_issues_response_roundtrip() {
        let resp = ServiceResponse::GitHubIssues {
            issues: vec![
                GitHubIssueRef {
                    number: 1,
                    title: "Bug".into(),
                    body: "b".into(),
                    state: "OPEN".into(),
                    url: "u".into(),
                    author: GitHubAuthorRef {
                        login: "a".into(),
                        name: None,
                    },
                    labels: vec![],
                    comments: vec![],
                },
                GitHubIssueRef {
                    number: 2,
                    title: "Feature".into(),
                    body: "b".into(),
                    state: "CLOSED".into(),
                    url: "u".into(),
                    author: GitHubAuthorRef {
                        login: "a".into(),
                        name: None,
                    },
                    labels: vec![],
                    comments: vec![],
                },
            ],
        };
        let json = serde_json::to_string(&resp).unwrap();
        let parsed: ServiceResponse = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceResponse::GitHubIssues { issues } => {
                assert_eq!(issues.len(), 2);
                assert_eq!(issues[0].number, 1);
                assert_eq!(issues[1].state, "CLOSED");
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_reviews_response_roundtrip() {
        let resp = ServiceResponse::GitHubReviews {
            reviews: vec![GitHubReviewComment {
                author: "reviewer".into(),
                body: "Changes requested".into(),
                path: "lib.rs".into(),
                line: None,
                state: "CHANGES_REQUESTED".into(),
                created_at: "2024-01-15T10:00:00Z".into(),
            }],
        };
        let json = serde_json::to_string(&resp).unwrap();
        let parsed: ServiceResponse = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceResponse::GitHubReviews { reviews } => {
                assert_eq!(reviews.len(), 1);
                assert_eq!(reviews[0].state, "CHANGES_REQUESTED");
                assert_eq!(reviews[0].line, None);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_discussion_response_roundtrip() {
        let resp = ServiceResponse::GitHubDiscussion {
            number: 10,
            title: "RFC: New API".into(),
            body: "Proposal details".into(),
            author: "octocat".into(),
            url: "https://github.com/octocat/hello-world/discussions/10".into(),
            comments: vec![GitHubDiscussionComment {
                author: "commenter".into(),
                body: "Great idea".into(),
                created_at: "2024-01-15T10:00:00Z".into(),
                replies: vec![GitHubDiscussionComment {
                    author: "octocat".into(),
                    body: "Thanks!".into(),
                    created_at: "2024-01-15T11:00:00Z".into(),
                    replies: vec![],
                }],
            }],
        };
        let json = serde_json::to_string(&resp).unwrap();
        let parsed: ServiceResponse = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceResponse::GitHubDiscussion {
                number, comments, ..
            } => {
                assert_eq!(number, 10);
                assert_eq!(comments.len(), 1);
                assert_eq!(comments[0].replies.len(), 1);
                assert_eq!(comments[0].replies[0].author, "octocat");
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_auth_response_roundtrip() {
        let resp = ServiceResponse::GitHubAuth {
            authenticated: true,
            user: Some("octocat".into()),
        };
        let json = serde_json::to_string(&resp).unwrap();
        let parsed: ServiceResponse = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceResponse::GitHubAuth {
                authenticated,
                user,
            } => {
                assert!(authenticated);
                assert_eq!(user, Some("octocat".into()));
            }
            _ => panic!("Wrong variant"),
        }
    }

    /// Verify the JSON wire format uses the exact field names
    /// the Haskell SocketClient.hs FromJSON instance expects.
    #[test]
    fn test_github_issue_response_wire_format() {
        let resp = ServiceResponse::GitHubIssue {
            number: 1,
            title: "t".into(),
            body: "b".into(),
            state: "open".into(),
            labels: vec![],
            url: "u".into(),
            author: "a".into(),
            comments: vec![],
        };
        let val: Value = serde_json::to_value(&resp).unwrap();
        let obj = val.as_object().unwrap();
        // Verify the type tag matches Haskell's FromJSON dispatch
        assert_eq!(obj["type"], "GitHubIssueResponse");
        // Verify all expected field names are present
        assert!(obj.contains_key("number"));
        assert!(obj.contains_key("title"));
        assert!(obj.contains_key("body"));
        assert!(obj.contains_key("state"));
        assert!(obj.contains_key("labels"));
        assert!(obj.contains_key("url"));
        assert!(obj.contains_key("author"));
        assert!(obj.contains_key("comments"));
    }

    /// Verify the PR JSON wire format uses the exact field names
    /// the Haskell SocketClient.hs FromJSON instance expects.
    #[test]
    fn test_github_pr_response_wire_format() {
        let resp = ServiceResponse::GitHubPR {
            number: 1,
            title: "t".into(),
            body: "b".into(),
            author: "a".into(),
            url: "u".into(),
            state: "open".into(),
            head_ref_name: "h".into(),
            base_ref_name: "main".into(),
            created_at: "2024-01-01T00:00:00Z".into(),
            merged_at: None,
            labels: vec![],
            comments: vec![],
            reviews: vec![],
        };
        let val: Value = serde_json::to_value(&resp).unwrap();
        let obj = val.as_object().unwrap();
        assert_eq!(obj["type"], "GitHubPRResponse");
        assert!(obj.contains_key("number"));
        assert!(obj.contains_key("title"));
        assert!(obj.contains_key("body"));
        assert!(obj.contains_key("author"));
        assert!(obj.contains_key("url"));
        assert!(obj.contains_key("state"));
        assert!(obj.contains_key("head_ref_name"));
        assert!(obj.contains_key("base_ref_name"));
        assert!(obj.contains_key("created_at"));
        assert!(obj.contains_key("labels"));
        assert!(obj.contains_key("comments"));
        assert!(obj.contains_key("reviews"));
        // merged_at is None so should be absent (skip_serializing_if)
        assert!(!obj.contains_key("merged_at"));
    }

    // =========================================================================
    // HookOutput comprehensive serialization tests
    // =========================================================================

    #[test]
    fn test_hook_output_pre_tool_use_allow_format() {
        let output = HookOutput::pre_tool_use_allow(Some("test reason".into()), None);
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
        let output = HookOutput::pre_tool_use_deny("not allowed".into());
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
        let output = HookOutput::pre_tool_use_allow(None, Some(modified.clone()));
        let json = serde_json::to_value(&output).unwrap();

        let specific = &json["hookSpecificOutput"];
        assert_eq!(specific["updatedInput"], modified);
    }

    #[test]
    fn test_hook_output_block_format() {
        let output = HookOutput::block("session terminated".into());
        let json = serde_json::to_value(&output).unwrap();

        assert_eq!(json["continue"], false);
        assert_eq!(json["stopReason"], "session terminated");
    }

    #[test]
    fn test_hook_output_post_tool_use_format() {
        let output = HookOutput::post_tool_use_allow(Some("additional context".into()));
        let json = serde_json::to_value(&output).unwrap();

        assert_eq!(json["continue"], true);
        let specific = &json["hookSpecificOutput"];
        assert_eq!(specific["hookEventName"], "PostToolUse");
        assert_eq!(specific["additionalContext"], "additional context");
    }

    #[test]
    fn test_hook_output_default() {
        let output = HookOutput::default();
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
        assert_eq!(input.session_id, "s");
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
        assert_eq!(input.session_id, "sess-123");
        assert_eq!(input.cwd, "/home/user");
        assert_eq!(input.permission_mode, "plan");
        assert_eq!(input.tool_name, Some("Write".into()));
        assert_eq!(input.prompt, Some("user prompt".into()));
        assert_eq!(input.stop_hook_active, Some(true));
    }
}
