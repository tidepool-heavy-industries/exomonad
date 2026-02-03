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

pub mod hook;
pub mod mcp;
pub mod service;

use clap::ValueEnum;
use serde::{Deserialize, Serialize};

// Re-export commonly used types
pub use hook::{
    ClaudePreToolUseOutput, ClaudeStopHookOutput, GeminiStopDecision, GeminiStopHookOutput,
    HookInput, HookSpecificOutput, InternalStopHookOutput, PermissionDecision, StopDecision,
};
pub use mcp::{McpError, ToolDefinition};
pub use service::{
    ChatMessage, ContentBlock, GitHubAuthorRef, GitHubDiscussionComment, GitHubIssueRef,
    GitHubLabelRef, GitHubPRRef, GitHubReviewComment, IssueState, ServiceRequest, ServiceResponse,
    StopReason, Tool, Usage,
};

// ============================================================================
// Protocol-level types
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

/// Hook event type for CLI hooks.
/// Includes both Claude-specific and Gemini-specific hook types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum, strum::Display)]
#[strum(serialize_all = "kebab-case")]
pub enum HookEventType {
    /// Before tool execution (can allow/deny/modify)
    PreToolUse,
    /// After tool completion
    PostToolUse,
    /// When a notification is shown
    Notification,
    /// When Claude Code wants to stop (main agent)
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
    /// Gemini: After agent finishes (equivalent to Claude's Stop for main agent)
    AfterAgent,
}
