use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Standardized telemetry event types for structured logging.
/// These events are used to track the lifecycle of agents, hooks, and external interactions.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum TelemetryEvent {
    /// An agent was spawned in a new worktree
    AgentSpawned {
        agent_id: String,
        issue_id: String,
        branch_name: String,
        worktree_path: String,
    },
    /// An agent stopped execution
    AgentStopped {
        agent_id: String,
        reason: String,
        duration_secs: Option<u64>,
    },
    /// A hook event was received from the CLI
    HookReceived {
        hook_type: String,
        session_id: Option<String>,
        tool_name: Option<String>,
    },
    /// A decision was made on a hook (allow/block)
    HookDecision {
        hook_type: String,
        decision: HookDecision,
        reason: Option<String>,
    },
    /// A host function (tool) was called by the WASM guest
    HostFunctionCalled {
        function_name: String,
        duration_ms: u64,
        success: bool,
    },
    /// A GitHub API call was made
    GithubApiCall {
        endpoint: String,
        method: String,
        status_code: Option<u16>,
        duration_ms: u64,
    },
    /// A Git command was executed
    GitCommand {
        command: String,
        exit_code: Option<i32>,
        duration_ms: u64,
    },
    /// An error occurred during execution
    ErrorOccurred {
        code: String,
        message: String,
        context: HashMap<String, String>,
    },
}

/// Decision made by a hook handler regarding a specific event.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum HookDecision {
    /// Allow the hook to proceed.
    Allow,
    /// Block the hook action.
    Block,
}

impl TelemetryEvent {
    /// Returns the event name as a string
    pub fn name(&self) -> &'static str {
        match self {
            TelemetryEvent::AgentSpawned { .. } => "agent_spawned",
            TelemetryEvent::AgentStopped { .. } => "agent_stopped",
            TelemetryEvent::HookReceived { .. } => "hook_received",
            TelemetryEvent::HookDecision { .. } => "hook_decision",
            TelemetryEvent::HostFunctionCalled { .. } => "host_function_called",
            TelemetryEvent::GithubApiCall { .. } => "github_api_call",
            TelemetryEvent::GitCommand { .. } => "git_command",
            TelemetryEvent::ErrorOccurred { .. } => "error_occurred",
        }
    }
}
