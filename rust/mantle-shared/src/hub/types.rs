//! Shared types for mantle-hub API.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

use crate::events::StreamEvent;

/// Session state enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SessionState {
    Pending,
    Running,
    Completed,
    Failed,
    Cancelled,
}

impl std::fmt::Display for SessionState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SessionState::Pending => write!(f, "pending"),
            SessionState::Running => write!(f, "running"),
            SessionState::Completed => write!(f, "completed"),
            SessionState::Failed => write!(f, "failed"),
            SessionState::Cancelled => write!(f, "cancelled"),
        }
    }
}

impl std::str::FromStr for SessionState {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "pending" => Ok(SessionState::Pending),
            "running" => Ok(SessionState::Running),
            "completed" => Ok(SessionState::Completed),
            "failed" => Ok(SessionState::Failed),
            "cancelled" => Ok(SessionState::Cancelled),
            _ => Err(format!("Unknown session state: {}", s)),
        }
    }
}

/// Session registration request (mantle → hub).
///
/// Note: session_id is generated server-side. The hub returns the generated ID
/// in the SessionInfo response.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionRegister {
    pub branch: String,
    pub worktree: PathBuf,
    pub prompt: String,
    pub model: String,
    #[serde(default)]
    pub parent_id: Option<String>,
}

/// Session result (container → hub via socket).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionResult {
    pub session_id: String,
    pub exit_code: i32,
    pub is_error: bool,
    #[serde(default)]
    pub result_text: Option<String>,
    #[serde(default)]
    pub structured_output: Option<serde_json::Value>,
    pub total_cost_usd: f64,
    pub num_turns: i64,
    pub cc_session_id: String,
    pub duration_secs: f64,
    #[serde(default)]
    pub model_usage: HashMap<String, ModelUsage>,
}

/// Model usage statistics.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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

/// Full session info returned by API.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionInfo {
    pub id: String,
    pub branch: String,
    pub worktree: PathBuf,
    pub prompt: String,
    pub model: String,
    pub state: SessionState,
    pub parent_id: Option<String>,
    pub created_at: String,
    pub updated_at: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<SessionResult>,
}

/// WebSocket events (hub → frontend).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum HubEvent {
    /// Session registered and pending
    SessionStarted { session: SessionInfo },
    /// Session metadata updated
    SessionUpdated { session: SessionInfo },
    /// Session completed with result
    SessionCompleted { session_id: String, result: SessionResult },
    /// Session failed with error
    SessionFailed { session_id: String, error: String },
    /// Real-time stream event from running session
    SessionEvent {
        session_id: String,
        event: StreamEvent,
        timestamp: String,
    },
}

/// Graph data for frontend visualization.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphData {
    pub nodes: Vec<GraphNode>,
    pub edges: Vec<GraphEdge>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphNode {
    pub id: String,
    pub branch: String,
    pub state: SessionState,
    pub prompt: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result_text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub structured_output: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub total_cost_usd: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub duration_secs: Option<f64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphEdge {
    pub source: String,
    pub target: String,
}
