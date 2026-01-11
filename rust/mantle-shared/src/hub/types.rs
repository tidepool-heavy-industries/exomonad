//! Shared types for mantle-hub API.
//!
//! Two-level entity model:
//! - **Session**: An orchestration run (tree of nodes)
//! - **Node**: An individual Claude Code execution within a session

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

use crate::events::StreamEvent;

// ============================================================================
// State Enums
// ============================================================================

/// Session state (derived from node states).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SessionState {
    /// At least one node is running
    Running,
    /// All nodes completed successfully
    Completed,
    /// At least one node failed (and none running)
    Failed,
}

impl std::fmt::Display for SessionState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SessionState::Running => write!(f, "running"),
            SessionState::Completed => write!(f, "completed"),
            SessionState::Failed => write!(f, "failed"),
        }
    }
}

impl std::str::FromStr for SessionState {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "running" => Ok(SessionState::Running),
            "completed" => Ok(SessionState::Completed),
            "failed" => Ok(SessionState::Failed),
            _ => Err(format!("Unknown session state: {}", s)),
        }
    }
}

/// Node state (individual Claude run).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum NodeState {
    Pending,
    Running,
    Completed,
    Failed,
    Cancelled,
}

impl std::fmt::Display for NodeState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NodeState::Pending => write!(f, "pending"),
            NodeState::Running => write!(f, "running"),
            NodeState::Completed => write!(f, "completed"),
            NodeState::Failed => write!(f, "failed"),
            NodeState::Cancelled => write!(f, "cancelled"),
        }
    }
}

impl std::str::FromStr for NodeState {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "pending" => Ok(NodeState::Pending),
            "running" => Ok(NodeState::Running),
            "completed" => Ok(NodeState::Completed),
            "failed" => Ok(NodeState::Failed),
            "cancelled" => Ok(NodeState::Cancelled),
            _ => Err(format!("Unknown node state: {}", s)),
        }
    }
}

// ============================================================================
// Session Types (the tree container)
// ============================================================================

/// Session registration request (mantle → hub).
///
/// Creates a new session with its root node atomically.
/// Returns SessionInfo with the generated session_id and node_id.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionRegister {
    pub branch: String,
    pub worktree: PathBuf,
    pub prompt: String,
    pub model: String,
}

/// Add a child node to an existing session.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeRegister {
    pub branch: String,
    pub worktree: PathBuf,
    pub prompt: String,
    pub model: String,
    /// Parent node ID (must exist in this session)
    pub parent_node_id: String,
}

/// Session info (the tree).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionInfo {
    pub id: String,
    /// Display name (root node's branch)
    pub name: String,
    pub state: SessionState,
    pub created_at: String,
    pub updated_at: String,
    /// Number of nodes in this session
    pub node_count: i64,
}

/// Session with all its nodes (for GET /api/sessions/{id}).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionWithNodes {
    pub session: SessionInfo,
    pub nodes: Vec<NodeInfo>,
}

// ============================================================================
// Node Types (individual Claude runs)
// ============================================================================

/// Node info (individual Claude run).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeInfo {
    pub id: String,
    pub session_id: String,
    pub parent_node_id: Option<String>,
    pub branch: String,
    pub worktree: PathBuf,
    pub prompt: String,
    pub model: String,
    pub state: NodeState,
    pub created_at: String,
    pub updated_at: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<NodeResult>,
}

/// Node result (container → hub via socket).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeResult {
    pub node_id: String,
    pub exit_code: i32,
    pub is_error: bool,
    #[serde(default)]
    pub result_text: Option<String>,
    #[serde(default)]
    pub structured_output: Option<serde_json::Value>,
    pub total_cost_usd: f64,
    pub num_turns: i64,
    /// Claude Code's internal session ID
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

// ============================================================================
// Event Types (StreamEvent storage)
// ============================================================================

/// Stored event (StreamEvent with metadata).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeEvent {
    pub id: i64,
    pub node_id: String,
    pub event_type: String,
    pub event: StreamEvent,
    pub timestamp: String,
}

// ============================================================================
// WebSocket Events
// ============================================================================

/// WebSocket events (hub → frontend).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum HubEvent {
    /// New session created
    SessionCreated { session: SessionInfo },
    /// Session state changed
    SessionUpdated { session: SessionInfo },
    /// New node added to session
    NodeCreated { node: NodeInfo },
    /// Node state changed
    NodeUpdated { node: NodeInfo },
    /// Node completed with result
    NodeCompleted { node_id: String, result: NodeResult },
    /// Node failed with error
    NodeFailed { node_id: String, error: String },
    /// Real-time stream event from running node
    NodeEvent {
        session_id: String,
        node_id: String,
        event: StreamEvent,
        timestamp: String,
    },
}

// ============================================================================
// Graph Visualization
// ============================================================================

/// Graph data for frontend visualization (single session's tree).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphData {
    pub session: SessionInfo,
    pub nodes: Vec<GraphNode>,
    pub edges: Vec<GraphEdge>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphNode {
    pub id: String,
    pub branch: String,
    pub state: NodeState,
    pub prompt: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent_id: Option<String>,
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

// ============================================================================
// API Response Types
// ============================================================================

/// Response from POST /api/sessions (creates session + root node).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionCreateResponse {
    pub session: SessionInfo,
    pub root_node: NodeInfo,
}

/// Response from POST /api/sessions/{sid}/nodes (creates child node).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeCreateResponse {
    pub node: NodeInfo,
}
