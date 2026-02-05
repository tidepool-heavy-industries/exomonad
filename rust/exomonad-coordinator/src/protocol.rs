//! Wire protocol for sidecar <-> coordinator communication.
//!
//! Requests arrive via `zellij pipe`, responses return via `cli_pipe_output`.

use serde::{Deserialize, Serialize};

/// Request from sidecar to coordinator (via pipe).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "op")]
pub enum CoordinatorRequest {
    /// Spawn a single agent. Sidecar has already resolved issue details.
    #[serde(rename = "spawn")]
    Spawn {
        request_id: String,
        agent_id: String,
        worktree_path: String,
        branch: String,
        agent_type: String,
        agent_command: String,
        setup_args: SetupArgs,
    },
    /// Cleanup agent(s).
    #[serde(rename = "cleanup")]
    Cleanup {
        request_id: String,
        agent_ids: Vec<String>,
        force: bool,
    },
    /// List all agents with current state.
    #[serde(rename = "list")]
    List { request_id: String },
    /// Get status of a specific agent.
    #[serde(rename = "status")]
    Status {
        request_id: String,
        agent_id: String,
    },
}

/// Fully resolved setup arguments (Haskell builds these, sidecar passes through).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetupArgs {
    pub project_dir: String,
    pub role: String,
    pub start_point: String,
    pub sidecar_path: String,
}

/// Response from coordinator back to sidecar.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "status")]
pub enum CoordinatorResponse {
    #[serde(rename = "ok")]
    Ok {
        request_id: String,
        result: serde_json::Value,
    },
    #[serde(rename = "error")]
    Error {
        request_id: String,
        code: String,
        message: String,
    },
}

impl CoordinatorResponse {
    pub fn ok(request_id: impl Into<String>, result: serde_json::Value) -> Self {
        Self::Ok {
            request_id: request_id.into(),
            result,
        }
    }

    pub fn error(
        request_id: impl Into<String>,
        code: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self::Error {
            request_id: request_id.into(),
            code: code.into(),
            message: message.into(),
        }
    }
}
