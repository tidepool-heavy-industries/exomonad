//! Session info command - query session metadata.
//!
//! Returns session details as JSON to stdout for Haskell consumption.

use serde::Serialize;
use std::path::PathBuf;
use thiserror::Error;

use super::state::StateManager;
use super::types::SessionMetadata;

/// Error type for session info operations.
#[derive(Debug, Error)]
pub enum InfoError {
    #[error("Session not found: {0}")]
    NotFound(String),

    #[error("State error: {0}")]
    State(#[from] super::state::StateError),
}

/// Output type for `session info` command.
///
/// This is the JSON format expected by the Haskell executor.
/// Field names match Haskell's `SessionMetadata` after camelToSnake transformation.
#[derive(Debug, Clone, Serialize)]
pub struct SessionInfoOutput {
    /// Session ID
    pub session_id: String,

    /// Git branch name
    pub branch: String,

    /// Worktree path
    pub worktree: PathBuf,

    /// Parent session ID (for forked sessions)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent_session: Option<String>,

    /// Child session IDs
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub child_sessions: Vec<String>,

    /// Session status: "pending", "running", "completed", "failed", "cancelled"
    pub status: String,

    /// Creation timestamp (ISO 8601)
    pub created_at: String,

    /// Last update timestamp (ISO 8601)
    pub updated_at: String,

    /// Last exit code
    pub last_exit_code: i32,

    /// Total cost across all runs (USD)
    pub total_cost_usd: f64,
}

impl From<&SessionMetadata> for SessionInfoOutput {
    fn from(meta: &SessionMetadata) -> Self {
        Self {
            session_id: meta.id.clone(),
            branch: meta.branch.clone(),
            worktree: meta.worktree.clone(),
            parent_session: meta.parent_id.clone(),
            child_sessions: meta.child_ids.clone(),
            status: meta.state.to_string(),
            created_at: meta.created_at.to_rfc3339(),
            updated_at: meta.updated_at.to_rfc3339(),
            last_exit_code: meta.last_exit_code.unwrap_or(0),
            total_cost_usd: meta.total_cost_usd,
        }
    }
}

/// Get session info by ID.
///
/// Returns the session metadata as JSON-serializable output.
pub fn session_info(state_manager: &StateManager, session_id: &str) -> Result<SessionInfoOutput, InfoError> {
    let state = state_manager.load()?;

    let meta = state
        .sessions
        .get(session_id)
        .ok_or_else(|| InfoError::NotFound(session_id.to_string()))?;

    Ok(SessionInfoOutput::from(meta))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::types::SessionMetadata;
    use std::path::PathBuf;

    #[test]
    fn test_session_info_output_from_metadata() {
        let meta = SessionMetadata::new(
            "test-123".to_string(),
            "implement/auth".to_string(),
            "implement/auth-abc123".to_string(),
            PathBuf::from("/tmp/worktree"),
            "sonnet".to_string(),
        );

        let output = SessionInfoOutput::from(&meta);

        assert_eq!(output.session_id, "test-123");
        assert_eq!(output.branch, "implement/auth-abc123");
        assert_eq!(output.status, "pending");
        assert_eq!(output.last_exit_code, 0);
        assert!(output.parent_session.is_none());
        assert!(output.child_sessions.is_empty());
    }

    #[test]
    fn test_session_info_output_serialization() {
        let meta = SessionMetadata::new(
            "test-123".to_string(),
            "test".to_string(),
            "test-abc123".to_string(),
            PathBuf::from("/tmp"),
            "sonnet".to_string(),
        );

        let output = SessionInfoOutput::from(&meta);
        let json = serde_json::to_string(&output).unwrap();

        // Verify field names match Haskell expectations
        assert!(json.contains("\"session_id\":\"test-123\""));
        assert!(json.contains("\"status\":\"pending\""));
        assert!(json.contains("\"created_at\":"));
        assert!(json.contains("\"last_exit_code\":0"));
    }
}
