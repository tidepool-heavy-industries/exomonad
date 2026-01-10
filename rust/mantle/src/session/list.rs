//! Session list command - list all sessions with optional filtering.
//!
//! Returns session summaries as JSON array to stdout.

use serde::Serialize;
use thiserror::Error;

use super::state::StateManager;
use super::types::{SessionMetadata, SessionState};

/// Error type for session list operations.
#[derive(Debug, Error)]
pub enum ListError {
    #[error("State error: {0}")]
    State(#[from] super::state::StateError),
}

/// Configuration for session list command.
#[derive(Debug, Clone, Default)]
pub struct ListConfig {
    /// Filter by session state
    pub state: Option<SessionState>,

    /// Include only sessions with this parent
    pub parent_id: Option<String>,

    /// Limit number of results
    pub limit: Option<usize>,
}

/// Summary output for a single session in list view.
#[derive(Debug, Clone, Serialize)]
pub struct SessionSummary {
    /// Session ID
    pub session_id: String,

    /// Semantic slug
    pub slug: String,

    /// Git branch name
    pub branch: String,

    /// Session status
    pub status: String,

    /// Creation timestamp (ISO 8601)
    pub created_at: String,

    /// Last update timestamp (ISO 8601)
    pub updated_at: String,

    /// Total cost (USD)
    pub total_cost_usd: f64,

    /// Total turns
    pub total_turns: i64,

    /// Has parent (is forked)
    pub is_fork: bool,

    /// Number of children
    pub child_count: usize,
}

impl From<&SessionMetadata> for SessionSummary {
    fn from(meta: &SessionMetadata) -> Self {
        Self {
            session_id: meta.id.clone(),
            slug: meta.slug.clone(),
            branch: meta.branch.clone(),
            status: meta.state.to_string(),
            created_at: meta.created_at.to_rfc3339(),
            updated_at: meta.updated_at.to_rfc3339(),
            total_cost_usd: meta.total_cost_usd,
            total_turns: meta.total_turns,
            is_fork: meta.parent_id.is_some(),
            child_count: meta.child_ids.len(),
        }
    }
}

/// Output for session list command.
#[derive(Debug, Clone, Serialize)]
pub struct ListOutput {
    /// Session summaries
    pub sessions: Vec<SessionSummary>,

    /// Total count (before limit)
    pub total: usize,
}

/// List sessions with optional filtering.
pub fn list_sessions(state_manager: &StateManager, config: &ListConfig) -> Result<ListOutput, ListError> {
    let state = state_manager.load()?;

    let mut sessions: Vec<_> = state
        .sessions
        .values()
        .filter(|meta| {
            // Filter by state if specified
            if let Some(ref filter_state) = config.state {
                if &meta.state != filter_state {
                    return false;
                }
            }

            // Filter by parent if specified
            if let Some(ref parent) = config.parent_id {
                if meta.parent_id.as_ref() != Some(parent) {
                    return false;
                }
            }

            true
        })
        .collect();

    // Sort by updated_at descending (most recent first)
    sessions.sort_by(|a, b| b.updated_at.cmp(&a.updated_at));

    let total = sessions.len();

    // Apply limit if specified
    if let Some(limit) = config.limit {
        sessions.truncate(limit);
    }

    let summaries = sessions.iter().map(|m| SessionSummary::from(*m)).collect();

    Ok(ListOutput {
        sessions: summaries,
        total,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_session_summary_from_metadata() {
        let mut meta = SessionMetadata::new(
            "test-123".to_string(),
            "implement/auth".to_string(),
            "implement/auth-abc123".to_string(),
            PathBuf::from("/tmp/worktree"),
            "sonnet".to_string(),
        );
        meta.parent_id = Some("parent-456".to_string());
        meta.child_ids = vec!["child-1".to_string(), "child-2".to_string()];

        let summary = SessionSummary::from(&meta);

        assert_eq!(summary.session_id, "test-123");
        assert_eq!(summary.slug, "implement/auth");
        assert_eq!(summary.status, "pending");
        assert!(summary.is_fork);
        assert_eq!(summary.child_count, 2);
    }

    #[test]
    fn test_list_output_serialization() {
        let output = ListOutput {
            sessions: vec![],
            total: 0,
        };

        let json = serde_json::to_string(&output).unwrap();
        assert!(json.contains("\"sessions\":[]"));
        assert!(json.contains("\"total\":0"));
    }
}
