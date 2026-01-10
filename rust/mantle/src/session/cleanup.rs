//! Session cleanup command - remove old sessions and their worktrees.
//!
//! Handles cleanup of completed, failed, or cancelled sessions.

use serde::Serialize;
use std::path::PathBuf;
use thiserror::Error;

use super::state::StateManager;
use super::types::SessionState;
use super::worktree::WorktreeManager;

/// Error type for session cleanup operations.
#[derive(Debug, Error)]
pub enum CleanupError {
    #[error("State error: {0}")]
    State(#[from] super::state::StateError),

    #[error("Worktree error: {0}")]
    Worktree(#[from] super::worktree::WorktreeError),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

/// Configuration for session cleanup command.
#[derive(Debug, Clone, Default)]
pub struct CleanupConfig {
    /// Only clean up completed sessions
    pub completed_only: bool,

    /// Only clean up failed sessions
    pub failed_only: bool,

    /// Clean up all terminal states (completed, failed, cancelled)
    pub all_terminal: bool,

    /// Dry run - show what would be deleted without deleting
    pub dry_run: bool,

    /// Specific session IDs to clean up
    pub session_ids: Vec<String>,
}

/// Information about a cleaned up session.
#[derive(Debug, Clone, Serialize)]
pub struct CleanedSession {
    /// Session ID
    pub session_id: String,

    /// Branch that was deleted
    pub branch: String,

    /// Worktree path that was removed
    pub worktree: PathBuf,

    /// Whether the worktree existed and was removed
    pub worktree_removed: bool,

    /// Whether the branch was removed
    pub branch_removed: bool,
}

/// Output for session cleanup command.
#[derive(Debug, Clone, Serialize)]
pub struct CleanupOutput {
    /// Sessions that were cleaned up
    pub cleaned: Vec<CleanedSession>,

    /// Sessions that were skipped (e.g., still running)
    pub skipped: Vec<String>,

    /// Whether this was a dry run
    pub dry_run: bool,

    /// Total space reclaimed (placeholder - not implemented)
    pub space_reclaimed_bytes: u64,
}

/// Clean up sessions based on configuration.
pub fn cleanup_sessions(
    state_manager: &StateManager,
    worktree_manager: &WorktreeManager,
    config: &CleanupConfig,
) -> Result<CleanupOutput, CleanupError> {
    let mut state = state_manager.load()?;
    let mut cleaned = Vec::new();
    let mut skipped = Vec::new();

    // Determine which sessions to clean
    let session_ids: Vec<String> = if !config.session_ids.is_empty() {
        // Clean specific sessions
        config.session_ids.clone()
    } else {
        // Filter by state
        state
            .sessions
            .values()
            .filter(|meta| should_clean(meta, config))
            .map(|meta| meta.id.clone())
            .collect()
    };

    for session_id in session_ids {
        // Extract data we need before any mutation
        let (branch, worktree, parent_id, session_state) = {
            let Some(meta) = state.sessions.get(&session_id) else {
                skipped.push(session_id);
                continue;
            };
            (
                meta.branch.clone(),
                meta.worktree.clone(),
                meta.parent_id.clone(),
                meta.state,
            )
        };

        // Don't clean running or pending sessions unless explicitly requested
        if matches!(session_state, SessionState::Running | SessionState::Pending)
            && config.session_ids.is_empty()
        {
            skipped.push(session_id);
            continue;
        }

        let mut worktree_removed = false;
        let mut branch_removed = false;

        if !config.dry_run {
            // Remove worktree (force to handle uncommitted changes)
            if worktree.exists() {
                match worktree_manager.remove(&worktree, true) {
                    Ok(_) => worktree_removed = true,
                    Err(e) => {
                        tracing::warn!("Failed to remove worktree {}: {}", worktree.display(), e);
                    }
                }
            }

            // Remove branch (force delete to handle unmerged branches)
            match worktree_manager.delete_branch(&branch, true) {
                Ok(_) => branch_removed = true,
                Err(e) => {
                    tracing::warn!("Failed to delete branch {}: {}", branch, e);
                }
            }

            // Remove from state
            state.sessions.remove(&session_id);
            if let Some(branch_mapping) = state.branch_to_session.get(&branch) {
                if branch_mapping == &session_id {
                    state.branch_to_session.remove(&branch);
                }
            }

            // Remove from parent's child_ids
            if let Some(pid) = parent_id {
                if let Some(parent) = state.sessions.get_mut(&pid) {
                    parent.child_ids.retain(|id| id != &session_id);
                }
            }
        } else {
            // Dry run - just check if worktree exists
            worktree_removed = worktree.exists();
            branch_removed = true; // Assume branch exists
        }

        cleaned.push(CleanedSession {
            session_id,
            branch,
            worktree,
            worktree_removed,
            branch_removed,
        });
    }

    // Save updated state (unless dry run)
    if !config.dry_run && !cleaned.is_empty() {
        state_manager.save(&state)?;
    }

    Ok(CleanupOutput {
        cleaned,
        skipped,
        dry_run: config.dry_run,
        space_reclaimed_bytes: 0, // Not implemented
    })
}

/// Determine if a session should be cleaned based on config.
fn should_clean(meta: &super::types::SessionMetadata, config: &CleanupConfig) -> bool {
    if config.completed_only {
        return meta.state == SessionState::Completed;
    }

    if config.failed_only {
        return meta.state == SessionState::Failed;
    }

    if config.all_terminal {
        return matches!(
            meta.state,
            SessionState::Completed | SessionState::Failed | SessionState::Cancelled
        );
    }

    // Default: clean all terminal states
    matches!(
        meta.state,
        SessionState::Completed | SessionState::Failed | SessionState::Cancelled
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::types::SessionMetadata;

    fn test_meta(id: &str, state: SessionState) -> SessionMetadata {
        let mut meta = SessionMetadata::new(
            id.to_string(),
            "test".to_string(),
            format!("test-{}", id),
            PathBuf::from(format!("/tmp/worktrees/{}", id)),
            "sonnet".to_string(),
        );
        meta.state = state;
        meta
    }

    #[test]
    fn test_should_clean_completed_only() {
        let config = CleanupConfig {
            completed_only: true,
            ..Default::default()
        };

        assert!(should_clean(&test_meta("1", SessionState::Completed), &config));
        assert!(!should_clean(&test_meta("2", SessionState::Failed), &config));
        assert!(!should_clean(&test_meta("3", SessionState::Running), &config));
    }

    #[test]
    fn test_should_clean_all_terminal() {
        let config = CleanupConfig {
            all_terminal: true,
            ..Default::default()
        };

        assert!(should_clean(&test_meta("1", SessionState::Completed), &config));
        assert!(should_clean(&test_meta("2", SessionState::Failed), &config));
        assert!(should_clean(&test_meta("3", SessionState::Cancelled), &config));
        assert!(!should_clean(&test_meta("4", SessionState::Running), &config));
        assert!(!should_clean(&test_meta("5", SessionState::Pending), &config));
    }

    #[test]
    fn test_cleanup_output_serialization() {
        let output = CleanupOutput {
            cleaned: vec![CleanedSession {
                session_id: "test-123".to_string(),
                branch: "test-abc123".to_string(),
                worktree: PathBuf::from("/tmp/wt"),
                worktree_removed: true,
                branch_removed: true,
            }],
            skipped: vec!["running-session".to_string()],
            dry_run: false,
            space_reclaimed_bytes: 0,
        };

        let json = serde_json::to_string(&output).unwrap();
        assert!(json.contains("\"session_id\":\"test-123\""));
        assert!(json.contains("\"worktree_removed\":true"));
        assert!(json.contains("\"skipped\":[\"running-session\"]"));
    }
}
