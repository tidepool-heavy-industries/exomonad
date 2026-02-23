pub mod agent_control;
pub mod claude_session_registry;
pub mod coordination;
pub mod copilot_review;
pub mod docker;
pub mod event_log;
pub mod event_queue;
pub mod external;
pub mod file_pr;
pub mod filesystem;
pub mod git;
pub mod git_worktree;
pub mod github;
pub mod github_poller;
pub mod inbox;
pub mod inbox_watcher;
pub mod local;
pub mod log;
pub mod merge_pr;
pub mod popup;
pub mod questions;
pub mod repo;
pub mod secrets;
pub mod synthetic_members;
pub mod team_registry;
pub mod teams_mailbox;
pub mod zellij_events;

pub use self::agent_control::{
    resolve_parent_tab_name, AgentControlService, AgentInfo, BatchCleanupResult, BatchSpawnResult,
    SpawnOptions, SpawnResult,
};
pub use self::event_log::EventLog;
pub use self::event_queue::EventQueue;
pub use self::filesystem::FileSystemService;
pub use self::git_worktree::GitWorktreeService;
pub use self::secrets::Secrets;
use thiserror::Error;

/// Errors that can occur during services validation.
///
/// These errors are returned by [`validate_git()`] and [`validate_gh_cli()`] when validating
/// service prerequisites (executables, paths, etc.).
#[derive(Debug, Error)]
pub enum ServicesError {
    /// Git executable not found on PATH (required for git operations).
    #[error("git executable not found on PATH")]
    GitNotFound,

    /// GitHub CLI (gh) not found on PATH (required when GitHub service is enabled).
    #[error("gh CLI not found on PATH (required when GitHub service is enabled)")]
    GhCliNotFound,

    /// Command execution failed during validation.
    #[error("command execution failed: {0}")]
    CommandFailed(String),
}

/// Validate that git is available on PATH.
pub fn validate_git() -> Result<(), ServicesError> {
    let git_check = std::process::Command::new("git").arg("--version").output();
    match git_check {
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Err(ServicesError::GitNotFound),
        Err(e) => Err(ServicesError::CommandFailed(format!(
            "git --version: {}",
            e
        ))),
        Ok(output) if !output.status.success() => Err(ServicesError::CommandFailed(format!(
            "git --version exited with: {}",
            output.status
        ))),
        Ok(_) => Ok(()),
    }
}

/// Validate that gh CLI is available on PATH.
pub fn validate_gh_cli() -> Result<(), ServicesError> {
    let gh_check = std::process::Command::new("gh").arg("--version").output();
    match gh_check {
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Err(ServicesError::GhCliNotFound),
        Err(e) => Err(ServicesError::CommandFailed(format!("gh --version: {}", e))),
        Ok(output) if !output.status.success() => Err(ServicesError::CommandFailed(format!(
            "gh --version exited with: {}",
            output.status
        ))),
        Ok(_) => Ok(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_services_error_display() {
        // Test error message formatting
        let err = ServicesError::GitNotFound;
        assert_eq!(err.to_string(), "git executable not found on PATH");

        let err = ServicesError::GhCliNotFound;
        assert_eq!(
            err.to_string(),
            "gh CLI not found on PATH (required when GitHub service is enabled)"
        );

        let err = ServicesError::CommandFailed("git --version failed".to_string());
        assert_eq!(
            err.to_string(),
            "command execution failed: git --version failed"
        );
    }
}
