pub mod acp_client;
pub mod acp_registry;
pub mod agent_control;
pub mod agent_resolver;
pub mod claude_session_registry;
pub mod command;
pub mod copilot_review;
pub mod delivery;
pub mod event_log;
pub mod event_queue;
pub mod external;
pub mod file_pr;
pub mod filesystem;
pub mod git;
pub mod git_worktree;
pub mod github;
pub mod github_poller;
pub mod inbox_watcher;
pub mod local;
pub mod log;
pub mod merge_pr;
pub mod mutex_registry;
pub mod repo;
pub mod resilience;
pub mod secrets;
pub mod supervisor_registry;
pub mod synthetic_members;
pub mod tmux_events;
pub mod tmux_ipc;

pub use self::acp_registry::AcpRegistry;
pub use self::agent_control::{
    resolve_role_context_path, resolve_working_dir, resolve_worktree_from_tab, AgentControlService,
    AgentInfo, AgentType, BatchCleanupResult, BatchSpawnResult, SpawnOptions, SpawnResult,
};
pub use self::agent_resolver::{AgentIdentityRecord, AgentResolver};
pub use self::claude_session_registry::ClaudeSessionRegistry;
pub use self::event_log::EventLog;
pub use self::event_queue::EventQueue;
pub use self::filesystem::FileSystemService;
pub use self::git_worktree::GitWorktreeService;
pub use self::github::GitHubClient;
pub use self::mutex_registry::MutexRegistry;
pub use self::secrets::Secrets;
pub use self::supervisor_registry::SupervisorRegistry;
use claude_teams_bridge::TeamRegistry;
use std::sync::Arc;
use thiserror::Error;

/// Shared services context, constructed once and threaded through the app.
///
/// Contains all shared registries and infrastructure services. Handlers and
/// services read what they need from this at construction time.
#[derive(Clone)]
pub struct Services {
    pub github_client: Option<Arc<GitHubClient>>,
    pub event_log: Option<Arc<EventLog>>,
    pub team_registry: Arc<TeamRegistry>,
    pub acp_registry: Arc<AcpRegistry>,
    pub supervisor_registry: Arc<SupervisorRegistry>,
    pub claude_session_registry: Arc<ClaudeSessionRegistry>,
    pub agent_resolver: Arc<AgentResolver>,
    pub event_queue: Arc<EventQueue>,
    pub mutex_registry: Arc<MutexRegistry>,
}

#[cfg(test)]
impl Services {
    /// Construct a Services with empty/default registries for testing.
    pub fn test() -> Self {
        Self {
            github_client: None,
            event_log: None,
            team_registry: Arc::new(TeamRegistry::new()),
            acp_registry: Arc::new(AcpRegistry::new()),
            supervisor_registry: Arc::new(SupervisorRegistry::new()),
            claude_session_registry: Arc::new(ClaudeSessionRegistry::new()),
            agent_resolver: Arc::new(AgentResolver::empty()),
            event_queue: Arc::new(EventQueue::new()),
            mutex_registry: Arc::new(MutexRegistry::new()),
        }
    }
}

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
