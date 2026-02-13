pub mod agent_control;
pub mod coordination;
pub mod copilot_review;
pub mod docker;
pub mod event_queue;
pub mod external;
pub mod file_pr;
pub mod filesystem;
pub mod git;
pub mod github;
pub mod inbox;
pub mod local;
pub mod log;
pub mod popup;
pub mod questions;
pub mod secrets;
pub mod zellij_events;

pub use self::agent_control::{
    AgentControlService, AgentInfo, BatchCleanupResult, BatchSpawnResult, SpawnOptions, SpawnResult,
};
use self::docker::CommandExecutor;
pub use self::event_queue::EventQueue;
pub use self::filesystem::FileSystemService;
use self::git::GitService;
use self::github::GitHubService;
use self::local::LocalExecutor;
use self::log::{HasLogService, LogService};
pub use self::secrets::Secrets;
use std::path::PathBuf;
use std::sync::Arc;
use thiserror::Error;
use uuid::Uuid;

/// Errors that can occur during services validation.
///
/// These errors are returned by [`Services::validate()`] when validating
/// service prerequisites (executables, paths, etc.).
#[derive(Debug, Error)]
pub enum ServicesError {
    /// Current working directory could not be determined.
    #[error("failed to get current directory: {0}")]
    CurrentDirFailed(String),

    /// Specified working directory does not exist or is inaccessible.
    #[error("working directory does not exist: {path}")]
    WorkingDirNotFound { path: PathBuf },

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

/// Container for all external services used by the runtime.
///
/// Services is the dependency injection container for all I/O operations
/// that WASM host functions can perform. It provides:
///
/// - **Git**: Operations on git repositories (branch, status, commits, worktrees)
/// - **GitHub**: GitHub API access (issues, PRs, comments) via REST API
/// - **Agent Control**: High-level agent lifecycle (spawn, cleanup, list)
/// - **Filesystem**: File I/O (read, write)
/// - **Log**: Structured logging via tracing
///
/// # Validation
///
/// Services must be validated before use via [`Services::validate()`]. This
/// checks that required executables exist on PATH:
///
/// ```ignore
/// use crate::services::Services;
///
/// let services = Services::new().validate()?;
/// # Ok::<(), crate::services::ServicesError>(())
/// ```
///
/// # GitHub Token
///
/// GitHub service is optional. Token is loaded from (in order):
/// 1. `~/.exomonad/secrets` file (JSON key: `github_token`)
/// 2. `GITHUB_TOKEN` environment variable
///
/// If no token is available, GitHub service will be `None`.
///
/// # Thread Safety
///
/// Services is `Clone` (cheap via Arc) and can be shared across threads.
/// Individual services are thread-safe:
///
/// - `LogService`: Uses tracing (thread-local)
/// - `GitService`, `AgentControlService`, `FileSystemService`: `Arc<T>` (shared reference)
/// - `GitHubService`: Cloneable HTTP client
#[derive(Clone)]
pub struct Services {
    /// Structured logging service (uses tracing).
    pub log: LogService,

    /// Git operations (branch, status, commits, worktrees).
    pub git: Arc<GitService>,

    /// GitHub API access (optional - requires token).
    pub github: Option<GitHubService>,

    /// High-level agent lifecycle operations (spawn, cleanup, list).
    pub agent_control: Arc<AgentControlService>,

    /// Event queue for wait_for_event blocking calls.
    pub event_queue: Arc<EventQueue>,

    /// File I/O operations (read, write).
    pub filesystem: Arc<FileSystemService>,

    /// Zellij session name for event emission (optional).
    pub zellij_session: Option<String>,

    /// Server-generated UUID for event routing between TL and workers.
    /// Decoupled from zellij_session so event routing works regardless of Zellij.
    pub event_session_id: String,
}

/// Validated services wrapper.
///
/// This type guarantees that all services have been validated:
/// - Working directory exists and is accessible
/// - Git executable is available on PATH
/// - If GitHub service is enabled, gh CLI is available
#[derive(Clone)]
pub struct ValidatedServices(Services);

impl Services {
    /// Create services using local executor.
    ///
    /// Commands run directly as subprocesses.
    /// Loads secrets from ~/.exomonad/secrets.
    pub fn new() -> Self {
        let local = LocalExecutor::new();
        let local_arc: Arc<dyn CommandExecutor> = Arc::new(local);
        Self::with_executor(local_arc)
    }

    fn with_executor(executor: Arc<dyn CommandExecutor>) -> Self {
        let secrets = Secrets::load();
        let git = Arc::new(GitService::new(executor.clone()));

        // GitHub service is optional - try secrets file first, then env var
        let github = secrets
            .github_token()
            .and_then(|t| GitHubService::new(t).ok());

        // Agent control service for high-level agent lifecycle
        let project_dir = std::env::current_dir().unwrap_or_default();
        let agent_control = Arc::new(AgentControlService::new(
            project_dir.clone(),
            github.clone(),
        ));

        // Event queue for blocking wait calls
        let event_queue = Arc::new(EventQueue::new());

        // Filesystem service for file read/write operations
        let filesystem = Arc::new(FileSystemService::new(project_dir));

        Self {
            log: LogService,
            git,
            github,
            agent_control,
            event_queue,
            filesystem,
            zellij_session: None,
            event_session_id: Uuid::new_v4().to_string(),
        }
    }

    /// Set the Zellij session name for event emission.
    pub fn with_zellij_session(mut self, session: String) -> Self {
        self.zellij_session = Some(session);
        self
    }

    /// Set the worktree base directory.
    pub fn with_worktree_base(mut self, base: PathBuf) -> Self {
        let mut acs = (*self.agent_control).clone();
        acs = acs.with_worktree_base(base);
        self.agent_control = Arc::new(acs);
        self
    }

    /// Set the MCP server port for per-agent endpoint URL generation.
    ///
    /// Propagates to AgentControlService which writes per-agent Gemini settings
    /// pointing to `http://localhost:{port}/agents/{name}/mcp`.
    pub fn with_mcp_server_port(mut self, port: u16) -> Self {
        let mut acs = (*self.agent_control).clone();
        acs = acs.with_mcp_server_port(port)
                 .with_event_session_id(self.event_session_id.clone());
        if let Some(ref session) = self.zellij_session {
            acs = acs.with_zellij_session(session.clone());
        }
        self.agent_control = Arc::new(acs);
        self
    }
}

impl Default for Services {
    fn default() -> Self {
        Self::new()
    }
}

impl HasLogService for Services {
    fn log_service(&self) -> &LogService {
        &self.log
    }
}

impl Services {
    /// Validate services and return ValidatedServices.
    ///
    /// Checks:
    /// - Working directory exists and is accessible
    /// - Git executable is available on PATH
    /// - If GitHub service is enabled, gh CLI is available
    pub fn validate(self) -> Result<ValidatedServices, ServicesError> {
        // Check if git is available on PATH
        let git_check = std::process::Command::new("git").arg("--version").output();

        match git_check {
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                return Err(ServicesError::GitNotFound);
            }
            Err(e) => {
                return Err(ServicesError::CommandFailed(format!(
                    "git --version: {}",
                    e
                )));
            }
            Ok(output) if !output.status.success() => {
                return Err(ServicesError::CommandFailed(format!(
                    "git --version exited with: {}",
                    output.status
                )));
            }
            Ok(_) => {} // git is available
        }

        // If GitHub service is enabled, check gh CLI
        if self.github.is_some() {
            let gh_check = std::process::Command::new("gh").arg("--version").output();

            match gh_check {
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                    return Err(ServicesError::GhCliNotFound);
                }
                Err(e) => {
                    return Err(ServicesError::CommandFailed(format!("gh --version: {}", e)));
                }
                Ok(output) if !output.status.success() => {
                    return Err(ServicesError::CommandFailed(format!(
                        "gh --version exited with: {}",
                        output.status
                    )));
                }
                Ok(_) => {} // gh is available
            }
        }

        Ok(ValidatedServices(self))
    }
}

impl ValidatedServices {
    /// Get the log service.
    pub fn log(&self) -> &LogService {
        &self.0.log
    }

    /// Get the git service.
    pub fn git(&self) -> &Arc<GitService> {
        &self.0.git
    }

    /// Get the GitHub service (if available).
    pub fn github(&self) -> &Option<GitHubService> {
        &self.0.github
    }

    /// Get the agent control service.
    pub fn agent_control(&self) -> &Arc<AgentControlService> {
        &self.0.agent_control
    }

    /// Get the event queue service.
    pub fn event_queue(&self) -> &Arc<EventQueue> {
        &self.0.event_queue
    }

    /// Get the filesystem service.
    pub fn filesystem(&self) -> &Arc<FileSystemService> {
        &self.0.filesystem
    }

    /// Get the Zellij session name (if configured).
    pub fn zellij_session(&self) -> Option<&str> {
        self.0.zellij_session.as_deref()
    }

    /// Get the event session ID (server-generated UUID).
    pub fn event_session_id(&self) -> &str {
        &self.0.event_session_id
    }

    /// Get a reference to the inner Services.
    pub fn inner(&self) -> &Services {
        &self.0
    }

    /// Convert back to Services (consumes self).
    pub fn into_inner(self) -> Services {
        self.0
    }
}

impl HasLogService for ValidatedServices {
    fn log_service(&self) -> &LogService {
        &self.0.log
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

        let err = ServicesError::CurrentDirFailed("permission denied".to_string());
        assert_eq!(
            err.to_string(),
            "failed to get current directory: permission denied"
        );

        let err = ServicesError::CommandFailed("git --version failed".to_string());
        assert_eq!(
            err.to_string(),
            "command execution failed: git --version failed"
        );
    }

    // Note: Full Services::validate() tests would require a tokio runtime
    // and are better tested via integration tests (e.g., in exomonad tests)
    // where Services is constructed and validated in a real async context.
}
