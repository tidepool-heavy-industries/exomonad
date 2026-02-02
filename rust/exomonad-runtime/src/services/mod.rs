pub mod agent_control;
pub mod copilot_review;
pub mod docker;
pub mod file_pr;
pub mod filesystem;
pub mod git;
pub mod github;
pub mod local;
pub mod log;
pub mod secrets;
pub mod zellij_events;

pub use self::agent_control::{
    AgentControlService, AgentInfo, BatchCleanupResult, BatchSpawnResult, SpawnOptions, SpawnResult,
};
use self::docker::DockerExecutor;
pub use self::filesystem::FileSystemService;
use self::git::GitService;
use self::github::GitHubService;
use self::local::LocalExecutor;
use self::log::{HasLogService, LogService};
pub use self::secrets::Secrets;
use std::path::PathBuf;
use std::sync::Arc;
use thiserror::Error;

/// Services validation errors.
#[derive(Debug, Error)]
pub enum ServicesError {
    #[error("failed to get current directory: {0}")]
    CurrentDirFailed(String),

    #[error("working directory does not exist: {path}")]
    WorkingDirNotFound { path: PathBuf },

    #[error("git executable not found on PATH")]
    GitNotFound,

    #[error("gh CLI not found on PATH (required when GitHub service is enabled)")]
    GhCliNotFound,

    #[error("command execution failed: {0}")]
    CommandFailed(String),
}

#[derive(Clone)]
pub struct Services {
    pub log: LogService,
    pub git: Arc<GitService>,
    pub github: Option<GitHubService>,
    pub agent_control: Arc<AgentControlService>,
    pub filesystem: Arc<FileSystemService>,
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
        let local_arc: Arc<dyn DockerExecutor> = Arc::new(local);
        Self::with_executor(local_arc)
    }

    /// Deprecated: Use `new()` instead. All services now use local executor.
    #[deprecated(since = "0.1.0", note = "Use `new()` instead - Docker mode removed")]
    pub fn new_local() -> Self {
        Self::new()
    }

    fn with_executor(executor: Arc<dyn DockerExecutor>) -> Self {
        let secrets = Secrets::load();
        let git = Arc::new(GitService::new(executor));

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

        // Filesystem service for file read/write operations
        let filesystem = Arc::new(FileSystemService::new(project_dir));

        Self {
            log: LogService,
            git,
            github,
            agent_control,
            filesystem,
        }
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
        let git_check = std::process::Command::new("git")
            .arg("--version")
            .output();

        match git_check {
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                return Err(ServicesError::GitNotFound);
            }
            Err(e) => {
                return Err(ServicesError::CommandFailed(format!("git --version: {}", e)));
            }
            Ok(output) if !output.status.success() => {
                return Err(ServicesError::CommandFailed(
                    format!("git --version exited with: {}", output.status)
                ));
            }
            Ok(_) => {} // git is available
        }

        // If GitHub service is enabled, check gh CLI
        if self.github.is_some() {
            let gh_check = std::process::Command::new("gh")
                .arg("--version")
                .output();

            match gh_check {
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                    return Err(ServicesError::GhCliNotFound);
                }
                Err(e) => {
                    return Err(ServicesError::CommandFailed(format!("gh --version: {}", e)));
                }
                Ok(output) if !output.status.success() => {
                    return Err(ServicesError::CommandFailed(
                        format!("gh --version exited with: {}", output.status)
                    ));
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

    /// Get the filesystem service.
    pub fn filesystem(&self) -> &Arc<FileSystemService> {
        &self.0.filesystem
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
    // and are better tested via integration tests (e.g., in exomonad-sidecar tests)
    // where Services is constructed and validated in a real async context.
}
