pub mod docker;
pub mod git;
pub mod github;
pub mod local;
pub mod log;

use self::docker::{DockerExecutor, DockerService};
use self::git::GitService;
use self::github::GitHubService;
use self::local::LocalExecutor;
use self::log::{HasLogService, LogService};
use std::sync::Arc;

#[derive(Clone)]
pub struct Services {
    pub log: LogService,
    pub git: Arc<GitService>,
    pub github: Option<GitHubService>,
}

impl Services {
    /// Create services using Docker executor (for containerized environments).
    pub fn new() -> Self {
        let docker = DockerService::new();
        let docker_arc: Arc<dyn DockerExecutor> = Arc::new(docker);
        Self::with_executor(docker_arc)
    }

    /// Create services using local executor (for local development).
    ///
    /// Commands run directly as subprocesses without Docker.
    pub fn new_local() -> Self {
        let local = LocalExecutor::new();
        let local_arc: Arc<dyn DockerExecutor> = Arc::new(local);
        Self::with_executor(local_arc)
    }

    fn with_executor(executor: Arc<dyn DockerExecutor>) -> Self {
        let git = Arc::new(GitService::new(executor));

        // GitHub service is optional (requires GITHUB_TOKEN env var)
        let github = std::env::var("GITHUB_TOKEN")
            .ok()
            .and_then(|t| GitHubService::new(t).ok());

        Self {
            log: LogService::default(),
            git,
            github,
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
