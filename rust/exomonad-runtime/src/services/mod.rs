pub mod docker;
pub mod git;
pub mod github;
pub mod log;

use self::docker::{DockerExecutor, DockerService};
use self::git::GitService;
use self::github::GitHubService;
use self::log::{HasLogService, LogService};
use std::sync::Arc;

#[derive(Clone)]
pub struct Services {
    pub docker: DockerService,
    pub log: LogService,
    pub git: Arc<GitService>,
    pub github: Option<GitHubService>,
}

impl Services {
    pub fn new() -> Self {
        let docker = DockerService::new();

        // DockerService implements DockerExecutor, so we can use it for GitService
        let docker_arc: Arc<dyn DockerExecutor> = Arc::new(docker.clone());
        let git = Arc::new(GitService::new(docker_arc));

        // GitHub service is optional (requires GITHUB_TOKEN env var)
        let github = std::env::var("GITHUB_TOKEN")
            .ok()
            .and_then(|t| GitHubService::new(t).ok());

        Self {
            docker,
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
