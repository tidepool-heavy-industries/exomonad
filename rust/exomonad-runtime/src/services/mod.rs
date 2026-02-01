pub mod agent_control;
pub mod ast_grep;
pub mod docker;
pub mod filesystem;
pub mod git;
pub mod github;
pub mod local;
pub mod log;
pub mod plan_store;
pub mod secrets;

pub use self::agent_control::{
    AgentControlService, AgentInfo, BatchCleanupResult, BatchSpawnResult, SpawnOptions, SpawnResult,
};
use self::ast_grep::AstGrepService;
use self::docker::{DockerExecutor, DockerService};
pub use self::filesystem::FileSystemService;
use self::git::GitService;
use self::github::GitHubService;
use self::local::LocalExecutor;
use self::log::{HasLogService, LogService};
use self::plan_store::PlanStore;
pub use self::secrets::Secrets;
use std::sync::Arc;

#[derive(Clone)]
pub struct Services {
    pub log: LogService,
    pub git: Arc<GitService>,
    pub github: Option<GitHubService>,
    pub agent_control: Arc<AgentControlService>,
    pub filesystem: Arc<FileSystemService>,
    pub ast_grep: Arc<AstGrepService>,
    pub plan_store: Arc<PlanStore>,
    pub secrets: Arc<Secrets>,
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
    /// Loads secrets from ~/.exomonad/secrets.
    pub fn new_local() -> Self {
        let local = LocalExecutor::new();
        let local_arc: Arc<dyn DockerExecutor> = Arc::new(local);
        Self::with_executor(local_arc)
    }

    fn with_executor(executor: Arc<dyn DockerExecutor>) -> Self {
        let secrets = Arc::new(Secrets::load());
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

        let ast_grep = Arc::new(AstGrepService::new());
        let plan_store = Arc::new(PlanStore::new());

        Self {
            log: LogService::default(),
            git,
            github,
            agent_control,
            filesystem,
            ast_grep,
            plan_store,
            secrets,
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
