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
pub use claude_teams_bridge::TeamRegistry;
use std::path::PathBuf;
use std::sync::Arc;
use thiserror::Error;

// ============================================================================
// Capability Traits — each consumer declares only what it needs
// ============================================================================

pub trait HasTeamRegistry: Send + Sync {
    fn team_registry(&self) -> &TeamRegistry;
}
pub trait HasAcpRegistry: Send + Sync {
    fn acp_registry(&self) -> &AcpRegistry;
}
pub trait HasAgentResolver: Send + Sync {
    fn agent_resolver(&self) -> &AgentResolver;
}
pub trait HasEventQueue: Send + Sync {
    fn event_queue(&self) -> &EventQueue;
}
pub trait HasEventLog: Send + Sync {
    fn event_log(&self) -> Option<&EventLog>;
}
pub trait HasProjectDir: Send + Sync {
    fn project_dir(&self) -> &std::path::Path;
}
pub trait HasSupervisorRegistry: Send + Sync {
    fn supervisor_registry(&self) -> &SupervisorRegistry;
}
pub trait HasClaudeSessionRegistry: Send + Sync {
    fn claude_session_registry(&self) -> &ClaudeSessionRegistry;
}
pub trait HasMutexRegistry: Send + Sync {
    fn mutex_registry(&self) -> &MutexRegistry;
}
pub trait HasGitHubClient: Send + Sync {
    fn github_client(&self) -> Option<&Arc<GitHubClient>>;
}
pub trait HasGitWorktreeService: Send + Sync {
    fn git_worktree_service(&self) -> &Arc<GitWorktreeService>;
}
pub trait HasTasksDir: Send + Sync {
    fn tasks_dir(&self) -> &std::path::Path;
}
pub trait HasTmuxIpc: Send + Sync {
    fn tmux_ipc(&self) -> &self::tmux_ipc::TmuxIpc;
}

// ============================================================================
// Services — the concrete type that implements all traits
// ============================================================================

/// Shared services context, constructed once and threaded through the app.
///
/// Contains all shared registries and infrastructure services. Handlers and
/// services access what they need via Has* capability trait bounds. Fields are
/// private — all access goes through the capability traits.
#[derive(Clone)]
pub struct Services {
    project_dir: PathBuf,
    tasks_dir: PathBuf,
    github_client: Option<Arc<GitHubClient>>,
    event_log: Option<Arc<EventLog>>,
    team_registry: Arc<TeamRegistry>,
    acp_registry: Arc<AcpRegistry>,
    supervisor_registry: Arc<SupervisorRegistry>,
    claude_session_registry: Arc<ClaudeSessionRegistry>,
    agent_resolver: Arc<AgentResolver>,
    event_queue: Arc<EventQueue>,
    mutex_registry: Arc<MutexRegistry>,
    git_wt: Arc<GitWorktreeService>,
    tmux_ipc: Arc<self::tmux_ipc::TmuxIpc>,
}

/// Builder for [`Services`]. All required fields are passed to `new()`; optional
/// fields (`github_client`, `event_log`) have `_opt` variants for propagating an
/// already-computed `Option`.
pub struct ServicesBuilder {
    project_dir: PathBuf,
    tasks_dir: PathBuf,
    git_wt: Arc<GitWorktreeService>,
    tmux_ipc: Arc<self::tmux_ipc::TmuxIpc>,
    github_client: Option<Arc<GitHubClient>>,
    event_log: Option<Arc<EventLog>>,
    team_registry: Arc<TeamRegistry>,
    acp_registry: Arc<AcpRegistry>,
    supervisor_registry: Arc<SupervisorRegistry>,
    claude_session_registry: Arc<ClaudeSessionRegistry>,
    agent_resolver: Arc<AgentResolver>,
    event_queue: Arc<EventQueue>,
    mutex_registry: Arc<MutexRegistry>,
}

impl ServicesBuilder {
    pub fn new(
        project_dir: PathBuf,
        tasks_dir: PathBuf,
        git_wt: Arc<GitWorktreeService>,
        tmux_ipc: Arc<self::tmux_ipc::TmuxIpc>,
    ) -> Self {
        Self {
            project_dir,
            tasks_dir,
            git_wt,
            tmux_ipc,
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

    pub fn github_client(mut self, c: Arc<GitHubClient>) -> Self {
        self.github_client = Some(c);
        self
    }
    pub fn github_client_opt(mut self, c: Option<Arc<GitHubClient>>) -> Self {
        self.github_client = c;
        self
    }
    pub fn event_log(mut self, l: Arc<EventLog>) -> Self {
        self.event_log = Some(l);
        self
    }
    pub fn event_log_opt(mut self, l: Option<Arc<EventLog>>) -> Self {
        self.event_log = l;
        self
    }
    pub fn team_registry(mut self, r: Arc<TeamRegistry>) -> Self {
        self.team_registry = r;
        self
    }
    pub fn acp_registry(mut self, r: Arc<AcpRegistry>) -> Self {
        self.acp_registry = r;
        self
    }
    pub fn supervisor_registry(mut self, r: Arc<SupervisorRegistry>) -> Self {
        self.supervisor_registry = r;
        self
    }
    pub fn claude_session_registry(mut self, r: Arc<ClaudeSessionRegistry>) -> Self {
        self.claude_session_registry = r;
        self
    }
    pub fn agent_resolver(mut self, r: Arc<AgentResolver>) -> Self {
        self.agent_resolver = r;
        self
    }
    pub fn event_queue(mut self, q: Arc<EventQueue>) -> Self {
        self.event_queue = q;
        self
    }
    pub fn mutex_registry(mut self, r: Arc<MutexRegistry>) -> Self {
        self.mutex_registry = r;
        self
    }

    pub fn build(self) -> Services {
        Services {
            project_dir: self.project_dir,
            tasks_dir: self.tasks_dir,
            github_client: self.github_client,
            event_log: self.event_log,
            team_registry: self.team_registry,
            acp_registry: self.acp_registry,
            supervisor_registry: self.supervisor_registry,
            claude_session_registry: self.claude_session_registry,
            agent_resolver: self.agent_resolver,
            event_queue: self.event_queue,
            mutex_registry: self.mutex_registry,
            git_wt: self.git_wt,
            tmux_ipc: self.tmux_ipc,
        }
    }
}

impl HasTeamRegistry for Services {
    fn team_registry(&self) -> &TeamRegistry {
        &self.team_registry
    }
}
impl HasAcpRegistry for Services {
    fn acp_registry(&self) -> &AcpRegistry {
        &self.acp_registry
    }
}
impl HasAgentResolver for Services {
    fn agent_resolver(&self) -> &AgentResolver {
        &self.agent_resolver
    }
}
impl HasEventQueue for Services {
    fn event_queue(&self) -> &EventQueue {
        &self.event_queue
    }
}
impl HasEventLog for Services {
    fn event_log(&self) -> Option<&EventLog> {
        self.event_log.as_deref()
    }
}
impl HasProjectDir for Services {
    fn project_dir(&self) -> &std::path::Path {
        &self.project_dir
    }
}
impl HasSupervisorRegistry for Services {
    fn supervisor_registry(&self) -> &SupervisorRegistry {
        &self.supervisor_registry
    }
}
impl HasClaudeSessionRegistry for Services {
    fn claude_session_registry(&self) -> &ClaudeSessionRegistry {
        &self.claude_session_registry
    }
}
impl HasMutexRegistry for Services {
    fn mutex_registry(&self) -> &MutexRegistry {
        &self.mutex_registry
    }
}
impl HasGitHubClient for Services {
    fn github_client(&self) -> Option<&Arc<GitHubClient>> {
        self.github_client.as_ref()
    }
}
impl HasGitWorktreeService for Services {
    fn git_worktree_service(&self) -> &Arc<GitWorktreeService> {
        &self.git_wt
    }
}
impl HasTasksDir for Services {
    fn tasks_dir(&self) -> &std::path::Path {
        &self.tasks_dir
    }
}
impl HasTmuxIpc for Services {
    fn tmux_ipc(&self) -> &self::tmux_ipc::TmuxIpc {
        &self.tmux_ipc
    }
}

#[cfg(test)]
impl Services {
    /// Construct a Services with empty/default registries for testing.
    pub fn test() -> Self {
        Self::test_with_project_dir(PathBuf::from("."))
    }

    /// Construct a test Services rooted at `project_dir`. The `git_wt` service
    /// is constructed from the same directory.
    pub fn test_with_project_dir(project_dir: PathBuf) -> Self {
        let git_wt = Arc::new(GitWorktreeService::new(project_dir.clone()));
        let tmux_ipc = Arc::new(self::tmux_ipc::TmuxIpc::new("test-session"));
        ServicesBuilder::new(
            project_dir,
            PathBuf::from(".claude/tasks"),
            git_wt,
            tmux_ipc,
        )
        .build()
    }

    /// Construct a test Services with a specific TmuxIpc.
    pub fn test_with_tmux(tmux_ipc: Arc<self::tmux_ipc::TmuxIpc>) -> Self {
        let project_dir = PathBuf::from(".");
        let git_wt = Arc::new(GitWorktreeService::new(project_dir.clone()));
        ServicesBuilder::new(
            project_dir,
            PathBuf::from(".claude/tasks"),
            git_wt,
            tmux_ipc,
        )
        .build()
    }

    /// Test-only setter for injecting a GitHubClient into an already-built
    /// Services. Production code must go through [`ServicesBuilder`].
    pub fn set_github_client(&mut self, c: Arc<GitHubClient>) {
        self.github_client = Some(c);
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
