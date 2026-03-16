use std::path::PathBuf;
use std::sync::Arc;

use crate::effects::EffectHandler;
use crate::services::acp_registry::AcpRegistry;
use crate::services::agent_control::AgentControlService;
use crate::services::claude_session_registry::ClaudeSessionRegistry;
use crate::services::event_queue::EventQueue;
use crate::services::filesystem::FileSystemService;
use crate::services::git::GitService;
use crate::services::git_worktree::GitWorktreeService;
use crate::services::github::GitHubService;
use crate::services::mutex_registry::MutexRegistry;
use claude_teams_bridge::TeamRegistry;

use super::{
    AgentHandler, CoordinationHandler, CopilotHandler, EventHandler, FilePRHandler, FsHandler,
    GitHandler, GitHubHandler, KvHandler, LogHandler, MergePRHandler, ProcessHandler, SessionHandler,
};

/// Core handlers every consumer needs: logging, key-value store, filesystem.
pub fn core_handlers(
    project_dir: PathBuf,
) -> Vec<Box<dyn EffectHandler>> {
    vec![
        Box::new(LogHandler::new()),
        Box::new(KvHandler::new(project_dir.clone())),
        Box::new(FsHandler::new(Arc::new(FileSystemService::new(
            project_dir,
        )))),
        Box::new(ProcessHandler::new()),
    ]
}

/// Git and GitHub handlers for dev tooling.
///
/// Includes: git, github, file_pr, merge_pr, copilot.
/// `github` is optional — if None, GitHubHandler is not registered.
pub fn git_handlers(
    git: Arc<GitService>,
    github: Option<GitHubService>,
    git_wt: Arc<GitWorktreeService>,
) -> Vec<Box<dyn EffectHandler>> {
    let file_pr_handler = FilePRHandler::new(git_wt.clone());
    let merge_pr_handler = MergePRHandler::new(git_wt);

    let mut handlers: Vec<Box<dyn EffectHandler>> = vec![
        Box::new(GitHandler::new(git)),
        Box::new(file_pr_handler),
        Box::new(merge_pr_handler),
        Box::new(CopilotHandler::new()),
    ];
    if let Some(gh) = github {
        handlers.push(Box::new(GitHubHandler::new(gh)));
    } else {
        tracing::warn!(
            "GitHub service not available; 'github' namespace handlers will not be registered."
        );
    }
    handlers
}

/// Orchestration handlers for agent spawning, messaging, and events.
#[allow(clippy::too_many_arguments)]
pub fn orchestration_handlers(
    agent_control: Arc<AgentControlService>,
    event_queue: Arc<EventQueue>,
    _tmux_session: Option<String>,
    project_dir: PathBuf,
    event_queue_scope: Option<String>,
    claude_session_registry: Arc<ClaudeSessionRegistry>,
    team_registry: Arc<TeamRegistry>,
    acp_registry: Arc<AcpRegistry>,
    mutex_registry: Arc<MutexRegistry>,
) -> Vec<Box<dyn EffectHandler>> {
    let agent_handler = AgentHandler::new(agent_control)
        .with_claude_session_registry(claude_session_registry.clone())
        .with_acp_registry(acp_registry.clone());

    let event_handler = EventHandler::new(event_queue, event_queue_scope, project_dir)
        .with_team_registry(team_registry.clone())
        .with_acp_registry(acp_registry.clone());

    vec![
        Box::new(agent_handler),
        Box::new(event_handler),
        Box::new(SessionHandler::new(claude_session_registry).with_team_registry(team_registry)),
        Box::new(CoordinationHandler::new(mutex_registry)),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::command::CommandExecutor;
    use std::future::Future;
    use std::pin::Pin;
    use tempfile::tempdir;

    struct MockExecutor;
    impl CommandExecutor for MockExecutor {
        fn exec<'a>(
            &'a self,
            _dir: &'a str,
            _cmd: &'a [&'a str],
        ) -> Pin<Box<dyn Future<Output = anyhow::Result<String>> + Send + 'a>> {
            Box::pin(async { Ok(String::new()) })
        }
    }

    #[test]
    fn test_core_handlers() {
        let tmp = tempdir().unwrap();
        let handlers = core_handlers(tmp.path().to_path_buf());
        assert_eq!(handlers.len(), 4);
        let namespaces: Vec<_> = handlers.iter().map(|h| h.namespace()).collect();
        assert!(namespaces.contains(&"log"));
        assert!(namespaces.contains(&"kv"));
        assert!(namespaces.contains(&"fs"));
        assert!(namespaces.contains(&"process"));
    }

    #[test]
    fn test_git_handlers_no_github() {
        let tmp = tempdir().unwrap();
        let git = Arc::new(GitService::new(Arc::new(MockExecutor)));
        let git_wt = Arc::new(GitWorktreeService::new(tmp.path().to_path_buf()));

        let handlers = git_handlers(git, None, git_wt);
        assert_eq!(handlers.len(), 4);
        let namespaces: Vec<_> = handlers.iter().map(|h| h.namespace()).collect();
        assert!(namespaces.contains(&"git"));
        assert!(namespaces.contains(&"file_pr"));
        assert!(namespaces.contains(&"merge_pr"));
        assert!(namespaces.contains(&"copilot"));
        assert!(!namespaces.contains(&"github"));
    }

    #[tokio::test]
    async fn test_git_handlers_with_github() {
        let tmp = tempdir().unwrap();
        let git = Arc::new(GitService::new(Arc::new(MockExecutor)));
        let git_wt = Arc::new(GitWorktreeService::new(tmp.path().to_path_buf()));
        let github = GitHubService::new("test-token".to_string()).unwrap();

        let handlers = git_handlers(git, Some(github), git_wt);
        assert_eq!(handlers.len(), 5);
        let namespaces: Vec<_> = handlers.iter().map(|h| h.namespace()).collect();
        assert!(namespaces.contains(&"github"));
    }
}
