use std::path::PathBuf;
use std::sync::Arc;

use crate::effects::EffectHandler;
use crate::services::acp_registry::AcpRegistry;
use crate::services::agent_control::AgentControlService;
use crate::services::claude_session_registry::ClaudeSessionRegistry;
use crate::services::event_log::EventLog;
use crate::services::event_queue::EventQueue;
use crate::services::filesystem::FileSystemService;
use crate::services::git::GitService;
use crate::services::git_worktree::GitWorktreeService;
use crate::services::github::GitHubService;
use crate::services::team_registry::TeamRegistry;

use super::{
    AgentHandler, CopilotHandler, EventHandler, FilePRHandler, FsHandler, GitHandler,
    GitHubHandler, KvHandler, LogHandler, MergePRHandler, PopupHandler, SessionHandler,
};

/// Core handlers every consumer needs: logging, key-value store, filesystem.
pub fn core_handlers(
    project_dir: PathBuf,
    event_log: Option<Arc<EventLog>>,
) -> Vec<Box<dyn EffectHandler>> {
    let log_handler = match event_log {
        Some(el) => LogHandler::new().with_event_log(el),
        None => LogHandler::new(),
    };
    vec![
        Box::new(log_handler),
        Box::new(KvHandler::new(project_dir.clone())),
        Box::new(FsHandler::new(Arc::new(FileSystemService::new(
            project_dir,
        )))),
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
    let mut handlers: Vec<Box<dyn EffectHandler>> = vec![
        Box::new(GitHandler::new(git)),
        Box::new(FilePRHandler::new(git_wt.clone())),
        Box::new(MergePRHandler::new(git_wt)),
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

/// Orchestration handlers for agent spawning, messaging, events, popups.
#[allow(clippy::too_many_arguments)]
pub fn orchestration_handlers(
    agent_control: Arc<AgentControlService>,
    event_queue: Arc<EventQueue>,
    zellij_session: Option<String>,
    _project_dir: PathBuf,
    remote_port: Option<u16>,
    event_queue_scope: Option<String>,
    claude_session_registry: Arc<ClaudeSessionRegistry>,
    team_registry: Arc<TeamRegistry>,
    acp_registry: Arc<AcpRegistry>,
) -> Vec<Box<dyn EffectHandler>> {
    vec![
        Box::new(
            AgentHandler::new(agent_control)
                .with_claude_session_registry(claude_session_registry.clone()),
        ),
        Box::new(PopupHandler::new(zellij_session)),
        Box::new(
            EventHandler::new(event_queue, remote_port, event_queue_scope)
                .with_team_registry(team_registry.clone())
                .with_acp_registry(acp_registry.clone()),
        ),
        Box::new(SessionHandler::new(claude_session_registry).with_team_registry(team_registry)),
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
        let handlers = core_handlers(tmp.path().to_path_buf(), None);
        assert_eq!(handlers.len(), 3);
        let namespaces: Vec<_> = handlers.iter().map(|h| h.namespace()).collect();
        assert!(namespaces.contains(&"log"));
        assert!(namespaces.contains(&"kv"));
        assert!(namespaces.contains(&"fs"));
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
