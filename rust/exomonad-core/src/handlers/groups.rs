use std::path::PathBuf;
use std::sync::Arc;

use crate::effects::EffectHandler;
use crate::services::agent_control::AgentControlService;
use crate::services::filesystem::FileSystemService;
use crate::services::git::GitService;
use crate::services::git_worktree::GitWorktreeService;
use crate::services::github::GitHubService;
use crate::services::Services;

use super::{
    AgentHandler, CoordinationHandler, CopilotHandler, EventHandler, FilePRHandler, FsHandler,
    GitHandler, GitHubHandler, KvHandler, LogHandler, MergePRHandler, ProcessHandler,
    SessionHandler, TasksHandler,
};

/// Core handlers every consumer needs: logging, key-value store, filesystem.
pub fn core_handlers(
    project_dir: PathBuf,
    services: &Services,
) -> Vec<Box<dyn EffectHandler>> {
    vec![
        Box::new(LogHandler::new(services)),
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
/// GitHub client from `services` is optional — if None, GitHubHandler is not registered.
pub fn git_handlers(
    services: &Services,
    git: Arc<GitService>,
    git_wt: Arc<GitWorktreeService>,
) -> Vec<Box<dyn EffectHandler>> {
    let mut handlers: Vec<Box<dyn EffectHandler>> = vec![
        Box::new(GitHandler::new(git)),
        Box::new(FilePRHandler::new(git_wt.clone(), services)),
        Box::new(MergePRHandler::new(git_wt, services)),
        Box::new(CopilotHandler::new()),
    ];
    if let Some(ref client) = services.github_client {
        handlers.push(Box::new(GitHubHandler::new(GitHubService::new(
            client.clone(),
        ))));
    } else {
        tracing::warn!(
            "GitHub service not available; 'github' namespace handlers will not be registered."
        );
    }
    handlers
}

/// Orchestration handlers for agent spawning, messaging, and events.
pub fn orchestration_handlers(
    agent_control: Arc<AgentControlService>,
    services: &Services,
    project_dir: PathBuf,
    event_queue_scope: Option<String>,
) -> Vec<Box<dyn EffectHandler>> {
    let tasks_dir = dirs::home_dir().unwrap_or_default().join(".claude/tasks");

    vec![
        Box::new(AgentHandler::new(agent_control, services)),
        Box::new(EventHandler::new(services, event_queue_scope, project_dir)),
        Box::new(SessionHandler::new(services)),
        Box::new(TasksHandler::new(tasks_dir, services)),
        Box::new(CoordinationHandler::new(services)),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::command::CommandExecutor;
    use crate::services::GitHubClient;
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
        let services = Services::test();
        let handlers = core_handlers(tmp.path().to_path_buf(), &services);
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

        let services = Services::test();
        let handlers = git_handlers(&services, git, git_wt);
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
        let client = GitHubClient::new(5);

        let mut services = Services::test();
        services.github_client = Some(client);
        let handlers = git_handlers(&services, git, git_wt);
        assert_eq!(handlers.len(), 5);
        let namespaces: Vec<_> = handlers.iter().map(|h| h.namespace()).collect();
        assert!(namespaces.contains(&"github"));
    }
}
