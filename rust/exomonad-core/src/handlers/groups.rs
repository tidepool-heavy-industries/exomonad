use std::path::PathBuf;
use std::sync::Arc;

use crate::effects::EffectHandler;
use crate::services::agent_control::AgentControlService;
use crate::services::claude_session_registry::ClaudeSessionRegistry;
use crate::services::event_log::EventLog;
use crate::services::event_queue::EventQueue;
use crate::services::filesystem::FileSystemService;
use crate::services::git::GitService;
use crate::services::git_worktree::GitWorktreeService;
use crate::services::github::GitHubService;
use crate::services::questions::QuestionRegistry;
use crate::services::team_registry::TeamRegistry;

use super::{
    AgentHandler, CopilotHandler, EventHandler, FilePRHandler, FsHandler, GitHandler,
    GitHubHandler, KvHandler, LogHandler, MergePRHandler, MessagingHandler, PopupHandler,
    SessionHandler,
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
///
/// Returns the handlers AND the QuestionRegistry (shared with MCP for answer_question).
#[allow(clippy::too_many_arguments)]
pub fn orchestration_handlers(
    agent_control: Arc<AgentControlService>,
    event_queue: Arc<EventQueue>,
    zellij_session: Option<String>,
    project_dir: PathBuf,
    remote_port: Option<u16>,
    event_queue_scope: Option<String>,
    claude_session_registry: Arc<ClaudeSessionRegistry>,
    team_registry: Arc<TeamRegistry>,
) -> (Vec<Box<dyn EffectHandler>>, Arc<QuestionRegistry>) {
    let question_registry = Arc::new(QuestionRegistry::new());

    let handlers: Vec<Box<dyn EffectHandler>> = vec![
        Box::new(
            AgentHandler::new(agent_control)
                .with_claude_session_registry(claude_session_registry.clone()),
        ),
        Box::new(PopupHandler::new(zellij_session)),
        Box::new(
            EventHandler::new(event_queue, remote_port, event_queue_scope)
                .with_team_registry(team_registry.clone()),
        ),
        Box::new(MessagingHandler::new(
            question_registry.clone(),
            project_dir,
        )),
        Box::new(SessionHandler::new(claude_session_registry).with_team_registry(team_registry)),
    ];

    (handlers, question_registry)
}
