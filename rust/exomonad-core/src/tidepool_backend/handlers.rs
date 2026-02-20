use std::sync::Arc;
use anyhow::Result;
use tidepool_bridge::ToCore;
use tidepool_effect::dispatch::{EffectContext as TidepoolEffectContext, EffectHandler};
use tidepool_effect::error::EffectError as TidepoolEffectError;
use tidepool_eval::value::Value;

use crate::effects::EffectContext;
use super::bridge_types::*;

// =============================================================================
// Shared Effect Handlers
// =============================================================================

/// Generic handler for GetToolInput effect. Works for any tool input type.
/// Takes ownership of the input on first call; subsequent calls error.
pub struct ToolInputHandler<T: ToCore> {
    pub input: Option<T>,
}

impl<T: ToCore + std::fmt::Debug> EffectHandler<EffectContext> for ToolInputHandler<T> {
    type Request = GetToolInputReq;
    fn handle(
        &mut self,
        _req: GetToolInputReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        let input = self.input.take().ok_or_else(|| {
            TidepoolEffectError::Handler("GetToolInput: tool_input already consumed".to_string())
        })?;
        input.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
    }
}

/// Shared Identity handler. Resolves agent identity from EffectContext.
pub struct IdentityHandler;

impl EffectHandler<EffectContext> for IdentityHandler {
    type Request = IdentityReq;
    fn handle(
        &mut self,
        req: IdentityReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            IdentityReq::GetAgentId => cx.respond(cx.user().agent_name.to_string()),
            IdentityReq::GetParentTab => {
                cx.respond(crate::services::agent_control::resolve_parent_tab_name(
                    cx.user(),
                ))
            }
            IdentityReq::GetOwnTab => {
                cx.respond(crate::services::agent_control::resolve_own_tab_name(
                    cx.user(),
                ))
            }
            IdentityReq::GetWorkingDir => {
                let dir = crate::services::agent_control::resolve_agent_working_dir(cx.user());
                cx.respond(dir.to_string_lossy().to_string())
            }
        }
    }
}

/// Shared Inbox handler. Read/write agent messages.
pub struct InboxHandler {
    pub project_dir: std::path::PathBuf,
}

impl EffectHandler<EffectContext> for InboxHandler {
    type Request = InboxReq;
    fn handle(
        &mut self,
        req: InboxReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            InboxReq::WriteMessage(inbox_name, from, text, subject) => {
                let inbox_path =
                    crate::services::inbox::inbox_path(&self.project_dir, &inbox_name);
                let msg = crate::services::inbox::create_message(
                    from,
                    text,
                    if subject.is_empty() {
                        None
                    } else {
                        Some(subject)
                    },
                );
                crate::services::inbox::append_message(&inbox_path, &msg).map_err(|e| {
                    TidepoolEffectError::Handler(format!("inbox write: {}", e))
                })?;
                cx.respond("ok".to_string())
            }
            InboxReq::ReadMessages(inbox_name) => {
                let inbox_path =
                    crate::services::inbox::inbox_path(&self.project_dir, &inbox_name);
                let messages = crate::services::inbox::read_unread(&inbox_path)
                    .map_err(|e| TidepoolEffectError::Handler(e.to_string()))?;
                let json: Vec<serde_json::Value> = messages
                    .iter()
                    .map(|m| {
                        serde_json::json!({
                            "from": m.from,
                            "text": m.text,
                            "timestamp": m.timestamp,
                            "read": m.read,
                        })
                    })
                    .collect();
                let json_str = serde_json::to_string(&json).unwrap_or_else(|_| "[]".to_string());
                cx.respond(json_str)
            }
            InboxReq::PollMessages(inbox_name, timeout_str) => {
                let timeout_secs: u64 = timeout_str.parse().unwrap_or(0);
                let inbox_path =
                    crate::services::inbox::inbox_path(&self.project_dir, &inbox_name);
                let timeout = std::time::Duration::from_secs(timeout_secs);
                let interval = std::time::Duration::from_secs(2);
                let messages = tokio::task::block_in_place(move || {
                    crate::services::inbox::poll_unread(&inbox_path, timeout, interval)
                })
                .map_err(|e| TidepoolEffectError::Handler(e.to_string()))?;
                let json: Vec<serde_json::Value> = messages
                    .iter()
                    .map(|m| {
                        serde_json::json!({
                            "from": m.from,
                            "text": m.text,
                            "timestamp": m.timestamp,
                            "read": m.read,
                        })
                    })
                    .collect();
                let json_str = serde_json::to_string(&json).unwrap_or_else(|_| "[]".to_string());
                cx.respond(json_str)
            }
        }
    }
}

/// Shared Questions handler. Resolves pending questions via registry.
pub struct QuestionsHandler {
    pub registry: Arc<crate::services::questions::QuestionRegistry>,
}

impl EffectHandler<EffectContext> for QuestionsHandler {
    type Request = QuestionsReq;
    fn handle(
        &mut self,
        req: QuestionsReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            QuestionsReq::ResolveQuestion(question_id, answer) => {
                let resolved = self.registry.resolve(&question_id, answer);
                tracing::info!(
                    question_id = %question_id,
                    resolved,
                    "Resolved question via QuestionRegistry"
                );
                cx.respond(resolved.to_string())
            }
        }
    }
}

/// Shared FormatOp handler. Temporary bridge for string formatting logic
/// that will move to inline Haskell once Tidepool supports Prelude functions.
pub struct FormatOpHandler;

impl EffectHandler<EffectContext> for FormatOpHandler {
    type Request = FormatOpReq;
    fn handle(
        &mut self,
        req: FormatOpReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            FormatOpReq::FormatNotification(status, message, agent_id) => {
                let formatted = match status.as_str() {
                    "success" => format!("[CHILD COMPLETE: {}] {}", agent_id, message),
                    "failure" => format!("[CHILD FAILED: {}] {}", agent_id, message),
                    other => format!("[CHILD STATUS {}: {}] {}", agent_id, other, message),
                };
                cx.respond(formatted)
            }
            FormatOpReq::FormatNoteSubject(content) => {
                let subject: String = content.chars().take(50).collect();
                cx.respond(subject)
            }
            FormatOpReq::DefaultMessage(status, message) => {
                let msg = if message.is_empty() {
                    match status.as_str() {
                        "success" => "Task completed successfully.".to_string(),
                        _ => "Task failed.".to_string(),
                    }
                } else {
                    message
                };
                cx.respond(msg)
            }
            FormatOpReq::FormatNote(agent_id, content) => {
                let formatted = format!("[note from {}] {}", agent_id, content);
                cx.respond(formatted)
            }
        }
    }
}

// =============================================================================
// Per-Tool Domain Op Handlers
// =============================================================================

/// Per-tool domain op handler for popup. Pure I/O — receives target tab from Haskell.
pub struct PopupOpHandler {
    pub zellij_session: Option<String>,
}

impl EffectHandler<EffectContext> for PopupOpHandler {
    type Request = PopupOpReq;
    fn handle(
        &mut self,
        req: PopupOpReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            PopupOpReq::ShowPopup(title, components, target_tab) => {
                let raw_json: serde_json::Value = if components.is_empty() {
                    serde_json::Value::Array(Vec::new())
                } else {
                    serde_json::from_str(&components).map_err(|e| {
                        TidepoolEffectError::Handler(format!("Invalid components JSON: {}", e))
                    })?
                };

                let input = crate::services::popup::PopupInput {
                    title,
                    raw_json,
                    target_tab: Some(target_tab),
                };

                let service =
                    crate::services::popup::PopupService::new(self.zellij_session.clone());

                let output = service
                    .show_popup(&input)
                    .map_err(|e| TidepoolEffectError::Handler(format!("popup: {}", e)))?;

                let values_json = serde_json::to_string(&output.values).map_err(|e| {
                    TidepoolEffectError::Handler(format!("popup serialize: {}", e))
                })?;

                cx.respond(PopupResponse {
                    button: output.button,
                    values: values_json,
                })
            }
        }
    }
}

/// Per-tool domain op handler for file_pr. Pure I/O — just calls the service.
pub struct FilePROpHandler {
    pub jj: Arc<crate::services::jj_workspace::JjWorkspaceService>,
}

impl EffectHandler<EffectContext> for FilePROpHandler {
    type Request = FilePROpReq;
    fn handle(
        &mut self,
        req: FilePROpReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            FilePROpReq::CreateOrUpdatePR(title, body, base, working_dir) => {
                let input = crate::services::file_pr::FilePRInput {
                    title,
                    body,
                    base_branch: if base.is_empty() { None } else { Some(base) },
                    working_dir: Some(working_dir),
                };
                let jj = self.jj.clone();
                let result = tokio::task::block_in_place(|| {
                    tokio::runtime::Handle::current()
                        .block_on(crate::services::file_pr::file_pr_async(&input, jj))
                })
                .map_err(|e| TidepoolEffectError::Handler(e.to_string()))?;
                cx.respond(FilePRToolResult {
                    pr_url: result.pr_url,
                    pr_number: result.pr_number.as_u64().to_string(),
                    head_branch: result.head_branch,
                    result_base: result.base_branch,
                    created: result.created.to_string(),
                })
            }
        }
    }
}

/// Per-tool domain op handler for merge_pr. Pure I/O — just calls the service.
pub struct MergePROpHandler {
    pub jj: Arc<crate::services::jj_workspace::JjWorkspaceService>,
}

impl EffectHandler<EffectContext> for MergePROpHandler {
    type Request = MergePROpReq;
    fn handle(
        &mut self,
        req: MergePROpReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            MergePROpReq::MergePullRequest(pr_num_str, strategy, working_dir) => {
                let pr_number: u64 = pr_num_str.parse().map_err(|e| {
                    TidepoolEffectError::Handler(format!("invalid pr_number: {}", e))
                })?;
                let pr = crate::domain::PRNumber::new(pr_number);
                let jj = self.jj.clone();
                let result = tokio::task::block_in_place(|| {
                    tokio::runtime::Handle::current().block_on(
                        crate::services::merge_pr::merge_pr_async(
                            pr,
                            &strategy,
                            &working_dir,
                            jj,
                        ),
                    )
                })
                .map_err(|e| TidepoolEffectError::Handler(e.to_string()))?;
                cx.respond(MergePRToolResult {
                    success: result.success.to_string(),
                    message: result.message,
                    jj_fetched: result.jj_fetched.to_string(),
                })
            }
        }
    }
}


/// Per-tool domain op handler for spawn_subtree.
pub struct SpawnSubtreeOpHandler {
    pub agent_control: Arc<crate::services::agent_control::AgentControlService>,
}

impl EffectHandler<EffectContext> for SpawnSubtreeOpHandler {
    type Request = SpawnSubtreeOpReq;
    fn handle(
        &mut self,
        req: SpawnSubtreeOpReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            SpawnSubtreeOpReq::SpawnSubtree(task, branch_name) => {
                let options = crate::services::agent_control::SpawnSubtreeOptions {
                    task,
                    branch_name: branch_name.clone(),
                    parent_session_id: None,
                };
                let ac = self.agent_control.clone();
                let bb = cx.user().birth_branch.clone();
                let result = tokio::task::block_in_place(|| {
                    tokio::runtime::Handle::current()
                        .block_on(ac.spawn_subtree(&options, &bb))
                })
                .map_err(|e| TidepoolEffectError::Handler(e.to_string()))?;
                cx.respond(SpawnSubtreeToolResult {
                    tab_name: result.tab_name,
                    branch_name,
                })
            }
        }
    }
}

/// Per-tool domain op handler for spawn_leaf.
pub struct SpawnLeafOpHandler {
    pub agent_control: Arc<crate::services::agent_control::AgentControlService>,
}

impl EffectHandler<EffectContext> for SpawnLeafOpHandler {
    type Request = SpawnLeafOpReq;
    fn handle(
        &mut self,
        req: SpawnLeafOpReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            SpawnLeafOpReq::SpawnLeaf(task, branch_name) => {
                let options = crate::services::agent_control::SpawnSubtreeOptions {
                    task,
                    branch_name: branch_name.clone(),
                    parent_session_id: None,
                };
                let ac = self.agent_control.clone();
                let bb = cx.user().birth_branch.clone();
                let result = tokio::task::block_in_place(|| {
                    tokio::runtime::Handle::current()
                        .block_on(ac.spawn_leaf_subtree(&options, &bb))
                })
                .map_err(|e| TidepoolEffectError::Handler(e.to_string()))?;
                cx.respond(SpawnLeafToolResult {
                    tab_name: result.tab_name,
                    branch_name,
                })
            }
        }
    }
}

/// Per-tool domain op handler for spawn_workers.
pub struct SpawnWorkerOpHandler {
    pub agent_control: Arc<crate::services::agent_control::AgentControlService>,
}

impl EffectHandler<EffectContext> for SpawnWorkerOpHandler {
    type Request = SpawnWorkerOpReq;
    fn handle(
        &mut self,
        req: SpawnWorkerOpReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            SpawnWorkerOpReq::SpawnWorker(name, prompt) => {
                let options = crate::services::agent_control::SpawnWorkerOptions {
                    name,
                    prompt,
                };
                let ac = self.agent_control.clone();
                let bb = cx.user().birth_branch.clone();
                let result = tokio::task::block_in_place(|| {
                    tokio::runtime::Handle::current()
                        .block_on(ac.spawn_worker(&options, &bb))
                })
                .map_err(|e| TidepoolEffectError::Handler(e.to_string()))?;
                cx.respond(SpawnWorkerToolResult {
                    tab_name: result.tab_name,
                })
            }
        }
    }
}

/// Per-tool domain op handler for notify_parent. Pure I/O — injects pre-formatted text.
pub struct NotifyOpHandler;

impl EffectHandler<EffectContext> for NotifyOpHandler {
    type Request = NotifyOpReq;
    fn handle(
        &mut self,
        req: NotifyOpReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            NotifyOpReq::DeliverNotification(parent_tab, formatted) => {
                crate::services::zellij_events::inject_input(&parent_tab, &formatted);
                cx.respond(NotifyToolResult {
                    ack: "delivered".to_string(),
                })
            }
        }
    }
}

/// Per-tool domain op handler for note. Pure I/O — injects pre-formatted text.
pub struct NoteOpHandler;

impl EffectHandler<EffectContext> for NoteOpHandler {
    type Request = NoteOpReq;
    fn handle(
        &mut self,
        req: NoteOpReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            NoteOpReq::InjectNote(parent_tab, formatted) => {
                crate::services::zellij_events::inject_input(&parent_tab, &formatted);
                cx.respond(NoteToolResult {
                    ack: "sent".to_string(),
                })
            }
        }
    }
}

/// Per-tool domain op handler for get_agent_messages. Handles timeout branching + inbox read.
pub struct MessagesOpHandler {
    pub project_dir: std::path::PathBuf,
}

impl EffectHandler<EffectContext> for MessagesOpHandler {
    type Request = MessagesOpReq;
    fn handle(
        &mut self,
        req: MessagesOpReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            MessagesOpReq::FetchMessages(agent_id, timeout_str) => {
                let timeout_secs: u64 = timeout_str.parse().unwrap_or(0);
                let inbox_path =
                    crate::services::inbox::inbox_path(&self.project_dir, &agent_id);

                let messages = if timeout_secs == 0 {
                    crate::services::inbox::read_unread(&inbox_path)
                        .map_err(|e| TidepoolEffectError::Handler(e.to_string()))?
                } else {
                    let timeout = std::time::Duration::from_secs(timeout_secs);
                    let interval = std::time::Duration::from_secs(2);
                    tokio::task::block_in_place(move || {
                        crate::services::inbox::poll_unread(&inbox_path, timeout, interval)
                    })
                    .map_err(|e| TidepoolEffectError::Handler(e.to_string()))?
                };

                let json: Vec<serde_json::Value> = messages
                    .iter()
                    .map(|m| {
                        serde_json::json!({
                            "from": m.from,
                            "text": m.text,
                            "timestamp": m.timestamp,
                            "read": m.read,
                        })
                    })
                    .collect();
                let json_str =
                    serde_json::to_string(&json).unwrap_or_else(|_| "[]".to_string());

                cx.respond(MessagesToolResult {
                    messages_json: json_str,
                    warning: "".to_string(),
                })
            }
        }
    }
}
