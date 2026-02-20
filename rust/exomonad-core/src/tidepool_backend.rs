//! Tidepool backend: Cranelift-compiled Haskell Core behind RuntimeBackend.
//!
//! Each MCP tool is compiled from Haskell at build time via `haskell_expr!`.
//! At runtime, tool calls flow through `EffectMachine`, which evaluates the
//! Core expression and routes yielded effects to per-tool `EffectHandler` impls
//! composed via frunk HLists.

use std::collections::HashMap;
use std::sync::Arc;

use crate::effects::EffectContext;
use crate::mcp::tools::MCPCallOutput;
use crate::mcp::ToolDefinition;
use crate::runtime_backend::RuntimeBackend;
use anyhow::Result;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use schemars::JsonSchema;
use serde_json::Value as JsonValue;
use tracing::debug;

use tidepool_bridge::{FromCore, ToCore};
use tidepool_bridge_derive::{FromCore, ToCore};
use tidepool_effect::dispatch::{
    DispatchEffect, EffectContext as TidepoolEffectContext, EffectHandler,
};
use tidepool_effect::error::EffectError as TidepoolEffectError;
use tidepool_eval::value::Value;
use tidepool_repr::{CoreExpr, DataConTable};

// =============================================================================
// Shared effect bridge types (reused across tools)
// =============================================================================

/// Generic tool input effect. All tools use `GetToolInput` as their first
/// effect to receive MCP args from Rust. The handler is generic over the
/// input type T via ToolInputHandler<T>.
#[derive(Debug, FromCore)]
enum GetToolInputReq {
    #[core(name = "GetToolInput")]
    Get,
}

/// Shared Identity effect. Provides agent identity information.
/// Constructor names match Haskell GADT exactly.
#[derive(Debug, FromCore)]
enum IdentityReq {
    #[core(name = "GetAgentId")]
    GetAgentId,
    #[core(name = "GetParentTab")]
    GetParentTab,
    #[core(name = "GetOwnTab")]
    GetOwnTab,
    #[core(name = "GetWorkingDir")]
    GetWorkingDir,
}

/// Shared Inbox effect. Read/write agent messages.
#[derive(Debug, FromCore)]
enum InboxReq {
    #[core(name = "WriteMessage")]
    WriteMessage(String, String, String, String),
    #[core(name = "ReadMessages")]
    ReadMessages(String),
    #[core(name = "PollMessages")]
    PollMessages(String, String),
}

/// Shared Questions effect. Resolves pending questions.
#[derive(Debug, FromCore)]
enum QuestionsReq {
    #[core(name = "ResolveQuestion")]
    ResolveQuestion(String, String),
}

/// Shared FormatOp effect. Temporary bridge until Tidepool supports Prelude
/// functions (string comparison, conditionals). Haskell decides *what* to format;
/// Rust decides *how* until inline Haskell logic is possible.
#[derive(Debug, FromCore)]
enum FormatOpReq {
    /// Format a parent notification: status, message, agentId → formatted string.
    #[core(name = "FormatNotification")]
    FormatNotification(String, String, String),
    /// Truncate content to a subject line (first 50 chars).
    #[core(name = "FormatNoteSubject")]
    FormatNoteSubject(String),
    /// Return message if non-empty, otherwise a default based on status.
    #[core(name = "DefaultMessage")]
    DefaultMessage(String, String),
    /// Format a note notification: agentId, content → formatted string.
    #[core(name = "FormatNote")]
    FormatNote(String, String),
}

// =============================================================================
// Per-tool bridge types
// =============================================================================

/// Tool input provided by Rust when Haskell yields `GetToolInput`.
/// Field order must match Haskell record: `ToolInput { tiTitle, tiComponents }`.
#[derive(Debug, FromCore, ToCore)]
#[core(name = "ToolInput")]
struct ToolInput {
    title: String,
    components: String,
}

/// Per-tool domain op for popup. Receives title, components, and target tab.
#[derive(Debug, FromCore)]
enum PopupOpReq {
    #[core(name = "ShowPopup")]
    ShowPopup(String, String, String),
}

/// Popup response returned to Haskell after showing the popup.
/// Field order must match Haskell record: `PopupResponse { prButton, prValues }`.
#[derive(Debug, FromCore, ToCore)]
#[core(name = "PopupResponse")]
struct PopupResponse {
    button: String,
    values: String,
}

// =============================================================================
// file_pr bridge types
// =============================================================================

/// Mirrors Haskell: `FilePRInput { fprTitle, fprBody, fprBaseBranch }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "FilePRInput")]
struct FilePRToolInput {
    title: String,
    body: String,
    base_branch: String,
}

/// Mirrors Haskell: `FilePRResult { fprPrUrl, fprPrNumber, fprHeadBranch, fprResultBase, fprCreated }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "FilePRResult")]
struct FilePRToolResult {
    pr_url: String,
    pr_number: String,
    head_branch: String,
    result_base: String,
    created: String,
}

/// Per-tool domain op for file_pr. Receives all args including working_dir from Identity.
#[derive(Debug, FromCore)]
enum FilePROpReq {
    #[core(name = "CreateOrUpdatePR")]
    CreateOrUpdatePR(String, String, String, String),
}

// =============================================================================
// merge_pr bridge types
// =============================================================================

/// Mirrors Haskell: `MergePRInput { mprPrNumber, mprStrategy }`
/// working_dir no longer in MCP args — resolved via Identity effect.
#[derive(Debug, FromCore, ToCore)]
#[core(name = "MergePRInput")]
struct MergePRToolInput {
    pr_number: String,
    strategy: String,
}

/// Mirrors Haskell: `MergePRResult { mprSuccess, mprMessage, mprJjFetched }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "MergePRResult")]
struct MergePRToolResult {
    success: String,
    message: String,
    jj_fetched: String,
}

/// Per-tool domain op for merge_pr.
#[derive(Debug, FromCore)]
enum MergePROpReq {
    #[core(name = "MergePullRequest")]
    MergePullRequest(String, String, String),
}

// =============================================================================
// notify_parent bridge types
// =============================================================================

/// Mirrors Haskell: `NotifyInput { niStatus, niMessage }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "NotifyInput")]
struct NotifyToolInput {
    status: String,
    message: String,
}

/// Mirrors Haskell: `NotifyResult { nrAck }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "NotifyResult")]
struct NotifyToolResult {
    ack: String,
}

/// Per-tool domain op for notify_parent. Pure I/O — just injects formatted text.
#[derive(Debug, FromCore)]
enum NotifyOpReq {
    #[core(name = "DeliverNotification")]
    DeliverNotification(String, String), // parentTab, formatted
}

// =============================================================================
// spawn_subtree bridge types
// =============================================================================

/// Mirrors Haskell: `SpawnSubtreeInput { ssiTask, ssiBranchName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnSubtreeInput")]
struct SpawnSubtreeToolInput {
    task: String,
    branch_name: String,
}

/// Mirrors Haskell: `SpawnSubtreeResult { ssrTabName, ssrBranchName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnSubtreeResult")]
struct SpawnSubtreeToolResult {
    tab_name: String,
    branch_name: String,
}

/// Per-tool domain op for spawn_subtree.
#[derive(Debug, FromCore)]
enum SpawnSubtreeOpReq {
    #[core(name = "SpawnSubtree")]
    SpawnSubtree(String, String),
}

// =============================================================================
// spawn_leaf_subtree bridge types
// =============================================================================

/// Mirrors Haskell: `SpawnLeafInput { sliTask, sliBranchName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnLeafInput")]
struct SpawnLeafToolInput {
    task: String,
    branch_name: String,
}

/// Mirrors Haskell: `SpawnLeafResult { slrTabName, slrBranchName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnLeafResult")]
struct SpawnLeafToolResult {
    tab_name: String,
    branch_name: String,
}

/// Per-tool domain op for spawn_leaf.
#[derive(Debug, FromCore)]
enum SpawnLeafOpReq {
    #[core(name = "SpawnLeaf")]
    SpawnLeaf(String, String),
}

// =============================================================================
// spawn_workers bridge types
// =============================================================================

/// Mirrors Haskell: `WorkerSpec { wsName, wsPrompt }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "WorkerSpec")]
struct WorkerSpecBridge {
    name: String,
    prompt: String,
}

/// Mirrors Haskell: `SpawnWorkerResult { swrTabName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnWorkerResult")]
struct SpawnWorkerToolResult {
    tab_name: String,
}

/// Per-tool domain op for spawn_workers.
#[derive(Debug, FromCore)]
enum SpawnWorkerOpReq {
    #[core(name = "SpawnWorker")]
    SpawnWorker(String, String),
}

// =============================================================================
// note bridge types
// =============================================================================

/// Mirrors Haskell: `NoteInput { niContent }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "NoteInput")]
struct NoteToolInput {
    content: String,
}

/// Mirrors Haskell: `NoteResult { nrAck }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "NoteResult")]
struct NoteToolResult {
    ack: String,
}

/// Per-tool domain op for note. Pure I/O — injects pre-formatted text.
#[derive(Debug, FromCore)]
enum NoteOpReq {
    #[core(name = "InjectNote")]
    InjectNote(String, String), // parentTab, formatted
}

// =============================================================================
// answer_question bridge types
// =============================================================================

/// Mirrors Haskell: `AnswerInput { aiAgentId, aiQuestionId, aiAnswer }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "AnswerInput")]
struct AnswerToolInput {
    agent_id: String,
    question_id: String,
    answer: String,
}

/// Mirrors Haskell: `AnswerResult { arStatus, arAgentId, arQuestionId }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "AnswerResult")]
struct AnswerToolResult {
    status: String,
    agent_id: String,
    question_id: String,
}

// =============================================================================
// get_agent_messages bridge types
// =============================================================================

/// Mirrors Haskell: `MessagesInput { miAgentId, miTimeoutSecs }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "MessagesInput")]
struct MessagesToolInput {
    agent_id: String,
    timeout_secs: String,
}

/// Mirrors Haskell: `MessagesResult { mrMessagesJson, mrWarning }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "MessagesResult")]
struct MessagesToolResult {
    messages_json: String,
    warning: String,
}

/// Per-tool domain op for get_agent_messages. Timeout branching + inbox read in Rust.
#[derive(Debug, FromCore)]
enum MessagesOpReq {
    #[core(name = "FetchMessages")]
    FetchMessages(String, String), // agentId, timeoutSecs
}

// =============================================================================
// Compiled Tool Bundle
// =============================================================================

/// A compiled Haskell tool: Core expression + data constructor table.
struct CompiledTool {
    expr: CoreExpr,
    table: Arc<DataConTable>,
}

// =============================================================================
// Shared Effect Handlers
// =============================================================================

/// Generic handler for GetToolInput effect. Works for any tool input type.
/// Takes ownership of the input on first call; subsequent calls error.
struct ToolInputHandler<T: ToCore> {
    input: Option<T>,
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
struct IdentityHandler;

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
struct InboxHandler {
    project_dir: std::path::PathBuf,
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
struct QuestionsHandler {
    registry: Arc<crate::services::questions::QuestionRegistry>,
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
struct FormatOpHandler;

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
struct PopupOpHandler {
    zellij_session: Option<String>,
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
struct FilePROpHandler {
    jj: Arc<crate::services::jj_workspace::JjWorkspaceService>,
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
struct MergePROpHandler {
    jj: Arc<crate::services::jj_workspace::JjWorkspaceService>,
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
struct SpawnSubtreeOpHandler {
    agent_control: Arc<crate::services::agent_control::AgentControlService>,
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
struct SpawnLeafOpHandler {
    agent_control: Arc<crate::services::agent_control::AgentControlService>,
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
struct SpawnWorkerOpHandler {
    agent_control: Arc<crate::services::agent_control::AgentControlService>,
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
struct NotifyOpHandler;

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
struct NoteOpHandler;

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
struct MessagesOpHandler {
    project_dir: std::path::PathBuf,
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

// =============================================================================
// Tool Definitions
// =============================================================================

/// Interactive popup element.
#[derive(Deserialize, Serialize, JsonSchema)]
struct PopupElement {
    #[schemars(with = "String")]
    #[serde(rename = "type")]
    /// Element type: text, choice, checkbox, textbox, slider, multiselect.
    kind: String,
    /// Element ID for result lookup.
    id: Option<String>,
    /// Label text.
    label: Option<String>,
    /// For text elements.
    content: Option<String>,
    /// For choice/multiselect.
    options: Option<Vec<String>>,
    /// Default value (type varies).
    default: Option<JsonValue>,
    /// For textbox.
    placeholder: Option<String>,
    /// For slider.
    min: Option<f64>,
    /// For slider.
    max: Option<f64>,
    /// For multiline textbox.
    rows: Option<u32>,
}

/// Named wizard pane.
#[derive(Deserialize, Serialize, JsonSchema)]
struct WizardPane {
    /// Title text.
    title: String,
    /// UI elements to display.
    elements: Vec<PopupElement>,
    /// Transition: string (goto) or object (branch by field value).
    then: Option<JsonValue>,
}

/// Show interactive popup form and get user response.
#[derive(Deserialize, Serialize, JsonSchema)]
struct PopupArgs {
    /// Optional popup title.
    title: Option<String>,
    /// UI elements to display.
    elements: Option<Vec<PopupElement>>,
    /// Named wizard panes. Use with 'start' for multi-pane wizard mode.
    panes: Option<HashMap<String, WizardPane>>,
    /// Starting pane name (required when using panes).
    start: Option<String>,
}

fn tool_def<A: schemars::JsonSchema>(name: &str, description: &str) -> ToolDefinition {
    let schema = schemars::schema_for!(A);
    ToolDefinition {
        name: name.to_string(),
        description: description.to_string(),
        input_schema: serde_json::to_value(schema).unwrap(),
    }
}

fn popup_tool_definition() -> ToolDefinition {
    tool_def::<PopupArgs>(
        "popup",
        "Show interactive popup form and get user response",
    )
}

/// Create or update a pull request.
#[derive(Deserialize, JsonSchema)]
struct FilePRArgs {
    /// PR title.
    title: String,
    /// PR body/description.
    body: String,
    /// Target branch. Auto-detected from dot-separated naming if omitted. Only set to override.
    base_branch: Option<String>,
}

fn file_pr_tool_definition() -> ToolDefinition {
    tool_def::<FilePRArgs>(
        "file_pr",
        "Create or update a pull request for the current branch. Idempotent \u{2014} safe to call multiple times (updates existing PR). Pushes the branch automatically. Base branch auto-detected from dot-separated naming (e.g. main.foo.bar targets main.foo).",
    )
}

/// Merge a GitHub pull request.
#[derive(Deserialize, JsonSchema)]
struct MergePRArgs {
    /// PR number to merge.
    pr_number: u64,
    /// Merge strategy: squash (default), merge, or rebase.
    strategy: Option<String>,
    /// Working directory for git/jj operations.
    working_dir: Option<String>,
}

fn merge_pr_tool_definition() -> ToolDefinition {
    tool_def::<MergePRArgs>(
        "merge_pr",
        "Merge a GitHub pull request and fetch changes via jj",
    )
}

/// Task completion details.
#[derive(Deserialize, JsonSchema)]
struct TaskCompletedArg {
    /// Task description.
    what: String,
    /// Verification command that was run.
    how: String,
}

/// Signal completion to parent agent.
#[derive(Deserialize, JsonSchema)]
struct NotifyParentArgs {
    /// 'success' = work is done and review-clean. 'failure' = exhausted retries, escalating to parent.
    status: String,
    /// One-line summary. On success: what was accomplished. On failure: what went wrong.
    message: String,
    /// PR number if one was filed. Enables parent to immediately merge without searching.
    pr_number: Option<u64>,
    /// Array of {what, how} pairs. 'what' = task description, 'how' = verification command that was run.
    tasks_completed: Option<Vec<TaskCompletedArg>>,
}

fn notify_parent_tool_definition() -> ToolDefinition {
    tool_def::<NotifyParentArgs>(
        "notify_parent",
        "Signal to your parent that you are DONE. Call as your final action \u{2014} after PR is filed, Copilot feedback addressed, and changes pushed. Status 'success' means work is review-clean. Status 'failure' means retries exhausted, escalating to parent.",
    )
}

/// Fork a worktree node.
#[derive(Deserialize, JsonSchema)]
struct SpawnSubtreeArgs {
    /// Description of the sub-problem to solve.
    task: String,
    /// Branch name suffix (will be prefixed with current branch).
    branch_name: String,
}

fn spawn_subtree_tool_definition() -> ToolDefinition {
    tool_def::<SpawnSubtreeArgs>(
        "spawn_subtree",
        "Fork a worktree node off your current branch. Use when decomposing work into sub-problems that may need further decomposition. The child gets full coordination tools (can spawn its own children).",
    )
}

/// Fork a worktree for a leaf agent.
#[derive(Deserialize, JsonSchema)]
struct SpawnLeafArgs {
    /// Description of the sub-problem to solve.
    task: String,
    /// Branch name suffix (will be prefixed with current branch).
    branch_name: String,
}

fn spawn_leaf_subtree_tool_definition() -> ToolDefinition {
    tool_def::<SpawnLeafArgs>(
        "spawn_leaf_subtree",
        "Fork a worktree for a Gemini leaf agent. Gets own branch for PR filing but cannot spawn children.",
    )
}

/// Worker specification.
#[derive(Deserialize, JsonSchema)]
struct WorkerSpec {
    /// Human-readable name for the leaf agent.
    name: String,
    /// Short description of the task.
    task: String,
    /// Raw prompt (escape hatch). If provided, all other fields except name are ignored.
    prompt: Option<String>,
    /// Numbered implementation steps.
    steps: Option<Vec<String>>,
    /// Freeform context: code snippets, examples, detailed specs.
    context: Option<String>,
    /// Acceptance criteria for completion.
    done_criteria: Option<Vec<String>>,
    /// Commands to verify the work.
    verify: Option<Vec<String>>,
    /// Things the agent must NOT do.
    boundary: Option<Vec<String>>,
    /// Files the agent should read before starting.
    read_first: Option<Vec<String>>,
}

/// Spawn multiple worker agents.
#[derive(Deserialize, JsonSchema)]
struct SpawnWorkersArgs {
    /// Array of worker specifications.
    specs: Vec<WorkerSpec>,
}

fn spawn_workers_tool_definition() -> ToolDefinition {
    tool_def::<SpawnWorkersArgs>(
        "spawn_workers",
        "Spawn multiple worker agents in one call. Each gets a Zellij pane in the current worktree.",
    )
}

/// Send a note.
#[derive(Deserialize, JsonSchema)]
struct NoteArgs {
    /// Note content.
    content: String,
}

fn note_tool_definition() -> ToolDefinition {
    tool_def::<NoteArgs>(
        "note",
        "Send a note to the team lead's inbox. Fire-and-forget.",
    )
}

/// Answer a pending question.
#[derive(Deserialize, JsonSchema)]
struct AnswerQuestionArgs {
    /// Agent that asked the question.
    agent_id: String,
    /// Question ID from the question message.
    question_id: String,
    /// Answer text.
    answer: String,
}

fn answer_question_tool_definition() -> ToolDefinition {
    tool_def::<AnswerQuestionArgs>(
        "answer_question",
        "Answer a pending question from an agent. Unblocks the agent immediately.",
    )
}

/// Read agent messages.
#[derive(Deserialize, JsonSchema)]
struct GetAgentMessagesArgs {
    /// Filter to messages from this agent (empty = all agents).
    agent_id: Option<String>,
    /// Long-poll timeout in seconds (0 = immediate return).
    timeout_secs: Option<u64>,
}

fn get_agent_messages_tool_definition() -> ToolDefinition {
    tool_def::<GetAgentMessagesArgs>(
        "get_agent_messages",
        "Read unread messages from agents. Supports long-polling with timeout.",
    )
}

// =============================================================================
// TidepoolBackend
// =============================================================================

/// Cranelift-compiled Haskell Core backend.
///
/// Each tool is compiled from Haskell at build time. At runtime, `call_tool`
/// builds a per-tool `EffectHandler`, wraps it in an HList, and runs it through
/// `EffectMachine`. The machine evaluates the Core expression, yielding effects
/// that the handler processes.
pub struct TidepoolBackend {
    /// Compiled Haskell tool expressions + data constructor tables.
    tools: HashMap<String, CompiledTool>,

    /// Agent control service for spawn operations.
    agent_control: Arc<crate::services::agent_control::AgentControlService>,

    /// JJ workspace service for file_pr and merge_pr.
    jj: Arc<crate::services::jj_workspace::JjWorkspaceService>,

    /// Question registry for answer_question (bridges oneshot channels).
    question_registry: Arc<crate::services::questions::QuestionRegistry>,

    /// Project directory for inbox file paths.
    project_dir: std::path::PathBuf,

    /// Zellij session name for popup and other UI services.
    zellij_session: Option<String>,

    /// Agent identity context, threaded as user data to effect handlers.
    ctx: EffectContext,
}

impl TidepoolBackend {
    pub fn new(zellij_session: Option<String>, ctx: EffectContext) -> Self {
        let mut tools = HashMap::new();

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/PopupEffect.hs::popupTool");
        tools.insert(
            "popup".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/FilePREffect.hs::filePRTool");
        tools.insert(
            "file_pr".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/MergePREffect.hs::mergePRTool");
        tools.insert(
            "merge_pr".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/NotifyEffect.hs::notifyTool");
        tools.insert(
            "notify_parent".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/SpawnEffect.hs::spawnSubtreeTool");
        tools.insert(
            "spawn_subtree".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/SpawnLeafEffect.hs::spawnLeafTool");
        tools.insert(
            "spawn_leaf_subtree".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/SpawnWorkersEffect.hs::spawnWorkersTool");
        tools.insert(
            "spawn_workers".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/NoteEffect.hs::noteTool");
        tools.insert(
            "note".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/AnswerEffect.hs::answerTool");
        tools.insert(
            "answer_question".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/MessagesEffect.hs::messagesTool");
        tools.insert(
            "get_agent_messages".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let working_dir = crate::services::agent_control::resolve_agent_working_dir(&ctx);
        let jj = Arc::new(crate::services::jj_workspace::JjWorkspaceService::new(
            working_dir.clone(),
        ));

        let agent_control = Arc::new(
            crate::services::agent_control::AgentControlService::new(
                working_dir.clone(),
                None,
                jj.clone(),
            )
            .with_birth_branch(ctx.birth_branch.clone())
            .with_zellij_session(zellij_session.clone().unwrap_or_default()),
        );

        let question_registry = Arc::new(crate::services::questions::QuestionRegistry::new());

        Self {
            tools,
            agent_control,
            jj,
            question_registry,
            project_dir: working_dir,
            zellij_session,
            ctx,
        }
    }

    /// Run a compiled Haskell tool through the EffectMachine with the given handlers.
    fn run_effect<H: DispatchEffect<EffectContext>>(
        &self,
        tool_name: &str,
        handlers: &mut H,
    ) -> Result<Value, anyhow::Error> {
        let tool = self
            .tools
            .get(tool_name)
            .ok_or_else(|| anyhow::anyhow!("No compiled tool: {}", tool_name))?;
        let mut heap = tidepool_eval::heap::VecHeap::new();
        let mut machine = tidepool_effect::EffectMachine::new(&tool.table, &mut heap)
            .map_err(|e| anyhow::anyhow!("EffectMachine init: {}", e))?;
        machine
            .run_with_user(&tool.expr, handlers, &self.ctx)
            .map_err(|e| anyhow::anyhow!("EffectMachine run: {}", e))
    }

    fn run_and_decode<R: FromCore>(
        &self,
        tool_name: &str,
        handlers: &mut impl DispatchEffect<EffectContext>,
        result_to_json: impl FnOnce(R) -> serde_json::Value,
    ) -> Result<MCPCallOutput> {
        match self.run_effect(tool_name, handlers) {
            Ok(result_value) => {
                let table = &self.tools[tool_name].table;
                let result = R::from_value(&result_value, table)
                    .map_err(|e| anyhow::anyhow!("Bridge decode error: {}", e))?;
                Ok(MCPCallOutput {
                    success: true,
                    result: Some(result_to_json(result)),
                    error: None,
                })
            }
            Err(e) => {
                tracing::error!(tool = %tool_name, error = %e, "Tidepool tool failed");
                Ok(MCPCallOutput {
                    success: false,
                    result: None,
                    error: Some(e.to_string()),
                })
            }
        }
    }

    async fn call_popup(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: PopupArgs = serde_json::from_value(args)?;
        let title = args.title.unwrap_or_default();

        let components = if let Some(panes) = args.panes {
            let mut wizard = serde_json::Map::new();
            wizard.insert("panes".to_string(), serde_json::to_value(panes)?);
            if let Some(s) = args.start {
                wizard.insert("start".to_string(), serde_json::Value::String(s));
            }
            serde_json::to_string(&JsonValue::Object(wizard)).unwrap_or_default()
        } else if let Some(elements) = args.elements {
            serde_json::to_string(&elements).unwrap_or_default()
        } else {
            "[]".to_string()
        };

        debug!(
            title = %title,
            components_len = components.len(),
            "Tidepool popup call via EffectMachine"
        );

        // HList order must match Haskell Eff '[PopupInput', Identity, PopupOp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(ToolInput { title, components }),
            },
            IdentityHandler,
            PopupOpHandler {
                zellij_session: self.zellij_session.clone(),
            },
        ];

        self.run_and_decode("popup", &mut handlers, |response: PopupResponse| {
            let values: JsonValue = serde_json::from_str(&response.values)
                .unwrap_or(JsonValue::Object(serde_json::Map::new()));
            serde_json::json!({
                "button": response.button,
                "values": values,
            })
        })
    }

    async fn call_file_pr(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: FilePRArgs = serde_json::from_value(args)?;
        debug!(title = %args.title, "Tidepool file_pr call via EffectMachine");

        // HList order must match Haskell Eff '[FilePRInput', Identity, FilePROp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(FilePRToolInput {
                    title: args.title,
                    body: args.body,
                    base_branch: args.base_branch.unwrap_or_default(),
                }),
            },
            IdentityHandler,
            FilePROpHandler {
                jj: self.jj.clone(),
            },
        ];

        self.run_and_decode("file_pr", &mut handlers, |result: FilePRToolResult| {
            serde_json::json!({
                "pr_url": result.pr_url,
                "pr_number": result.pr_number.parse::<u64>().unwrap_or(0),
                "head_branch": result.head_branch,
                "base_branch": result.result_base,
                "created": result.created == "true",
            })
        })
    }

    async fn call_merge_pr(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: MergePRArgs = serde_json::from_value(args)?;
        debug!(
            pr_number = args.pr_number,
            strategy = %args.strategy.as_deref().unwrap_or("squash"),
            "Tidepool merge_pr call via EffectMachine"
        );

        // HList order must match Haskell Eff '[MergePRInput', Identity, MergePROp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(MergePRToolInput {
                    pr_number: args.pr_number.to_string(),
                    strategy: args.strategy.unwrap_or_else(|| "squash".to_string()),
                }),
            },
            IdentityHandler,
            MergePROpHandler {
                jj: self.jj.clone(),
            },
        ];

        self.run_and_decode("merge_pr", &mut handlers, |result: MergePRToolResult| {
            serde_json::json!({
                "success": result.success == "true",
                "message": result.message,
                "jj_fetched": result.jj_fetched == "true",
            })
        })
    }

    async fn call_notify_parent(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: NotifyParentArgs = serde_json::from_value(args)?;
        debug!(status = %args.status, "Tidepool notify_parent call via EffectMachine");

        // HList order must match Haskell Eff '[NotifyInput', Identity, FormatOp, NotifyOp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(NotifyToolInput {
                    status: args.status,
                    message: args.message,
                }),
            },
            IdentityHandler,
            FormatOpHandler,
            NotifyOpHandler,
        ];

        self.run_and_decode(
            "notify_parent",
            &mut handlers,
            |result: NotifyToolResult| serde_json::json!({"ack": result.ack}),
        )
    }

    async fn call_spawn_subtree(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: SpawnSubtreeArgs = serde_json::from_value(args)?;
        debug!(branch = %args.branch_name, "Tidepool spawn_subtree call via EffectMachine");

        // HList order must match Haskell Eff '[SpawnSubtreeInput', SpawnSubtreeOp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(SpawnSubtreeToolInput {
                    task: args.task,
                    branch_name: args.branch_name,
                }),
            },
            SpawnSubtreeOpHandler {
                agent_control: self.agent_control.clone(),
            },
        ];

        self.run_and_decode(
            "spawn_subtree",
            &mut handlers,
            |result: SpawnSubtreeToolResult| {
                serde_json::json!({
                    "tab_name": result.tab_name,
                    "branch_name": result.branch_name,
                })
            },
        )
    }

    async fn call_spawn_leaf(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: SpawnLeafArgs = serde_json::from_value(args)?;
        debug!(branch = %args.branch_name, "Tidepool spawn_leaf_subtree call via EffectMachine");

        // HList order must match Haskell Eff '[SpawnLeafInput', SpawnLeafOp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(SpawnLeafToolInput {
                    task: args.task,
                    branch_name: args.branch_name,
                }),
            },
            SpawnLeafOpHandler {
                agent_control: self.agent_control.clone(),
            },
        ];

        self.run_and_decode(
            "spawn_leaf_subtree",
            &mut handlers,
            |result: SpawnLeafToolResult| {
                serde_json::json!({
                    "tab_name": result.tab_name,
                    "branch_name": result.branch_name,
                })
            },
        )
    }

    async fn call_spawn_workers(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: SpawnWorkersArgs = serde_json::from_value(args)?;
        debug!(count = args.specs.len(), "Tidepool spawn_workers call via EffectMachine");

        // Pre-render specs into WorkerSpecBridge (name + prompt)
        let worker_specs: Vec<WorkerSpecBridge> = args.specs
            .iter()
            .map(|spec| {
                let prompt = if let Some(p) = &spec.prompt {
                    p.clone()
                } else {
                    let mut parts = vec![format!("Task: {}", spec.task)];
                    if let Some(steps) = &spec.steps {
                        parts.push("Steps:".to_string());
                        for (i, s) in steps.iter().enumerate() {
                            parts.push(format!("{}. {}", i + 1, s));
                        }
                    }
                    if let Some(context) = &spec.context {
                        parts.push(format!("\nContext:\n{}", context));
                    }
                    if let Some(done) = &spec.done_criteria {
                        parts.push("Done when:".to_string());
                        for d in done {
                            parts.push(format!("- {}", d));
                        }
                    }
                    if let Some(verify) = &spec.verify {
                        parts.push("Verify:".to_string());
                        for v in verify {
                            parts.push(format!("$ {}", v));
                        }
                    }
                    if let Some(boundary) = &spec.boundary {
                        parts.push("DO NOT:".to_string());
                        for b in boundary {
                            parts.push(format!("- {}", b));
                        }
                    }
                    if let Some(read) = &spec.read_first {
                        parts.push("Read first:".to_string());
                        for r in read {
                            parts.push(format!("- {}", r));
                        }
                    }
                    parts.join("\n")
                };
                WorkerSpecBridge { name: spec.name.clone(), prompt }
            })
            .collect();

        let spec_names: Vec<String> =
            worker_specs.iter().map(|s| s.name.clone()).collect();

        // HList order must match Haskell Eff '[SpawnWorkersInput', Identity, SpawnWorkerOp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(worker_specs),
            },
            IdentityHandler,
            SpawnWorkerOpHandler {
                agent_control: self.agent_control.clone(),
            },
        ];

        self.run_and_decode(
            "spawn_workers",
            &mut handlers,
            |results: Vec<SpawnWorkerToolResult>| {
                let workers: Vec<_> = spec_names
                    .iter()
                    .zip(results.iter())
                    .map(|(name, r)| {
                        serde_json::json!({
                            "name": name,
                            "tab_name": r.tab_name,
                            "success": true,
                        })
                    })
                    .collect();
                serde_json::json!({ "workers": workers })
            },
        )
    }

    async fn call_note(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: NoteArgs = serde_json::from_value(args)?;
        debug!(agent = %self.ctx.agent_name, "Tidepool note call via EffectMachine");

        // HList order must match Haskell Eff '[NoteInput', Identity, FormatOp, Inbox, NoteOp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(NoteToolInput {
                    content: args.content,
                }),
            },
            IdentityHandler,
            FormatOpHandler,
            InboxHandler {
                project_dir: self.project_dir.clone(),
            },
            NoteOpHandler,
        ];

        self.run_and_decode("note", &mut handlers, |_: NoteToolResult| {
            serde_json::json!({"ack": true})
        })
    }

    async fn call_answer_question(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: AnswerQuestionArgs = serde_json::from_value(args)?;
        debug!(
            agent = %args.agent_id,
            question_id = %args.question_id,
            "Tidepool answer_question call via EffectMachine"
        );

        // HList order must match Haskell Eff '[AnswerInput', Inbox, Questions]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(AnswerToolInput {
                    agent_id: args.agent_id,
                    question_id: args.question_id,
                    answer: args.answer,
                }),
            },
            InboxHandler {
                project_dir: self.project_dir.clone(),
            },
            QuestionsHandler {
                registry: self.question_registry.clone(),
            },
        ];

        self.run_and_decode(
            "answer_question",
            &mut handlers,
            |result: AnswerToolResult| {
                serde_json::json!({
                    "status": result.status,
                    "agent_id": result.agent_id,
                    "question_id": result.question_id,
                })
            },
        )
    }

    async fn call_get_messages(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: GetAgentMessagesArgs = serde_json::from_value(args)?;
        debug!(
            agent = %args.agent_id.as_deref().unwrap_or(""),
            timeout_secs = args.timeout_secs.unwrap_or(0),
            "Tidepool get_agent_messages call via EffectMachine"
        );

        // HList order must match Haskell Eff '[MessagesInput', MessagesOp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(MessagesToolInput {
                    agent_id: args.agent_id.unwrap_or_default(),
                    timeout_secs: args.timeout_secs.unwrap_or(0).to_string(),
                }),
            },
            MessagesOpHandler {
                project_dir: self.project_dir.clone(),
            },
        ];

        self.run_and_decode(
            "get_agent_messages",
            &mut handlers,
            |result: MessagesToolResult| {
                let messages: JsonValue = serde_json::from_str(&result.messages_json)
                    .unwrap_or(JsonValue::Array(Vec::new()));
                let count = messages.as_array().map(|a| a.len()).unwrap_or(0);
                serde_json::json!({
                    "messages": messages,
                    "count": count,
                })
            },
        )
    }
}

#[async_trait]
impl RuntimeBackend for TidepoolBackend {
    async fn list_tools(&self, role: &str) -> Result<Vec<ToolDefinition>> {
        debug!(role = %role, "Listing tools from tidepool backend");
        Ok(match role {
            "tl" => vec![
                popup_tool_definition(),
                file_pr_tool_definition(),
                merge_pr_tool_definition(),
                notify_parent_tool_definition(),
                spawn_subtree_tool_definition(),
                spawn_leaf_subtree_tool_definition(),
                spawn_workers_tool_definition(),
                note_tool_definition(),
                answer_question_tool_definition(),
                get_agent_messages_tool_definition(),
            ],
            "dev" => vec![
                file_pr_tool_definition(),
                notify_parent_tool_definition(),
                note_tool_definition(),
            ],
            "worker" => vec![
                notify_parent_tool_definition(),
                note_tool_definition(),
            ],
            _ => vec![],
        })
    }

    async fn call_tool(
        &self,
        role: &str,
        tool_name: &str,
        args: JsonValue,
    ) -> Result<MCPCallOutput> {
        debug!(role = %role, tool = %tool_name, "Executing tool via tidepool");
        match tool_name {
            "popup" => self.call_popup(args).await,
            "file_pr" => self.call_file_pr(args).await,
            "merge_pr" => self.call_merge_pr(args).await,
            "notify_parent" => self.call_notify_parent(args).await,
            "spawn_subtree" => self.call_spawn_subtree(args).await,
            "spawn_leaf_subtree" => self.call_spawn_leaf(args).await,
            "spawn_workers" => self.call_spawn_workers(args).await,
            "note" => self.call_note(args).await,
            "answer_question" => self.call_answer_question(args).await,
            "get_agent_messages" => self.call_get_messages(args).await,
            _ => Ok(MCPCallOutput {
                success: false,
                result: None,
                error: Some(format!("Unknown tool: {}", tool_name)),
            }),
        }
    }

    async fn handle_hook(&self, input: &JsonValue) -> Result<JsonValue> {
        debug!("Handling hook via tidepool (passthrough)");
        let _ = input;
        Ok(serde_json::json!({"continue": true}))
    }

    async fn reload_if_changed(&self) -> Result<bool> {
        Ok(false)
    }

    fn content_hash(&self) -> String {
        "tidepool-static".to_string()
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    struct MockIdentity;
    impl EffectHandler<EffectContext> for MockIdentity {
        type Request = IdentityReq;
        fn handle(
            &mut self,
            req: IdentityReq,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            match req {
                IdentityReq::GetAgentId => cx.respond("test".to_string()),
                IdentityReq::GetParentTab => cx.respond("parent-tab".to_string()),
                IdentityReq::GetOwnTab => cx.respond("test-tab".to_string()),
                IdentityReq::GetWorkingDir => cx.respond(".".to_string()),
            }
        }
    }

    struct MockInbox;
    impl EffectHandler<EffectContext> for MockInbox {
        type Request = InboxReq;
        fn handle(
            &mut self,
            req: InboxReq,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            match req {
                InboxReq::WriteMessage(_, _, _, _) => cx.respond("ok".to_string()),
                InboxReq::ReadMessages(_) => cx.respond("[]".to_string()),
                InboxReq::PollMessages(_, _) => cx.respond("[]".to_string()),
            }
        }
    }

    struct MockQuestions;
    impl EffectHandler<EffectContext> for MockQuestions {
        type Request = QuestionsReq;
        fn handle(
            &mut self,
            req: QuestionsReq,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            match req {
                QuestionsReq::ResolveQuestion(_, _) => cx.respond("true".to_string()),
            }
        }
    }

    #[test]
    fn test_popup_tool_definition_schema() {
        let def = popup_tool_definition();
        assert_eq!(def.name, "popup");
        assert_eq!(def.input_schema["type"], "object");
        assert!(def.input_schema["properties"]["elements"].is_object());
        assert!(def.input_schema["properties"]["panes"].is_object());
        assert!(def.input_schema["properties"]["title"].is_object());
        assert!(def.input_schema["properties"]["start"].is_object());
    }

    #[test]
    fn test_tool_input_roundtrip() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );
        let table = &backend.tools["popup"].table;
        let input = ToolInput {
            title: "Pick one".to_string(),
            components: r#"[{"type":"choice"}]"#.to_string(),
        };
        let value = input.to_value(table).unwrap();
        let back = ToolInput::from_value(&value, table).unwrap();
        assert_eq!(input.title, back.title);
        assert_eq!(input.components, back.components);
    }

    #[test]
    fn test_popup_response_roundtrip() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );
        let table = &backend.tools["popup"].table;
        let resp = PopupResponse {
            button: "submit".to_string(),
            values: r#"{"pick":"A"}"#.to_string(),
        };
        let value = resp.to_value(table).unwrap();
        let back = PopupResponse::from_value(&value, table).unwrap();
        assert_eq!(resp.button, back.button);
        assert_eq!(resp.values, back.values);
    }

    #[tokio::test]
    async fn test_list_tools_returns_popup() {
        let ctx = EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        };
        let backend = TidepoolBackend::new(None, ctx);
        let tools = backend.list_tools("tl").await.unwrap();
        assert!(tools.iter().any(|t| t.name == "popup"));
    }

    #[test]
    fn test_file_pr_tool_definition_schema() {
        let def = file_pr_tool_definition();
        assert_eq!(def.name, "file_pr");
        assert_eq!(def.input_schema["type"], "object");
        assert!(def.input_schema["properties"]["title"].is_object());
        assert!(def.input_schema["properties"]["body"].is_object());
    }

    #[test]
    fn test_merge_pr_tool_definition_schema() {
        let def = merge_pr_tool_definition();
        assert_eq!(def.name, "merge_pr");
        assert_eq!(def.input_schema["type"], "object");
        assert!(def.input_schema["properties"]["pr_number"].is_object());
    }

    #[test]
    fn test_notify_parent_tool_definition_schema() {
        let def = notify_parent_tool_definition();
        assert_eq!(def.name, "notify_parent");
        assert_eq!(def.input_schema["type"], "object");
        assert!(def.input_schema["properties"]["status"].is_object());
    }

    #[tokio::test]
    async fn test_list_tools_by_role() {
        let ctx = EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        };
        let backend = TidepoolBackend::new(None, ctx);

        let tl = backend.list_tools("tl").await.unwrap();
        assert_eq!(tl.len(), 10);
        assert!(tl.iter().any(|t| t.name == "popup"));
        assert!(tl.iter().any(|t| t.name == "file_pr"));
        assert!(tl.iter().any(|t| t.name == "merge_pr"));
        assert!(tl.iter().any(|t| t.name == "notify_parent"));
        assert!(tl.iter().any(|t| t.name == "spawn_subtree"));
        assert!(tl.iter().any(|t| t.name == "spawn_leaf_subtree"));
        assert!(tl.iter().any(|t| t.name == "spawn_workers"));
        assert!(tl.iter().any(|t| t.name == "note"));
        assert!(tl.iter().any(|t| t.name == "answer_question"));
        assert!(tl.iter().any(|t| t.name == "get_agent_messages"));

        let dev = backend.list_tools("dev").await.unwrap();
        assert_eq!(dev.len(), 3);
        assert!(dev.iter().any(|t| t.name == "file_pr"));
        assert!(dev.iter().any(|t| t.name == "notify_parent"));
        assert!(dev.iter().any(|t| t.name == "note"));

        let worker = backend.list_tools("worker").await.unwrap();
        assert_eq!(worker.len(), 2);
        assert!(worker.iter().any(|t| t.name == "notify_parent"));
        assert!(worker.iter().any(|t| t.name == "note"));

        let unknown = backend.list_tools("unknown").await.unwrap();
        assert!(unknown.is_empty());
    }

    #[test]
    fn test_note_tool_definition_schema() {
        let def = note_tool_definition();
        assert_eq!(def.name, "note");
        assert!(def.input_schema["properties"]["content"].is_object());
    }

    #[test]
    fn test_answer_question_tool_definition_schema() {
        let def = answer_question_tool_definition();
        assert_eq!(def.name, "answer_question");
        assert!(def.input_schema["properties"]["agent_id"].is_object());
        assert!(def.input_schema["properties"]["question_id"].is_object());
        assert!(def.input_schema["properties"]["answer"].is_object());
    }

    #[test]
    fn test_get_agent_messages_tool_definition_schema() {
        let def = get_agent_messages_tool_definition();
        assert_eq!(def.name, "get_agent_messages");
        assert!(def.input_schema["properties"]["agent_id"].is_object());
        assert!(def.input_schema["properties"]["timeout_secs"].is_object());
    }

    #[test]
    fn test_note_bridge_roundtrip() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );
        let table = &backend.tools["note"].table;
        let input = NoteToolInput {
            content: "hello TL".to_string(),
        };
        let value = input.to_value(table).unwrap();
        let back = NoteToolInput::from_value(&value, table).unwrap();
        assert_eq!(input.content, back.content);
    }

    #[test]
    fn test_answer_bridge_roundtrip() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );
        let table = &backend.tools["answer_question"].table;
        let input = AnswerToolInput {
            agent_id: "worker-1".to_string(),
            question_id: "q-123".to_string(),
            answer: "yes".to_string(),
        };
        let value = input.to_value(table).unwrap();
        let back = AnswerToolInput::from_value(&value, table).unwrap();
        assert_eq!(input.agent_id, back.agent_id);
        assert_eq!(input.question_id, back.question_id);
        assert_eq!(input.answer, back.answer);
    }

    #[test]
    fn test_messages_bridge_roundtrip() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );
        let table = &backend.tools["get_agent_messages"].table;
        let input = MessagesToolInput {
            agent_id: "worker-1".to_string(),
            timeout_secs: "30".to_string(),
        };
        let value = input.to_value(table).unwrap();
        let back = MessagesToolInput::from_value(&value, table).unwrap();
        assert_eq!(input.agent_id, back.agent_id);
        assert_eq!(input.timeout_secs, back.timeout_secs);
    }

    #[test]
    fn test_note_effect_pipeline() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );

        let tool_input = NoteToolInput {
            content: "progress update".to_string(),
        };

        // Mock NoteOp handler (tag 4): accepts InjectNote, returns NoteResult
        struct MockNoteOp;
        impl EffectHandler<EffectContext> for MockNoteOp {
            type Request = NoteOpReq;
            fn handle(
                &mut self,
                req: NoteOpReq,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                match req {
                    NoteOpReq::InjectNote(_, _) => {
                        cx.respond(NoteToolResult {
                            ack: "sent".to_string(),
                        })
                    }
                }
            }
        }

        // Eff '[NoteInput', Identity, FormatOp, Inbox, NoteOp]
        let tool = &backend.tools["note"];
        let mut handlers = frunk::hlist![
            ToolInputHandler { input: Some(tool_input) },
            MockIdentity,
            FormatOpHandler,
            MockInbox,
            MockNoteOp,
        ];
        let mut heap = tidepool_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = NoteToolResult::from_value(&result, &tool.table)
            .expect("Should decode NoteToolResult");
        assert_eq!(response.ack, "sent");
    }

    #[test]
    fn test_answer_effect_pipeline() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );

        let tool_input = AnswerToolInput {
            agent_id: "worker-1".to_string(),
            question_id: "q-42".to_string(),
            answer: "use option B".to_string(),
        };

        // Eff '[AnswerInput', Inbox, Questions]
        let tool = &backend.tools["answer_question"];
        let mut handlers = frunk::hlist![
            ToolInputHandler { input: Some(tool_input) },
            MockInbox,
            MockQuestions,
        ];
        let mut heap = tidepool_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = AnswerToolResult::from_value(&result, &tool.table)
            .expect("Should decode AnswerToolResult");
        assert_eq!(response.status, "answered");
        assert_eq!(response.agent_id, "worker-1");
        assert_eq!(response.question_id, "q-42");
    }

    #[test]
    fn test_messages_effect_pipeline() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );

        let tool_input = MessagesToolInput {
            agent_id: "worker-1".to_string(),
            timeout_secs: "0".to_string(),
        };

        // Mock MessagesOp handler (tag 1)
        struct MockMessagesOp;
        impl EffectHandler<EffectContext> for MockMessagesOp {
            type Request = MessagesOpReq;
            fn handle(
                &mut self,
                req: MessagesOpReq,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                match req {
                    MessagesOpReq::FetchMessages(agent, _timeout) => {
                        cx.respond(MessagesToolResult {
                            messages_json: format!(
                                "[{{\"from\":\"{}\",\"text\":\"hello\"}}]",
                                agent
                            ),
                            warning: "".to_string(),
                        })
                    }
                }
            }
        }

        // Eff '[MessagesInput', MessagesOp]
        let tool = &backend.tools["get_agent_messages"];
        let mut handlers = frunk::hlist![
            ToolInputHandler { input: Some(tool_input) },
            MockMessagesOp,
        ];
        let mut heap = tidepool_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = MessagesToolResult::from_value(&result, &tool.table)
            .expect("Should decode MessagesToolResult");
        assert!(response.messages_json.contains("worker-1"));
        assert!(response.warning.is_empty());
    }

    #[test]
    fn test_spawn_subtree_tool_definition_schema() {
        let def = spawn_subtree_tool_definition();
        assert_eq!(def.name, "spawn_subtree");
        assert!(def.input_schema["properties"]["task"].is_object());
        assert!(def.input_schema["properties"]["branch_name"].is_object());
    }

    #[test]
    fn test_spawn_leaf_tool_definition_schema() {
        let def = spawn_leaf_subtree_tool_definition();
        assert_eq!(def.name, "spawn_leaf_subtree");
        assert!(def.input_schema["properties"]["task"].is_object());
    }

    #[test]
    fn test_spawn_workers_tool_definition_schema() {
        let def = spawn_workers_tool_definition();
        assert_eq!(def.name, "spawn_workers");
        assert!(def.input_schema["properties"]["specs"].is_object());
    }

    #[test]
    fn test_spawn_subtree_bridge_roundtrip() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );
        let table = &backend.tools["spawn_subtree"].table;
        let input = SpawnSubtreeToolInput {
            task: "implement feature".to_string(),
            branch_name: "feature-a".to_string(),
        };
        let value = input.to_value(table).unwrap();
        let back = SpawnSubtreeToolInput::from_value(&value, table).unwrap();
        assert_eq!(input.task, back.task);
        assert_eq!(input.branch_name, back.branch_name);
    }

    #[test]
    fn test_spawn_leaf_bridge_roundtrip() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );
        let table = &backend.tools["spawn_leaf_subtree"].table;
        let input = SpawnLeafToolInput {
            task: "implement leaf".to_string(),
            branch_name: "leaf-1".to_string(),
        };
        let value = input.to_value(table).unwrap();
        let back = SpawnLeafToolInput::from_value(&value, table).unwrap();
        assert_eq!(input.task, back.task);
        assert_eq!(input.branch_name, back.branch_name);
    }

    #[test]
    fn test_worker_spec_bridge_roundtrip() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );
        let table = &backend.tools["spawn_workers"].table;
        let input = WorkerSpecBridge {
            name: "worker-1".to_string(),
            prompt: "do something".to_string(),
        };
        let value = input.to_value(table).unwrap();
        let back = WorkerSpecBridge::from_value(&value, table).unwrap();
        assert_eq!(input.name, back.name);
        assert_eq!(input.prompt, back.prompt);
    }

    #[test]
    fn test_spawn_subtree_effect_pipeline() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );

        let tool_input = SpawnSubtreeToolInput {
            task: "build feature".to_string(),
            branch_name: "feat".to_string(),
        };

        // Mock SpawnSubtreeOp handler (tag 1)
        struct MockSpawnOp;
        impl EffectHandler<EffectContext> for MockSpawnOp {
            type Request = SpawnSubtreeOpReq;
            fn handle(
                &mut self,
                req: SpawnSubtreeOpReq,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                match req {
                    SpawnSubtreeOpReq::SpawnSubtree(_task, branch) => {
                        let result = SpawnSubtreeToolResult {
                            tab_name: format!("🧠 {}", branch),
                            branch_name: format!("main.{}", branch),
                        };
                        cx.respond(result)
                    }
                }
            }
        }

        // Eff '[SpawnSubtreeInput', SpawnSubtreeOp]
        let tool = &backend.tools["spawn_subtree"];
        let mut handlers = frunk::hlist![
            ToolInputHandler { input: Some(tool_input) },
            MockSpawnOp,
        ];
        let mut heap = tidepool_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = SpawnSubtreeToolResult::from_value(&result, &tool.table)
            .expect("Should decode SpawnSubtreeToolResult");
        assert!(response.tab_name.contains("feat"));
        assert_eq!(response.branch_name, "main.feat");
    }

    #[test]
    fn test_spawn_leaf_effect_pipeline() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );

        let tool_input = SpawnLeafToolInput {
            task: "build leaf".to_string(),
            branch_name: "leaf-1".to_string(),
        };

        // Mock SpawnLeafOp handler (tag 1)
        struct MockLeafOp;
        impl EffectHandler<EffectContext> for MockLeafOp {
            type Request = SpawnLeafOpReq;
            fn handle(
                &mut self,
                req: SpawnLeafOpReq,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                match req {
                    SpawnLeafOpReq::SpawnLeaf(_task, branch) => {
                        let result = SpawnLeafToolResult {
                            tab_name: format!("♊ {}", branch),
                            branch_name: format!("main.{}", branch),
                        };
                        cx.respond(result)
                    }
                }
            }
        }

        // Eff '[SpawnLeafInput', SpawnLeafOp]
        let tool = &backend.tools["spawn_leaf_subtree"];
        let mut handlers = frunk::hlist![
            ToolInputHandler { input: Some(tool_input) },
            MockLeafOp,
        ];
        let mut heap = tidepool_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = SpawnLeafToolResult::from_value(&result, &tool.table)
            .expect("Should decode SpawnLeafToolResult");
        assert!(response.tab_name.contains("leaf-1"));
    }

    #[test]
    fn test_file_pr_bridge_roundtrip() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );
        let table = &backend.tools["file_pr"].table;
        let input = FilePRToolInput {
            title: "feat: add tests".to_string(),
            body: "Adds unit tests".to_string(),
            base_branch: "main".to_string(),
        };
        let value = input.to_value(table).unwrap();
        let back = FilePRToolInput::from_value(&value, table).unwrap();
        assert_eq!(input.title, back.title);
        assert_eq!(input.body, back.body);
        assert_eq!(input.base_branch, back.base_branch);
    }

    #[test]
    fn test_merge_pr_bridge_roundtrip() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );
        let table = &backend.tools["merge_pr"].table;
        let input = MergePRToolInput {
            pr_number: "42".to_string(),
            strategy: "squash".to_string(),
        };
        let value = input.to_value(table).unwrap();
        let back = MergePRToolInput::from_value(&value, table).unwrap();
        assert_eq!(input.pr_number, back.pr_number);
        assert_eq!(input.strategy, back.strategy);
    }

    #[test]
    fn test_notify_bridge_roundtrip() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );
        let table = &backend.tools["notify_parent"].table;
        let input = NotifyToolInput {
            status: "success".to_string(),
            message: "All done".to_string(),
        };
        let value = input.to_value(table).unwrap();
        let back = NotifyToolInput::from_value(&value, table).unwrap();
        assert_eq!(input.status, back.status);
        assert_eq!(input.message, back.message);
    }

    #[test]
    fn test_file_pr_effect_pipeline() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );

        let tool_input = FilePRToolInput {
            title: "Test PR".to_string(),
            body: "Test body".to_string(),
            base_branch: "main".to_string(),
        };

        // Mock FilePROp handler (tag 2)
        struct MockFilePROp;
        impl EffectHandler<EffectContext> for MockFilePROp {
            type Request = FilePROpReq;
            fn handle(
                &mut self,
                req: FilePROpReq,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                match req {
                    FilePROpReq::CreateOrUpdatePR(_title, _body, base, _dir) => {
                        let result = FilePRToolResult {
                            pr_url: "https://github.com/test/test/pull/1".to_string(),
                            pr_number: "1".to_string(),
                            head_branch: "test-branch".to_string(),
                            result_base: base,
                            created: "true".to_string(),
                        };
                        cx.respond(result)
                    }
                }
            }
        }

        // Eff '[FilePRInput', Identity, FilePROp]
        let tool = &backend.tools["file_pr"];
        let mut handlers = frunk::hlist![
            ToolInputHandler { input: Some(tool_input) },
            MockIdentity,
            MockFilePROp,
        ];
        let mut heap = tidepool_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = FilePRToolResult::from_value(&result, &tool.table)
            .expect("Should decode FilePRToolResult");
        assert_eq!(response.pr_number, "1");
        assert_eq!(response.result_base, "main");
        assert_eq!(response.created, "true");
    }

    #[test]
    fn test_merge_pr_effect_pipeline() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );

        let tool_input = MergePRToolInput {
            pr_number: "42".to_string(),
            strategy: "squash".to_string(),
        };

        // Mock MergePROp handler (tag 2)
        struct MockMergePROp;
        impl EffectHandler<EffectContext> for MockMergePROp {
            type Request = MergePROpReq;
            fn handle(
                &mut self,
                req: MergePROpReq,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                match req {
                    MergePROpReq::MergePullRequest(pr_num, _strategy, _dir) => {
                        let result = MergePRToolResult {
                            success: "true".to_string(),
                            message: format!("Merged PR #{}", pr_num),
                            jj_fetched: "true".to_string(),
                        };
                        cx.respond(result)
                    }
                }
            }
        }

        // Eff '[MergePRInput', Identity, MergePROp]
        let tool = &backend.tools["merge_pr"];
        let mut handlers = frunk::hlist![
            ToolInputHandler { input: Some(tool_input) },
            MockIdentity,
            MockMergePROp,
        ];
        let mut heap = tidepool_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = MergePRToolResult::from_value(&result, &tool.table)
            .expect("Should decode MergePRToolResult");
        assert_eq!(response.success, "true");
        assert!(response.message.contains("42"));
        assert_eq!(response.jj_fetched, "true");
    }

    #[test]
    fn test_notify_effect_pipeline() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );

        let tool_input = NotifyToolInput {
            status: "success".to_string(),
            message: "All tests pass".to_string(),
        };

        // Mock NotifyOp handler (tag 3)
        struct MockNotifyOp;
        impl EffectHandler<EffectContext> for MockNotifyOp {
            type Request = NotifyOpReq;
            fn handle(
                &mut self,
                req: NotifyOpReq,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                match req {
                    NotifyOpReq::DeliverNotification(_, _) => {
                        cx.respond(NotifyToolResult {
                            ack: "delivered".to_string(),
                        })
                    }
                }
            }
        }

        // Eff '[NotifyInput', Identity, FormatOp, NotifyOp]
        let tool = &backend.tools["notify_parent"];
        let mut handlers = frunk::hlist![
            ToolInputHandler { input: Some(tool_input) },
            MockIdentity,
            FormatOpHandler,
            MockNotifyOp,
        ];
        let mut heap = tidepool_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = NotifyToolResult::from_value(&result, &tool.table)
            .expect("Should decode NotifyToolResult");
        assert_eq!(response.ack, "delivered");
    }

    #[tokio::test]
    async fn test_call_unknown_tool() {
        let ctx = EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        };
        let backend = TidepoolBackend::new(None, ctx);
        let result = backend
            .call_tool("tl", "nonexistent", serde_json::json!({}))
            .await
            .unwrap();
        assert!(!result.success);
        assert!(result.error.unwrap().contains("Unknown tool"));
    }

    #[test]
    fn test_minimal_single_effect() {
        // Minimal test: Haskell `send GetToolInput` (returns Int).
        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/PopupMinimal.hs::minimal");

        struct Echo42;
        impl EffectHandler<EffectContext> for Echo42 {
            type Request = GetToolInputReq;
            fn handle(
                &mut self,
                _req: GetToolInputReq,
                _cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                Ok(Value::Lit(tidepool_repr::Literal::LitInt(42)))
            }
        }

        let ctx = EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        };
        let mut heap = tidepool_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_effect::EffectMachine::new(&table, &mut heap).unwrap();
        let mut handlers = frunk::hlist![Echo42];

        let result = machine.run_with_user(&expr, &mut handlers, &ctx).unwrap();
        match result {
            Value::Lit(tidepool_repr::Literal::LitInt(n)) => assert_eq!(n, 42),
            other => panic!("Expected Lit(42), got {:?}", other),
        }
    }

    #[test]
    fn test_effect_machine_full_pipeline() {
        // Full pipeline: popupTool yields GetToolInput (tag 0), GetOwnTab (tag 1),
        // then ShowPopup (tag 2). Eff '[PopupInput', Identity, PopupOp]
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );

        let tool_input = ToolInput {
            title: "Test Title".to_string(),
            components: "[]".to_string(),
        };

        // Mock PopupOp handler (tag 2)
        struct MockPopupOp;
        impl EffectHandler<EffectContext> for MockPopupOp {
            type Request = PopupOpReq;
            fn handle(
                &mut self,
                req: PopupOpReq,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                match req {
                    PopupOpReq::ShowPopup(title, _components, _tab) => {
                        let response = PopupResponse {
                            button: "submit".to_string(),
                            values: format!("{{\"from\":\"{}\"}}", title),
                        };
                        cx.respond(response)
                    }
                }
            }
        }

        let tool = &backend.tools["popup"];
        let mut handlers = frunk::hlist![
            ToolInputHandler { input: Some(tool_input) },
            MockIdentity,
            MockPopupOp,
        ];

        let mut heap = tidepool_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
            .expect("EffectMachine should complete successfully");

        let response = PopupResponse::from_value(&result, &tool.table)
            .expect("Should decode PopupResponse from result");
        assert_eq!(response.button, "submit");
        assert!(response.values.contains("Test Title"));
    }

}
