//! Tidepool backend: Cranelift-compiled Haskell Core behind RuntimeBackend.
//!
//! Haskell defines the popup computation (`PopupEffect.hs`), compiled to Core
//! at build time via `haskell_expr!`. Rust handles effects via `BridgeDispatcher`.
//! The `EffectMachine` bridges them: evaluates Core, yields effects to Rust,
//! feeds responses back into Haskell continuations.
//!
//! The `BridgeDispatcher` implements tidepool's `DispatchEffect` trait (sync),
//! routing effect tags to service calls. Each effect gets native Rust types
//! that mirror the Haskell Core representation via `FromCore`/`ToCore`.

use std::sync::Arc;

use crate::effects::EffectContext;
use crate::mcp::tools::MCPCallOutput;
use crate::mcp::ToolDefinition;
use crate::runtime_backend::RuntimeBackend;
use anyhow::Result;
use async_trait::async_trait;
use serde_json::Value as JsonValue;
use tracing::debug;

// Derive macros emit `core_bridge::`, `core_eval::`, `core_repr::` paths.
// In exomonad these crates are renamed with `tidepool-` prefix, so we alias them.
use tidepool_core_bridge as core_bridge;
use tidepool_core_eval as core_eval;
use tidepool_core_repr as core_repr;

use tidepool_core_bridge::{FromCore, ToCore};
use tidepool_core_bridge_derive::{FromCore, ToCore};
use tidepool_core_effect::dispatch::{DispatchEffect, EffectContext as TidepoolEffectContext};
use tidepool_core_effect::error::EffectError as TidepoolEffectError;
use tidepool_core_eval::value::Value;
use tidepool_core_repr::{CoreExpr, DataConTable};

// =============================================================================
// Native effect types (FromCore/ToCore — no proto)
// =============================================================================

/// Tool input provided by Rust when Haskell yields `GetToolInput`.
/// Field order must match Haskell record: `ToolInput { tiTitle, tiComponents }`.
#[derive(Debug, FromCore, ToCore)]
#[core(name = "ToolInput")]
struct ToolInput {
    title: String,
    components: String,
}

/// Popup effect request, decoded from Haskell GADT constructors.
/// Variant names must match Haskell constructor names exactly.
#[derive(Debug, FromCore)]
enum PopupReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "ShowPopup")]
    ShowPopup(String, String),
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

/// Mirrors Haskell GADT constructors for `FilePR`.
#[derive(Debug, FromCore)]
enum FilePRReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "CreateOrUpdatePR")]
    CreateOrUpdatePR(String, String, String),
}

// =============================================================================
// merge_pr bridge types
// =============================================================================

/// Mirrors Haskell: `MergePRInput { mprPrNumber, mprStrategy, mprWorkingDir }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "MergePRInput")]
struct MergePRToolInput {
    pr_number: String,
    strategy: String,
    working_dir: String,
}

/// Mirrors Haskell: `MergePRResult { mprSuccess, mprMessage, mprJjFetched }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "MergePRResult")]
struct MergePRToolResult {
    success: String,
    message: String,
    jj_fetched: String,
}

/// Mirrors Haskell GADT constructors for `MergePR`.
#[derive(Debug, FromCore)]
enum MergePRReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
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

/// Mirrors Haskell GADT constructors for `Notify`.
#[derive(Debug, FromCore)]
enum NotifyReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "NotifyParent")]
    NotifyParent(String, String),
}

// =============================================================================
// spawn_subtree bridge types
// =============================================================================

/// Mirrors Haskell: `SpawnSubtreeInput { ssiTask, ssiBranchName, ssiParentSessionId }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnSubtreeInput")]
struct SpawnSubtreeToolInput {
    task: String,
    branch_name: String,
    parent_session_id: String,
}

/// Mirrors Haskell: `SpawnSubtreeResult { ssrTabName, ssrBranchName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnSubtreeResult")]
struct SpawnSubtreeToolResult {
    tab_name: String,
    branch_name: String,
}

/// Mirrors Haskell GADT constructors for `SpawnSubtreeOp`.
#[derive(Debug, FromCore)]
enum SpawnSubtreeReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "SpawnSubtree")]
    SpawnSubtree(String, String, String),
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

/// Mirrors Haskell GADT constructors for `SpawnLeafOp`.
#[derive(Debug, FromCore)]
enum SpawnLeafReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
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
#[allow(dead_code)]
struct SpawnWorkerToolResult {
    tab_name: String,
}

/// Mirrors Haskell GADT constructors for `SpawnWorkersOp`.
#[derive(Debug, FromCore)]
#[allow(dead_code)]
enum SpawnWorkersReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
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

/// Mirrors Haskell GADT constructors for `NoteOp`.
#[derive(Debug, FromCore)]
enum NoteReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "SendNote")]
    SendNote(String),
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

/// Mirrors Haskell GADT constructors for `AnswerOp`.
#[derive(Debug, FromCore)]
enum AnswerReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "AnswerQuestion")]
    AnswerQuestion(String, String, String),
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

/// Mirrors Haskell GADT constructors for `MessagesOp`.
#[derive(Debug, FromCore)]
enum MessagesReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "GetMessages")]
    GetMessages(String, String),
}

// =============================================================================
// Bridge Dispatcher
// =============================================================================

/// Routes tidepool effect tags to exomonad service calls.
///
/// Implements `DispatchEffect<EffectContext>` (sync). The popup service is
/// synchronous, so there's no need for async dispatch. This also avoids the
/// `!Send` issue with `EffectMachine` (which holds `&mut dyn Heap`).
///
/// The exomonad `EffectContext` (agent_name + birth_branch) is threaded through
/// as the tidepool user data type `U`.
pub struct BridgeDispatcher {
    /// Zellij session name for popup service.
    zellij_session: Option<String>,
    /// Tag → handler name for logging/debugging.
    tag_names: Vec<String>,
    /// Tool input consumed by `GetToolInput` effect (one-shot).
    tool_input: Option<ToolInput>,
}

impl BridgeDispatcher {
    fn new(
        zellij_session: Option<String>,
        tag_names: Vec<String>,
        tool_input: Option<ToolInput>,
    ) -> Self {
        Self {
            zellij_session,
            tag_names,
            tool_input,
        }
    }

    /// Handle ShowPopup effect: call the popup service and return response.
    fn handle_show_popup(
        &self,
        title: String,
        components: String,
        ctx: &EffectContext,
        table: &DataConTable,
    ) -> Result<Value, TidepoolEffectError> {
        let raw_json: serde_json::Value = if components.is_empty() {
            serde_json::Value::Array(Vec::new())
        } else {
            serde_json::from_str(&components).map_err(|e| {
                TidepoolEffectError::Handler(format!("Invalid components JSON: {}", e))
            })?
        };

        let target_tab = crate::services::agent_control::resolve_own_tab_name(ctx);

        let input = crate::services::popup::PopupInput {
            title,
            raw_json,
            target_tab: Some(target_tab),
        };

        let service = crate::services::popup::PopupService::new(self.zellij_session.clone());

        let output = service
            .show_popup(&input)
            .map_err(|e| TidepoolEffectError::Handler(format!("popup: {}", e)))?;

        let values_json = serde_json::to_string(&output.values)
            .map_err(|e| TidepoolEffectError::Handler(format!("popup serialize: {}", e)))?;

        let response = PopupResponse {
            button: output.button,
            values: values_json,
        };

        response
            .to_value(table)
            .map_err(TidepoolEffectError::Bridge)
    }
}

impl DispatchEffect<EffectContext> for BridgeDispatcher {
    fn dispatch(
        &mut self,
        tag: u64,
        request: &Value,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        let tag_name = self
            .tag_names
            .get(tag as usize)
            .map(|s| s.as_str())
            .unwrap_or("unknown");
        debug!(tag = tag, name = tag_name, "Tidepool effect dispatch");

        match tag {
            0 => {
                // Popup effect (position 0 in Eff '[Popup])
                let req = PopupReq::from_value(request, cx.table())
                    .map_err(TidepoolEffectError::Bridge)?;
                match req {
                    PopupReq::GetToolInput => {
                        let input = self.tool_input.take().ok_or_else(|| {
                            TidepoolEffectError::Handler(
                                "GetToolInput: tool_input already consumed or not set".to_string(),
                            )
                        })?;
                        input
                            .to_value(cx.table())
                            .map_err(TidepoolEffectError::Bridge)
                    }
                    PopupReq::ShowPopup(title, components) => {
                        self.handle_show_popup(title, components, cx.user(), cx.table())
                    }
                }
            }
            _ => Err(TidepoolEffectError::UnhandledEffect { tag }),
        }
    }
}

// =============================================================================
// Tool Definitions (hardcoded until Haskell Core compiler pipeline is wired up)
// =============================================================================

fn popup_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "popup".to_string(),
        description: "Show interactive popup form and get user response".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "title": {
                    "type": "string",
                    "description": "Optional popup title"
                },
                "elements": {
                    "type": "array",
                    "description": "UI elements to display",
                    "items": {
                        "type": "object",
                        "required": ["type"],
                        "properties": {
                            "type": {
                                "type": "string",
                                "enum": ["text", "choice", "checkbox", "textbox", "slider", "multiselect"]
                            },
                            "id": { "type": "string", "description": "Element ID for result lookup" },
                            "label": { "type": "string" },
                            "content": { "type": "string", "description": "For text elements" },
                            "options": {
                                "type": "array",
                                "items": { "type": "string" },
                                "description": "For choice/multiselect"
                            },
                            "default": { "description": "Default value (type varies)" },
                            "placeholder": { "type": "string", "description": "For textbox" },
                            "min": { "type": "number", "description": "For slider" },
                            "max": { "type": "number", "description": "For slider" },
                            "rows": { "type": "integer", "description": "For multiline textbox" }
                        }
                    }
                },
                "panes": {
                    "type": "object",
                    "description": "Named wizard panes. Use with 'start' for multi-pane wizard mode.",
                    "additionalProperties": {
                        "type": "object",
                        "required": ["title", "elements"],
                        "properties": {
                            "title": { "type": "string" },
                            "elements": { "type": "array", "items": { "type": "object" } },
                            "then": { "description": "Transition: string (goto) or object (branch by field value)" }
                        }
                    }
                },
                "start": {
                    "type": "string",
                    "description": "Starting pane name (required when using panes)"
                }
            },
            "required": []
        }),
    }
}

fn file_pr_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "file_pr".to_string(),
        description: "Create or update a pull request for the current branch. Auto-detects base branch from dot-separated naming.".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "title": { "type": "string", "description": "PR title" },
                "body": { "type": "string", "description": "PR body/description" },
                "base_branch": { "type": "string", "description": "Target branch. Auto-detected from dot-separated naming if omitted." }
            },
            "required": ["title", "body"]
        }),
    }
}

fn merge_pr_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "merge_pr".to_string(),
        description: "Merge a GitHub pull request and fetch changes via jj".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "pr_number": { "type": "integer", "description": "PR number to merge" },
                "strategy": { "type": "string", "description": "Merge strategy: squash (default), merge, or rebase" }
            },
            "required": ["pr_number"]
        }),
    }
}

fn notify_parent_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "notify_parent".to_string(),
        description: "Signal to your parent that you are DONE. Injects notification into parent's Zellij pane.".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "status": { "type": "string", "description": "'success' or 'failure'", "enum": ["success", "failure"] },
                "message": { "type": "string", "description": "One-line summary of what was accomplished or what went wrong" }
            },
            "required": ["status", "message"]
        }),
    }
}

fn spawn_subtree_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "spawn_subtree".to_string(),
        description: "Fork a worktree node off your current branch. The child gets full coordination tools (can spawn its own children).".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "task": { "type": "string", "description": "Description of the sub-problem to solve" },
                "branch_name": { "type": "string", "description": "Branch name suffix (will be prefixed with current branch)" }
            },
            "required": ["task", "branch_name"]
        }),
    }
}

fn spawn_leaf_subtree_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "spawn_leaf_subtree".to_string(),
        description: "Fork a worktree for a Gemini leaf agent. Gets own branch for PR filing but cannot spawn children.".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "task": { "type": "string", "description": "Description of the sub-problem to solve" },
                "branch_name": { "type": "string", "description": "Branch name suffix (will be prefixed with current branch)" }
            },
            "required": ["task", "branch_name"]
        }),
    }
}

fn spawn_workers_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "spawn_workers".to_string(),
        description: "Spawn multiple worker agents in one call. Each gets a Zellij pane in the current worktree.".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "specs": {
                    "type": "array",
                    "description": "Array of worker specifications",
                    "items": {
                        "type": "object",
                        "required": ["name", "task"],
                        "properties": {
                            "name": { "type": "string", "description": "Human-readable name for the leaf agent" },
                            "task": { "type": "string", "description": "Short description of the task" },
                            "prompt": { "type": "string", "description": "Raw prompt (escape hatch). If provided, all other fields except name are ignored." },
                            "steps": { "type": "array", "items": { "type": "string" }, "description": "Numbered implementation steps" },
                            "context": { "type": "string", "description": "Freeform context: code snippets, examples, detailed specs" },
                            "done_criteria": { "type": "array", "items": { "type": "string" }, "description": "Acceptance criteria for completion" },
                            "verify": { "type": "array", "items": { "type": "string" }, "description": "Commands to verify the work" },
                            "boundary": { "type": "array", "items": { "type": "string" }, "description": "Things the agent must NOT do" },
                            "read_first": { "type": "array", "items": { "type": "string" }, "description": "Files the agent should read before starting" }
                        }
                    }
                }
            },
            "required": ["specs"]
        }),
    }
}

fn note_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "note".to_string(),
        description: "Send a note to the team lead's inbox. Fire-and-forget.".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "content": { "type": "string", "description": "Note content" }
            },
            "required": ["content"]
        }),
    }
}

fn answer_question_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "answer_question".to_string(),
        description: "Answer a pending question from an agent. Unblocks the agent immediately.".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "agent_id": { "type": "string", "description": "Agent that asked the question" },
                "question_id": { "type": "string", "description": "Question ID from the question message" },
                "answer": { "type": "string", "description": "Answer text" }
            },
            "required": ["agent_id", "question_id", "answer"]
        }),
    }
}

fn get_agent_messages_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "get_agent_messages".to_string(),
        description: "Read unread messages from agents. Supports long-polling with timeout.".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "agent_id": { "type": "string", "description": "Filter to messages from this agent (empty = all agents)" },
                "timeout_secs": { "type": "integer", "description": "Long-poll timeout in seconds (0 = immediate return)", "default": 0 }
            },
            "required": []
        }),
    }
}

// =============================================================================
// TidepoolBackend
// =============================================================================

/// Cranelift-compiled Haskell Core backend.
///
/// The popup computation is compiled from Haskell at build time via `haskell_expr!`.
/// At runtime, `call_popup` feeds it through the `EffectMachine`, which evaluates
/// the Core expression and routes yielded effects to `BridgeDispatcher`.
pub struct TidepoolBackend {
    /// Data constructor table from compiled PopupEffect.hs.
    table: Arc<DataConTable>,

    /// Compiled Haskell Core expression for `popupTool`.
    popup_expr: CoreExpr,

    /// Compiled file_pr expression + its DataConTable.
    file_pr_expr: CoreExpr,
    file_pr_table: Arc<DataConTable>,

    /// Compiled merge_pr expression + its DataConTable.
    merge_pr_expr: CoreExpr,
    merge_pr_table: Arc<DataConTable>,

    /// Compiled notify_parent expression + its DataConTable.
    notify_expr: CoreExpr,
    notify_table: Arc<DataConTable>,

    /// Compiled spawn_subtree expression + its DataConTable.
    spawn_subtree_expr: CoreExpr,
    spawn_subtree_table: Arc<DataConTable>,

    /// Compiled spawn_leaf expression + its DataConTable.
    spawn_leaf_expr: CoreExpr,
    spawn_leaf_table: Arc<DataConTable>,

    /// Compiled spawn_workers expression + its DataConTable.
    #[allow(dead_code)]
    spawn_workers_expr: CoreExpr,
    #[allow(dead_code)]
    spawn_workers_table: Arc<DataConTable>,

    /// Agent control service for spawn operations.
    agent_control: Arc<crate::services::agent_control::AgentControlService>,

    /// Compiled note expression + its DataConTable.
    note_expr: CoreExpr,
    note_table: Arc<DataConTable>,

    /// Compiled answer_question expression + its DataConTable.
    answer_expr: CoreExpr,
    answer_table: Arc<DataConTable>,

    /// Compiled get_agent_messages expression + its DataConTable.
    messages_expr: CoreExpr,
    messages_table: Arc<DataConTable>,

    /// Question registry for answer_question (bridges oneshot channels).
    question_registry: Arc<crate::services::questions::QuestionRegistry>,

    /// Project directory for inbox file paths.
    project_dir: std::path::PathBuf,

    /// JJ workspace service for file_pr and merge_pr.
    jj: Arc<crate::services::jj_workspace::JjWorkspaceService>,

    /// Zellij session name for popup and other UI services.
    zellij_session: Option<String>,

    /// Tag → effect name mapping for dispatch and logging.
    tag_names: Vec<String>,

    /// Agent identity context, threaded as user data to effect handlers.
    ctx: EffectContext,
}

impl TidepoolBackend {
    pub fn new(zellij_session: Option<String>, ctx: EffectContext) -> Self {
        let (popup_expr, table) =
            tidepool_macro::haskell_expr!("haskell/PopupEffect.hs::popupTool");
        let (file_pr_expr, file_pr_table) =
            tidepool_macro::haskell_expr!("haskell/FilePREffect.hs::filePRTool");
        let (merge_pr_expr, merge_pr_table) =
            tidepool_macro::haskell_expr!("haskell/MergePREffect.hs::mergePRTool");
        let (notify_expr, notify_table) =
            tidepool_macro::haskell_expr!("haskell/NotifyEffect.hs::notifyTool");
        let (spawn_subtree_expr, spawn_subtree_table) =
            tidepool_macro::haskell_expr!("haskell/SpawnEffect.hs::spawnSubtreeTool");
        let (spawn_leaf_expr, spawn_leaf_table) =
            tidepool_macro::haskell_expr!("haskell/SpawnLeafEffect.hs::spawnLeafTool");
        let (spawn_workers_expr, spawn_workers_table) =
            tidepool_macro::haskell_expr!("haskell/SpawnWorkersEffect.hs::spawnWorkersTool");
        let (note_expr, note_table) =
            tidepool_macro::haskell_expr!("haskell/NoteEffect.hs::noteTool");
        let (answer_expr, answer_table) =
            tidepool_macro::haskell_expr!("haskell/AnswerEffect.hs::answerTool");
        let (messages_expr, messages_table) =
            tidepool_macro::haskell_expr!("haskell/MessagesEffect.hs::messagesTool");

        let working_dir = crate::services::agent_control::resolve_agent_working_dir(&ctx);
        let jj = Arc::new(crate::services::jj_workspace::JjWorkspaceService::new(working_dir.clone()));

        let agent_control = Arc::new(
            crate::services::agent_control::AgentControlService::new(
                working_dir.clone(),
                None, // No GitHubService needed for spawn operations
                jj.clone(),
            )
            .with_birth_branch(ctx.birth_branch.clone())
            .with_zellij_session(zellij_session.clone().unwrap_or_default()),
        );

        let question_registry = Arc::new(crate::services::questions::QuestionRegistry::new());

        Self {
            table: Arc::new(table),
            popup_expr,
            file_pr_expr,
            file_pr_table: Arc::new(file_pr_table),
            merge_pr_expr,
            merge_pr_table: Arc::new(merge_pr_table),
            notify_expr,
            notify_table: Arc::new(notify_table),
            spawn_subtree_expr,
            spawn_subtree_table: Arc::new(spawn_subtree_table),
            spawn_leaf_expr,
            spawn_leaf_table: Arc::new(spawn_leaf_table),
            spawn_workers_expr,
            spawn_workers_table: Arc::new(spawn_workers_table),
            agent_control,
            note_expr,
            note_table: Arc::new(note_table),
            answer_expr,
            answer_table: Arc::new(answer_table),
            messages_expr,
            messages_table: Arc::new(messages_table),
            question_registry,
            project_dir: working_dir.clone(),
            jj,
            zellij_session,
            tag_names: vec!["Popup".into()],
            ctx,
        }
    }

    /// Handle popup tool call by running the Haskell computation through EffectMachine.
    ///
    /// Flow: parse MCP args → create dispatcher with tool_input → EffectMachine evaluates
    /// popupTool → Haskell yields GetToolInput → Rust provides args → Haskell yields
    /// ShowPopup → Rust calls PopupService → Haskell returns PopupResponse → decode result.
    async fn call_popup(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let title = args
            .get("title")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        // Components can come from "elements" (array) or "panes" (wizard object).
        let components = if let Some(panes) = args.get("panes") {
            let start = args.get("start");
            let mut wizard = serde_json::Map::new();
            wizard.insert("panes".to_string(), panes.clone());
            if let Some(s) = start {
                wizard.insert("start".to_string(), s.clone());
            }
            serde_json::to_string(&JsonValue::Object(wizard)).unwrap_or_default()
        } else if let Some(elements) = args.get("elements") {
            serde_json::to_string(elements).unwrap_or_default()
        } else {
            "[]".to_string()
        };

        debug!(
            title = %title,
            components_len = components.len(),
            "Tidepool popup call via EffectMachine"
        );

        let tool_input = ToolInput { title, components };

        let mut dispatcher = BridgeDispatcher::new(
            self.zellij_session.clone(),
            self.tag_names.clone(),
            Some(tool_input),
        );

        // EffectMachine is !Send (holds &mut dyn Heap), but popup dispatch is sync.
        // No await points below — the async fn signature is required by RuntimeBackend trait.
        let mut heap = tidepool_core_eval::heap::VecHeap::new();
        let machine_result = tidepool_core_effect::EffectMachine::new(&self.table, &mut heap);

        let mut machine = match machine_result {
            Ok(m) => m,
            Err(e) => {
                return Ok(MCPCallOutput {
                    success: false,
                    result: None,
                    error: Some(format!("EffectMachine init failed: {}", e)),
                })
            }
        };

        match machine.run_with_user(&self.popup_expr, &mut dispatcher, &self.ctx) {
            Ok(response_value) => {
                let response = PopupResponse::from_value(&response_value, &self.table)
                    .map_err(|e| anyhow::anyhow!("Bridge decode error: {}", e))?;
                let values: JsonValue = serde_json::from_str(&response.values)
                    .unwrap_or(JsonValue::Object(serde_json::Map::new()));
                Ok(MCPCallOutput {
                    success: true,
                    result: Some(serde_json::json!({
                        "button": response.button,
                        "values": values,
                    })),
                    error: None,
                })
            }
            Err(e) => Ok(MCPCallOutput {
                success: false,
                result: None,
                error: Some(format!("Popup error: {}", e)),
            }),
        }
    }

    /// Handle file_pr tool: parse MCP args, call file_pr service directly.
    ///
    /// Bypasses EffectMachine at runtime (async service). The haskell_expr! compilation
    /// validates the Haskell; mock tests verify the bridge; runtime calls services directly.
    async fn call_file_pr(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let title = args.get("title").and_then(|v| v.as_str()).unwrap_or("").to_string();
        let body = args.get("body").and_then(|v| v.as_str()).unwrap_or("").to_string();
        let base_branch = args.get("base_branch").and_then(|v| v.as_str()).map(|s| s.to_string());

        let working_dir = crate::services::agent_control::resolve_agent_working_dir(&self.ctx);

        let input = crate::services::file_pr::FilePRInput {
            title: title.clone(),
            body,
            base_branch,
            working_dir: Some(working_dir.to_string_lossy().to_string()),
        };

        debug!(title = %title, "Tidepool file_pr call");

        match crate::services::file_pr::file_pr_async(&input, self.jj.clone()).await {
            Ok(output) => Ok(MCPCallOutput {
                success: true,
                result: Some(serde_json::json!({
                    "pr_url": output.pr_url,
                    "pr_number": output.pr_number.as_u64(),
                    "head_branch": output.head_branch,
                    "base_branch": output.base_branch,
                    "created": output.created,
                })),
                error: None,
            }),
            Err(e) => {
                tracing::error!(error = %e, "Tidepool file_pr failed");
                Ok(MCPCallOutput {
                    success: false,
                    result: None,
                    error: Some(e.to_string()),
                })
            }
        }
    }

    /// Handle merge_pr tool: parse MCP args, call merge_pr service directly.
    async fn call_merge_pr(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let pr_number = args
            .get("pr_number")
            .and_then(|v| v.as_u64())
            .unwrap_or(0);
        let strategy = args
            .get("strategy")
            .and_then(|v| v.as_str())
            .unwrap_or("squash")
            .to_string();

        let working_dir = crate::services::agent_control::resolve_agent_working_dir(&self.ctx);
        let working_dir_str = working_dir.to_string_lossy().to_string();

        debug!(
            pr_number = pr_number,
            strategy = %strategy,
            "Tidepool merge_pr call"
        );

        let pr = crate::domain::PRNumber::new(pr_number);

        match crate::services::merge_pr::merge_pr_async(
            pr,
            &strategy,
            &working_dir_str,
            self.jj.clone(),
        )
        .await
        {
            Ok(output) => Ok(MCPCallOutput {
                success: true,
                result: Some(serde_json::json!({
                    "success": output.success,
                    "message": output.message,
                    "jj_fetched": output.jj_fetched,
                })),
                error: None,
            }),
            Err(e) => {
                tracing::error!(error = %e, "Tidepool merge_pr failed");
                Ok(MCPCallOutput {
                    success: false,
                    result: None,
                    error: Some(e.to_string()),
                })
            }
        }
    }

    /// Handle notify_parent tool: inject notification into parent's Zellij pane.
    ///
    /// Injects text directly via zellij_events::inject_input.
    async fn call_notify_parent(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let status = args
            .get("status")
            .and_then(|v| v.as_str())
            .unwrap_or("success")
            .to_string();
        let message = args
            .get("message")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        let agent_id = self.ctx.agent_name.to_string();
        let tab_name = crate::services::agent_control::resolve_parent_tab_name(&self.ctx);

        let notification = match status.as_str() {
            "success" => format!(
                "[CHILD COMPLETE: {}] {}",
                agent_id,
                if message.is_empty() {
                    "Task completed successfully."
                } else {
                    &message
                }
            ),
            "failure" => format!(
                "[CHILD FAILED: {}] {}",
                agent_id,
                if message.is_empty() {
                    "Task failed."
                } else {
                    &message
                }
            ),
            other => format!("[CHILD STATUS {}: {}] {}", agent_id, other, message),
        };

        debug!(
            agent_id = %agent_id,
            status = %status,
            tab = %tab_name,
            "Tidepool notify_parent call"
        );

        crate::services::zellij_events::inject_input(&tab_name, &notification);

        Ok(MCPCallOutput {
            success: true,
            result: Some(serde_json::json!({"ack": "delivered"})),
            error: None,
        })
    }

    /// Handle spawn_subtree tool: parse MCP args, call AgentControlService.
    async fn call_spawn_subtree(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let task = args.get("task").and_then(|v| v.as_str()).unwrap_or("").to_string();
        let branch_name = args
            .get("branch_name")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        debug!(branch = %branch_name, "Tidepool spawn_subtree call");

        let options = crate::services::agent_control::SpawnSubtreeOptions {
            task,
            branch_name: branch_name.clone(),
            parent_session_id: None,
        };

        match self
            .agent_control
            .spawn_subtree(&options, &self.ctx.birth_branch)
            .await
        {
            Ok(result) => Ok(MCPCallOutput {
                success: true,
                result: Some(serde_json::json!({
                    "tab_name": result.tab_name,
                    "branch_name": branch_name,
                    "agent_dir": result.agent_dir.to_string_lossy(),
                })),
                error: None,
            }),
            Err(e) => {
                tracing::error!(error = %e, "Tidepool spawn_subtree failed");
                Ok(MCPCallOutput {
                    success: false,
                    result: None,
                    error: Some(e.to_string()),
                })
            }
        }
    }

    /// Handle spawn_leaf_subtree tool: parse MCP args, call AgentControlService.
    async fn call_spawn_leaf(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let task = args.get("task").and_then(|v| v.as_str()).unwrap_or("").to_string();
        let branch_name = args
            .get("branch_name")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        debug!(branch = %branch_name, "Tidepool spawn_leaf_subtree call");

        let options = crate::services::agent_control::SpawnSubtreeOptions {
            task,
            branch_name: branch_name.clone(),
            parent_session_id: None,
        };

        match self
            .agent_control
            .spawn_leaf_subtree(&options, &self.ctx.birth_branch)
            .await
        {
            Ok(result) => Ok(MCPCallOutput {
                success: true,
                result: Some(serde_json::json!({
                    "tab_name": result.tab_name,
                    "branch_name": branch_name,
                    "agent_dir": result.agent_dir.to_string_lossy(),
                })),
                error: None,
            }),
            Err(e) => {
                tracing::error!(error = %e, "Tidepool spawn_leaf_subtree failed");
                Ok(MCPCallOutput {
                    success: false,
                    result: None,
                    error: Some(e.to_string()),
                })
            }
        }
    }

    /// Handle spawn_workers tool: iterate specs array, call spawn_worker per item.
    async fn call_spawn_workers(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let specs = args
            .get("specs")
            .and_then(|v| v.as_array())
            .cloned()
            .unwrap_or_default();

        debug!(count = specs.len(), "Tidepool spawn_workers call");

        let mut results = Vec::new();
        for spec in &specs {
            let name = spec.get("name").and_then(|v| v.as_str()).unwrap_or("").to_string();
            let task = spec.get("task").and_then(|v| v.as_str()).unwrap_or("").to_string();

            // Build prompt from spec fields (same as WASM handler)
            let prompt = if let Some(p) = spec.get("prompt").and_then(|v| v.as_str()) {
                p.to_string()
            } else {
                let mut parts = vec![format!("Task: {}", task)];
                if let Some(steps) = spec.get("steps").and_then(|v| v.as_array()) {
                    parts.push("Steps:".to_string());
                    for (i, s) in steps.iter().enumerate() {
                        if let Some(text) = s.as_str() {
                            parts.push(format!("{}. {}", i + 1, text));
                        }
                    }
                }
                if let Some(context) = spec.get("context").and_then(|v| v.as_str()) {
                    parts.push(format!("\nContext:\n{}", context));
                }
                if let Some(done) = spec.get("done_criteria").and_then(|v| v.as_array()) {
                    parts.push("Done when:".to_string());
                    for d in done {
                        if let Some(text) = d.as_str() {
                            parts.push(format!("- {}", text));
                        }
                    }
                }
                if let Some(verify) = spec.get("verify").and_then(|v| v.as_array()) {
                    parts.push("Verify:".to_string());
                    for v in verify {
                        if let Some(text) = v.as_str() {
                            parts.push(format!("$ {}", text));
                        }
                    }
                }
                if let Some(boundary) = spec.get("boundary").and_then(|v| v.as_array()) {
                    parts.push("DO NOT:".to_string());
                    for b in boundary {
                        if let Some(text) = b.as_str() {
                            parts.push(format!("- {}", text));
                        }
                    }
                }
                if let Some(read) = spec.get("read_first").and_then(|v| v.as_array()) {
                    parts.push("Read first:".to_string());
                    for r in read {
                        if let Some(text) = r.as_str() {
                            parts.push(format!("- {}", text));
                        }
                    }
                }
                parts.join("\n")
            };

            let options = crate::services::agent_control::SpawnWorkerOptions {
                name: name.clone(),
                prompt,
            };

            match self
                .agent_control
                .spawn_worker(&options, &self.ctx.birth_branch)
                .await
            {
                Ok(result) => {
                    results.push(serde_json::json!({
                        "name": name,
                        "tab_name": result.tab_name,
                        "success": true,
                    }));
                }
                Err(e) => {
                    tracing::error!(name = %name, error = %e, "Tidepool spawn_worker failed");
                    results.push(serde_json::json!({
                        "name": name,
                        "success": false,
                        "error": e.to_string(),
                    }));
                }
            }
        }

        Ok(MCPCallOutput {
            success: true,
            result: Some(serde_json::json!({ "workers": results })),
            error: None,
        })
    }

    /// Handle note tool: write a note to the TL's inbox and inject into parent pane.
    async fn call_note(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let content = args
            .get("content")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        let agent_id = self.ctx.agent_name.to_string();

        debug!(agent = %agent_id, "Tidepool note call");

        let tl_inbox = crate::services::inbox::inbox_path(&self.project_dir, "team-lead");
        let msg = crate::services::inbox::create_message(
            agent_id.clone(),
            content.clone(),
            Some(content.chars().take(50).collect()),
        );

        if let Err(e) = crate::services::inbox::append_message(&tl_inbox, &msg) {
            tracing::error!(error = %e, "Tidepool note: inbox write failed");
            return Ok(MCPCallOutput {
                success: false,
                result: None,
                error: Some(e.to_string()),
            });
        }

        // Best-effort pane injection
        let tab_name = crate::services::agent_control::resolve_parent_tab_name(&self.ctx);
        let formatted = format!("[note from {}] {}", agent_id, content);
        crate::services::zellij_events::inject_input(&tab_name, &formatted);

        Ok(MCPCallOutput {
            success: true,
            result: Some(serde_json::json!({"ack": true})),
            error: None,
        })
    }

    /// Handle answer_question tool: write answer to agent's inbox, resolve oneshot channel.
    async fn call_answer_question(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let agent_id = args
            .get("agent_id")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let question_id = args
            .get("question_id")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let answer = args
            .get("answer")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        debug!(
            agent = %agent_id,
            question_id = %question_id,
            "Tidepool answer_question call"
        );

        // Write answer to agent's inbox
        let agent_inbox = crate::services::inbox::inbox_path(&self.project_dir, &agent_id);
        let msg = crate::services::inbox::create_message(
            "team-lead".to_string(),
            answer.clone(),
            Some(format!("Answer to {}", question_id)),
        );

        if let Err(e) = crate::services::inbox::append_message(&agent_inbox, &msg) {
            tracing::error!(error = %e, "Tidepool answer_question: inbox write failed");
            return Ok(MCPCallOutput {
                success: false,
                result: None,
                error: Some(e.to_string()),
            });
        }

        // Resolve the oneshot channel so send_question unblocks immediately
        let resolved = self.question_registry.resolve(&question_id, answer);
        tracing::info!(
            agent = %agent_id,
            question_id = %question_id,
            resolved,
            "Resolved question via QuestionRegistry"
        );

        Ok(MCPCallOutput {
            success: true,
            result: Some(serde_json::json!({
                "status": "answered",
                "agent_id": agent_id,
                "question_id": question_id,
            })),
            error: None,
        })
    }

    /// Handle get_agent_messages tool: read unread messages from inbox.
    async fn call_get_messages(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let agent_id = args
            .get("agent_id")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let timeout_secs: i64 = args
            .get("timeout_secs")
            .and_then(|v| v.as_i64())
            .unwrap_or(0);

        debug!(
            agent = %agent_id,
            timeout_secs,
            "Tidepool get_agent_messages call"
        );

        let tl_inbox = crate::services::inbox::inbox_path(&self.project_dir, "team-lead");

        let all_messages = if timeout_secs > 0 {
            let timeout = std::time::Duration::from_secs(timeout_secs as u64);
            let interval = std::time::Duration::from_secs(2);
            let tl_inbox_clone = tl_inbox.clone();
            match tokio::task::spawn_blocking(move || {
                crate::services::inbox::poll_unread(&tl_inbox_clone, timeout, interval)
            })
            .await
            {
                Ok(Ok(msgs)) => msgs,
                Ok(Err(e)) => {
                    return Ok(MCPCallOutput {
                        success: false,
                        result: None,
                        error: Some(e.to_string()),
                    });
                }
                Err(e) => {
                    return Ok(MCPCallOutput {
                        success: false,
                        result: None,
                        error: Some(format!("spawn_blocking failed: {}", e)),
                    });
                }
            }
        } else {
            match crate::services::inbox::read_unread(&tl_inbox) {
                Ok(msgs) => msgs,
                Err(e) => {
                    return Ok(MCPCallOutput {
                        success: false,
                        result: None,
                        error: Some(e.to_string()),
                    });
                }
            }
        };

        // Filter by agent_id if provided, otherwise return all
        let filtered: Vec<_> = if agent_id.is_empty() {
            all_messages
        } else {
            all_messages
                .into_iter()
                .filter(|m| m.from == agent_id)
                .collect()
        };

        let messages_json: Vec<serde_json::Value> = filtered
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

        Ok(MCPCallOutput {
            success: true,
            result: Some(serde_json::json!({
                "messages": messages_json,
                "count": messages_json.len(),
            })),
            error: None,
        })
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

    // The following tests use haskell_expr! which requires nix + tidepool-extract at
    // compile time. They exercise the full pipeline: Haskell compilation → Core embedding
    // → EffectMachine evaluation → BridgeDispatcher routing.

    #[test]
    fn test_tool_input_roundtrip() {
        let backend = TidepoolBackend::new(
            None,
            EffectContext {
                agent_name: crate::AgentName::from("test"),
                birth_branch: crate::BirthBranch::root(),
            },
        );
        let input = ToolInput {
            title: "Pick one".to_string(),
            components: r#"[{"type":"choice"}]"#.to_string(),
        };
        let value = input.to_value(&backend.table).unwrap();
        let back = ToolInput::from_value(&value, &backend.table).unwrap();
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
        let resp = PopupResponse {
            button: "submit".to_string(),
            values: r#"{"pick":"A"}"#.to_string(),
        };
        let value = resp.to_value(&backend.table).unwrap();
        let back = PopupResponse::from_value(&value, &backend.table).unwrap();
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
        assert_eq!(tl.len(), 10); // popup, file_pr, merge_pr, notify_parent, spawn_subtree, spawn_leaf_subtree, spawn_workers, note, answer_question, get_agent_messages
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
        assert_eq!(dev.len(), 3); // file_pr, notify_parent, note
        assert!(dev.iter().any(|t| t.name == "file_pr"));
        assert!(dev.iter().any(|t| t.name == "notify_parent"));
        assert!(dev.iter().any(|t| t.name == "note"));

        let worker = backend.list_tools("worker").await.unwrap();
        assert_eq!(worker.len(), 2); // notify_parent, note
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
        let input = NoteToolInput {
            content: "hello TL".to_string(),
        };
        let value = input.to_value(&backend.note_table).unwrap();
        let back = NoteToolInput::from_value(&value, &backend.note_table).unwrap();
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
        let input = AnswerToolInput {
            agent_id: "worker-1".to_string(),
            question_id: "q-123".to_string(),
            answer: "yes".to_string(),
        };
        let value = input.to_value(&backend.answer_table).unwrap();
        let back = AnswerToolInput::from_value(&value, &backend.answer_table).unwrap();
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
        let input = MessagesToolInput {
            agent_id: "worker-1".to_string(),
            timeout_secs: "30".to_string(),
        };
        let value = input.to_value(&backend.messages_table).unwrap();
        let back = MessagesToolInput::from_value(&value, &backend.messages_table).unwrap();
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

        struct MockNote {
            tool_input: Option<NoteToolInput>,
        }
        impl DispatchEffect<EffectContext> for MockNote {
            fn dispatch(
                &mut self,
                tag: u64,
                request: &Value,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                assert_eq!(tag, 0);
                let req =
                    NoteReq::from_value(request, cx.table()).map_err(TidepoolEffectError::Bridge)?;
                match req {
                    NoteReq::GetToolInput => {
                        let input = self.tool_input.take().ok_or_else(|| {
                            TidepoolEffectError::Handler("tool_input already consumed".into())
                        })?;
                        input.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                    }
                    NoteReq::SendNote(_content) => {
                        let result = NoteToolResult {
                            ack: "sent".to_string(),
                        };
                        result.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                    }
                }
            }
        }

        let mut dispatcher = MockNote {
            tool_input: Some(tool_input),
        };
        let mut heap = tidepool_core_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_core_effect::EffectMachine::new(&backend.note_table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&backend.note_expr, &mut dispatcher, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = NoteToolResult::from_value(&result, &backend.note_table)
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

        struct MockAnswer {
            tool_input: Option<AnswerToolInput>,
        }
        impl DispatchEffect<EffectContext> for MockAnswer {
            fn dispatch(
                &mut self,
                tag: u64,
                request: &Value,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                assert_eq!(tag, 0);
                let req = AnswerReq::from_value(request, cx.table())
                    .map_err(TidepoolEffectError::Bridge)?;
                match req {
                    AnswerReq::GetToolInput => {
                        let input = self.tool_input.take().ok_or_else(|| {
                            TidepoolEffectError::Handler("tool_input already consumed".into())
                        })?;
                        input.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                    }
                    AnswerReq::AnswerQuestion(agent, qid, _answer) => {
                        let result = AnswerToolResult {
                            status: "answered".to_string(),
                            agent_id: agent,
                            question_id: qid,
                        };
                        result.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                    }
                }
            }
        }

        let mut dispatcher = MockAnswer {
            tool_input: Some(tool_input),
        };
        let mut heap = tidepool_core_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_core_effect::EffectMachine::new(&backend.answer_table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&backend.answer_expr, &mut dispatcher, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = AnswerToolResult::from_value(&result, &backend.answer_table)
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

        struct MockMessages {
            tool_input: Option<MessagesToolInput>,
        }
        impl DispatchEffect<EffectContext> for MockMessages {
            fn dispatch(
                &mut self,
                tag: u64,
                request: &Value,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                assert_eq!(tag, 0);
                let req = MessagesReq::from_value(request, cx.table())
                    .map_err(TidepoolEffectError::Bridge)?;
                match req {
                    MessagesReq::GetToolInput => {
                        let input = self.tool_input.take().ok_or_else(|| {
                            TidepoolEffectError::Handler("tool_input already consumed".into())
                        })?;
                        input.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                    }
                    MessagesReq::GetMessages(agent, _timeout) => {
                        let result = MessagesToolResult {
                            messages_json: format!("[{{\"from\":\"{}\",\"text\":\"hello\"}}]", agent),
                            warning: "".to_string(),
                        };
                        result.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                    }
                }
            }
        }

        let mut dispatcher = MockMessages {
            tool_input: Some(tool_input),
        };
        let mut heap = tidepool_core_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_core_effect::EffectMachine::new(&backend.messages_table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&backend.messages_expr, &mut dispatcher, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = MessagesToolResult::from_value(&result, &backend.messages_table)
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
        let input = SpawnSubtreeToolInput {
            task: "implement feature".to_string(),
            branch_name: "feature-a".to_string(),
            parent_session_id: "".to_string(),
        };
        let value = input.to_value(&backend.spawn_subtree_table).unwrap();
        let back = SpawnSubtreeToolInput::from_value(&value, &backend.spawn_subtree_table).unwrap();
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
        let input = SpawnLeafToolInput {
            task: "implement leaf".to_string(),
            branch_name: "leaf-1".to_string(),
        };
        let value = input.to_value(&backend.spawn_leaf_table).unwrap();
        let back = SpawnLeafToolInput::from_value(&value, &backend.spawn_leaf_table).unwrap();
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
        let input = WorkerSpecBridge {
            name: "worker-1".to_string(),
            prompt: "do something".to_string(),
        };
        let value = input.to_value(&backend.spawn_workers_table).unwrap();
        let back = WorkerSpecBridge::from_value(&value, &backend.spawn_workers_table).unwrap();
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
            parent_session_id: "".to_string(),
        };

        struct MockSpawn {
            tool_input: Option<SpawnSubtreeToolInput>,
        }
        impl DispatchEffect<EffectContext> for MockSpawn {
            fn dispatch(
                &mut self,
                tag: u64,
                request: &Value,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                assert_eq!(tag, 0);
                let req = SpawnSubtreeReq::from_value(request, cx.table())
                    .map_err(TidepoolEffectError::Bridge)?;
                match req {
                    SpawnSubtreeReq::GetToolInput => {
                        let input = self.tool_input.take().ok_or_else(|| {
                            TidepoolEffectError::Handler("tool_input already consumed".into())
                        })?;
                        input.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                    }
                    SpawnSubtreeReq::SpawnSubtree(_task, branch, _session) => {
                        let result = SpawnSubtreeToolResult {
                            tab_name: format!("🧠 {}", branch),
                            branch_name: format!("main.{}", branch),
                        };
                        result.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                    }
                }
            }
        }

        let mut dispatcher = MockSpawn {
            tool_input: Some(tool_input),
        };
        let mut heap = tidepool_core_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_core_effect::EffectMachine::new(&backend.spawn_subtree_table, &mut heap)
                .unwrap();

        let result = machine
            .run_with_user(&backend.spawn_subtree_expr, &mut dispatcher, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = SpawnSubtreeToolResult::from_value(&result, &backend.spawn_subtree_table)
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

        struct MockLeaf {
            tool_input: Option<SpawnLeafToolInput>,
        }
        impl DispatchEffect<EffectContext> for MockLeaf {
            fn dispatch(
                &mut self,
                tag: u64,
                request: &Value,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                assert_eq!(tag, 0);
                let req = SpawnLeafReq::from_value(request, cx.table())
                    .map_err(TidepoolEffectError::Bridge)?;
                match req {
                    SpawnLeafReq::GetToolInput => {
                        let input = self.tool_input.take().ok_or_else(|| {
                            TidepoolEffectError::Handler("tool_input already consumed".into())
                        })?;
                        input.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                    }
                    SpawnLeafReq::SpawnLeaf(_task, branch) => {
                        let result = SpawnLeafToolResult {
                            tab_name: format!("♊ {}", branch),
                            branch_name: format!("main.{}", branch),
                        };
                        result.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                    }
                }
            }
        }

        let mut dispatcher = MockLeaf {
            tool_input: Some(tool_input),
        };
        let mut heap = tidepool_core_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_core_effect::EffectMachine::new(&backend.spawn_leaf_table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&backend.spawn_leaf_expr, &mut dispatcher, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = SpawnLeafToolResult::from_value(&result, &backend.spawn_leaf_table)
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
        let input = FilePRToolInput {
            title: "feat: add tests".to_string(),
            body: "Adds unit tests".to_string(),
            base_branch: "main".to_string(),
        };
        let value = input.to_value(&backend.file_pr_table).unwrap();
        let back = FilePRToolInput::from_value(&value, &backend.file_pr_table).unwrap();
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
        let input = MergePRToolInput {
            pr_number: "42".to_string(),
            strategy: "squash".to_string(),
            working_dir: ".".to_string(),
        };
        let value = input.to_value(&backend.merge_pr_table).unwrap();
        let back = MergePRToolInput::from_value(&value, &backend.merge_pr_table).unwrap();
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
        let input = NotifyToolInput {
            status: "success".to_string(),
            message: "All done".to_string(),
        };
        let value = input.to_value(&backend.notify_table).unwrap();
        let back = NotifyToolInput::from_value(&value, &backend.notify_table).unwrap();
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

        struct MockFilePR {
            tool_input: Option<FilePRToolInput>,
        }
        impl DispatchEffect<EffectContext> for MockFilePR {
            fn dispatch(
                &mut self,
                tag: u64,
                request: &Value,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                assert_eq!(tag, 0);
                let req =
                    FilePRReq::from_value(request, cx.table()).map_err(TidepoolEffectError::Bridge)?;
                match req {
                    FilePRReq::GetToolInput => {
                        let input = self.tool_input.take().ok_or_else(|| {
                            TidepoolEffectError::Handler("tool_input already consumed".into())
                        })?;
                        input
                            .to_value(cx.table())
                            .map_err(TidepoolEffectError::Bridge)
                    }
                    FilePRReq::CreateOrUpdatePR(_title, _body, base) => {
                        let result = FilePRToolResult {
                            pr_url: "https://github.com/test/test/pull/1".to_string(),
                            pr_number: "1".to_string(),
                            head_branch: "test-branch".to_string(),
                            result_base: base,
                            created: "true".to_string(),
                        };
                        result
                            .to_value(cx.table())
                            .map_err(TidepoolEffectError::Bridge)
                    }
                }
            }
        }

        let mut dispatcher = MockFilePR {
            tool_input: Some(tool_input),
        };
        let mut heap = tidepool_core_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_core_effect::EffectMachine::new(&backend.file_pr_table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&backend.file_pr_expr, &mut dispatcher, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = FilePRToolResult::from_value(&result, &backend.file_pr_table)
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
            working_dir: ".".to_string(),
        };

        struct MockMergePR {
            tool_input: Option<MergePRToolInput>,
        }
        impl DispatchEffect<EffectContext> for MockMergePR {
            fn dispatch(
                &mut self,
                tag: u64,
                request: &Value,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                assert_eq!(tag, 0);
                let req = MergePRReq::from_value(request, cx.table())
                    .map_err(TidepoolEffectError::Bridge)?;
                match req {
                    MergePRReq::GetToolInput => {
                        let input = self.tool_input.take().ok_or_else(|| {
                            TidepoolEffectError::Handler("tool_input already consumed".into())
                        })?;
                        input
                            .to_value(cx.table())
                            .map_err(TidepoolEffectError::Bridge)
                    }
                    MergePRReq::MergePullRequest(pr_num, _strategy, _dir) => {
                        let result = MergePRToolResult {
                            success: "true".to_string(),
                            message: format!("Merged PR #{}", pr_num),
                            jj_fetched: "true".to_string(),
                        };
                        result
                            .to_value(cx.table())
                            .map_err(TidepoolEffectError::Bridge)
                    }
                }
            }
        }

        let mut dispatcher = MockMergePR {
            tool_input: Some(tool_input),
        };
        let mut heap = tidepool_core_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_core_effect::EffectMachine::new(&backend.merge_pr_table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&backend.merge_pr_expr, &mut dispatcher, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = MergePRToolResult::from_value(&result, &backend.merge_pr_table)
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

        struct MockNotify {
            tool_input: Option<NotifyToolInput>,
        }
        impl DispatchEffect<EffectContext> for MockNotify {
            fn dispatch(
                &mut self,
                tag: u64,
                request: &Value,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                assert_eq!(tag, 0);
                let req =
                    NotifyReq::from_value(request, cx.table()).map_err(TidepoolEffectError::Bridge)?;
                match req {
                    NotifyReq::GetToolInput => {
                        let input = self.tool_input.take().ok_or_else(|| {
                            TidepoolEffectError::Handler("tool_input already consumed".into())
                        })?;
                        input
                            .to_value(cx.table())
                            .map_err(TidepoolEffectError::Bridge)
                    }
                    NotifyReq::NotifyParent(_status, _message) => {
                        let result = NotifyToolResult {
                            ack: "delivered".to_string(),
                        };
                        result
                            .to_value(cx.table())
                            .map_err(TidepoolEffectError::Bridge)
                    }
                }
            }
        }

        let mut dispatcher = MockNotify {
            tool_input: Some(tool_input),
        };
        let mut heap = tidepool_core_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_core_effect::EffectMachine::new(&backend.notify_table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&backend.notify_expr, &mut dispatcher, &backend.ctx)
            .expect("EffectMachine should complete");

        let response = NotifyToolResult::from_value(&result, &backend.notify_table)
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
    fn test_bridge_dispatcher_unhandled_effect() {
        let table = DataConTable::new();
        let mut dispatcher = BridgeDispatcher::new(None, vec!["Popup".into()], None);
        let dummy_value = Value::Lit(tidepool_core_repr::Literal::LitInt(0));
        let ctx = EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        };
        let tidepool_cx = TidepoolEffectContext::with_user(&table, &ctx);

        let result = dispatcher.dispatch(99, &dummy_value, &tidepool_cx);
        assert!(matches!(
            result,
            Err(TidepoolEffectError::UnhandledEffect { tag: 99 })
        ));
    }

    #[test]
    fn test_minimal_single_effect() {
        // Minimal test: Haskell `send GetToolInput` (returns Int).
        // Tests that the EffectMachine can evaluate the simplest possible
        // freer-simple expression compiled from real Haskell source.
        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/PopupMinimal.hs::minimal");

        // Handler for tag 0 (Popup effect): return 42
        struct Echo42;
        impl tidepool_core_effect::dispatch::DispatchEffect<EffectContext> for Echo42 {
            fn dispatch(
                &mut self,
                tag: u64,
                _request: &Value,
                _cx: &tidepool_core_effect::dispatch::EffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                assert_eq!(tag, 0, "Expected Popup effect at tag 0");
                Ok(Value::Lit(tidepool_core_repr::Literal::LitInt(42)))
            }
        }

        let ctx = EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        };
        let mut heap = tidepool_core_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_core_effect::EffectMachine::new(&table, &mut heap).unwrap();
        let mut handler = Echo42;

        let result = machine.run_with_user(&expr, &mut handler, &ctx).unwrap();
        match result {
            Value::Lit(tidepool_core_repr::Literal::LitInt(n)) => assert_eq!(n, 42),
            other => panic!("Expected Lit(42), got {:?}", other),
        }
    }

    #[test]
    fn test_effect_machine_full_pipeline() {
        // Full pipeline: popupTool yields GetToolInput, then ShowPopup.
        // Uses a mock dispatcher that returns a fake PopupResponse for ShowPopup,
        // avoiding the real PopupService (which blocks waiting for Zellij).
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

        // Mock dispatcher: GetToolInput returns tool_input, ShowPopup returns fake response
        struct MockDispatcher {
            tool_input: Option<ToolInput>,
        }
        impl DispatchEffect<EffectContext> for MockDispatcher {
            fn dispatch(
                &mut self,
                tag: u64,
                request: &Value,
                cx: &TidepoolEffectContext<'_, EffectContext>,
            ) -> Result<Value, TidepoolEffectError> {
                assert_eq!(tag, 0, "Expected Popup effect at tag 0");
                let req = PopupReq::from_value(request, cx.table())
                    .map_err(TidepoolEffectError::Bridge)?;
                match req {
                    PopupReq::GetToolInput => {
                        let input = self.tool_input.take().ok_or_else(|| {
                            TidepoolEffectError::Handler("tool_input already consumed".into())
                        })?;
                        input.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                    }
                    PopupReq::ShowPopup(title, _components) => {
                        let response = PopupResponse {
                            button: "submit".to_string(),
                            values: format!("{{\"from\":\"{}\"}}", title),
                        };
                        response.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                    }
                }
            }
        }

        let mut dispatcher = MockDispatcher {
            tool_input: Some(tool_input),
        };

        let mut heap = tidepool_core_eval::heap::VecHeap::new();
        let mut machine =
            tidepool_core_effect::EffectMachine::new(&backend.table, &mut heap).unwrap();

        let result = machine
            .run_with_user(&backend.popup_expr, &mut dispatcher, &backend.ctx)
            .expect("EffectMachine should complete successfully");

        let response = PopupResponse::from_value(&result, &backend.table)
            .expect("Should decode PopupResponse from result");
        assert_eq!(response.button, "submit");
        assert!(response.values.contains("Test Title"));
    }
}
