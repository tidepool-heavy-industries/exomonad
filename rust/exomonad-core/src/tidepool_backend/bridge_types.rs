use std::sync::Arc;
use tidepool_bridge_derive::{FromCore, ToCore};
use tidepool_repr::{CoreExpr, DataConTable};

// =============================================================================
// Shared effect bridge types (reused across tools)
// =============================================================================

/// Generic tool input effect. All tools use `GetToolInput` as their first
/// effect to receive MCP args from Rust. The handler is generic over the
/// input type T via ToolInputHandler<T>.
#[derive(Debug, FromCore)]
pub enum GetToolInputReq {
    #[core(name = "GetToolInput")]
    Get,
}

/// Shared Identity effect. Provides agent identity information.
/// Constructor names match Haskell GADT exactly.
#[derive(Debug, FromCore)]
pub enum IdentityReq {
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
pub enum InboxReq {
    #[core(name = "WriteMessage")]
    WriteMessage(String, String, String, String),
    #[core(name = "ReadMessages")]
    ReadMessages(String),
    #[core(name = "PollMessages")]
    PollMessages(String, String),
}

/// Shared Questions effect. Resolves pending questions.
#[derive(Debug, FromCore)]
pub enum QuestionsReq {
    #[core(name = "ResolveQuestion")]
    ResolveQuestion(String, String),
}

/// Shared FormatOp effect. Temporary bridge until Tidepool supports Prelude
/// functions (string comparison, conditionals). Haskell decides *what* to format;
/// Rust decides *how* until inline Haskell logic is possible.
#[derive(Debug, FromCore)]
pub enum FormatOpReq {
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
pub struct ToolInput {
    pub title: String,
    pub components: String,
}

/// Per-tool domain op for popup. Receives title, components, and target tab.
#[derive(Debug, FromCore)]
pub enum PopupOpReq {
    #[core(name = "ShowPopup")]
    ShowPopup(String, String, String),
}

/// Popup response returned to Haskell after showing the popup.
/// Field order must match Haskell record: `PopupResponse { prButton, prValues }`.
#[derive(Debug, FromCore, ToCore)]
#[core(name = "PopupResponse")]
pub struct PopupResponse {
    pub button: String,
    pub values: String,
}

// =============================================================================
// file_pr bridge types
// =============================================================================

/// Mirrors Haskell: `FilePRInput { fprTitle, fprBody, fprBaseBranch }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "FilePRInput")]
pub struct FilePRToolInput {
    pub title: String,
    pub body: String,
    pub base_branch: String,
}

/// Mirrors Haskell: `FilePRResult { fprPrUrl, fprPrNumber, fprHeadBranch, fprResultBase, fprCreated }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "FilePRResult")]
pub struct FilePRToolResult {
    pub pr_url: String,
    pub pr_number: String,
    pub head_branch: String,
    pub result_base: String,
    pub created: String,
}

/// Per-tool domain op for file_pr. Receives all args including working_dir from Identity.
#[derive(Debug, FromCore)]
pub enum FilePROpReq {
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
pub struct MergePRToolInput {
    pub pr_number: String,
    pub strategy: String,
}

/// Mirrors Haskell: `MergePRResult { mprSuccess, mprMessage, mprJjFetched }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "MergePRResult")]
pub struct MergePRToolResult {
    pub success: String,
    pub message: String,
    pub jj_fetched: String,
}

/// Per-tool domain op for merge_pr.
#[derive(Debug, FromCore)]
pub enum MergePROpReq {
    #[core(name = "MergePullRequest")]
    MergePullRequest(String, String, String),
}

// =============================================================================
// notify_parent bridge types
// =============================================================================

/// Mirrors Haskell: `NotifyInput { niStatus, niMessage }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "NotifyInput")]
pub struct NotifyToolInput {
    pub status: String,
    pub message: String,
}

/// Mirrors Haskell: `NotifyResult { nrAck }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "NotifyResult")]
pub struct NotifyToolResult {
    pub ack: String,
}

/// Per-tool domain op for notify_parent. Pure I/O — just injects formatted text.
#[derive(Debug, FromCore)]
pub enum NotifyOpReq {
    #[core(name = "DeliverNotification")]
    DeliverNotification(String, String), // parentTab, formatted
}

// =============================================================================
// spawn_subtree bridge types
// =============================================================================

/// Mirrors Haskell: `SpawnSubtreeInput { ssiTask, ssiBranchName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnSubtreeInput")]
pub struct SpawnSubtreeToolInput {
    pub task: String,
    pub branch_name: String,
}

/// Mirrors Haskell: `SpawnSubtreeResult { ssrTabName, ssrBranchName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnSubtreeResult")]
pub struct SpawnSubtreeToolResult {
    pub tab_name: String,
    pub branch_name: String,
}

/// Per-tool domain op for spawn_subtree.
#[derive(Debug, FromCore)]
pub enum SpawnSubtreeOpReq {
    #[core(name = "SpawnSubtree")]
    SpawnSubtree(String, String),
}

// =============================================================================
// spawn_leaf_subtree bridge types
// =============================================================================

/// Mirrors Haskell: `SpawnLeafInput { sliTask, sliBranchName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnLeafInput")]
pub struct SpawnLeafToolInput {
    pub task: String,
    pub branch_name: String,
}

/// Mirrors Haskell: `SpawnLeafResult { slrTabName, slrBranchName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnLeafResult")]
pub struct SpawnLeafToolResult {
    pub tab_name: String,
    pub branch_name: String,
}

/// Per-tool domain op for spawn_leaf.
#[derive(Debug, FromCore)]
pub enum SpawnLeafOpReq {
    #[core(name = "SpawnLeaf")]
    SpawnLeaf(String, String),
}

// =============================================================================
// spawn_workers bridge types
// =============================================================================

/// Mirrors Haskell: `WorkerSpec { wsName, wsPrompt }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "WorkerSpec")]
pub struct WorkerSpecBridge {
    pub name: String,
    pub prompt: String,
}

/// Mirrors Haskell: `SpawnWorkerResult { swrTabName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnWorkerResult")]
pub struct SpawnWorkerToolResult {
    pub tab_name: String,
}

/// Per-tool domain op for spawn_workers.
#[derive(Debug, FromCore)]
pub enum SpawnWorkerOpReq {
    #[core(name = "SpawnWorker")]
    SpawnWorker(String, String),
}

// =============================================================================
// note bridge types
// =============================================================================

/// Mirrors Haskell: `NoteInput { niContent }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "NoteInput")]
pub struct NoteToolInput {
    pub content: String,
}

/// Mirrors Haskell: `NoteResult { nrAck }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "NoteResult")]
pub struct NoteToolResult {
    pub ack: String,
}

/// Per-tool domain op for note. Pure I/O — injects pre-formatted text.
#[derive(Debug, FromCore)]
pub enum NoteOpReq {
    #[core(name = "InjectNote")]
    InjectNote(String, String), // parentTab, formatted
}

// =============================================================================
// answer_question bridge types
// =============================================================================

/// Mirrors Haskell: `AnswerInput { aiAgentId, aiQuestionId, aiAnswer }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "AnswerInput")]
pub struct AnswerToolInput {
    pub agent_id: String,
    pub question_id: String,
    pub answer: String,
}

/// Mirrors Haskell: `AnswerResult { arStatus, arAgentId, arQuestionId }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "AnswerResult")]
pub struct AnswerToolResult {
    pub status: String,
    pub agent_id: String,
    pub question_id: String,
}

// =============================================================================
// get_agent_messages bridge types
// =============================================================================

/// Mirrors Haskell: `MessagesInput { miAgentId, miTimeoutSecs }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "MessagesInput")]
pub struct MessagesToolInput {
    pub agent_id: String,
    pub timeout_secs: String,
}

/// Mirrors Haskell: `MessagesResult { mrMessagesJson, mrWarning }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "MessagesResult")]
pub struct MessagesToolResult {
    pub messages_json: String,
    pub warning: String,
}

/// Per-tool domain op for get_agent_messages. Timeout branching + inbox read in Rust.
#[derive(Debug, FromCore)]
pub enum MessagesOpReq {
    #[core(name = "FetchMessages")]
    FetchMessages(String, String), // agentId, timeoutSecs
}

// =============================================================================
// Compiled Tool Bundle
// =============================================================================

/// A compiled Haskell tool: Core expression + data constructor table.
pub struct CompiledTool {
    pub expr: CoreExpr,
    pub table: Arc<DataConTable>,
}
