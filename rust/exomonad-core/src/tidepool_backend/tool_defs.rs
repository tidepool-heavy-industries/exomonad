use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use schemars::JsonSchema;
use serde_json::Value as JsonValue;
use crate::mcp::ToolDefinition;

// =============================================================================
// Tool Definitions
// =============================================================================

/// Interactive popup element.
#[derive(Deserialize, Serialize, JsonSchema)]
pub struct PopupElement {
    #[schemars(with = "String")]
    #[serde(rename = "type")]
    /// Element type: text, choice, checkbox, textbox, slider, multiselect.
    pub kind: String,
    /// Element ID for result lookup.
    pub id: Option<String>,
    /// Label text.
    pub label: Option<String>,
    /// For text elements.
    pub content: Option<String>,
    /// For choice/multiselect.
    pub options: Option<Vec<String>>,
    /// Default value (type varies).
    pub default: Option<JsonValue>,
    /// For textbox.
    pub placeholder: Option<String>,
    /// For slider.
    pub min: Option<f64>,
    /// For slider.
    pub max: Option<f64>,
    /// For multiline textbox.
    pub rows: Option<u32>,
}

/// Named wizard pane.
#[derive(Deserialize, Serialize, JsonSchema)]
pub struct WizardPane {
    /// Title text.
    pub title: String,
    /// UI elements to display.
    pub elements: Vec<PopupElement>,
    /// Transition: string (goto) or object (branch by field value).
    pub then: Option<JsonValue>,
}

/// Show interactive popup form and get user response.
#[derive(Deserialize, Serialize, JsonSchema)]
pub struct PopupArgs {
    /// Optional popup title.
    pub title: Option<String>,
    /// UI elements to display.
    pub elements: Option<Vec<PopupElement>>,
    /// Named wizard panes. Use with 'start' for multi-pane wizard mode.
    pub panes: Option<HashMap<String, WizardPane>>,
    /// Starting pane name (required when using panes).
    pub start: Option<String>,
}

pub fn tool_def<A: schemars::JsonSchema>(name: &str, description: &str) -> ToolDefinition {
    let schema = schemars::schema_for!(A);
    ToolDefinition {
        name: name.to_string(),
        description: description.to_string(),
        input_schema: serde_json::to_value(schema).unwrap(),
    }
}

pub fn popup_tool_definition() -> ToolDefinition {
    tool_def::<PopupArgs>(
        "popup",
        "Show interactive popup form and get user response",
    )
}

/// Create or update a pull request.
#[derive(Deserialize, JsonSchema)]
pub struct FilePRArgs {
    /// PR title.
    pub title: String,
    /// PR body/description.
    pub body: String,
    /// Target branch. Auto-detected from dot-separated naming if omitted. Only set to override.
    pub base_branch: Option<String>,
}

pub fn file_pr_tool_definition() -> ToolDefinition {
    tool_def::<FilePRArgs>(
        "file_pr",
        "Create or update a pull request for the current branch. Idempotent \u{2014} safe to call multiple times (updates existing PR). Pushes the branch automatically. Base branch auto-detected from dot-separated naming (e.g. main.foo.bar targets main.foo).",
    )
}

/// Merge a GitHub pull request.
#[derive(Deserialize, JsonSchema)]
pub struct MergePRArgs {
    /// PR number to merge.
    pub pr_number: u64,
    /// Merge strategy: squash (default), merge, or rebase.
    pub strategy: Option<String>,
    /// Working directory for git/jj operations.
    pub working_dir: Option<String>,
}

pub fn merge_pr_tool_definition() -> ToolDefinition {
    tool_def::<MergePRArgs>(
        "merge_pr",
        "Merge a GitHub pull request and fetch changes via jj",
    )
}

/// Task completion details.
#[derive(Deserialize, JsonSchema)]
pub struct TaskCompletedArg {
    /// Task description.
    pub what: String,
    /// Verification command that was run.
    pub how: String,
}

/// Signal completion to parent agent.
#[derive(Deserialize, JsonSchema)]
pub struct NotifyParentArgs {
    /// 'success' = work is done and review-clean. 'failure' = exhausted retries, escalating to parent.
    pub status: String,
    /// One-line summary. On success: what was accomplished. On failure: what went wrong.
    pub message: String,
    /// PR number if one was filed. Enables parent to immediately merge without searching.
    pub pr_number: Option<u64>,
    /// Array of {what, how} pairs. 'what' = task description, 'how' = verification command that was run.
    pub tasks_completed: Option<Vec<TaskCompletedArg>>,
}

pub fn notify_parent_tool_definition() -> ToolDefinition {
    tool_def::<NotifyParentArgs>(
        "notify_parent",
        "Signal to your parent that you are DONE. Call as your final action \u{2014} after PR is filed, Copilot feedback addressed, and changes pushed. Status 'success' means work is review-clean. Status 'failure' means retries exhausted, escalating to parent.",
    )
}

/// Fork a worktree node.
#[derive(Deserialize, JsonSchema)]
pub struct SpawnSubtreeArgs {
    /// Description of the sub-problem to solve.
    pub task: String,
    /// Branch name suffix (will be prefixed with current branch).
    pub branch_name: String,
}

pub fn spawn_subtree_tool_definition() -> ToolDefinition {
    tool_def::<SpawnSubtreeArgs>(
        "spawn_subtree",
        "Fork a worktree node off your current branch. Use when decomposing work into sub-problems that may need further decomposition. The child gets full coordination tools (can spawn its own children).",
    )
}

/// Fork a worktree for a leaf agent.
#[derive(Deserialize, JsonSchema)]
pub struct SpawnLeafArgs {
    /// Description of the sub-problem to solve.
    pub task: String,
    /// Branch name suffix (will be prefixed with current branch).
    pub branch_name: String,
}

pub fn spawn_leaf_subtree_tool_definition() -> ToolDefinition {
    tool_def::<SpawnLeafArgs>(
        "spawn_leaf_subtree",
        "Fork a worktree for a Gemini leaf agent. Gets own branch for PR filing but cannot spawn children.",
    )
}

/// Worker specification.
#[derive(Deserialize, JsonSchema)]
pub struct WorkerSpec {
    /// Human-readable name for the leaf agent.
    pub name: String,
    /// Short description of the task.
    pub task: String,
    /// Raw prompt (escape hatch). If provided, all other fields except name are ignored.
    pub prompt: Option<String>,
    /// Numbered implementation steps.
    pub steps: Option<Vec<String>>,
    /// Freeform context: code snippets, examples, detailed specs.
    pub context: Option<String>,
    /// Acceptance criteria for completion.
    pub done_criteria: Option<Vec<String>>,
    /// Commands to verify the work.
    pub verify: Option<Vec<String>>,
    /// Things the agent must NOT do.
    pub boundary: Option<Vec<String>>,
    /// Files the agent should read before starting.
    pub read_first: Option<Vec<String>>,
}

/// Spawn multiple worker agents.
#[derive(Deserialize, JsonSchema)]
pub struct SpawnWorkersArgs {
    /// Array of worker specifications.
    pub specs: Vec<WorkerSpec>,
}

pub fn spawn_workers_tool_definition() -> ToolDefinition {
    tool_def::<SpawnWorkersArgs>(
        "spawn_workers",
        "Spawn multiple worker agents in one call. Each gets a Zellij pane in the current worktree.",
    )
}

/// Send a note.
#[derive(Deserialize, JsonSchema)]
pub struct NoteArgs {
    /// Note content.
    pub content: String,
}

pub fn note_tool_definition() -> ToolDefinition {
    tool_def::<NoteArgs>(
        "note",
        "Send a note to the team lead's inbox. Fire-and-forget.",
    )
}

/// Answer a pending question.
#[derive(Deserialize, JsonSchema)]
pub struct AnswerQuestionArgs {
    /// Agent that asked the question.
    pub agent_id: String,
    /// Question ID from the question message.
    pub question_id: String,
    /// Answer text.
    pub answer: String,
}

pub fn answer_question_tool_definition() -> ToolDefinition {
    tool_def::<AnswerQuestionArgs>(
        "answer_question",
        "Answer a pending question from an agent. Unblocks the agent immediately.",
    )
}

/// Read agent messages.
#[derive(Deserialize, JsonSchema)]
pub struct GetAgentMessagesArgs {
    /// Filter to messages from this agent (empty = all agents).
    pub agent_id: Option<String>,
    /// Long-poll timeout in seconds (0 = immediate return).
    pub timeout_secs: Option<u64>,
}

pub fn get_agent_messages_tool_definition() -> ToolDefinition {
    tool_def::<GetAgentMessagesArgs>(
        "get_agent_messages",
        "Read unread messages from agents. Supports long-polling with timeout.",
    )
}
