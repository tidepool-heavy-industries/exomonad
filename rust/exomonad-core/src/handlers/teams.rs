//! Teams effect handler for the `teams.*` namespace.
//!
//! Implements task management for Gemini agents participating in Claude Code Teams.
//! Reads/writes the same filesystem structures that Claude's native TaskCreate/TaskUpdate/TaskList
//! tools use (`~/.claude/tasks/{team}/`).
//!
//! Also provides `report_status` (fire-and-forget note to TL inbox) and `ask_question`
//! (blocking question via QuestionRegistry oneshot channels — no polling).

use crate::effects::{
    dispatch_teams_effect, EffectError, EffectHandler, EffectResult, TeamsEffects,
};
use crate::services::inbox;
use crate::services::questions::QuestionRegistry;
use async_trait::async_trait;
use exomonad_proto::effects::teams::*;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

/// Task file format on disk (camelCase to match Claude Code Teams protocol).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct TaskFile {
    id: String,
    subject: String,
    description: String,
    active_form: String,
    status: String,
    blocks: Vec<String>,
    blocked_by: Vec<String>,
    owner: String,
}

impl TaskFile {
    fn to_proto(&self) -> TaskInfo {
        let status = match self.status.as_str() {
            "pending" => TaskStatus::Pending as i32,
            "in_progress" => TaskStatus::InProgress as i32,
            "completed" => TaskStatus::Completed as i32,
            _ => TaskStatus::Unspecified as i32,
        };

        TaskInfo {
            id: self.id.clone(),
            subject: self.subject.clone(),
            description: self.description.clone(),
            active_form: self.active_form.clone(),
            status,
            blocks: self.blocks.clone(),
            blocked_by: self.blocked_by.clone(),
            owner: self.owner.clone(),
        }
    }
}

/// Teams effect handler.
///
/// Handles task management (`teams.claim_task`, `teams.complete_task`, etc.)
/// and messaging (`teams.report_status`, `teams.ask_question`) effects.
pub struct TeamsHandler {
    question_registry: Arc<QuestionRegistry>,
}

impl TeamsHandler {
    pub fn new(question_registry: Arc<QuestionRegistry>) -> Self {
        Self { question_registry }
    }
}

#[async_trait]
impl EffectHandler for TeamsHandler {
    fn namespace(&self) -> &str {
        "teams"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_teams_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl TeamsEffects for TeamsHandler {
    async fn claim_task(&self, req: ClaimTaskRequest) -> EffectResult<ClaimTaskResponse> {
        let team_name = get_team_name()?;
        let agent_id = get_agent_id();
        let task_path = get_task_path(&team_name, &req.task_id)?;

        tracing::info!(
            task_id = %req.task_id,
            agent = %agent_id,
            "Claiming task"
        );

        let content = tokio::fs::read_to_string(&task_path)
            .await
            .map_err(|e| EffectError::not_found(format!("task/{}: {}", req.task_id, e)))?;

        let mut task: TaskFile = serde_json::from_str(&content).map_err(|e| {
            EffectError::custom("teams_error", format!("Failed to parse task file: {}", e))
        })?;

        // Verify task is claimable
        if task.status != "pending" {
            return Ok(ClaimTaskResponse {
                success: false,
                task: Some(task.to_proto()),
                error: format!("Task is '{}', not 'pending'", task.status),
            });
        }

        if !task.owner.is_empty() {
            return Ok(ClaimTaskResponse {
                success: false,
                task: Some(task.to_proto()),
                error: format!("Task already owned by '{}'", task.owner),
            });
        }

        // Check blockedBy — all must be resolved (completed)
        if !task.blocked_by.is_empty() {
            for blocker_id in &task.blocked_by {
                let blocker_path = get_task_path(&team_name, blocker_id)?;
                if let Ok(blocker_content) = tokio::fs::read_to_string(&blocker_path).await {
                    if let Ok(blocker) = serde_json::from_str::<TaskFile>(&blocker_content) {
                        if blocker.status != "completed" {
                            return Ok(ClaimTaskResponse {
                                success: false,
                                task: Some(task.to_proto()),
                                error: format!(
                                    "Blocked by task #{} (status: {})",
                                    blocker_id, blocker.status
                                ),
                            });
                        }
                    }
                }
            }
        }

        // Claim it
        task.status = "in_progress".to_string();
        task.owner = agent_id.clone();

        let new_content = serde_json::to_string_pretty(&task).map_err(|e| {
            EffectError::custom("teams_error", format!("Failed to serialize task: {}", e))
        })?;

        tokio::fs::write(&task_path, new_content)
            .await
            .map_err(|e| {
                EffectError::custom("teams_error", format!("Failed to write task file: {}", e))
            })?;

        tracing::info!(
            task_id = %req.task_id,
            agent = %agent_id,
            "Task claimed"
        );

        Ok(ClaimTaskResponse {
            success: true,
            task: Some(task.to_proto()),
            error: String::new(),
        })
    }

    async fn complete_task(&self, req: CompleteTaskRequest) -> EffectResult<CompleteTaskResponse> {
        let team_name = get_team_name()?;
        let agent_id = get_agent_id();
        let task_path = get_task_path(&team_name, &req.task_id)?;

        tracing::info!(
            task_id = %req.task_id,
            agent = %agent_id,
            "Completing task"
        );

        let content = tokio::fs::read_to_string(&task_path)
            .await
            .map_err(|e| EffectError::not_found(format!("task/{}: {}", req.task_id, e)))?;

        let mut task: TaskFile = serde_json::from_str(&content).map_err(|e| {
            EffectError::custom("teams_error", format!("Failed to parse task file: {}", e))
        })?;

        // Verify ownership
        if task.owner != agent_id {
            return Ok(CompleteTaskResponse {
                success: false,
                task: Some(task.to_proto()),
                error: format!("Task owned by '{}', not '{}'", task.owner, agent_id),
            });
        }

        task.status = "completed".to_string();

        let new_content = serde_json::to_string_pretty(&task).map_err(|e| {
            EffectError::custom("teams_error", format!("Failed to serialize task: {}", e))
        })?;

        tokio::fs::write(&task_path, new_content)
            .await
            .map_err(|e| {
                EffectError::custom("teams_error", format!("Failed to write task file: {}", e))
            })?;

        // Report completion to TL inbox
        let team_name_clone = team_name.clone();
        let agent_id_clone = agent_id.clone();
        let summary = if req.summary.is_empty() {
            format!("Completed task #{}: {}", task.id, task.subject)
        } else {
            format!(
                "Completed task #{}: {}\n{}",
                task.id, task.subject, req.summary
            )
        };

        let tl_inbox = inbox::inbox_path(&team_name_clone, "team-lead");
        let msg = inbox::create_message(agent_id_clone, summary, None);
        tokio::task::spawn_blocking(move || inbox::append_message(&tl_inbox, &msg))
            .await
            .map_err(|e| EffectError::custom("teams_error", e.to_string()))?
            .map_err(|e| EffectError::custom("teams_error", e.to_string()))?;

        tracing::info!(
            task_id = %req.task_id,
            agent = %agent_id,
            "Task completed"
        );

        Ok(CompleteTaskResponse {
            success: true,
            task: Some(task.to_proto()),
            error: String::new(),
        })
    }

    async fn list_tasks(&self, _req: ListTasksRequest) -> EffectResult<ListTasksResponse> {
        let team_name = get_team_name()?;
        let tasks_dir = get_tasks_dir(&team_name)?;

        tracing::debug!(team = %team_name, "Listing tasks");

        if !tasks_dir.exists() {
            return Ok(ListTasksResponse { tasks: Vec::new() });
        }

        let mut entries = tokio::fs::read_dir(&tasks_dir).await.map_err(|e| {
            EffectError::custom("teams_error", format!("Failed to read tasks dir: {}", e))
        })?;

        let mut tasks = Vec::new();
        while let Some(entry) = entries.next_entry().await.map_err(|e| {
            EffectError::custom("teams_error", format!("Failed to read dir entry: {}", e))
        })? {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) != Some("json") {
                continue;
            }

            match tokio::fs::read_to_string(&path).await {
                Ok(content) => match serde_json::from_str::<TaskFile>(&content) {
                    Ok(task) => tasks.push(task.to_proto()),
                    Err(e) => {
                        tracing::warn!(
                            path = %path.display(),
                            error = %e,
                            "Skipping malformed task file"
                        );
                    }
                },
                Err(e) => {
                    tracing::warn!(
                        path = %path.display(),
                        error = %e,
                        "Failed to read task file"
                    );
                }
            }
        }

        tracing::debug!(count = tasks.len(), "Listed tasks");
        Ok(ListTasksResponse { tasks })
    }

    async fn get_task(&self, req: GetTaskRequest) -> EffectResult<GetTaskResponse> {
        let team_name = get_team_name()?;
        let task_path = get_task_path(&team_name, &req.task_id)?;

        let content = tokio::fs::read_to_string(&task_path)
            .await
            .map_err(|e| EffectError::not_found(format!("task/{}: {}", req.task_id, e)))?;

        let task: TaskFile = serde_json::from_str(&content).map_err(|e| {
            EffectError::custom("teams_error", format!("Failed to parse task file: {}", e))
        })?;

        Ok(GetTaskResponse {
            task: Some(task.to_proto()),
        })
    }

    async fn report_status(&self, req: ReportStatusRequest) -> EffectResult<ReportStatusResponse> {
        let team_name = get_team_name()?;
        let agent_id = get_agent_id();

        tracing::info!(agent = %agent_id, "Reporting status to TL");

        let tl_inbox = inbox::inbox_path(&team_name, "team-lead");
        let msg = inbox::create_message(agent_id, req.content, None);

        tokio::task::spawn_blocking(move || inbox::append_message(&tl_inbox, &msg))
            .await
            .map_err(|e| EffectError::custom("teams_error", e.to_string()))?
            .map_err(|e| EffectError::custom("teams_error", e.to_string()))?;

        Ok(ReportStatusResponse { ack: true })
    }

    async fn ask_question(&self, req: AskQuestionRequest) -> EffectResult<AskQuestionResponse> {
        let team_name = get_team_name()?;
        let agent_id = get_agent_id();

        tracing::info!(agent = %agent_id, "Asking question to TL");

        // Register in QuestionRegistry for oneshot wakeup
        let (question_id, rx) = self.question_registry.register();

        // Send question to TL inbox with question_id for correlation
        let tl_inbox = inbox::inbox_path(&team_name, "team-lead");
        let text = format!("[QUESTION q_id={}] {}", question_id, req.question);
        let msg = inbox::create_message(
            agent_id.clone(),
            text,
            Some(req.question.chars().take(50).collect()),
        );

        tokio::task::spawn_blocking(move || inbox::append_message(&tl_inbox, &msg))
            .await
            .map_err(|e| EffectError::custom("teams_error", e.to_string()))?
            .map_err(|e| EffectError::custom("teams_error", e.to_string()))?;

        // Await answer with 5 minute timeout (no polling)
        let timeout = Duration::from_secs(300);
        match tokio::time::timeout(timeout, rx).await {
            Ok(Ok(answer)) => {
                tracing::info!(
                    agent = %agent_id,
                    question_id = %question_id,
                    "Answer received"
                );
                Ok(AskQuestionResponse { answer })
            }
            Ok(Err(_)) => {
                // Channel closed (sender dropped) — should not happen normally
                self.question_registry.cancel(&question_id);
                Err(EffectError::custom(
                    "teams_error",
                    "Question channel closed unexpectedly",
                ))
            }
            Err(_) => {
                // Timeout
                self.question_registry.cancel(&question_id);
                tracing::error!(
                    agent = %agent_id,
                    question_id = %question_id,
                    "Question timed out after 300s"
                );
                Err(EffectError::timeout(format!(
                    "ask_question({}) timed out after 300s",
                    question_id
                )))
            }
        }
    }
}

// ============================================================================
// Shared helpers
// ============================================================================

fn get_team_name() -> Result<String, EffectError> {
    if let Ok(name) = std::env::var("EXOMONAD_TEAM_NAME") {
        return Ok(name);
    }
    if let Ok(name) = std::env::var("CLAUDE_TEAM_NAME") {
        return Ok(name);
    }

    Err(EffectError::custom(
        "teams_error",
        "EXOMONAD_TEAM_NAME or CLAUDE_TEAM_NAME not set",
    ))
}

fn get_agent_id() -> String {
    crate::mcp::agent_identity::get_agent_id()
}

fn get_tasks_dir(team_name: &str) -> Result<PathBuf, EffectError> {
    dirs::home_dir()
        .map(|h| h.join(".claude").join("tasks").join(team_name))
        .ok_or_else(|| EffectError::custom("teams_error", "Could not find home directory"))
}

fn get_task_path(team_name: &str, task_id: &str) -> Result<PathBuf, EffectError> {
    Ok(get_tasks_dir(team_name)?.join(format!("{}.json", task_id)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn write_task_file(dir: &std::path::Path, task: &TaskFile) {
        let path = dir.join(format!("{}.json", task.id));
        let content = serde_json::to_string_pretty(task).unwrap();
        std::fs::write(path, content).unwrap();
    }

    fn make_task(id: &str, status: &str, owner: &str) -> TaskFile {
        TaskFile {
            id: id.to_string(),
            subject: format!("Task {}", id),
            description: format!("Description for task {}", id),
            active_form: format!("Working on task {}", id),
            status: status.to_string(),
            blocks: Vec::new(),
            blocked_by: Vec::new(),
            owner: owner.to_string(),
        }
    }

    #[test]
    fn test_task_file_roundtrip() {
        let task = make_task("1", "pending", "");
        let json = serde_json::to_string_pretty(&task).unwrap();
        let parsed: TaskFile = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed.id, "1");
        assert_eq!(parsed.status, "pending");
        assert!(parsed.owner.is_empty());

        // Verify camelCase on disk
        let raw: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert!(raw.get("activeForm").is_some());
        assert!(raw.get("blockedBy").is_some());
    }

    #[test]
    fn test_task_to_proto() {
        let task = make_task("42", "in_progress", "agent-1");
        let proto = task.to_proto();

        assert_eq!(proto.id, "42");
        assert_eq!(proto.status, TaskStatus::InProgress as i32);
        assert_eq!(proto.owner, "agent-1");
    }

    #[tokio::test]
    async fn test_list_tasks_reads_dir() {
        let dir = TempDir::new().unwrap();
        let tasks_dir = dir.path();

        write_task_file(tasks_dir, &make_task("1", "pending", ""));
        write_task_file(tasks_dir, &make_task("2", "in_progress", "agent-1"));
        write_task_file(tasks_dir, &make_task("3", "completed", "agent-2"));

        // Read them back
        let mut entries = tokio::fs::read_dir(tasks_dir).await.unwrap();
        let mut tasks = Vec::new();
        while let Some(entry) = entries.next_entry().await.unwrap() {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) == Some("json") {
                let content = tokio::fs::read_to_string(&path).await.unwrap();
                let task: TaskFile = serde_json::from_str(&content).unwrap();
                tasks.push(task.to_proto());
            }
        }

        assert_eq!(tasks.len(), 3);
    }

    #[test]
    fn test_task_file_camelcase_compat() {
        // Verify we can parse what Claude Code Teams writes
        let json = r#"{
            "id": "1",
            "subject": "Fix the bug",
            "description": "There is a bug",
            "activeForm": "Fixing the bug",
            "status": "pending",
            "blocks": [],
            "blockedBy": ["2"],
            "owner": ""
        }"#;

        let task: TaskFile = serde_json::from_str(json).unwrap();
        assert_eq!(task.id, "1");
        assert_eq!(task.active_form, "Fixing the bug");
        assert_eq!(task.blocked_by, vec!["2"]);
    }
}
