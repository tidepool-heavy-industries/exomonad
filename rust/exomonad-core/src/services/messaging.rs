//! Mailbox-based messaging service for agent↔TL communication.
//!
//! Two messaging paths, unified on Teams inboxes:
//!
//! **Agent-side (WASM effects):** `handlers/messaging.rs` writes to Teams inboxes
//! directly via `append_to_inbox`.
//!
//! **TL-side (direct Rust tools):** This module reads/writes Teams inboxes at
//! `~/.claude/teams/{team}/inboxes/{agent}.json`.
//!
//! **Agent-local (within worktree):** `MessagingService` still uses JSONL files
//! for agent-internal polling (send_question waits for answer via inbox.jsonl).
//!
//! The Teams inbox format is a JSON array of `TeamsMessage` objects.

use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::time::Duration;
use thiserror::Error;
use tokio::time::sleep;
use uuid::Uuid;

/// Errors from the messaging service.
#[derive(Debug, Error)]
pub enum MessagingError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("Question timed out after {timeout_secs}s (question_id: {question_id})")]
    Timeout {
        question_id: String,
        timeout_secs: u64,
    },
}

/// A message in the outbox (agent → TL).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum OutboxMessage {
    #[serde(rename = "note")]
    Note {
        id: String,
        content: String,
        timestamp: String,
    },
    #[serde(rename = "question")]
    Question {
        id: String,
        content: String,
        context: String,
        timestamp: String,
        status: String,
    },
}

/// An answer in the inbox (TL → agent).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InboxAnswer {
    pub question_id: String,
    pub answer: String,
    pub timestamp: String,
}

/// Messaging service for a single agent directory.
pub struct MessagingService {
    messages_dir: PathBuf,
}

impl MessagingService {
    /// Create a service rooted at an agent directory.
    pub fn new(agent_dir: &Path) -> Self {
        Self {
            messages_dir: agent_dir.join(".exomonad").join("messages"),
        }
    }

    /// Create a service using the current working directory.
    pub fn from_cwd() -> Self {
        let cwd = std::env::current_dir().unwrap_or_default();
        Self::new(&cwd)
    }

    fn outbox_path(&self) -> PathBuf {
        self.messages_dir.join("outbox.jsonl")
    }

    fn inbox_path(&self) -> PathBuf {
        self.messages_dir.join("inbox.jsonl")
    }

    async fn ensure_dir(&self) -> Result<(), MessagingError> {
        tokio::fs::create_dir_all(&self.messages_dir).await?;
        Ok(())
    }

    /// Append a message to the outbox.
    async fn append_outbox(&self, msg: &OutboxMessage) -> Result<(), MessagingError> {
        self.ensure_dir().await?;
        let mut line = serde_json::to_string(msg)?;
        line.push('\n');
        tokio::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(self.outbox_path())
            .await?
            .write_all_buf(&mut line.as_bytes())
            .await?;
        Ok(())
    }

    /// Send a note (fire-and-forget).
    pub async fn send_note(&self, content: &str) -> Result<String, MessagingError> {
        let id = Uuid::new_v4().to_string();
        let msg = OutboxMessage::Note {
            id: id.clone(),
            content: content.to_string(),
            timestamp: Utc::now().to_rfc3339(),
        };

        tracing::info!(id = %id, "Sending note to outbox");
        self.append_outbox(&msg).await?;
        tracing::info!(id = %id, "Note written to outbox");

        Ok(id)
    }

    /// Send a question and poll for an answer.
    pub async fn send_question(
        &self,
        content: &str,
        context: &str,
        timeout_secs: u64,
        poll_interval_secs: u64,
    ) -> Result<String, MessagingError> {
        let question_id = format!("q-{}", Uuid::new_v4());
        let msg = OutboxMessage::Question {
            id: question_id.clone(),
            content: content.to_string(),
            context: context.to_string(),
            timestamp: Utc::now().to_rfc3339(),
            status: "pending".to_string(),
        };

        tracing::info!(question_id = %question_id, "Sending question to outbox");
        self.append_outbox(&msg).await?;

        // Poll inbox for answer
        let deadline = tokio::time::Instant::now() + Duration::from_secs(timeout_secs);
        let poll = Duration::from_secs(poll_interval_secs);

        tracing::info!(
            question_id = %question_id,
            timeout_secs = timeout_secs,
            poll_interval_secs = poll_interval_secs,
            "Polling inbox for answer"
        );

        loop {
            if tokio::time::Instant::now() >= deadline {
                tracing::error!(question_id = %question_id, "Question timed out");
                return Err(MessagingError::Timeout {
                    question_id,
                    timeout_secs,
                });
            }

            if let Some(answer) = self.check_inbox(&question_id).await? {
                tracing::info!(question_id = %question_id, "Answer received");
                return Ok(answer);
            }

            sleep(poll).await;
        }
    }

    /// Check inbox for an answer to a specific question.
    async fn check_inbox(&self, question_id: &str) -> Result<Option<String>, MessagingError> {
        let inbox = self.inbox_path();
        if !inbox.exists() {
            return Ok(None);
        }

        let contents = tokio::fs::read_to_string(&inbox).await?;
        for line in contents.lines() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            if let Ok(answer) = serde_json::from_str::<InboxAnswer>(line) {
                if answer.question_id == question_id {
                    return Ok(Some(answer.answer));
                }
            }
        }

        Ok(None)
    }
}

// ============================================================================
// Teams inbox types (shared with handlers/messaging.rs)
// ============================================================================

/// Teams message format used by Claude Code Teams.
/// Matches the format in `handlers/messaging.rs`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TeamsMessage {
    pub from: String,
    pub text: String,
    pub summary: String,
    pub timestamp: String,
    pub color: String,
    pub read: bool,
}

// ============================================================================
// TL-side operations (read/write Teams inboxes)
// ============================================================================

/// Get the Teams inboxes directory for a team.
fn get_teams_inbox_dir(team_name: &str) -> Result<PathBuf, MessagingError> {
    let home = dirs::home_dir().ok_or_else(|| {
        MessagingError::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "Could not find home directory",
        ))
    })?;
    Ok(home
        .join(".claude")
        .join("teams")
        .join(team_name)
        .join("inboxes"))
}

/// Read unread messages from an agent's Teams inbox.
///
/// Reads from `~/.claude/teams/{team}/inboxes/{agent_id}.json`.
/// Returns only unread messages (where `read == false`).
pub async fn read_agent_inbox(
    team_name: &str,
    agent_id: &str,
) -> Result<Vec<TeamsMessage>, MessagingError> {
    let inbox_dir = get_teams_inbox_dir(team_name)?;
    let inbox_path = inbox_dir.join(format!("{}.json", agent_id));

    if !inbox_path.exists() {
        return Ok(Vec::new());
    }

    let contents = tokio::fs::read_to_string(&inbox_path).await?;
    if contents.trim().is_empty() {
        return Ok(Vec::new());
    }

    let messages: Vec<TeamsMessage> = serde_json::from_str(&contents).map_err(|e| {
        tracing::warn!(
            path = %inbox_path.display(),
            error = %e,
            "Failed to parse Teams inbox"
        );
        MessagingError::Json(e)
    })?;

    let unread: Vec<TeamsMessage> = messages.into_iter().filter(|m| !m.read).collect();

    Ok(unread)
}

/// Read the TL's own Teams inbox for messages from agents.
///
/// Agents write to the TL's inbox (e.g., `team-lead.json`).
/// This reads `~/.claude/teams/{team}/inboxes/team-lead.json`.
pub async fn read_tl_inbox(team_name: &str) -> Result<Vec<TeamsMessage>, MessagingError> {
    read_agent_inbox(team_name, "team-lead").await
}

/// Write a message to an agent's Teams inbox (TL answering a question).
///
/// Appends to `~/.claude/teams/{team}/inboxes/{agent_id}.json`.
pub async fn write_to_agent_inbox(
    team_name: &str,
    agent_id: &str,
    answer: &str,
) -> Result<(), MessagingError> {
    let inbox_dir = get_teams_inbox_dir(team_name)?;
    tokio::fs::create_dir_all(&inbox_dir).await?;

    let inbox_path = inbox_dir.join(format!("{}.json", agent_id));

    tracing::info!(
        team = %team_name,
        agent = %agent_id,
        path = %inbox_path.display(),
        "Writing answer to agent Teams inbox"
    );

    // Read existing messages
    let mut messages: Vec<TeamsMessage> = if inbox_path.exists() {
        let contents = tokio::fs::read_to_string(&inbox_path).await?;
        if contents.trim().is_empty() {
            Vec::new()
        } else {
            serde_json::from_str(&contents)?
        }
    } else {
        Vec::new()
    };

    // Append the answer as a new message
    messages.push(TeamsMessage {
        from: "team-lead".to_string(),
        text: answer.to_string(),
        summary: answer.chars().take(50).collect(),
        timestamp: Utc::now().to_rfc3339(),
        color: "blue".to_string(),
        read: false,
    });

    let new_content = serde_json::to_string(&messages)?;
    tokio::fs::write(&inbox_path, new_content).await?;

    Ok(())
}

/// Scan all agent inboxes in a team for unread messages to the TL.
///
/// Reads from `~/.claude/teams/{team}/inboxes/team-lead.json` which is where
/// agents send messages to the TL. Groups messages by sender (`from` field).
pub async fn scan_all_agent_messages_teams(
    team_name: &str,
) -> Result<Vec<(String, Vec<TeamsMessage>)>, MessagingError> {
    let messages = read_tl_inbox(team_name).await?;

    if messages.is_empty() {
        return Ok(Vec::new());
    }

    // Group by sender
    let mut by_sender: std::collections::HashMap<String, Vec<TeamsMessage>> =
        std::collections::HashMap::new();
    for msg in messages {
        by_sender
            .entry(msg.from.clone())
            .or_default()
            .push(msg);
    }

    Ok(by_sender.into_iter().collect())
}

// ============================================================================
// Legacy TL-side operations (read .exomonad/agents/ outboxes)
// Kept for backward compatibility during migration.
// ============================================================================

/// Read all outbox messages from an agent directory (legacy path).
pub async fn read_agent_outbox(agent_dir: &Path) -> Result<Vec<OutboxMessage>, MessagingError> {
    let outbox = agent_dir
        .join(".exomonad")
        .join("messages")
        .join("outbox.jsonl");

    if !outbox.exists() {
        return Ok(Vec::new());
    }

    let contents = tokio::fs::read_to_string(&outbox).await?;
    let mut messages = Vec::new();

    for line in contents.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        match serde_json::from_str::<OutboxMessage>(line) {
            Ok(msg) => messages.push(msg),
            Err(e) => {
                tracing::warn!(error = %e, line = line, "Skipping malformed outbox line");
            }
        }
    }

    Ok(messages)
}

/// Write an answer to an agent's inbox (legacy path).
pub async fn write_agent_answer(
    agent_dir: &Path,
    question_id: &str,
    answer: &str,
) -> Result<(), MessagingError> {
    let messages_dir = agent_dir.join(".exomonad").join("messages");
    tokio::fs::create_dir_all(&messages_dir).await?;

    let inbox = messages_dir.join("inbox.jsonl");
    let entry = InboxAnswer {
        question_id: question_id.to_string(),
        answer: answer.to_string(),
        timestamp: Utc::now().to_rfc3339(),
    };

    let mut line = serde_json::to_string(&entry)?;
    line.push('\n');

    tracing::info!(question_id = %question_id, agent_dir = %agent_dir.display(), "Writing answer to agent inbox");

    tokio::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&inbox)
        .await?
        .write_all_buf(&mut line.as_bytes())
        .await?;

    Ok(())
}

/// Scan all agent directories and collect their outbox messages (legacy path).
pub async fn scan_all_agent_messages(
    project_dir: &Path,
    subrepo: Option<&str>,
) -> Result<Vec<(String, Vec<OutboxMessage>)>, MessagingError> {
    let agents_dir = match subrepo {
        Some(sub) => project_dir.join(sub).join(".exomonad").join("agents"),
        None => project_dir.join(".exomonad").join("agents"),
    };

    if !agents_dir.exists() {
        return Ok(Vec::new());
    }

    let mut results = Vec::new();
    let mut entries = tokio::fs::read_dir(&agents_dir).await?;

    while let Some(entry) = entries.next_entry().await? {
        if !entry.file_type().await?.is_dir() {
            continue;
        }

        let agent_id = entry.file_name().to_string_lossy().to_string();
        let agent_path = entry.path();

        match read_agent_outbox(&agent_path).await {
            Ok(messages) if !messages.is_empty() => {
                results.push((agent_id, messages));
            }
            Ok(_) => {}
            Err(e) => {
                tracing::warn!(
                    agent = %agent_id,
                    error = %e,
                    "Failed to read agent outbox"
                );
            }
        }
    }

    Ok(results)
}

// ============================================================================
// Tokio AsyncWrite helper
// ============================================================================
use tokio::io::AsyncWriteExt;

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_send_note_writes_jsonl() {
        let dir = TempDir::new().unwrap();
        let svc = MessagingService::new(dir.path());

        let id = svc.send_note("hello from agent").await.unwrap();
        assert!(!id.is_empty());

        let outbox = tokio::fs::read_to_string(svc.outbox_path()).await.unwrap();
        let msg: OutboxMessage = serde_json::from_str(outbox.trim()).unwrap();

        match msg {
            OutboxMessage::Note { content, .. } => assert_eq!(content, "hello from agent"),
            _ => panic!("Expected note"),
        }
    }

    #[tokio::test]
    async fn test_send_question_with_answer() {
        let dir = TempDir::new().unwrap();
        let svc = MessagingService::new(dir.path());

        // We need to know the question_id, so let's write the question first,
        // then read it, then write the answer. But that requires async coordination.
        // Instead, test the round-trip via write_agent_answer + check_inbox.

        let question_id = "q-test-123";
        write_agent_answer(dir.path(), question_id, "yes, fix the bug")
            .await
            .unwrap();

        let answer = svc.check_inbox(question_id).await.unwrap();
        assert_eq!(answer, Some("yes, fix the bug".to_string()));
    }

    #[tokio::test]
    async fn test_send_question_timeout() {
        let dir = TempDir::new().unwrap();
        let svc = MessagingService::new(dir.path());

        // Write question to outbox first so the directory exists
        svc.send_note("setup").await.unwrap();

        let result = svc.send_question("test?", "", 1, 1).await;
        assert!(result.is_err());
        match result.unwrap_err() {
            MessagingError::Timeout { timeout_secs, .. } => assert_eq!(timeout_secs, 1),
            e => panic!("Expected timeout, got: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_read_agent_outbox() {
        let dir = TempDir::new().unwrap();
        let svc = MessagingService::new(dir.path());

        svc.send_note("note 1").await.unwrap();
        svc.send_note("note 2").await.unwrap();

        let messages = read_agent_outbox(dir.path()).await.unwrap();
        assert_eq!(messages.len(), 2);
    }

    #[tokio::test]
    async fn test_scan_all_empty() {
        let dir = TempDir::new().unwrap();
        let result = scan_all_agent_messages(dir.path(), None).await.unwrap();
        assert!(result.is_empty());
    }

    #[tokio::test]
    async fn test_scan_all_with_agents() {
        let dir = TempDir::new().unwrap();

        // Create a fake agent directory with a message
        let agent_dir = dir
            .path()
            .join(".exomonad")
            .join("agents")
            .join("gh-42-fix-bug-claude");
        let svc = MessagingService::new(&agent_dir);
        svc.send_note("found the bug").await.unwrap();

        let results = scan_all_agent_messages(dir.path(), None).await.unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].0, "gh-42-fix-bug-claude");
        assert_eq!(results[0].1.len(), 1);
    }

    // ========================================================================
    // Teams inbox tests
    // ========================================================================

    #[tokio::test]
    async fn test_teams_message_roundtrip() {
        let msg = TeamsMessage {
            from: "agent-1".to_string(),
            text: "hello TL".to_string(),
            summary: "hello TL".to_string(),
            timestamp: "2026-02-09T12:00:00Z".to_string(),
            color: "green".to_string(),
            read: false,
        };

        let json = serde_json::to_string(&vec![msg]).unwrap();
        let parsed: Vec<TeamsMessage> = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].from, "agent-1");
        assert_eq!(parsed[0].text, "hello TL");
        assert!(!parsed[0].read);
    }

    #[tokio::test]
    async fn test_teams_read_filters_unread() {
        let dir = TempDir::new().unwrap();
        let inbox_path = dir.path().join("agent-1.json");

        let messages = vec![
            TeamsMessage {
                from: "agent-1".to_string(),
                text: "read msg".to_string(),
                summary: "read msg".to_string(),
                timestamp: "2026-02-09T12:00:00Z".to_string(),
                color: "green".to_string(),
                read: true,
            },
            TeamsMessage {
                from: "agent-1".to_string(),
                text: "unread msg".to_string(),
                summary: "unread msg".to_string(),
                timestamp: "2026-02-09T12:01:00Z".to_string(),
                color: "green".to_string(),
                read: false,
            },
        ];

        tokio::fs::write(&inbox_path, serde_json::to_string(&messages).unwrap())
            .await
            .unwrap();

        // read_agent_inbox needs a real home dir, so test the filtering logic directly
        let contents = tokio::fs::read_to_string(&inbox_path).await.unwrap();
        let all: Vec<TeamsMessage> = serde_json::from_str(&contents).unwrap();
        let unread: Vec<TeamsMessage> = all.into_iter().filter(|m| !m.read).collect();
        assert_eq!(unread.len(), 1);
        assert_eq!(unread[0].text, "unread msg");
    }
}
