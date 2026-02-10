//! Mailbox-based messaging service for agent↔TL communication.
//!
//! All messaging flows through Teams inboxes at
//! `~/.claude/teams/{team}/inboxes/{agent}.json`.
//!
//! **Agent-side (WASM effects):** `handlers/messaging.rs` writes to Teams inboxes
//! directly via `append_to_inbox`.
//!
//! **TL-side (direct Rust tools):** This module reads/writes Teams inboxes.
//!
//! The Teams inbox format is a JSON array of `TeamsMessage` objects.

use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use thiserror::Error;

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
        by_sender.entry(msg.from.clone()).or_default().push(msg);
    }

    Ok(by_sender.into_iter().collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

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
