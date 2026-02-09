//! Messaging effect handler for the `messaging.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::messaging`.

use crate::effects::{
    dispatch_messaging_effect, EffectError, EffectHandler, EffectResult, MessagingEffects,
};
use async_trait::async_trait;
use chrono::Utc;
use exomonad_proto::effects::messaging::*;
use serde::{Deserialize, Serialize};
use std::fs::OpenOptions;
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::time::Duration;
use tokio::time::sleep;

#[cfg(unix)]
use nix::fcntl::{Flock, FlockArg};

/// Teams message format used by Claude Code Teams.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct TeamsMessage {
    from: String,
    text: String,
    summary: String,
    timestamp: String,
    color: String,
    read: bool,
}

/// Messaging effect handler.
///
/// Handles `messaging.send_note` and `messaging.send_question` effects
/// by reading/writing JSON inbox files in the Teams directory.
pub struct MessagingHandler;

impl MessagingHandler {
    pub fn new() -> Self {
        Self
    }
}

impl Default for MessagingHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl EffectHandler for MessagingHandler {
    fn namespace(&self) -> &str {
        "messaging"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_messaging_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl MessagingEffects for MessagingHandler {
    async fn send_note(&self, req: SendNoteRequest) -> EffectResult<SendNoteResponse> {
        let team_name = get_team_name()?;
        let agent_id = get_agent_id();
        let inbox_dir = get_inbox_dir(&team_name)?;
        let tl_inbox = inbox_dir.join("team-lead.json");

        let msg = TeamsMessage {
            from: agent_id,
            text: req.content.clone(),
            summary: req.content.chars().take(50).collect(),
            timestamp: Utc::now().to_rfc3339(),
            color: "green".to_string(),
            read: false,
        };

        tokio::task::spawn_blocking(move || append_to_inbox(&tl_inbox, msg))
            .await
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

        Ok(SendNoteResponse { ack: true })
    }

    async fn send_question(&self, req: SendQuestionRequest) -> EffectResult<SendQuestionResponse> {
        let team_name = get_team_name()?;
        let agent_id = get_agent_id();
        let inbox_dir = get_inbox_dir(&team_name)?;
        let tl_inbox = inbox_dir.join("team-lead.json");
        let my_inbox = inbox_dir.join(format!("{}.json", agent_id));

        let text = format!("[QUESTION] {}", req.content);
        let msg = TeamsMessage {
            from: agent_id.clone(),
            text,
            summary: req.content.chars().take(50).collect(),
            timestamp: Utc::now().to_rfc3339(),
            color: "green".to_string(),
            read: false,
        };

        tokio::task::spawn_blocking(move || append_to_inbox(&tl_inbox, msg))
            .await
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

        // Poll for answer
        let timeout = Duration::from_secs(300);
        let interval = Duration::from_secs(5);
        let start = std::time::Instant::now();

        loop {
            if start.elapsed() >= timeout {
                return Err(EffectError::custom(
                    "messaging_error",
                    format!("Question timed out after {}s", timeout.as_secs()),
                ));
            }

            let my_inbox_clone = my_inbox.clone();
            if let Some(answer) =
                tokio::task::spawn_blocking(move || consume_unread_from_inbox(&my_inbox_clone))
                    .await
                    .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
                    .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
            {
                return Ok(SendQuestionResponse {
                    answer: answer.text,
                });
            }

            sleep(interval).await;
        }
    }
}

fn get_team_name() -> Result<String, EffectError> {
    if let Ok(name) = std::env::var("EXOMONAD_TEAM_NAME") {
        return Ok(name);
    }

    let config_path = std::env::current_dir()
        .unwrap_or_default()
        .join(".exomonad")
        .join("config.toml");

    if config_path.exists() {
        if let Ok(content) = std::fs::read_to_string(&config_path) {
            for line in content.lines() {
                let line = line.trim();
                if line.starts_with("team_name") {
                    if let Some(val) = line.split('=').nth(1) {
                        return Ok(val.trim().trim_matches('"').trim_matches('\'').to_string());
                    }
                }
            }
        }
    }

    Err(EffectError::custom(
        "messaging_error",
        "EXOMONAD_TEAM_NAME not set and no team_name in .exomonad/config.toml",
    ))
}

fn get_agent_id() -> String {
    if let Ok(id) = std::env::var("EXOMONAD_AGENT_ID") {
        return id;
    }

    std::env::current_dir()
        .ok()
        .and_then(|path| path.file_name().map(|n| n.to_string_lossy().to_string()))
        .unwrap_or_else(|| "unknown-agent".to_string())
}

fn get_inbox_dir(team: &str) -> Result<PathBuf, EffectError> {
    dirs::home_dir()
        .map(|h| h.join(".claude").join("teams").join(team).join("inboxes"))
        .ok_or_else(|| EffectError::custom("messaging_error", "Could not find home directory"))
}

fn append_to_inbox(path: &Path, msg: TeamsMessage) -> anyhow::Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(path)?;

    #[cfg(unix)]
    let mut locked_file = Flock::lock(file, FlockArg::LockExclusive).map_err(|(_, e)| e)?;
    #[cfg(not(unix))]
    let mut locked_file = file;

    let mut content = String::new();
    locked_file.read_to_string(&mut content)?;

    let mut messages: Vec<TeamsMessage> = if content.is_empty() {
        Vec::new()
    } else {
        serde_json::from_str(&content)?
    };

    messages.push(msg);
    atomic_write(path, &messages, &mut locked_file)?;

    Ok(())
}

fn consume_unread_from_inbox(path: &Path) -> anyhow::Result<Option<TeamsMessage>> {
    if !path.exists() {
        return Ok(None);
    }

    let file = OpenOptions::new().read(true).write(true).open(path)?;

    #[cfg(unix)]
    let mut locked_file = Flock::lock(file, FlockArg::LockExclusive).map_err(|(_, e)| e)?;
    #[cfg(not(unix))]
    let mut locked_file = file;

    let mut content = String::new();
    locked_file.read_to_string(&mut content)?;
    let mut messages: Vec<TeamsMessage> = serde_json::from_str(&content)?;

    // Find first unread message
    let mut found_idx = None;
    for (idx, msg) in messages.iter().enumerate() {
        if !msg.read {
            found_idx = Some(idx);
            break;
        }
    }

    if let Some(idx) = found_idx {
        let msg = messages[idx].clone();
        messages[idx].read = true;
        atomic_write(path, &messages, &mut locked_file)?;
        return Ok(Some(msg));
    }

    Ok(None)
}

/// Helper for atomic write to avoid corruption on crash.
/// Writes to .tmp then renames.
fn atomic_write(
    path: &Path,
    messages: &[TeamsMessage],
    #[allow(unused_variables)] locked_file: &mut std::fs::File,
) -> anyhow::Result<()> {
    let new_content = serde_json::to_string(messages)?;

    let tmp_path = path.with_extension("tmp");
    let mut tmp_file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(&tmp_path)?;

    tmp_file.write_all(new_content.as_bytes())?;
    tmp_file.sync_all()?;

    // Perform the rename. On Unix, this is atomic.
    std::fs::rename(tmp_path, path)?;

    // After rename, the original file handle `locked_file` is still pointing to the old inode
    // (on Unix). We should probably also truncate/write to the original file to keep it in sync
    // or just rely on rename and accept that the next opener gets the new file.
    // However, if we want to be strictly correct with the lock, we should write to the locked file.
    // But Claude Teams logic might expect the file to be replaced.
    // For now, let's also write to the locked file to ensure anyone holding the lock sees the update.
    locked_file.set_len(0)?;
    locked_file.seek(SeekFrom::Start(0))?;
    locked_file.write_all(new_content.as_bytes())?;
    locked_file.sync_all()?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;

    #[test]
    fn test_append_and_consume() {
        let tmp = NamedTempFile::new().unwrap();
        let path = tmp.path();

        let msg = TeamsMessage {
            from: "agent".to_string(),
            text: "hello".to_string(),
            summary: "hello".to_string(),
            timestamp: "2026-02-09T12:00:00Z".to_string(),
            color: "green".to_string(),
            read: false,
        };

        append_to_inbox(path, msg.clone()).unwrap();

        let consumed = consume_unread_from_inbox(path).unwrap().unwrap();
        assert_eq!(consumed.text, "hello");
        assert_eq!(consumed.from, "agent");

        // Second poll should be None because it was marked as read
        let polled2 = consume_unread_from_inbox(path).unwrap();
        assert!(polled2.is_none());
    }

    #[test]
    fn test_parse_failure_propagation() {
        let tmp = NamedTempFile::new().unwrap();
        let path = tmp.path();
        std::fs::write(path, "invalid json").unwrap();

        let msg = TeamsMessage {
            from: "a".to_string(),
            text: "t".to_string(),
            summary: "s".to_string(),
            timestamp: "now".to_string(),
            color: "c".to_string(),
            read: false,
        };

        let result = append_to_inbox(path, msg);
        assert!(result.is_err());
    }
}