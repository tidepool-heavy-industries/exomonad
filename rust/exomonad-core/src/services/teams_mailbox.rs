//! Claude Teams inbox writer.
//!
//! Writes messages to `~/.claude/teams/{team_name}/inboxes/{recipient}.json`.
//! Auto-creates the inboxes/ directory if it doesn't exist (CC lazily initializes it).

use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use tracing::{debug, info};

/// Message format for Claude Teams inbox.
/// Full file is a JSON array of these messages: `[ {msg1}, {msg2}, ... ]`.
/// Format matches CC 2.1.39's native SendMessage output.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct TeamsMessage {
    pub from: String,
    pub text: String,
    pub summary: String,
    pub timestamp: String,
    pub read: bool,
}

/// Write a message to a Claude Teams inbox file.
///
/// Appends to the existing JSON array if the file exists, or creates a new array.
/// Auto-creates the inboxes/ directory (CC lazily initializes it on first SendMessage).
/// Writes atomically by writing to a temp file and renaming.
pub fn write_to_inbox(
    team_name: &str,
    recipient: &str,
    from: &str,
    text: &str,
    summary: &str,
) -> std::io::Result<()> {
    let home = dirs::home_dir().ok_or_else(|| {
        std::io::Error::new(std::io::ErrorKind::NotFound, "HOME directory not found")
    })?;

    write_to_inbox_at_base(&home, team_name, recipient, from, text, summary)
}

/// Write a message to a Claude Teams inbox file at a specific base path.
fn write_to_inbox_at_base(
    base: &Path,
    team_name: &str,
    recipient: &str,
    from: &str,
    text: &str,
    summary: &str,
) -> std::io::Result<()> {
    let inbox_dir = base
        .join(".claude")
        .join("teams")
        .join(team_name)
        .join("inboxes");

    if !inbox_dir.exists() {
        std::fs::create_dir_all(&inbox_dir)?;
        info!(dir = %inbox_dir.display(), "Created Teams inbox directory");
    }

    let inbox_file = inbox_dir.join(format!("{}.json", recipient));

    let mut messages: Vec<TeamsMessage> = if inbox_file.exists() {
        let content = std::fs::read_to_string(&inbox_file)?;
        serde_json::from_str(&content).unwrap_or_default()
    } else {
        Vec::new()
    };

    let timestamp = Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Millis, true);
    let new_message = TeamsMessage {
        from: from.to_string(),
        text: text.to_string(),
        summary: summary.to_string(),
        timestamp,
        read: false,
    };
    messages.push(new_message);

    info!(
        team = %team_name,
        recipient = %recipient,
        file = %inbox_file.display(),
        count = messages.len(),
        "Writing to Teams inbox"
    );

    let json = serde_json::to_string_pretty(&messages)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string()))?;

    // Atomic write: temp file + rename
    let tmp_file = inbox_dir.join(format!(".{}.json.tmp", recipient));
    std::fs::write(&tmp_file, &json)?;
    std::fs::rename(&tmp_file, &inbox_file)?;

    debug!(bytes = json.len(), "Teams inbox write complete");

    Ok(())
}

/// Get the Teams inbox file path for a given team and recipient.
pub fn inbox_path(team_name: &str, recipient: &str) -> Option<PathBuf> {
    dirs::home_dir().map(|home| {
        home.join(".claude")
            .join("teams")
            .join(team_name)
            .join("inboxes")
            .join(format!("{}.json", recipient))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_teams_mailbox_auto_creates_inbox_dir() {
        let tmp = tempdir().unwrap();
        let base = tmp.path();

        // Pre-create team dir but NOT inboxes/ (matches CC lazy init behavior)
        let team_dir = base.join(".claude").join("teams").join("test-team");
        std::fs::create_dir_all(&team_dir).unwrap();

        let result = write_to_inbox_at_base(base, "test-team", "lead", "agent", "msg", "sum");
        assert!(result.is_ok());

        assert!(team_dir.join("inboxes").exists());
        assert!(team_dir.join("inboxes").join("lead.json").exists());
    }

    #[test]
    fn test_teams_mailbox_append() -> std::io::Result<()> {
        let tmp = tempdir()?;
        let base = tmp.path();
        let team_name = "test-team";
        let recipient = "test-recipient";

        // Pre-create inbox directory
        let inbox_dir = base
            .join(".claude")
            .join("teams")
            .join(team_name)
            .join("inboxes");
        std::fs::create_dir_all(&inbox_dir)?;

        write_to_inbox_at_base(
            base,
            team_name,
            recipient,
            "agent1",
            "Hello from agent1",
            "Message 1",
        )?;
        write_to_inbox_at_base(
            base,
            team_name,
            recipient,
            "agent2",
            "Hello from agent2",
            "Message 2",
        )?;

        let inbox_file = inbox_dir.join(format!("{}.json", recipient));

        let content = std::fs::read_to_string(&inbox_file)?;
        let messages: Vec<TeamsMessage> = serde_json::from_str(&content).unwrap();

        assert_eq!(messages.len(), 2);
        assert_eq!(messages[0].from, "agent1");
        assert_eq!(messages[0].text, "Hello from agent1");
        assert!(!messages[0].read);
        assert_eq!(messages[1].from, "agent2");
        assert_eq!(messages[1].summary, "Message 2");

        Ok(())
    }
}
