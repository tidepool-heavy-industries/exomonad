//! Claude Teams inbox writer.
//!
//! Writes messages to `~/.claude/teams/{team_name}/inboxes/{recipient}.json`.
//! Claude Code's native file watcher picks up inbox writes and injects them
//! as `<teammate-message>` into the LLM context.
//!
//! This replaces Zellij STDIN injection for Claude-to-Claude communication,
//! eliminating the Ink paste timing hack.

use serde::Serialize;
use std::path::PathBuf;
use tracing::{debug, info};

/// Message format for Claude Teams inbox.
#[derive(Debug, Serialize)]
pub struct TeamsMessage {
    #[serde(rename = "type")]
    pub message_type: String,
    pub recipient: String,
    pub content: String,
    pub summary: String,
}

/// Write a message to a Claude Teams inbox file.
///
/// Creates parent directories if needed. Writes atomically by writing to a
/// temp file and renaming.
pub fn write_to_inbox(
    team_name: &str,
    recipient: &str,
    message: &TeamsMessage,
) -> std::io::Result<()> {
    let home = dirs::home_dir().ok_or_else(|| {
        std::io::Error::new(std::io::ErrorKind::NotFound, "HOME directory not found")
    })?;

    let inbox_dir = home
        .join(".claude")
        .join("teams")
        .join(team_name)
        .join("inboxes");

    std::fs::create_dir_all(&inbox_dir)?;

    let inbox_file = inbox_dir.join(format!("{}.json", recipient));

    info!(
        team = %team_name,
        recipient = %recipient,
        file = %inbox_file.display(),
        "Writing to Teams inbox"
    );

    let json = serde_json::to_string_pretty(message).map_err(|e| {
        std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string())
    })?;

    // Atomic write: temp file + rename
    let tmp_file = inbox_dir.join(format!(".{}.json.tmp", recipient));
    std::fs::write(&tmp_file, &json)?;
    std::fs::rename(&tmp_file, &inbox_file)?;

    debug!(
        bytes = json.len(),
        "Teams inbox write complete"
    );

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
