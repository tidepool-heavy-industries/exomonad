use crate::paths;
use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::io;
use std::path::Path;
use tracing::{debug, info};

/// Message in a Claude Code Teams inbox file.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct TeamsMessage {
    pub from: String,
    pub text: String,
    pub summary: String,
    pub timestamp: String,
    pub read: bool,
}

/// Write a message to a Claude Teams inbox file.
/// Returns the timestamp of the written message for delivery verification.
pub fn write_to_inbox(
    team: &str,
    recipient: &str,
    from: &str,
    text: &str,
    summary: &str,
) -> io::Result<String> {
    let home = dirs::home_dir()
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "HOME directory not found"))?;
    write_to_inbox_at_base(&home, team, recipient, from, text, summary)
}

pub(crate) fn write_to_inbox_at_base(
    base: &Path,
    team: &str,
    recipient: &str,
    from: &str,
    text: &str,
    summary: &str,
) -> io::Result<String> {
    let inbox_dir = paths::inbox_dir_at(base, team);

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
    messages.push(TeamsMessage {
        from: from.to_string(),
        text: text.to_string(),
        summary: summary.to_string(),
        timestamp: timestamp.clone(),
        read: false,
    });

    info!(
        team = %team,
        recipient = %recipient,
        file = %inbox_file.display(),
        count = messages.len(),
        "Writing to Teams inbox"
    );

    let json = serde_json::to_string_pretty(&messages)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    // Atomic write: temp file + rename
    let tmp_file = inbox_dir.join(format!(".{}.json.tmp", recipient));
    std::fs::write(&tmp_file, &json)?;
    std::fs::rename(&tmp_file, &inbox_file)?;

    debug!(bytes = json.len(), "Teams inbox write complete");
    Ok(timestamp)
}

/// Read all messages from an inbox.
pub fn read_inbox(team: &str, recipient: &str) -> io::Result<Vec<TeamsMessage>> {
    let path = paths::inbox_path(team, recipient)
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "HOME directory not found"))?;
    if !path.exists() {
        return Ok(Vec::new());
    }
    let content = std::fs::read_to_string(&path)?;
    serde_json::from_str(&content)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))
}

/// Check if a specific message (by timestamp) was read by CC.
pub fn is_message_read(team: &str, recipient: &str, timestamp: &str) -> bool {
    let Some(path) = paths::inbox_path(team, recipient) else {
        debug!(team, recipient, "is_message_read: no inbox path");
        return false;
    };
    let Ok(content) = std::fs::read_to_string(&path) else {
        debug!(team, recipient, path = %path.display(), "is_message_read: cannot read inbox file");
        return false;
    };
    let Ok(messages) = serde_json::from_str::<Vec<TeamsMessage>>(&content) else {
        debug!(team, recipient, "is_message_read: cannot parse inbox JSON");
        return false;
    };
    match messages.iter().find(|m| m.timestamp == timestamp) {
        Some(m) => m.read,
        None => {
            debug!(team, recipient, timestamp, total_messages = messages.len(), "is_message_read: message not found");
            false
        }
    }
}

/// Get unread messages from an inbox.
pub fn unread_messages(team: &str, recipient: &str) -> io::Result<Vec<TeamsMessage>> {
    read_inbox(team, recipient).map(|msgs| msgs.into_iter().filter(|m| !m.read).collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_write_creates_inbox_dir() {
        let tmp = tempdir().unwrap();
        let base = tmp.path();
        let team_dir = base.join(".claude").join("teams").join("test-team");
        std::fs::create_dir_all(&team_dir).unwrap();

        let result = write_to_inbox_at_base(base, "test-team", "lead", "agent", "msg", "sum");
        assert!(result.is_ok());
        assert!(team_dir.join("inboxes").exists());
        assert!(team_dir.join("inboxes").join("lead.json").exists());
    }

    #[test]
    fn test_write_and_read() {
        let tmp = tempdir().unwrap();
        let base = tmp.path();
        let inbox_dir = paths::inbox_dir_at(base, "test-team");
        std::fs::create_dir_all(&inbox_dir).unwrap();

        write_to_inbox_at_base(base, "test-team", "r", "a1", "Hello", "sum1").unwrap();
        write_to_inbox_at_base(base, "test-team", "r", "a2", "World", "sum2").unwrap();

        let inbox_file = inbox_dir.join("r.json");
        let content = std::fs::read_to_string(&inbox_file).unwrap();
        let messages: Vec<TeamsMessage> = serde_json::from_str(&content).unwrap();

        assert_eq!(messages.len(), 2);
        assert_eq!(messages[0].from, "a1");
        assert_eq!(messages[0].text, "Hello");
        assert!(!messages[0].read);
        assert_eq!(messages[1].from, "a2");
        assert_eq!(messages[1].summary, "sum2");
    }

    #[test]
    fn test_unread_filters_read_messages() {
        let tmp = tempdir().unwrap();
        let base = tmp.path();
        let inbox_dir = paths::inbox_dir_at(base, "t");
        std::fs::create_dir_all(&inbox_dir).unwrap();

        // Write two messages, mark one as read manually
        write_to_inbox_at_base(base, "t", "r", "a", "msg1", "s").unwrap();
        write_to_inbox_at_base(base, "t", "r", "a", "msg2", "s").unwrap();

        let inbox_file = inbox_dir.join("r.json");
        let content = std::fs::read_to_string(&inbox_file).unwrap();
        let mut messages: Vec<TeamsMessage> = serde_json::from_str(&content).unwrap();
        messages[0].read = true;
        std::fs::write(&inbox_file, serde_json::to_string(&messages).unwrap()).unwrap();

        // read_inbox and unread_messages need real HOME, so test via file directly
        let content2 = std::fs::read_to_string(&inbox_file).unwrap();
        let all: Vec<TeamsMessage> = serde_json::from_str(&content2).unwrap();
        let unread: Vec<&TeamsMessage> = all.iter().filter(|m| !m.read).collect();
        assert_eq!(all.len(), 2);
        assert_eq!(unread.len(), 1);
        assert_eq!(unread[0].text, "msg2");
    }
}
