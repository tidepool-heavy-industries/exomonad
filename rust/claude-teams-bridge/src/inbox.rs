use crate::paths;
use crate::file_lock::{FileLock, fsync_dir};
use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::io;
use std::path::Path;
use std::time::Duration;
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

    let _lock = FileLock::acquire(&inbox_file, Duration::from_secs(30))?;

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
    let thread_id = format!("{:?}", std::thread::current().id());
    let tmp_file = inbox_dir.join(format!(".{}.{}.{}.json.tmp", recipient, Utc::now().timestamp_nanos_opt().unwrap_or(0), thread_id));
    std::fs::write(&tmp_file, &json)?;
    std::fs::rename(&tmp_file, &inbox_file)?;

    if let Err(e) = fsync_dir(&inbox_dir) {
        debug!(error = %e, "fsync on inbox dir failed");
    }

    debug!(bytes = json.len(), "Teams inbox write complete");

    let msg_count = messages.len();
    drop(_lock);

    // Trigger compaction when inbox grows large
    if msg_count > 1000 {
        if let Err(e) = compact_inbox_at_base(base, team, recipient) {
            debug!(error = %e, "Inbox compaction failed (non-fatal)");
        }
    }

    Ok(timestamp)
}

/// Compact an inbox file to keep it at a manageable size.
pub fn compact_inbox(team: &str, recipient: &str) -> io::Result<()> {
    let home = dirs::home_dir()
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "HOME directory not found"))?;
    compact_inbox_at_base(&home, team, recipient)
}

pub(crate) fn compact_inbox_at_base(base: &Path, team: &str, recipient: &str) -> io::Result<()> {
    let inbox_dir = paths::inbox_dir_at(base, team);
    let inbox_file = inbox_dir.join(format!("{}.json", recipient));
    if !inbox_file.exists() {
        return Ok(());
    }

    let _lock = FileLock::acquire(&inbox_file, Duration::from_secs(30))?;

    let content = std::fs::read_to_string(&inbox_file)?;
    let messages: Vec<TeamsMessage> = serde_json::from_str(&content).unwrap_or_default();

    let mut compacted = Vec::new();
    // Always keep unread messages
    let (unread, read): (Vec<_>, Vec<_>) = messages.into_iter().partition(|m| !m.read);
    compacted.extend(unread);

    // For read messages: keep last 500
    let read_len = read.len();
    let read_to_keep: Vec<TeamsMessage> = if read_len > 500 {
        read.into_iter().skip(read_len - 500).collect()
    } else {
        read
    };

    // For idle notifications among kept read messages: keep only last 10 per sender
    let (idle, non_idle): (Vec<_>, Vec<_>) = read_to_keep.into_iter().partition(is_idle_notification);
    compacted.extend(non_idle);

    // Group idle by sender, keep last 10 each
    let mut idle_by_sender: std::collections::HashMap<String, Vec<TeamsMessage>> = std::collections::HashMap::new();
    for msg in idle {
        idle_by_sender.entry(msg.from.clone()).or_default().push(msg);
    }
    for (_sender, mut msgs) in idle_by_sender {
        let len = msgs.len();
        if len > 10 {
            msgs.drain(..len - 10);
        }
        compacted.extend(msgs);
    }

    // Sort by timestamp to maintain order
    compacted.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));

    let json = serde_json::to_string_pretty(&compacted)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;
    let tmp_file = inbox_dir.join(format!(".{}.compact.json.tmp", recipient));
    std::fs::write(&tmp_file, &json)?;
    std::fs::rename(&tmp_file, &inbox_file)?;
    if let Err(e) = fsync_dir(&inbox_dir) {
        debug!(error = %e, "fsync on inbox dir failed during compaction");
    }

    info!(team = %team, recipient = %recipient, before = read_len, after = compacted.len(), "Inbox compacted");
    Ok(())
}

fn is_idle_notification(msg: &TeamsMessage) -> bool {
    msg.summary.to_lowercase().contains("idle")
        || msg.text.to_lowercase().contains("idle")
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
            debug!(
                team,
                recipient,
                timestamp,
                total_messages = messages.len(),
                "is_message_read: message not found"
            );
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

    #[test]
    fn test_concurrent_writes_no_lost_messages() {
        use std::sync::Arc;
        let tmp = tempdir().unwrap();
        let base = Arc::new(tmp.path().to_path_buf());
        let team = "test-team";
        let recipient = "lead";

        // Pre-create the directory to avoid races in create_dir_all
        let inbox_dir = paths::inbox_dir_at(&base, team);
        std::fs::create_dir_all(&inbox_dir).unwrap();

        let mut handles = vec![];
        for i in 0..10 {
            let base = Arc::clone(&base);
            let handle = std::thread::spawn(move || {
                write_to_inbox_at_base(
                    &base,
                    team,
                    recipient,
                    &format!("agent-{}", i),
                    &format!("message-{}", i),
                    "summary",
                )
            });
            handles.push(handle);
            std::thread::sleep(Duration::from_millis(20));
        }

        for handle in handles {
            handle.join().unwrap().unwrap();
        }

        let inbox_file = inbox_dir.join(format!("{}.json", recipient));
        let content = std::fs::read_to_string(&inbox_file).unwrap();
        let messages: Vec<TeamsMessage> = serde_json::from_str(&content).unwrap();

        assert_eq!(messages.len(), 10);
        for i in 0..10 {
            let msg_exists = messages.iter().any(|m| m.text == format!("message-{}", i));
            assert!(msg_exists, "Message {} missing", i);
        }
    }

    #[test]
    fn test_compact_inbox_preserves_unread() {
        let tmp = tempdir().unwrap();
        let base = tmp.path();
        let team = "t";
        let recipient = "r";
        let inbox_dir = paths::inbox_dir_at(base, team);
        std::fs::create_dir_all(&inbox_dir).unwrap();

        // Create 5 unread + 5 read messages
        let mut messages = Vec::new();
        for i in 0..10 {
            messages.push(TeamsMessage {
                from: "agent".to_string(),
                text: format!("msg{}", i),
                summary: "sum".to_string(),
                timestamp: format!("2026-03-24T12:00:0{:02}Z", i),
                read: i >= 5,
            });
        }
        let inbox_file = inbox_dir.join(format!("{}.json", recipient));
        std::fs::write(&inbox_file, serde_json::to_string(&messages).unwrap()).unwrap();

        compact_inbox_at_base(base, team, recipient).unwrap();

        let content = std::fs::read_to_string(&inbox_file).unwrap();
        let compacted: Vec<TeamsMessage> = serde_json::from_str(&content).unwrap();
        assert_eq!(compacted.len(), 10);
    }

    #[test]
    fn test_compact_inbox_trims_read_messages() {
        let tmp = tempdir().unwrap();
        let base = tmp.path();
        let team = "t";
        let recipient = "r";
        let inbox_dir = paths::inbox_dir_at(base, team);
        std::fs::create_dir_all(&inbox_dir).unwrap();

        // Create 600 read messages
        let mut messages = Vec::new();
        for i in 0..600 {
            messages.push(TeamsMessage {
                from: "agent".to_string(),
                text: format!("msg{}", i),
                summary: "sum".to_string(),
                timestamp: format!("2026-03-24T12:00:0{:03}Z", i),
                read: true,
            });
        }
        let inbox_file = inbox_dir.join(format!("{}.json", recipient));
        std::fs::write(&inbox_file, serde_json::to_string(&messages).unwrap()).unwrap();

        compact_inbox_at_base(base, team, recipient).unwrap();

        let content = std::fs::read_to_string(&inbox_file).unwrap();
        let compacted: Vec<TeamsMessage> = serde_json::from_str(&content).unwrap();
        assert_eq!(compacted.len(), 500);
        assert_eq!(compacted[0].text, "msg100");
    }

    #[test]
    fn test_compact_inbox_deduplicates_idle() {
        let tmp = tempdir().unwrap();
        let base = tmp.path();
        let team = "t";
        let recipient = "r";
        let inbox_dir = paths::inbox_dir_at(base, team);
        std::fs::create_dir_all(&inbox_dir).unwrap();

        // Create 20 read idle messages from same sender
        let mut messages = Vec::new();
        for i in 0..20 {
            messages.push(TeamsMessage {
                from: "agent".to_string(),
                text: format!("idle{}", i),
                summary: "idle".to_string(),
                timestamp: format!("2026-03-24T12:00:0{:02}Z", i),
                read: true,
            });
        }
        let inbox_file = inbox_dir.join(format!("{}.json", recipient));
        std::fs::write(&inbox_file, serde_json::to_string(&messages).unwrap()).unwrap();

        compact_inbox_at_base(base, team, recipient).unwrap();

        let content = std::fs::read_to_string(&inbox_file).unwrap();
        let compacted: Vec<TeamsMessage> = serde_json::from_str(&content).unwrap();
        assert_eq!(compacted.len(), 10);
        assert_eq!(compacted[0].text, "idle10");
    }
}
