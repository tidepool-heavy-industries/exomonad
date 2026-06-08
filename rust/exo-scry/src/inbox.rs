//! Read and write the Claude Teams inbox substrate.
//!
//! Each member's inbox is `~/.claude/teams/{team}/inboxes/{member}.json` — a
//! JSON array of messages. Appending here is exactly how a message becomes a
//! `<teammate-message>`: Claude Code's InboxPoller watches these files and
//! delivers new entries into the member's conversation.
//!
//! Writes are guarded by an exclusive lock file and committed via temp-then-
//! rename so a concurrent reader never sees a half-written array. This is
//! best-effort cross-process coordination, adequate for a standalone tool.

use crate::error::{Result, ScryError};
use crate::teams;
use serde::{Deserialize, Serialize};
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::time::Duration;

/// One inbox message, matching Claude Code's on-disk shape.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InboxMessage {
    pub from: String,
    pub text: String,
    #[serde(default)]
    pub summary: String,
    #[serde(default)]
    pub timestamp: String,
    #[serde(default)]
    pub read: bool,
}

fn inbox_path(team: &str, member: &str) -> Result<PathBuf> {
    Ok(teams::teams_root()?
        .join(team)
        .join("inboxes")
        .join(format!("{member}.json")))
}

/// Read a member's inbox (empty if the inbox file doesn't exist yet).
pub fn read_inbox(team: &str, member: &str) -> Result<Vec<InboxMessage>> {
    let path = inbox_path(team, member)?;
    match std::fs::read(&path) {
        Ok(bytes) => {
            serde_json::from_slice(&bytes).map_err(|source| ScryError::Json { path, source })
        }
        Err(e) if e.kind() == ErrorKind::NotFound => Ok(Vec::new()),
        Err(e) => Err(ScryError::Io(e)),
    }
}

/// Append a message to a member's inbox, stamping it now (RFC3339, UTC, millis)
/// and marking it unread — the form Claude Code's InboxPoller expects.
pub fn send_message(
    team: &str,
    to: &str,
    from: &str,
    text: &str,
    summary: &str,
) -> Result<InboxMessage> {
    let path = inbox_path(team, to)?;
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let msg = InboxMessage {
        from: from.to_string(),
        text: text.to_string(),
        summary: summary.to_string(),
        timestamp: chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Millis, true),
        read: false,
    };

    let _lock = FileLock::acquire(&path)?;
    let mut msgs = read_inbox(team, to)?;
    msgs.push(msg.clone());
    let bytes = serde_json::to_vec_pretty(&msgs).map_err(|source| ScryError::Json {
        path: path.clone(),
        source,
    })?;
    let tmp = path.with_extension("json.tmp");
    std::fs::write(&tmp, bytes)?;
    std::fs::rename(&tmp, &path)?;
    Ok(msg)
}

/// An exclusive lock held for the lifetime of a write, via an `O_EXCL` lock file
/// next to the target. Bounded spin so a stale lock can't wedge us forever.
struct FileLock(PathBuf);

impl FileLock {
    fn acquire(target: &Path) -> Result<Self> {
        let lock = PathBuf::from(format!("{}.lock", target.display()));
        for _ in 0..50 {
            match std::fs::OpenOptions::new()
                .write(true)
                .create_new(true)
                .open(&lock)
            {
                Ok(_) => return Ok(FileLock(lock)),
                Err(e) if e.kind() == ErrorKind::AlreadyExists => {
                    std::thread::sleep(Duration::from_millis(20));
                }
                Err(e) => return Err(ScryError::Io(e)),
            }
        }
        Err(ScryError::Io(std::io::Error::new(
            ErrorKind::WouldBlock,
            "inbox lock contended",
        )))
    }
}

impl Drop for FileLock {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.0);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inbox_message_serde_roundtrip() {
        let msg = InboxMessage {
            from: "alice".into(),
            text: "hello".into(),
            summary: "greeting".into(),
            timestamp: "2023-01-01T00:00:00Z".into(),
            read: true,
        };
        let json = serde_json::to_string(&msg).unwrap();
        let back: InboxMessage = serde_json::from_str(&json).unwrap();
        assert_eq!(back.from, msg.from);
        assert_eq!(back.text, msg.text);
        assert_eq!(back.summary, msg.summary);
        assert_eq!(back.timestamp, msg.timestamp);
        assert_eq!(back.read, msg.read);
    }

    #[test]
    fn inbox_message_defaults_on_missing_fields() {
        let json = r#"{"from":"a","text":"hi"}"#;
        let msg: InboxMessage = serde_json::from_str(json).unwrap();
        assert_eq!(msg.from, "a");
        assert_eq!(msg.text, "hi");
        assert_eq!(msg.summary, "");
        assert_eq!(msg.timestamp, "");
        assert_eq!(msg.read, false);
    }

    #[test]
    fn inbox_roundtrip_under_temp_home() {
        let temp = std::env::temp_dir().join(format!("exo-scry-inbox-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&temp);
        std::fs::create_dir_all(&temp).unwrap();

        // Save original HOME to restore later
        let old_home = std::env::var_os("HOME");
        std::env::set_var("HOME", &temp);

        // Case 3: read_inbox missing file is empty
        let msgs = read_inbox("team1", "member1").unwrap();
        assert!(msgs.is_empty());

        // Case 4: send_message then read_inbox
        let sent = send_message("team1", "member1", "from-me", "hello world", "sum").unwrap();
        assert_eq!(sent.from, "from-me");
        assert_eq!(sent.text, "hello world");
        assert_eq!(sent.summary, "sum");
        assert!(!sent.timestamp.is_empty());
        assert!(!sent.read);

        let msgs = read_inbox("team1", "member1").unwrap();
        assert_eq!(msgs.len(), 1);
        assert_eq!(msgs[0].from, "from-me");
        assert_eq!(msgs[0].text, "hello world");
        assert_eq!(msgs[0].summary, "sum");
        assert!(!msgs[0].timestamp.is_empty());
        assert!(!msgs[0].read);

        // Second message (append)
        send_message("team1", "member1", "someone", "else", "another").unwrap();
        let msgs = read_inbox("team1", "member1").unwrap();
        assert_eq!(msgs.len(), 2);
        assert_eq!(msgs[1].from, "someone");

        // Restore HOME
        if let Some(h) = old_home {
            std::env::set_var("HOME", h);
        } else {
            std::env::remove_var("HOME");
        }

        std::fs::remove_dir_all(&temp).unwrap();
    }
}
