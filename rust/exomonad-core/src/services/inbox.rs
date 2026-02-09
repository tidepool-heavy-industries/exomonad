use anyhow::{anyhow, Result};
use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::RwLock;

#[cfg(unix)]
use std::os::unix::io::{AsRawFd, RawFd};

/// Global lock to ensure thread-safety within the same process.
/// `fcntl` advisory locks are per-process, so we need this for multi-threaded access.
/// Use an `RwLock` so concurrent readers can proceed without blocking each other.
static INBOX_LOCK: RwLock<()> = RwLock::new(());

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct InboxMessage {
    pub from: String,
    pub text: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    pub timestamp: String, // ISO 8601
    #[serde(default = "default_color")]
    pub color: String,
    #[serde(default)]
    pub read: bool,
}

fn default_color() -> String {
    "blue".to_string()
}

/// Resolve `~/.claude/teams/{team}/inboxes/{agent}.json`.
pub fn inbox_path(team_name: &str, agent_name: &str) -> PathBuf {
    let mut path = dirs::home_dir().unwrap_or_else(|| PathBuf::from("."));
    path.push(".claude");
    path.push("teams");
    path.push(team_name);
    path.push("inboxes");
    path.push(format!("{}.json", agent_name));
    path
}

/// Helper to wrap file operations with fcntl advisory locking.
///
/// Uses `F_SETLKW` for blocking wait on lock acquisition.
/// Lock is released when the file is closed (or explicitly unlocked).
#[cfg(unix)]
fn with_lock<F, T>(fd: RawFd, exclusive: bool, f: F) -> Result<T>
where
    F: FnOnce() -> Result<T>,
{
    let lock_type = if exclusive {
        libc::F_WRLCK
    } else {
        libc::F_RDLCK
    };

    let mut fl: libc::flock = unsafe { std::mem::zeroed() };
    fl.l_type = lock_type as i16;
    fl.l_whence = libc::SEEK_SET as i16;
    fl.l_start = 0;
    fl.l_len = 0; // Lock entire file

    let res = unsafe { libc::fcntl(fd, libc::F_SETLKW, &fl) };
    if res == -1 {
        return Err(anyhow!(
            "Failed to lock file: {}",
            std::io::Error::last_os_error()
        ));
    }

    let result = f();

    // Unlock
    fl.l_type = libc::F_UNLCK as i16;
    unsafe { libc::fcntl(fd, libc::F_SETLK, &fl) };

    result
}

#[cfg(not(unix))]
fn with_lock<F, T>(_fd: i32, _exclusive: bool, f: F) -> Result<T>
where
    F: FnOnce() -> Result<T>,
{
    // On non-unix targets, we just execute the closure without fcntl locking.
    // In a real production environment, we'd implement Windows-specific locking.
    f()
}

/// Read all messages from the inbox. Return empty vec if file missing or empty.
pub fn read_inbox(path: &Path) -> Result<Vec<InboxMessage>> {
    let _thread_lock = INBOX_LOCK
        .read()
        .map_err(|e| anyhow!("RwLock poisoned: {}", e))?;

    let mut file = match File::open(path) {
        Ok(f) => f,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(e.into()),
    };

    #[cfg(unix)]
    let fd = file.as_raw_fd();
    #[cfg(not(unix))]
    let fd = 0;

    with_lock(fd, false, || {
        file.seek(SeekFrom::Start(0))?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;
        if content.trim().is_empty() {
            return Ok(Vec::new());
        }
        let messages: Vec<InboxMessage> = serde_json::from_str(&content)?;
        Ok(messages)
    })
}

/// Append a message to the inbox with an exclusive lock.
/// Creates the file with `[]` if missing.
pub fn append_message(path: &Path, message: &InboxMessage) -> Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let _thread_lock = INBOX_LOCK
        .write()
        .map_err(|e| anyhow!("RwLock poisoned: {}", e))?;

    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(path)?;

    #[cfg(unix)]
    let fd = file.as_raw_fd();
    #[cfg(not(unix))]
    let fd = 0;

    with_lock(fd, true, || {
        file.seek(SeekFrom::Start(0))?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;

        let mut messages: Vec<InboxMessage> = if content.trim().is_empty() {
            Vec::new()
        } else {
            serde_json::from_str(&content)?
        };

        messages.push(message.clone());

        let json = serde_json::to_string_pretty(&messages)?;
        file.set_len(0)?;
        file.seek(SeekFrom::Start(0))?;
        file.write_all(json.as_bytes())?;
        file.sync_all()?;
        Ok(())
    })
}

/// Read all messages with `read == false`.
pub fn read_unread(path: &Path) -> Result<Vec<InboxMessage>> {
    let messages = read_inbox(path)?;
    Ok(messages.into_iter().filter(|m| !m.read).collect())
}

/// Set `read = true` on all messages in the inbox.
pub fn mark_all_read(path: &Path) -> Result<()> {
    let _thread_lock = INBOX_LOCK
        .write()
        .map_err(|e| anyhow!("RwLock poisoned: {}", e))?;

    let mut file = match OpenOptions::new().read(true).write(true).open(path) {
        Ok(f) => f,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(e) => return Err(e.into()),
    };

    #[cfg(unix)]
    let fd = file.as_raw_fd();
    #[cfg(not(unix))]
    let fd = 0;

    with_lock(fd, true, || {
        file.seek(SeekFrom::Start(0))?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;

        if content.trim().is_empty() {
            return Ok(());
        }

        let mut messages: Vec<InboxMessage> = serde_json::from_str(&content)?;
        let mut changed = false;
        for m in &mut messages {
            if !m.read {
                m.read = true;
                changed = true;
            }
        }

        if changed {
            let json = serde_json::to_string_pretty(&messages)?;
            file.set_len(0)?;
            file.seek(SeekFrom::Start(0))?;
            file.write_all(json.as_bytes())?;
            file.sync_all()?;
        }
        Ok(())
    })
}

/// Helper to create an InboxMessage with current timestamp.
pub fn create_message(from: String, text: String, summary: Option<String>) -> InboxMessage {
    InboxMessage {
        from,
        text,
        summary,
        timestamp: Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
        color: default_color(),
        read: false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_inbox_path() {
        let path = inbox_path("myteam", "myagent");
        assert!(path.to_string_lossy().contains("myteam"));
        assert!(path.to_string_lossy().contains("myagent.json"));
    }

    #[test]
    fn test_read_empty_nonexistent() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("inbox.json");

        let messages = read_inbox(&path).unwrap();
        assert!(messages.is_empty());
    }

    #[test]
    fn test_append_and_read() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("inbox.json");

        let msg = create_message("me".to_string(), "hello".to_string(), None);
        append_message(&path, &msg).unwrap();

        let messages = read_inbox(&path).unwrap();
        assert_eq!(messages.len(), 1);
        assert_eq!(messages[0].from, "me");
        assert_eq!(messages[0].text, "hello");
        assert!(!messages[0].read);
    }

    #[test]
    fn test_read_unread_and_mark_read() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("inbox.json");

        let msg1 = create_message("me".to_string(), "msg1".to_string(), None);
        let mut msg2 = create_message("me".to_string(), "msg2".to_string(), None);
        msg2.read = true;

        append_message(&path, &msg1).unwrap();
        append_message(&path, &msg2).unwrap();

        let unread = read_unread(&path).unwrap();
        assert_eq!(unread.len(), 1);
        assert_eq!(unread[0].text, "msg1");

        mark_all_read(&path).unwrap();

        let unread_after = read_unread(&path).unwrap();
        assert!(unread_after.is_empty());

        let all = read_inbox(&path).unwrap();
        assert_eq!(all.len(), 2);
        assert!(all.iter().all(|m| m.read));
    }

    #[test]
    fn test_concurrent_appends() {
        use std::sync::Arc;
        use std::thread;

        let dir = tempdir().unwrap();
        let path = Arc::new(dir.path().join("inbox.json"));
        let num_threads = 10;
        let msgs_per_thread = 5;

        let mut handles = Vec::new();
        for t in 0..num_threads {
            let path = Arc::clone(&path);
            handles.push(thread::spawn(move || {
                for i in 0..msgs_per_thread {
                    let msg = create_message(
                        format!("thread-{}", t),
                        format!("msg-{}", i),
                        None,
                    );
                    append_message(&path, &msg).unwrap();
                }
            }));
        }

        for handle in handles {
            handle.join().unwrap();
        }

        let messages = read_inbox(&path).unwrap();
        assert_eq!(messages.len(), num_threads * msgs_per_thread);
    }

    #[test]
    fn test_json_roundtrip() {
        let msg = InboxMessage {
            from: "alice".to_string(),
            text: "hi".to_string(),
            summary: Some("greet".to_string()),
            timestamp: "2023-01-01T00:00:00Z".to_string(),
            color: "red".to_string(),
            read: false,
        };

        let json = serde_json::to_string(&msg).unwrap();
        let decoded: InboxMessage = serde_json::from_str(&json).unwrap();
        assert_eq!(msg, decoded);
    }
}
