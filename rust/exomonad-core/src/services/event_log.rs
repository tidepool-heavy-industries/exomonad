//! Structured event logging service — JSONL append-only writer.
//!
//! Writes one JSON line per event to `.exo/events.jsonl` for post-hoc
//! reconstruction of agent runs. Query with DuckDB or jq.

use std::path::{Path, PathBuf};
use std::sync::Mutex;

/// Append-only JSONL event log.
///
/// All agents share one `exomonad serve` process, so there is only one writer.
/// The `Mutex` serializes concurrent tokio tasks. `O_APPEND` provides additional
/// safety at the OS level for writes <4KB.
pub struct EventLog {
    dir: PathBuf,
    lock: Mutex<()>,
}

impl EventLog {
    /// Open (or create) the event log directory at `dir`.
    pub fn open(dir: PathBuf) -> std::io::Result<Self> {
        std::fs::create_dir_all(&dir)?;
        Ok(Self {
            dir,
            lock: Mutex::new(()),
        })
    }

    /// Append a structured event. Returns the generated event ID.
    pub fn append(
        &self,
        event_type: &str,
        agent_id: &str,
        data: &serde_json::Value,
    ) -> std::io::Result<String> {
        let event_id = uuid::Uuid::new_v4().to_string();
        let ts = chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Millis, true);

        let line = serde_json::json!({
            "ts": ts,
            "id": event_id,
            "type": event_type,
            "agent_id": agent_id,
            "data": data,
        });

        let sanitized_id = agent_id.replace(['/', '\\', '\0'], "_");
        let path = self.dir.join(format!("{}.jsonl", sanitized_id));

        let _guard = self.lock.lock().unwrap_or_else(|e| e.into_inner());

        use std::io::Write;
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&path)?;
        writeln!(file, "{}", line)?;

        Ok(event_id)
    }

    /// Path to the log directory.
    pub fn dir(&self) -> &Path {
        &self.dir
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_append_and_read_back() {
        let dir = tempfile::tempdir().unwrap();
        let log = EventLog::open(dir.path().to_path_buf()).unwrap();

        let data = serde_json::json!({"slug": "feature-a", "agent_type": "claude"});
        let id = log.append("agent.spawned", "root", &data).unwrap();
        assert!(!id.is_empty());

        let path = dir.path().join("root.jsonl");
        let content = std::fs::read_to_string(&path).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(content.trim()).unwrap();
        assert_eq!(parsed["type"], "agent.spawned");
        assert_eq!(parsed["agent_id"], "root");
        assert_eq!(parsed["data"]["slug"], "feature-a");
        assert!(parsed["ts"].as_str().unwrap().contains("T"));
        assert_eq!(parsed["id"], id);
    }

    #[test]
    fn test_multiple_appends() {
        let dir = tempfile::tempdir().unwrap();
        let log = EventLog::open(dir.path().to_path_buf()).unwrap();

        log.append("a", "agent-x", &serde_json::json!({})).unwrap();
        log.append("b", "agent-y", &serde_json::json!({})).unwrap();

        let path_x = dir.path().join("agent-x.jsonl");
        let path_y = dir.path().join("agent-y.jsonl");

        assert!(path_x.exists());
        assert!(path_y.exists());

        let content_x = std::fs::read_to_string(&path_x).unwrap();
        let content_y = std::fs::read_to_string(&path_y).unwrap();

        assert_eq!(content_x.trim().lines().count(), 1);
        assert_eq!(content_y.trim().lines().count(), 1);
    }

    #[test]
    fn test_agent_id_sanitization() {
        let dir = tempfile::tempdir().unwrap();
        let log = EventLog::open(dir.path().to_path_buf()).unwrap();

        log.append("a", "feature/bug", &serde_json::json!({}))
            .unwrap();
        let path = dir.path().join("feature_bug.jsonl");
        assert!(path.exists());
    }
}
