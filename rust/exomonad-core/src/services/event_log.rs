//! Structured event logging service — JSONL append-only writer.
//!
//! Writes one JSON line per event to `.exo/events.jsonl` for post-hoc
//! reconstruction of agent runs. Query with DuckDB or jq.

use std::path::PathBuf;
use std::sync::Mutex;

/// Append-only JSONL event log.
///
/// All agents share one `exomonad serve` process, so there is only one writer.
/// The `Mutex` serializes concurrent tokio tasks. `O_APPEND` provides additional
/// safety at the OS level for writes <4KB.
pub struct EventLog {
    path: PathBuf,
    lock: Mutex<()>,
}

impl EventLog {
    /// Open (or create) the event log at `path`.
    pub fn open(path: PathBuf) -> std::io::Result<Self> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        Ok(Self {
            path,
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

        let _guard = self.lock.lock().unwrap_or_else(|e| e.into_inner());

        use std::io::Write;
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.path)?;
        writeln!(file, "{}", line)?;

        Ok(event_id)
    }

    /// Path to the JSONL file.
    pub fn path(&self) -> &std::path::Path {
        &self.path
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_append_and_read_back() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("events.jsonl");
        let log = EventLog::open(path.clone()).unwrap();

        let data = serde_json::json!({"slug": "feature-a", "agent_type": "claude"});
        let id = log.append("agent.spawned", "root", &data).unwrap();
        assert!(!id.is_empty());

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
        let path = dir.path().join("events.jsonl");
        let log = EventLog::open(path.clone()).unwrap();

        log.append("a", "x", &serde_json::json!({})).unwrap();
        log.append("b", "y", &serde_json::json!({})).unwrap();

        let content = std::fs::read_to_string(&path).unwrap();
        let lines: Vec<&str> = content.trim().lines().collect();
        assert_eq!(lines.len(), 2);
    }
}
