//! In-memory registry mapping agent identity to Claude Code session UUIDs.
//!
//! Populated by the SessionStart hook (via `session.register_claude_id` effect).
//! Queried by `spawn_subtree` to enable `--resume --fork-session` context inheritance.

use crate::domain::ClaudeSessionUuid;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::info;

/// Maps agent identity keys to Claude Code session UUIDs.
pub struct ClaudeSessionRegistry {
    inner: Arc<Mutex<HashMap<String, ClaudeSessionUuid>>>,
}

impl ClaudeSessionRegistry {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Register a Claude session UUID for the given agent identity key.
    pub async fn register(&self, key: &str, claude_uuid: ClaudeSessionUuid) {
        info!(key = %key, claude_uuid = %claude_uuid, "Registering Claude session ID");
        let mut map = self.inner.lock().await;
        map.insert(key.to_string(), claude_uuid);
    }

    /// Look up the Claude session UUID for the given agent identity key.
    pub async fn get(&self, key: &str) -> Option<ClaudeSessionUuid> {
        let map = self.inner.lock().await;
        map.get(key).cloned()
    }
}
