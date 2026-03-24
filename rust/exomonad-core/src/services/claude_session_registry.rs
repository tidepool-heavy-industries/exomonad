//! In-memory registry mapping agent identity to Claude Code session UUIDs.
//!
//! Populated by the SessionStart hook (via `session.register_claude_id` effect).
//! Queried by `spawn_subtree` to enable `--resume --fork-session` context inheritance.

use crate::domain::{BirthBranch, ClaudeSessionUuid};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::info;

/// Maps agent identity keys to Claude Code session UUIDs.
pub struct ClaudeSessionRegistry {
    inner: Arc<Mutex<HashMap<BirthBranch, ClaudeSessionUuid>>>,
}

impl Default for ClaudeSessionRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl ClaudeSessionRegistry {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Register a Claude session UUID for the given agent identity key.
    pub async fn register(&self, key: BirthBranch, claude_uuid: ClaudeSessionUuid) {
        info!(key = %key, claude_uuid = %claude_uuid, "Registering Claude session ID");
        let mut map = self.inner.lock().await;
        map.insert(key, claude_uuid);
    }

    /// Look up the Claude session UUID for the given agent identity key.
    pub async fn get(&self, key: &BirthBranch) -> Option<ClaudeSessionUuid> {
        let map = self.inner.lock().await;
        map.get(key).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_get_missing_returns_none() {
        let reg = ClaudeSessionRegistry::new();
        assert!(reg.get(&BirthBranch::from("nonexistent")).await.is_none());
    }

    #[tokio::test]
    async fn test_register_then_get() {
        let reg = ClaudeSessionRegistry::new();
        let uuid = ClaudeSessionUuid::from("uuid-123");
        reg.register(BirthBranch::from("root"), uuid.clone()).await;
        let result = reg.get(&BirthBranch::from("root")).await;
        assert_eq!(result, Some(uuid));
    }

    #[tokio::test]
    async fn test_register_overwrites() {
        let reg = ClaudeSessionRegistry::new();
        reg.register(BirthBranch::from("root"), ClaudeSessionUuid::from("uuid-1"))
            .await;
        reg.register(BirthBranch::from("root"), ClaudeSessionUuid::from("uuid-2"))
            .await;
        let result = reg.get(&BirthBranch::from("root")).await;
        assert_eq!(result, Some(ClaudeSessionUuid::from("uuid-2")));
    }

    #[tokio::test]
    async fn test_default_same_as_new() {
        let reg = ClaudeSessionRegistry::default();
        assert!(reg.get(&BirthBranch::from("any")).await.is_none());
    }
}
