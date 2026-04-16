//! In-memory registry mapping agent identity to Claude Code session UUIDs.
//!
//! Populated by the SessionStart hook (via `session.register_claude_id` effect).
//! Queried by `spawn_subtree` to enable `--resume --fork-session` context inheritance.

use crate::domain::{AgentName, ClaudeSessionUuid};
use crate::services::AgentResolver;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::info;

/// Maps agent identity keys to Claude Code session UUIDs.
pub struct ClaudeSessionRegistry {
    inner: Arc<Mutex<HashMap<String, ClaudeSessionUuid>>>,
    resolver: Arc<AgentResolver>,
}

impl ClaudeSessionRegistry {
    pub fn new(resolver: Arc<AgentResolver>) -> Self {
        Self {
            inner: Arc::new(Mutex::new(HashMap::new())),
            resolver,
        }
    }

    /// Register a Claude session UUID for the given agent identity key.
    pub async fn register(&self, key: &str, claude_uuid: ClaudeSessionUuid) {
        info!(key = %key, claude_uuid = %claude_uuid, "Registering Claude session ID");
        {
            let mut map = self.inner.lock().await;
            map.insert(key.to_string(), claude_uuid.clone());
        }

        // Persist to disk via AgentResolver if it exists and the key is an AgentName.
        // We avoid persisting slug aliases to prevent redundant I/O.
        if let Ok(name) = AgentName::try_from(key.to_string()) {
            let _ = self
                .resolver
                .update_record(&name, |r| r.claude_session_uuid = Some(claude_uuid))
                .await;
        }
    }

    /// Warm the registry without persisting to disk.
    pub async fn warm(&self, key: &str, claude_uuid: ClaudeSessionUuid) {
        let mut map = self.inner.lock().await;
        map.insert(key.to_string(), claude_uuid);
    }

    /// Look up the Claude session UUID for the given agent identity key.
    pub async fn get(&self, key: &str) -> Option<ClaudeSessionUuid> {
        {
            let map = self.inner.lock().await;
            if let Some(uuid) = map.get(key) {
                return Some(uuid.clone());
            }
        }

        // Fallback to disk via AgentResolver
        let name = AgentName::from(key);
        if let Some(record) = self.resolver.get(&name).await {
            if let Some(uuid) = record.claude_session_uuid {
                let mut map = self.inner.lock().await;
                map.insert(key.to_string(), uuid.clone());
                return Some(uuid);
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::AgentIdentityRecord;
    use tempfile::TempDir;

    async fn setup() -> (ClaudeSessionRegistry, Arc<AgentResolver>, TempDir) {
        let tmp = TempDir::new().unwrap();
        let resolver = Arc::new(AgentResolver::load(tmp.path().to_path_buf()).await);
        let reg = ClaudeSessionRegistry::new(resolver.clone());
        (reg, resolver, tmp)
    }

    #[tokio::test]
    async fn test_get_missing_returns_none() {
        let (reg, _, _) = setup().await;
        assert!(reg.get("nonexistent").await.is_none());
    }

    #[tokio::test]
    async fn test_register_then_get() {
        let (reg, _, _) = setup().await;
        let uuid = ClaudeSessionUuid::from("uuid-123");
        reg.register("root", uuid.clone()).await;
        let result = reg.get("root").await;
        assert_eq!(result, Some(uuid));
    }

    #[tokio::test]
    async fn test_register_overwrites() {
        let (reg, _, _) = setup().await;
        reg.register("root", ClaudeSessionUuid::from("uuid-1"))
            .await;
        reg.register("root", ClaudeSessionUuid::from("uuid-2"))
            .await;
        let result = reg.get("root").await;
        assert_eq!(result, Some(ClaudeSessionUuid::from("uuid-2")));
    }

    #[tokio::test]
    async fn test_fallback_to_disk() {
        let (reg, resolver, _) = setup().await;

        // Manually register an agent so we have a record to update
        let name = AgentName::from("feature-a-claude");
        let record = AgentIdentityRecord {
            agent_name: name.clone(),
            slug: crate::domain::Slug::from("feature-a"),
            agent_type: crate::services::agent_control::AgentType::Claude,
            birth_branch: crate::domain::BirthBranch::from("main.feature-a"),
            parent_branch: crate::domain::BirthBranch::from("main"),
            working_dir: std::path::PathBuf::from("."),
            display_name: "🤖 feature-a".to_string(),
            topology: crate::services::agent_control::Topology::WorktreePerAgent,
            claude_session_uuid: None,
            supervisor: None,
        };
        resolver.register(record).await.unwrap();

        let uuid = ClaudeSessionUuid::from("uuid-disk");
        reg.register(name.as_str(), uuid.clone()).await;

        // Clear in-memory cache
        {
            let mut map = reg.inner.lock().await;
            map.clear();
        }

        // Should fall back to disk
        let result = reg.get(name.as_str()).await;
        assert_eq!(result, Some(uuid));
    }

    #[tokio::test]
    async fn test_server_restart_simulation() {
        let tmp = TempDir::new().unwrap();
        let project_dir = tmp.path().to_path_buf();

        // Stage 1: Initial run
        let resolver = Arc::new(AgentResolver::load(project_dir.clone()).await);
        let reg = ClaudeSessionRegistry::new(resolver.clone());

        let name = AgentName::from("feature-a-claude");
        let record = AgentIdentityRecord {
            agent_name: name.clone(),
            slug: crate::domain::Slug::from("feature-a"),
            agent_type: crate::services::agent_control::AgentType::Claude,
            birth_branch: crate::domain::BirthBranch::from("main.feature-a"),
            parent_branch: crate::domain::BirthBranch::from("main"),
            working_dir: std::path::PathBuf::from("."),
            display_name: "🤖 feature-a".to_string(),
            topology: crate::services::agent_control::Topology::WorktreePerAgent,
            claude_session_uuid: None,
            supervisor: None,
        };
        resolver.register(record).await.unwrap();

        let uuid = ClaudeSessionUuid::from("uuid-persisted");
        reg.register(name.as_str(), uuid.clone()).await;

        // Stage 2: Restart (new objects, same disk)
        let resolver_restart = Arc::new(AgentResolver::load(project_dir.clone()).await);
        let reg_restart = ClaudeSessionRegistry::new(resolver_restart.clone());

        // The record should be loaded by AgentResolver from disk
        let result = reg_restart.get(name.as_str()).await;
        assert_eq!(result, Some(uuid));
    }
}
