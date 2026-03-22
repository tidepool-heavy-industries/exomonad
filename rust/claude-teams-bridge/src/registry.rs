//! In-memory registry mapping agent identity to Claude Teams info.
//!
//! Populated by the SessionStart hook (via `session.register_team` effect)
//! when a Claude agent creates its isolated team on startup.
//! Queried by `notify_parent` and GitHub poller to route messages via
//! Teams inbox instead of tmux STDIN injection.

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::info;

/// Info about an agent's Claude Teams membership.
#[derive(Debug, Clone)]
pub struct TeamInfo {
    pub team_name: String,
    pub inbox_name: String,
}

/// Maps agent identity keys to Claude Teams info.
pub struct TeamRegistry {
    inner: Arc<Mutex<HashMap<String, TeamInfo>>>,
}

impl Default for TeamRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl TeamRegistry {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub async fn register(&self, key: &str, info: TeamInfo) {
        info!(
            key = %key,
            team_name = %info.team_name,
            inbox_name = %info.inbox_name,
            "Registering Claude Teams info"
        );
        let mut map = self.inner.lock().await;
        map.insert(key.to_string(), info);
    }

    pub async fn get(&self, key: &str) -> Option<TeamInfo> {
        let map = self.inner.lock().await;
        map.get(key).cloned()
    }

    /// Get all entries for a given team name.
    pub async fn get_all_for_team(&self, team_name: &str) -> Vec<(String, TeamInfo)> {
        let map = self.inner.lock().await;
        map.iter()
            .filter(|(_, info)| info.team_name == team_name)
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }

    pub async fn deregister(&self, key: &str) {
        info!(key = %key, "Deregistering Claude Teams info");
        let mut map = self.inner.lock().await;
        map.remove(key);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_get_missing_returns_none() {
        let reg = TeamRegistry::new();
        assert!(reg.get("nonexistent").await.is_none());
    }

    #[tokio::test]
    async fn test_register_then_get() {
        let reg = TeamRegistry::new();
        reg.register(
            "root",
            TeamInfo {
                team_name: "exo-root".into(),
                inbox_name: "root-inbox".into(),
            },
        )
        .await;
        let result = reg.get("root").await.unwrap();
        assert_eq!(result.team_name, "exo-root");
        assert_eq!(result.inbox_name, "root-inbox");
    }

    #[tokio::test]
    async fn test_deregister() {
        let reg = TeamRegistry::new();
        reg.register(
            "root",
            TeamInfo {
                team_name: "exo-root".into(),
                inbox_name: "root-inbox".into(),
            },
        )
        .await;
        assert!(reg.get("root").await.is_some());
        reg.deregister("root").await;
        assert!(reg.get("root").await.is_none());
    }

    #[tokio::test]
    async fn test_register_overwrites() {
        let reg = TeamRegistry::new();
        reg.register(
            "root",
            TeamInfo {
                team_name: "team1".into(),
                inbox_name: "inbox1".into(),
            },
        )
        .await;
        reg.register(
            "root",
            TeamInfo {
                team_name: "team2".into(),
                inbox_name: "inbox2".into(),
            },
        )
        .await;
        let result = reg.get("root").await.unwrap();
        assert_eq!(result.team_name, "team2");
    }

    #[tokio::test]
    async fn test_get_all_for_team() {
        let reg = TeamRegistry::new();
        reg.register(
            "agent-1",
            TeamInfo {
                team_name: "team-a".into(),
                inbox_name: "inbox-1".into(),
            },
        )
        .await;
        reg.register(
            "agent-2",
            TeamInfo {
                team_name: "team-a".into(),
                inbox_name: "inbox-2".into(),
            },
        )
        .await;
        reg.register(
            "agent-3",
            TeamInfo {
                team_name: "team-b".into(),
                inbox_name: "inbox-3".into(),
            },
        )
        .await;

        let results = reg.get_all_for_team("team-a").await;
        assert_eq!(results.len(), 2);
        let keys: Vec<&str> = results.iter().map(|(k, _)| k.as_str()).collect();
        assert!(keys.contains(&"agent-1"));
        assert!(keys.contains(&"agent-2"));
    }

    #[tokio::test]
    async fn test_get_all_for_team_empty() {
        let reg = TeamRegistry::new();
        let results = reg.get_all_for_team("nonexistent").await;
        assert!(results.is_empty());
    }

    #[tokio::test]
    async fn test_default_same_as_new() {
        let reg = TeamRegistry::default();
        assert!(reg.get("any").await.is_none());
    }

    #[tokio::test]
    async fn test_multiple_keys_coexist() {
        let reg = TeamRegistry::new();
        reg.register(
            "root",
            TeamInfo {
                team_name: "root-team".into(),
                inbox_name: "root-inbox".into(),
            },
        )
        .await;
        reg.register(
            "agent",
            TeamInfo {
                team_name: "agent-team".into(),
                inbox_name: "agent-inbox".into(),
            },
        )
        .await;
        assert_eq!(reg.get("root").await.unwrap().team_name, "root-team");
        assert_eq!(reg.get("agent").await.unwrap().team_name, "agent-team");
    }
}
