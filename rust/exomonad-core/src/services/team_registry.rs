//! In-memory registry mapping agent identity to Claude Teams info.
//!
//! Populated by the SessionStart hook (via `session.register_team` effect)
//! when a Claude agent creates its isolated team on startup.
//! Queried by `notify_parent` and GitHub poller to route messages via
//! Teams inbox instead of Zellij STDIN injection.

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::info;

/// Info about an agent's Claude Teams membership.
#[derive(Debug, Clone)]
pub struct TeamInfo {
    /// Team name this agent created (e.g., "exo-root").
    pub team_name: String,
    /// Agent's inbox name within its team (used as filename).
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

    /// Register a team for the given agent identity key.
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

    /// Look up the team info for the given agent identity key.
    pub async fn get(&self, key: &str) -> Option<TeamInfo> {
        let map = self.inner.lock().await;
        map.get(key).cloned()
    }
}
