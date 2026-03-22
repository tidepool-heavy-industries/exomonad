//! Maps child birth-branches to their supervisor's identity.
//!
//! Populated by `session.register_supervisor` when a TL spawns children.
//! Queried by `notify_parent` to resolve the supervisor for routing.

use crate::domain::{AgentName, TeamName};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::info;

/// Supervisor identity for routing child → parent messages.
#[derive(Debug, Clone)]
pub struct SupervisorInfo {
    pub supervisor: AgentName,
    pub team: TeamName,
}

/// Maps child birth-branches to their supervisor.
pub struct SupervisorRegistry {
    inner: Arc<Mutex<HashMap<String, SupervisorInfo>>>,
}

impl Default for SupervisorRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl SupervisorRegistry {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Register children as supervised by the given supervisor.
    pub async fn register(&self, children: &[String], info: SupervisorInfo) {
        let mut map = self.inner.lock().await;
        for child in children {
            info!(
                child = %child,
                supervisor = %info.supervisor,
                team = %info.team,
                "Registering supervisor for child"
            );
            map.insert(child.clone(), info.clone());
        }
    }

    /// Look up the supervisor for a given child birth-branch.
    pub async fn lookup(&self, birth_branch: &str) -> Option<SupervisorInfo> {
        let map = self.inner.lock().await;
        map.get(birth_branch).cloned()
    }

    /// Remove children from the registry.
    pub async fn deregister(&self, children: &[String]) {
        let mut map = self.inner.lock().await;
        for child in children {
            info!(child = %child, "Deregistering supervisor for child");
            map.remove(child);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_info() -> SupervisorInfo {
        SupervisorInfo {
            supervisor: AgentName::from("tl-1"),
            team: TeamName::from("my-team"),
        }
    }

    #[tokio::test]
    async fn test_lookup_missing_returns_none() {
        let reg = SupervisorRegistry::new();
        assert!(reg.lookup("nonexistent").await.is_none());
    }

    #[tokio::test]
    async fn test_register_then_lookup() {
        let reg = SupervisorRegistry::new();
        let info = test_info();
        reg.register(&["main.child-1".into(), "main.child-2".into()], info)
            .await;

        let result = reg.lookup("main.child-1").await.unwrap();
        assert_eq!(result.supervisor.as_str(), "tl-1");
        assert_eq!(result.team.as_str(), "my-team");

        let result2 = reg.lookup("main.child-2").await.unwrap();
        assert_eq!(result2.supervisor.as_str(), "tl-1");
    }

    #[tokio::test]
    async fn test_deregister() {
        let reg = SupervisorRegistry::new();
        reg.register(&["main.child-1".into()], test_info()).await;
        assert!(reg.lookup("main.child-1").await.is_some());

        reg.deregister(&["main.child-1".into()]).await;
        assert!(reg.lookup("main.child-1").await.is_none());
    }

    #[tokio::test]
    async fn test_register_overwrites() {
        let reg = SupervisorRegistry::new();
        reg.register(&["child".into()], test_info()).await;

        let new_info = SupervisorInfo {
            supervisor: AgentName::from("tl-2"),
            team: TeamName::from("other-team"),
        };
        reg.register(&["child".into()], new_info).await;

        let result = reg.lookup("child").await.unwrap();
        assert_eq!(result.supervisor.as_str(), "tl-2");
    }

    #[tokio::test]
    async fn test_default() {
        let reg = SupervisorRegistry::default();
        assert!(reg.lookup("any").await.is_none());
    }
}
