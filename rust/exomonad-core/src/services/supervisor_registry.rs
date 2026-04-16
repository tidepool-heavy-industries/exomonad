//! Maps child birth-branches to their supervisor's identity.
//!
//! Populated by `session.register_supervisor` when a TL spawns children.
//! Queried by `notify_parent` to resolve the supervisor for routing.

use crate::domain::{AgentName, TeamName};
use crate::services::AgentResolver;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::info;

/// Supervisor identity for routing child → parent messages.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SupervisorEntry {
    pub supervisor: AgentName,
    pub team: TeamName,
}

/// Maps child birth-branches to their supervisor.
pub struct SupervisorRegistry {
    inner: Arc<Mutex<HashMap<String, SupervisorEntry>>>,
    resolver: Arc<AgentResolver>,
}

impl SupervisorRegistry {
    pub fn new(resolver: Arc<AgentResolver>) -> Self {
        Self {
            inner: Arc::new(Mutex::new(HashMap::new())),
            resolver,
        }
    }

    /// Register children as supervised by the given supervisor.
    pub async fn register(&self, children: &[String], info: SupervisorEntry) {
        {
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

        // Persist to disk via AgentResolver outside the lock
        for child in children {
            if let Ok(birth_branch) = crate::domain::BirthBranch::try_from(child.to_string()) {
                if let Some(record) = self.resolver.find_by_birth_branch(&birth_branch).await {
                    let _ = self
                        .resolver
                        .update_record(&record.agent_name, |r| r.supervisor = Some(info.clone()))
                        .await;
                }
            }
        }
    }

    /// Warm the registry without persisting to disk.
    pub async fn warm(&self, key: &str, info: SupervisorEntry) {
        let mut map = self.inner.lock().await;
        map.insert(key.to_string(), info);
    }

    /// Look up the supervisor for a given child birth-branch.
    pub async fn lookup(&self, birth_branch: &str) -> Option<SupervisorEntry> {
        {
            let map = self.inner.lock().await;
            if let Some(info) = map.get(birth_branch) {
                return Some(info.clone());
            }
        }

        // Fallback to disk via AgentResolver outside the lock
        if let Ok(bb) = crate::domain::BirthBranch::try_from(birth_branch.to_string()) {
            if let Some(record) = self.resolver.find_by_birth_branch(&bb).await {
                if let Some(supervisor) = record.supervisor {
                    let mut map = self.inner.lock().await;
                    map.insert(birth_branch.to_string(), supervisor.clone());
                    return Some(supervisor);
                }
            }
        }

        None
    }

    /// Remove children from the registry.
    pub async fn deregister(&self, children: &[String]) {
        {
            let mut map = self.inner.lock().await;
            for child in children {
                info!(child = %child, "Deregistering supervisor for child");
                map.remove(child);
            }
        }

        // Also remove from disk via AgentResolver outside the lock
        for child in children {
            if let Ok(bb) = crate::domain::BirthBranch::try_from(child.to_string()) {
                if let Some(record) = self.resolver.find_by_birth_branch(&bb).await {
                    let _ = self
                        .resolver
                        .update_record(&record.agent_name, |r| r.supervisor = None)
                        .await;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::AgentIdentityRecord;
    use tempfile::TempDir;

    async fn setup() -> (SupervisorRegistry, Arc<AgentResolver>, TempDir) {
        let tmp = TempDir::new().unwrap();
        let resolver = Arc::new(AgentResolver::load(tmp.path().to_path_buf()).await);
        let reg = SupervisorRegistry::new(resolver.clone());
        (reg, resolver, tmp)
    }

    fn test_info() -> SupervisorEntry {
        SupervisorEntry {
            supervisor: AgentName::from("tl-1"),
            team: TeamName::from("my-team"),
        }
    }

    #[tokio::test]
    async fn test_lookup_missing_returns_none() {
        let (reg, _, _) = setup().await;
        assert!(reg.lookup("nonexistent").await.is_none());
    }

    #[tokio::test]
    async fn test_register_then_lookup() {
        let (reg, _, _) = setup().await;
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
        let (reg, _, _) = setup().await;
        reg.register(&["main.child-1".into()], test_info()).await;
        assert!(reg.lookup("main.child-1").await.is_some());

        reg.deregister(&["main.child-1".into()]).await;
        assert!(reg.lookup("main.child-1").await.is_none());
    }

    #[tokio::test]
    async fn test_register_overwrites() {
        let (reg, _, _) = setup().await;
        reg.register(&["child".into()], test_info()).await;

        let new_info = SupervisorEntry {
            supervisor: AgentName::from("tl-2"),
            team: TeamName::from("other-team"),
        };
        reg.register(&["child".into()], new_info).await;

        let result = reg.lookup("child").await.unwrap();
        assert_eq!(result.supervisor.as_str(), "tl-2");
    }

    #[tokio::test]
    async fn test_fallback_to_disk() {
        let (reg, resolver, _) = setup().await;

        // Manually register an agent
        let branch = "main.feature-a";
        let record = AgentIdentityRecord {
            agent_name: AgentName::from("feature-a-claude"),
            slug: crate::domain::Slug::from("feature-a"),
            agent_type: crate::services::agent_control::AgentType::Claude,
            birth_branch: crate::domain::BirthBranch::from(branch),
            parent_branch: crate::domain::BirthBranch::from("main"),
            working_dir: std::path::PathBuf::from("."),
            display_name: "🤖 feature-a".to_string(),
            topology: crate::services::agent_control::Topology::WorktreePerAgent,
            claude_session_uuid: None,
            supervisor: None,
        };
        resolver.register(record).await.unwrap();

        let info = test_info();
        reg.register(&[branch.to_string()], info.clone()).await;

        // Clear in-memory cache
        {
            let mut map = reg.inner.lock().await;
            map.clear();
        }

        // Should fall back to disk
        let result = reg.lookup(branch).await;
        assert_eq!(result, Some(info));
    }

    #[tokio::test]
    async fn test_server_restart_simulation() {
        let tmp = TempDir::new().unwrap();
        let project_dir = tmp.path().to_path_buf();

        // Stage 1: Initial run
        let resolver = Arc::new(AgentResolver::load(project_dir.clone()).await);
        let reg = SupervisorRegistry::new(resolver.clone());

        let branch = "main.feature-a";
        let record = AgentIdentityRecord {
            agent_name: AgentName::from("feature-a-claude"),
            slug: crate::domain::Slug::from("feature-a"),
            agent_type: crate::services::agent_control::AgentType::Claude,
            birth_branch: crate::domain::BirthBranch::from(branch),
            parent_branch: crate::domain::BirthBranch::from("main"),
            working_dir: std::path::PathBuf::from("."),
            display_name: "🤖 feature-a".to_string(),
            topology: crate::services::agent_control::Topology::WorktreePerAgent,
            claude_session_uuid: None,
            supervisor: None,
        };
        resolver.register(record).await.unwrap();

        let info = test_info();
        reg.register(&[branch.to_string()], info.clone()).await;

        // Stage 2: Restart
        let resolver_restart = Arc::new(AgentResolver::load(project_dir.clone()).await);
        let reg_restart = SupervisorRegistry::new(resolver_restart.clone());

        let result = reg_restart.lookup(branch).await;
        assert_eq!(result, Some(info));
    }
}
