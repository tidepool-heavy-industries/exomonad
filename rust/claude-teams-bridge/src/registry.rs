//! Two-tier registry mapping agent identity to Claude Teams info.
//!
//! **Tier 1 (in-memory):** Populated by the SessionStart hook (via `session.register_team`
//! effect) when an exomonad agent creates its team on startup.
//!
//! **Tier 2 (config.json fallback):** On Tier 1 miss, scans the sender's team's
//! `~/.claude/teams/{team}/config.json` on disk. This finds CC-native teammates
//! (spawned via Claude Code's Task/SendMessage) that never run exomonad MCP.
//!
//! The blessed entry point is `TeamRegistry::resolve()`, which tries both tiers.
//! `TeamRegistry::get()` is Tier 1 only (for callers that don't need disk fallback).
//! `TeamRegistry::resolve_from_config()` is Tier 2 only (synchronous, for callers
//! that already know the team name).

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::{debug, info};

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
        let team_name = info.team_name.clone();
        info!(
            key = %key,
            team_name = %team_name,
            inbox_name = %info.inbox_name,
            "Registering Claude Teams info"
        );
        let mut map = self.inner.lock().await;
        let previous = map.insert(key.to_string(), info);
        let old_team_name = previous
            .and_then(|prev| (prev.team_name != team_name).then_some(prev.team_name));
        drop(map);

        // Best-effort sync to disk for backward-compatible API
        if let Err(e) = self.persist_config(&team_name).await {
            debug!(team = %team_name, error = %e, "Failed to sync team config to disk (non-fatal)");
        }

        if let Some(old_team) = old_team_name {
            if let Err(e) = self.persist_config(&old_team).await {
                debug!(team = %old_team, error = %e, "Failed to sync previous team config after move (non-fatal)");
            }
        }
    }

    /// Explicitly register a member and sync to disk. Returns IO error if persistence fails.
    pub async fn register_member(&self, key: &str, info: TeamInfo) -> std::io::Result<()> {
        let team_name = info.team_name.clone();
        let mut map = self.inner.lock().await;
        let previous = map.insert(key.to_string(), info);
        let old_team_name = previous
            .and_then(|prev| (prev.team_name != team_name).then_some(prev.team_name));
        drop(map);

        self.persist_config(&team_name).await?;

        if let Some(old_team) = old_team_name {
            self.persist_config(&old_team).await?;
        }
        Ok(())
    }

    pub async fn get(&self, key: &str) -> Option<TeamInfo> {
        let map = self.inner.lock().await;
        map.get(key).cloned()
    }

    /// Get all entries for a given team name, sorted by key for deterministic ordering.
    pub async fn get_all_for_team(&self, team_name: &str) -> Vec<(String, TeamInfo)> {
        let map = self.inner.lock().await;
        let mut entries: Vec<(String, TeamInfo)> = map
            .iter()
            .filter(|(_, info)| info.team_name == team_name)
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        entries.sort_by(|(a, _), (b, _)| a.cmp(b));
        entries
    }

    /// Resolve the team lead's name for a given team.
    ///
    /// Reads `config.json` to find `leadAgentId`, then resolves that UUID to
    /// a member name. Falls back to the first in-memory entry if config.json
    /// is unavailable or the lead is not a member.
    pub async fn resolve_lead(&self, team_name: &str) -> Option<String> {
        // Primary: read config.json's leadAgentId and map to member name
        if let Ok(config) = crate::config::read_team_config(team_name) {
            if let Some(member) = config
                .members
                .iter()
                .find(|m| m.agent_id == config.lead_agent_id)
            {
                debug!(
                    team = %team_name,
                    lead_agent_id = %config.lead_agent_id,
                    lead_name = %member.name,
                    "Resolved team lead from config.json"
                );
                return Some(member.name.clone());
            }
            // leadAgentId didn't match any member — try member name directly
            // (exomonad sets leadAgentId to the member name, not UUID)
            if config
                .members
                .iter()
                .any(|m| m.name == config.lead_agent_id)
            {
                debug!(
                    team = %team_name,
                    lead_name = %config.lead_agent_id,
                    "Resolved team lead from config.json (name match)"
                );
                return Some(config.lead_agent_id);
            }
        }
        // Fallback: first in-memory entry
        let entries = self.get_all_for_team(team_name).await;
        entries.first().map(|(k, _)| k.clone())
    }

    /// Batch lookup: get team info for two keys in a single lock acquisition.
    pub async fn get_pair(&self, key1: &str, key2: &str) -> (Option<TeamInfo>, Option<TeamInfo>) {
        let map = self.inner.lock().await;
        (map.get(key1).cloned(), map.get(key2).cloned())
    }

    /// Two-tier lookup: in-memory first, then config.json scan scoped by sender's team.
    ///
    /// Tier 1: Check the in-memory registry (exomonad agents that called `register_team`).
    /// Tier 2: On miss, scan the sender's team's `config.json` on disk (CC-native agents
    /// that never run exomonad MCP). The sender's team scopes the search to disambiguate
    /// agents with the same name across different teams.
    pub async fn resolve(
        &self,
        recipient: &str,
        sender_team_hint: Option<&str>,
    ) -> Option<TeamInfo> {
        // Tier 1: in-memory (exomonad agents)
        if let Some(info) = self.get(recipient).await {
            return Some(info);
        }
        // Tier 2: disk scan scoped to sender's team (CC-native agents)
        if let Some(team_name) = sender_team_hint {
            return Self::resolve_from_config(team_name, recipient);
        }
        None
    }

    /// Scan a team's config.json for a member by name (synchronous, no async).
    ///
    /// Reads `~/.claude/teams/{team_name}/config.json` and searches for a member
    /// matching `recipient`. Returns `TeamInfo` with the team name and member name
    /// as inbox key. Useful for callers that already know the team name and want
    /// to skip the in-memory check.
    pub fn resolve_from_config(team_name: &str, recipient: &str) -> Option<TeamInfo> {
        let config = crate::config::read_team_config(team_name).ok()?;
        let member = config.members.iter().find(|m| m.name == recipient)?;
        debug!(
            team = %team_name,
            recipient = %recipient,
            "Resolved agent from config.json (Tier 2)"
        );
        Some(TeamInfo {
            team_name: team_name.to_string(),
            inbox_name: member.name.clone(),
        })
    }

    pub async fn deregister(&self, key: &str) {
        info!(key = %key, "Deregistering Claude Teams info");
        let mut map = self.inner.lock().await;
        let team_name = map.get(key).map(|info| info.team_name.clone());
        map.remove(key);
        drop(map);

        if let Some(team) = team_name {
            // Best-effort sync to disk for backward-compatible API
            if let Err(e) = self.persist_config(&team).await {
                debug!(team = %team, error = %e, "Failed to sync team config to disk after deregister (non-fatal)");
            }
        }
    }

    /// Explicitly deregister a member and sync to disk. Returns IO error if persistence fails.
    pub async fn remove_member(&self, key: &str) -> std::io::Result<()> {
        let mut map = self.inner.lock().await;
        let team_name = map.get(key).map(|info| info.team_name.clone());
        map.remove(key);
        drop(map);

        if let Some(team) = team_name {
            self.persist_config(&team).await?;
        }
        Ok(())
    }

    /// Synchronize in-memory registry state for a team with its on-disk config.json.
    ///
    /// CC-native members (those without backendType: "exomonad") are preserved as-is.
    /// Exomonad members are updated or added based on the current in-memory registry.
    /// Deduplication by inbox_name applies only to exomonad/in-memory entries written
    /// by this bridge; existing CC-native members already present on disk are not
    /// deduplicated or otherwise validated here.
    async fn persist_config(&self, team_name: &str) -> std::io::Result<()> {
        let path = crate::paths::config_path(team_name).ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::NotFound, "HOME directory not found")
        })?;

        // Acquire advisory lock to prevent read-modify-write races
        let _lock = crate::file_lock::FileLock::acquire(&path, std::time::Duration::from_secs(10))?;

        let mut config = match crate::config::read_team_config(team_name) {
            Ok(c) => c,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                // If config doesn't exist, we can't persist to it.
                // Claude Code owns the team lifecycle; we only augment existing teams.
                return Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("team config.json not found for team '{team_name}'"),
                ));
            }
            Err(e) => return Err(e),
        };

        let in_memory = self.get_all_for_team(team_name).await;
        if in_memory.is_empty()
            && config
                .members
                .iter()
                .all(|m| m.backend_type.as_deref() != Some("exomonad"))
        {
            // Optimization: nothing to do if no in-memory entries and no exomonad members on disk
            return Ok(());
        }

        // 1. Collect CC-native members to keep
        let mut new_members: Vec<crate::config::TeamMember> = config
            .members
            .into_iter()
            .filter(|m| m.backend_type.as_deref() != Some("exomonad"))
            .collect();

        // 2. Add/update in-memory members (deduplicated by inbox_name)
        let mut unique_in_memory = HashMap::new();
        for (key, info) in in_memory {
            // Prefer the key that matches the inbox_name if available (usually the agent name)
            if !unique_in_memory.contains_key(&info.inbox_name) || key == info.inbox_name {
                unique_in_memory.insert(info.inbox_name.clone(), key);
            }
        }

        for (inbox_name, key) in unique_in_memory {
            let existing = new_members.iter().find(|m| m.name == inbox_name);
            if existing.is_none() {
                new_members.push(crate::config::TeamMember {
                    agent_id: key,
                    name: inbox_name,
                    agent_type: "exomonad-agent".into(),
                    model: "gemini".into(),
                    joined_at: chrono::Utc::now().timestamp() as u64,
                    cwd: std::env::current_dir()
                        .unwrap_or_default()
                        .to_string_lossy()
                        .to_string(),
                    backend_type: Some("exomonad".into()),
                });
            }
        }

        config.members = new_members;
        crate::config::write_team_config(team_name, &config)
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
        assert_eq!(
            keys,
            vec!["agent-1", "agent-2"],
            "entries must be sorted by key"
        );
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

    #[tokio::test]
    async fn test_get_pair_single_lock() {
        let reg = TeamRegistry::new();
        reg.register(
            "agent-a",
            TeamInfo {
                team_name: "team".into(),
                inbox_name: "a".into(),
            },
        )
        .await;
        let (a, b) = reg.get_pair("agent-a", "missing").await;
        assert!(a.is_some());
        assert!(b.is_none());
    }

    #[tokio::test]
    async fn test_resolve_lead_from_config() {
        let team_name = "test-resolve-lead";
        let config = crate::config::TeamConfig {
            name: team_name.into(),
            description: "test".into(),
            created_at: 0,
            lead_agent_id: "uuid-lead".into(),
            lead_session_id: "session".into(),
            members: vec![
                crate::config::TeamMember {
                    agent_id: "uuid-lead".into(),
                    name: "team-lead".into(),
                    agent_type: "claude".into(),
                    model: "opus".into(),
                    joined_at: 0,
                    cwd: "/tmp".into(),
                    backend_type: None,
                },
                crate::config::TeamMember {
                    agent_id: "uuid-worker".into(),
                    name: "alpha-worker".into(),
                    agent_type: "claude".into(),
                    model: "haiku".into(),
                    joined_at: 0,
                    cwd: "/tmp".into(),
                    backend_type: None,
                },
            ],
        };
        crate::config::write_team_config(team_name, &config).unwrap();

        let reg = TeamRegistry::new();
        // Without config.json, alphabetical "alpha-worker" would have been picked.
        // With resolve_lead, "team-lead" is picked via leadAgentId.
        let lead = reg.resolve_lead(team_name).await;
        assert_eq!(lead, Some("team-lead".to_string()));

        // Cleanup
        if let Some(dir) = crate::paths::teams_base_dir() {
            let _ = std::fs::remove_dir_all(dir.join(team_name));
        }
    }

    #[tokio::test]
    async fn test_resolve_lead_fallback_to_in_memory() {
        let reg = TeamRegistry::new();
        reg.register(
            "beta",
            TeamInfo {
                team_name: "ephemeral-team".into(),
                inbox_name: "beta".into(),
            },
        )
        .await;
        // No config.json on disk, falls back to in-memory first entry
        let lead = reg.resolve_lead("ephemeral-team").await;
        assert_eq!(lead, Some("beta".to_string()));
    }

    #[tokio::test]
    async fn test_resolve_tier1_in_memory() {
        let reg = TeamRegistry::new();
        reg.register(
            "agent-1",
            TeamInfo {
                team_name: "my-team".into(),
                inbox_name: "agent-1".into(),
            },
        )
        .await;
        // Tier 1 hit — no sender hint needed
        let result = reg.resolve("agent-1", None).await.unwrap();
        assert_eq!(result.team_name, "my-team");
        assert_eq!(result.inbox_name, "agent-1");
    }

    #[tokio::test]
    async fn test_resolve_no_hint_no_memory_returns_none() {
        let reg = TeamRegistry::new();
        // Not in memory, no sender hint → None
        assert!(reg.resolve("unknown-agent", None).await.is_none());
    }

    #[tokio::test]
    async fn test_resolve_tier1_takes_priority_over_hint() {
        let reg = TeamRegistry::new();
        reg.register(
            "agent-1",
            TeamInfo {
                team_name: "in-memory-team".into(),
                inbox_name: "agent-1".into(),
            },
        )
        .await;
        // Even with a sender hint, Tier 1 (in-memory) wins
        let result = reg.resolve("agent-1", Some("other-team")).await.unwrap();
        assert_eq!(result.team_name, "in-memory-team");
    }

    #[tokio::test]
    async fn test_resolve_from_config_nonexistent_team() {
        // Team doesn't exist on disk → None
        assert!(TeamRegistry::resolve_from_config("nonexistent-team-xyz", "agent").is_none());
    }

    #[tokio::test]
    async fn test_resolve_tier2_from_config() {
        // Create a real team config on disk
        let team_name = "test-resolve-tier2";
        let config = crate::config::TeamConfig {
            name: team_name.into(),
            description: "test".into(),
            created_at: 0,
            lead_agent_id: "lead".into(),
            lead_session_id: "session".into(),
            members: vec![crate::config::TeamMember {
                agent_id: "uuid-123".into(),
                name: "supervisor".into(),
                agent_type: "claude".into(),
                model: "opus".into(),
                joined_at: 0,
                cwd: "/tmp".into(),
                backend_type: None,
            }],
        };
        crate::config::write_team_config(team_name, &config).unwrap();

        // Resolve via Tier 2
        let result = TeamRegistry::resolve_from_config(team_name, "supervisor").unwrap();
        assert_eq!(result.team_name, team_name);
        assert_eq!(result.inbox_name, "supervisor");

        // Non-member returns None
        assert!(TeamRegistry::resolve_from_config(team_name, "nonexistent").is_none());

        // Cleanup
        if let Some(dir) = crate::paths::teams_base_dir() {
            let _ = std::fs::remove_dir_all(dir.join(team_name));
        }
    }

    #[tokio::test]
    async fn test_resolve_tier2_scoped_by_sender_team() {
        // Create two teams with same member name
        let team_a = "test-resolve-scope-a";
        let team_b = "test-resolve-scope-b";
        for (team, cwd) in [(team_a, "/tmp/a"), (team_b, "/tmp/b")] {
            let config = crate::config::TeamConfig {
                name: team.into(),
                description: "test".into(),
                created_at: 0,
                lead_agent_id: "lead".into(),
                lead_session_id: "session".into(),
                members: vec![crate::config::TeamMember {
                    agent_id: format!("uuid-{}", team),
                    name: "team-lead".into(),
                    agent_type: "claude".into(),
                    model: "opus".into(),
                    joined_at: 0,
                    cwd: cwd.into(),
                    backend_type: None,
                }],
            };
            crate::config::write_team_config(team, &config).unwrap();
        }

        let reg = TeamRegistry::new();
        // Sender is in team_a → resolve "team-lead" picks team_a's config
        reg.register(
            "sender",
            TeamInfo {
                team_name: team_a.into(),
                inbox_name: "sender".into(),
            },
        )
        .await;

        let result = reg.resolve("team-lead", Some(team_a)).await.unwrap();
        assert_eq!(result.team_name, team_a);

        let result_b = reg.resolve("team-lead", Some(team_b)).await.unwrap();
        assert_eq!(result_b.team_name, team_b);

        // Cleanup
        if let Some(dir) = crate::paths::teams_base_dir() {
            let _ = std::fs::remove_dir_all(dir.join(team_a));
            let _ = std::fs::remove_dir_all(dir.join(team_b));
        }
    }
}
