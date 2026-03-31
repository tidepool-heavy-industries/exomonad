//! Canonical agent identity resolution.
//!
//! Provides a single source of truth for agent identity. Each agent's identity is
//! written as `identity.json` at spawn time with all derived fields pre-computed.
//! Consumers read from the resolver — no re-derivation, no suffix stripping, no fallbacks.

use crate::domain::{AgentName, BirthBranch, Slug};
use crate::services::agent_control::{AgentType, Topology};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

/// Pre-computed identity record written to `.exo/agents/{agent_name}/identity.json`.
///
/// All fields are determined at spawn time. Consumers never need to re-derive
/// slug from agent_name, or birth_branch from worktree git state, or display_name
/// from agent_type — it's all here.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AgentIdentityRecord {
    /// Suffixed internal name (e.g., "feature-a-claude").
    pub agent_name: AgentName,
    /// Bare slug without type suffix (e.g., "feature-a").
    pub slug: Slug,
    /// Agent type (Claude, Gemini, Shoal, Process).
    pub agent_type: AgentType,
    /// Git branch identity (e.g., "main.feature-a").
    pub birth_branch: BirthBranch,
    /// Parent's birth branch (e.g., "main"). Workers inherit parent's branch.
    pub parent_branch: BirthBranch,
    /// Relative working directory (e.g., ".exo/worktrees/feature-a/" or ".").
    pub working_dir: PathBuf,
    /// tmux display name (e.g., "🤖 feature-a").
    pub display_name: String,
    /// Workspace topology.
    pub topology: Topology,
}

const IDENTITY_FILENAME: &str = "identity.json";

/// Canonical agent identity service.
///
/// Loaded at server startup by scanning `.exo/agents/*/identity.json`.
/// Updated at spawn time (register) and cleanup time (deregister).
/// `get()` returns `None` if no record exists — callers decide whether that's
/// an error or triggers legacy resolution.
pub struct AgentResolver {
    records: RwLock<HashMap<AgentName, AgentIdentityRecord>>,
    project_dir: PathBuf,
}

impl AgentResolver {
    /// Construct an empty resolver (no agents loaded). Useful for tests.
    pub fn empty() -> Self {
        Self {
            records: RwLock::new(HashMap::new()),
            project_dir: PathBuf::from("."),
        }
    }

    /// Scan `.exo/agents/*/identity.json` and load all records into memory.
    pub async fn load(project_dir: PathBuf) -> Self {
        let mut records = HashMap::new();
        let agents_dir = project_dir.join(".exo/agents");

        if agents_dir.exists() {
            match tokio::fs::read_dir(&agents_dir).await {
                Ok(mut entries) => {
                    while let Ok(Some(entry)) = entries.next_entry().await {
                        let identity_path = entry.path().join(IDENTITY_FILENAME);
                        if identity_path.exists() {
                            match tokio::fs::read_to_string(&identity_path).await {
                                Ok(contents) => {
                                    match serde_json::from_str::<AgentIdentityRecord>(&contents) {
                                        Ok(record) => {
                                            debug!(
                                                agent = %record.agent_name,
                                                branch = %record.birth_branch,
                                                "Loaded agent identity"
                                            );
                                            records.insert(record.agent_name.clone(), record);
                                        }
                                        Err(e) => {
                                            warn!(
                                                path = %identity_path.display(),
                                                error = %e,
                                                "Failed to parse identity.json, skipping"
                                            );
                                        }
                                    }
                                }
                                Err(e) => {
                                    warn!(
                                        path = %identity_path.display(),
                                        error = %e,
                                        "Failed to read identity.json, skipping"
                                    );
                                }
                            }
                        }
                    }
                }
                Err(e) => {
                    warn!(
                        path = %agents_dir.display(),
                        error = %e,
                        "Failed to read agents directory"
                    );
                }
            }
        }

        let count = records.len();
        if count > 0 {
            info!(count, "AgentResolver loaded identity records");
        }

        Self {
            records: RwLock::new(records),
            project_dir,
        }
    }

    /// Look up an agent's identity by name. Returns `None` if not registered.
    pub async fn get(&self, name: &AgentName) -> Option<AgentIdentityRecord> {
        self.records.read().await.get(name).cloned()
    }

    /// Non-async access to the records map for synchronous callers (e.g., delivery).
    /// Returns a reference to the inner RwLock for `try_read()` usage.
    pub fn records_ref(&self) -> &RwLock<HashMap<AgentName, AgentIdentityRecord>> {
        &self.records
    }

    /// Register a new agent identity (writes to disk and memory).
    pub async fn register(&self, record: AgentIdentityRecord) -> anyhow::Result<()> {
        // Slug collision detection: warn if another active agent has the same slug
        // but different agent_name (potential working_dir collision for worktree agents).
        {
            let existing = self.records.read().await;
            for (existing_name, existing_record) in existing.iter() {
                if existing_record.slug == record.slug
                    && existing_name != &record.agent_name
                    && existing_record.topology == Topology::WorktreePerAgent
                    && record.topology == Topology::WorktreePerAgent
                {
                    warn!(
                        new_agent = %record.agent_name,
                        existing_agent = %existing_name,
                        slug = %record.slug,
                        "Slug collision detected: two worktree agents share the same slug"
                    );
                }
            }
        }

        // Write to disk
        let agent_dir = self
            .project_dir
            .join(".exo/agents")
            .join(record.agent_name.as_str());
        tokio::fs::create_dir_all(&agent_dir).await?;
        let identity_path = agent_dir.join(IDENTITY_FILENAME);
        let json = serde_json::to_string_pretty(&record)?;
        tokio::fs::write(&identity_path, &json).await?;

        info!(
            agent = %record.agent_name,
            branch = %record.birth_branch,
            slug = %record.slug,
            topology = ?record.topology,
            "Registered agent identity"
        );

        // Write to memory
        self.records
            .write()
            .await
            .insert(record.agent_name.clone(), record);

        Ok(())
    }

    /// Remove an agent's identity (removes from disk and memory).
    pub async fn deregister(&self, name: &AgentName) -> anyhow::Result<()> {
        // Remove from memory
        self.records.write().await.remove(name);

        // Remove from disk (just the identity.json, not the whole dir — cleanup handles that)
        let identity_path = self
            .project_dir
            .join(".exo/agents")
            .join(name.as_str())
            .join(IDENTITY_FILENAME);
        if identity_path.exists() {
            tokio::fs::remove_file(&identity_path).await?;
        }

        info!(agent = %name, "Deregistered agent identity");
        Ok(())
    }

    /// Get all registered agents.
    pub async fn all(&self) -> Vec<AgentIdentityRecord> {
        self.records.read().await.values().cloned().collect()
    }

    /// Resolve an agent's birth branch and working directory, trying in-memory
    /// records first, then probing disk (identity.json, git branch, .birth_branch file).
    ///
    /// `worktree_base` is typically `.exo/worktrees/` — where agent worktrees live.
    /// Returns `(birth_branch, working_dir)` on success.
    pub async fn resolve_or_probe(
        &self,
        worktree_base: &std::path::Path,
        agent_name: &str,
    ) -> anyhow::Result<(BirthBranch, PathBuf)> {
        let name = AgentName::from(agent_name);

        // 1. Try in-memory record (canonical, fastest)
        if let Some(record) = self.get(&name).await {
            debug!(agent = %agent_name, branch = %record.birth_branch, "Resolved from in-memory AgentResolver");
            return Ok((record.birth_branch, record.working_dir));
        }

        // 2. Try identity.json on disk (registered but not yet loaded)
        if let Some(exo_dir) = worktree_base.parent() {
            let identity_path = exo_dir
                .join("agents")
                .join(agent_name)
                .join("identity.json");
            if let Ok(contents) = tokio::fs::read_to_string(&identity_path).await {
                if let Ok(record) = serde_json::from_str::<AgentIdentityRecord>(&contents) {
                    debug!(agent = %agent_name, branch = %record.birth_branch, "Resolved from identity.json on disk");
                    return Ok((record.birth_branch, record.working_dir));
                }
            }
        }

        // 3. Try git branch from worktree directory.
        // New convention: worktree dir = agent_name (suffixed). Fallback: bare slug (legacy).
        let worktree_path = worktree_base.join(agent_name);
        let worktree_path = if worktree_path.exists() {
            worktree_path
        } else {
            let slug = agent_name
                .trim_end_matches("-claude")
                .trim_end_matches("-gemini")
                .trim_end_matches("-shoal")
                .trim_end_matches("-process");
            worktree_base.join(slug)
        };
        if let Ok(output) = tokio::process::Command::new("git")
            .args(["branch", "--show-current"])
            .current_dir(&worktree_path)
            .output()
            .await
        {
            if output.status.success() {
                let branch = String::from_utf8_lossy(&output.stdout).trim().to_string();
                if !branch.is_empty() {
                    debug!(agent = %agent_name, branch = %branch, "Resolved from git worktree branch");
                    let bb = BirthBranch::from(branch.as_str());
                    let wd = crate::services::agent_control::resolve_working_dir(bb.as_str());
                    return Ok((bb, wd));
                }
            }
        }

        // 4. Try .birth_branch file (legacy workers)
        if let Some(exo_dir) = worktree_base.parent() {
            let bb_file = exo_dir
                .join("agents")
                .join(agent_name)
                .join(".birth_branch");
            if let Ok(contents) = tokio::fs::read_to_string(&bb_file).await {
                let branch = contents.trim().to_string();
                debug!(agent = %agent_name, branch = %branch, "Resolved from .birth_branch file");
                let bb = BirthBranch::from(branch.as_str());
                let wd = crate::services::agent_control::resolve_working_dir(bb.as_str());
                return Ok((bb, wd));
            }
        }

        // 5. Error — no resolution path succeeded
        anyhow::bail!(
            "Failed to resolve birth branch for agent '{}' from resolver, identity.json, worktree, or .birth_branch",
            agent_name
        )
    }

    /// Check if a slug would collide with an existing worktree agent.
    pub async fn check_slug_collision(
        &self,
        slug: &Slug,
        exclude_name: Option<&AgentName>,
    ) -> Option<AgentName> {
        let records = self.records.read().await;
        for (name, record) in records.iter() {
            if &record.slug == slug
                && record.topology == Topology::WorktreePerAgent
                && exclude_name.map_or(true, |ex| ex != name)
            {
                return Some(name.clone());
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn test_record(name: &str, slug: &str, branch: &str) -> AgentIdentityRecord {
        AgentIdentityRecord {
            agent_name: AgentName::from(name),
            slug: Slug::from(slug),
            agent_type: AgentType::Claude,
            birth_branch: BirthBranch::from(branch),
            parent_branch: BirthBranch::from("main"),
            working_dir: PathBuf::from(format!(".exo/worktrees/{}/", name)),
            display_name: format!("🤖 {}", name),
            topology: Topology::WorktreePerAgent,
        }
    }

    fn worker_record(name: &str, slug: &str, parent_branch: &str) -> AgentIdentityRecord {
        AgentIdentityRecord {
            agent_name: AgentName::from(name),
            slug: Slug::from(slug),
            agent_type: AgentType::Gemini,
            birth_branch: BirthBranch::from(parent_branch),
            parent_branch: BirthBranch::from(parent_branch),
            working_dir: PathBuf::from("."),
            display_name: format!("💎 {}", name),
            topology: Topology::SharedDir,
        }
    }

    #[tokio::test]
    async fn register_and_get() {
        let tmp = TempDir::new().unwrap();
        let resolver = AgentResolver::load(tmp.path().to_path_buf()).await;

        let record = test_record("feature-a-claude", "feature-a", "main.feature-a-claude");
        resolver.register(record.clone()).await.unwrap();

        let got = resolver.get(&AgentName::from("feature-a-claude")).await;
        assert_eq!(got, Some(record));
    }

    #[tokio::test]
    async fn get_missing_returns_none() {
        let tmp = TempDir::new().unwrap();
        let resolver = AgentResolver::load(tmp.path().to_path_buf()).await;

        let got = resolver.get(&AgentName::from("nonexistent")).await;
        assert_eq!(got, None);
    }

    #[tokio::test]
    async fn deregister_removes_from_memory_and_disk() {
        let tmp = TempDir::new().unwrap();
        let resolver = AgentResolver::load(tmp.path().to_path_buf()).await;

        let record = test_record("feature-a-claude", "feature-a", "main.feature-a-claude");
        resolver.register(record).await.unwrap();

        let name = AgentName::from("feature-a-claude");
        resolver.deregister(&name).await.unwrap();

        assert_eq!(resolver.get(&name).await, None);

        // Verify file is gone
        let identity_path = tmp
            .path()
            .join(".exo/agents/feature-a-claude/identity.json");
        assert!(!identity_path.exists());
    }

    #[tokio::test]
    async fn load_from_disk() {
        let tmp = TempDir::new().unwrap();

        // Write an identity.json to disk manually
        let agent_dir = tmp.path().join(".exo/agents/feature-a-claude");
        tokio::fs::create_dir_all(&agent_dir).await.unwrap();
        let record = test_record("feature-a-claude", "feature-a", "main.feature-a-claude");
        let json = serde_json::to_string_pretty(&record).unwrap();
        tokio::fs::write(agent_dir.join("identity.json"), &json)
            .await
            .unwrap();

        // Load resolver — should find the record
        let resolver = AgentResolver::load(tmp.path().to_path_buf()).await;
        let got = resolver.get(&AgentName::from("feature-a-claude")).await;
        assert_eq!(got, Some(record));
    }

    #[tokio::test]
    async fn load_skips_malformed_json() {
        let tmp = TempDir::new().unwrap();

        let agent_dir = tmp.path().join(".exo/agents/bad-agent");
        tokio::fs::create_dir_all(&agent_dir).await.unwrap();
        tokio::fs::write(agent_dir.join("identity.json"), "not valid json")
            .await
            .unwrap();

        let resolver = AgentResolver::load(tmp.path().to_path_buf()).await;
        assert_eq!(resolver.all().await.len(), 0);
    }

    #[tokio::test]
    async fn load_skips_dirs_without_identity() {
        let tmp = TempDir::new().unwrap();

        // Agent dir with routing.json but no identity.json
        let agent_dir = tmp.path().join(".exo/agents/legacy-agent");
        tokio::fs::create_dir_all(&agent_dir).await.unwrap();
        tokio::fs::write(agent_dir.join("routing.json"), "{}")
            .await
            .unwrap();

        let resolver = AgentResolver::load(tmp.path().to_path_buf()).await;
        assert_eq!(resolver.all().await.len(), 0);
    }

    #[tokio::test]
    async fn slug_collision_detected_for_worktree_agents() {
        let tmp = TempDir::new().unwrap();
        let resolver = AgentResolver::load(tmp.path().to_path_buf()).await;

        let record1 = test_record("feature-a-claude", "feature-a", "main.feature-a-claude");
        resolver.register(record1).await.unwrap();

        // Same slug, different parent → collision for worktree agents
        let record2 = test_record("feature-a-gemini", "feature-a", "dev.feature-a-gemini");
        // register still succeeds but logs a warning
        resolver.register(record2).await.unwrap();

        // check_slug_collision should find the existing agent
        let collision = resolver
            .check_slug_collision(
                &Slug::from("feature-a"),
                Some(&AgentName::from("new-agent-claude")),
            )
            .await;
        assert!(collision.is_some());
    }

    #[tokio::test]
    async fn no_slug_collision_for_shared_dir_agents() {
        let tmp = TempDir::new().unwrap();
        let resolver = AgentResolver::load(tmp.path().to_path_buf()).await;

        let record = worker_record("worker-a-gemini", "worker-a", "main");
        resolver.register(record).await.unwrap();

        // SharedDir agents don't count as slug collisions
        let collision = resolver
            .check_slug_collision(
                &Slug::from("worker-a"),
                Some(&AgentName::from("other-agent")),
            )
            .await;
        assert!(collision.is_none());
    }

    #[tokio::test]
    async fn register_overwrites_existing() {
        let tmp = TempDir::new().unwrap();
        let resolver = AgentResolver::load(tmp.path().to_path_buf()).await;

        let record1 = test_record("feature-a-claude", "feature-a", "main.feature-a-claude");
        resolver.register(record1).await.unwrap();

        // Re-register with different branch (e.g., after respawn)
        let record2 = test_record("feature-a-claude", "feature-a", "dev.feature-a-claude");
        resolver.register(record2.clone()).await.unwrap();

        let got = resolver.get(&AgentName::from("feature-a-claude")).await;
        assert_eq!(got.unwrap().birth_branch, record2.birth_branch);
    }

    #[tokio::test]
    async fn all_returns_all_records() {
        let tmp = TempDir::new().unwrap();
        let resolver = AgentResolver::load(tmp.path().to_path_buf()).await;

        resolver
            .register(test_record(
                "feature-a-claude",
                "feature-a",
                "main.feature-a-claude",
            ))
            .await
            .unwrap();
        resolver
            .register(worker_record("worker-b-gemini", "worker-b", "main"))
            .await
            .unwrap();

        assert_eq!(resolver.all().await.len(), 2);
    }

    #[tokio::test]
    async fn identity_json_roundtrip() {
        let record = test_record("feature-a-claude", "feature-a", "main.feature-a-claude");
        let json = serde_json::to_string_pretty(&record).unwrap();
        let deserialized: AgentIdentityRecord = serde_json::from_str(&json).unwrap();
        assert_eq!(record, deserialized);
    }

    #[tokio::test]
    async fn worker_identity_json_roundtrip() {
        let record = worker_record("research-gemini", "research", "main.tl-branch");
        let json = serde_json::to_string_pretty(&record).unwrap();
        let deserialized: AgentIdentityRecord = serde_json::from_str(&json).unwrap();
        assert_eq!(record, deserialized);
    }

    #[tokio::test]
    async fn check_slug_collision_excludes_self() {
        let tmp = TempDir::new().unwrap();
        let resolver = AgentResolver::load(tmp.path().to_path_buf()).await;

        let record = test_record("feature-a-claude", "feature-a", "main.feature-a-claude");
        resolver.register(record).await.unwrap();

        // Checking against own name should not report collision
        let collision = resolver
            .check_slug_collision(
                &Slug::from("feature-a"),
                Some(&AgentName::from("feature-a-claude")),
            )
            .await;
        assert!(collision.is_none());
    }

    #[tokio::test]
    async fn deregister_nonexistent_is_ok() {
        let tmp = TempDir::new().unwrap();
        let resolver = AgentResolver::load(tmp.path().to_path_buf()).await;

        // Should not error
        resolver
            .deregister(&AgentName::from("nonexistent"))
            .await
            .unwrap();
    }
}
