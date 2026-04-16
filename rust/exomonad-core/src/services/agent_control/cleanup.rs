use super::*;

/// Statistics for stale agent garbage collection.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct GcStats {
    /// Number of directories scanned in .exo/agents/
    pub scanned: usize,
    /// Number of directories pruned because their tmux window/pane is gone
    pub pruned_dead_tmux: usize,
    /// Number of orphan directories (no routing.json) pruned because they are old
    pub pruned_orphan: usize,
    /// Number of live agent directories kept
    pub kept_live: usize,
    /// Number of recent orphan directories (likely in-flight spawn) kept
    pub kept_recent: usize,
}

impl<
        C: super::super::HasGitHubClient
            + super::super::HasAcpRegistry
            + super::super::HasTeamRegistry
            + super::super::HasAgentResolver
            + super::super::HasProjectDir
            + super::super::HasGitWorktreeService
            + super::super::HasTmuxIpc
            + 'static,
    > AgentControlService<C>
{
    /// Garbage collect stale agent directories in `.exo/agents/`.
    ///
    /// Scans all subdirectories, checks liveness via tmux (from routing.json),
    /// and prunes dead or orphan directories.
    pub async fn gc_stale_agents(&self) -> Result<GcStats> {
        let mut stats = GcStats::default();
        let agents_dir = self.ctx.project_dir().join(".exo/agents");

        if !agents_dir.exists() {
            info!(path = %agents_dir.display(), "Agents directory does not exist, skipping GC");
            return Ok(stats);
        }

        let mut dir_entries = fs::read_dir(&agents_dir).await?;
        while let Some(entry) = dir_entries.next_entry().await? {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }

            stats.scanned += 1;

            match RoutingInfo::read_from_dir(&path).await {
                Ok(routing) => {
                    let is_live = if let Some(wid) = &routing.window_id {
                        match self.tmux() {
                            Ok(tmux) => match tmux.window_exists(wid).await {
                                Ok(exists) => exists,
                                Err(e) => {
                                    warn!(path = %path.display(), error = %e, "Tmux liveness check failed, skipping agent");
                                    true
                                }
                            }
                            Err(_) => {
                                warn!(path = %path.display(), "Tmux session unavailable, skipping liveness check for agent");
                                stats.kept_live += 1;
                                continue;
                            }
                        }
                    } else if let Some(pid) = &routing.pane_id {
                        match self.tmux() {
                            Ok(tmux) => match tmux.pane_exists(pid).await {
                                Ok(exists) => exists,
                                Err(e) => {
                                    warn!(path = %path.display(), error = %e, "Tmux liveness check failed, skipping agent");
                                    true
                                }
                            }
                            Err(_) => {
                                warn!(path = %path.display(), "Tmux session unavailable, skipping liveness check for agent");
                                stats.kept_live += 1;
                                continue;
                            }
                        }
                    } else {
                        info!(path = %path.display(), reason = "neither window_id nor pane_id set in routing.json", "Pruning orphan agent directory");
                        if let Err(e) = fs::remove_dir_all(&path).await {
                            warn!(path = %path.display(), error = %e, "Failed to prune orphan agent directory");
                        } else {
                            stats.pruned_orphan += 1;
                        }
                        continue;
                    };

                    if is_live {
                        stats.kept_live += 1;
                    } else {
                        info!(path = %path.display(), reason = "dead tmux target", "Pruning dead agent directory");
                        if let Err(e) = fs::remove_dir_all(&path).await {
                            warn!(path = %path.display(), error = %e, "Failed to prune dead agent directory");
                        } else {
                            stats.pruned_dead_tmux += 1;
                        }
                    }
                }
                Err(_) => {
                    // No routing.json or parse fail
                    match fs::metadata(&path).await {
                        Ok(meta) => {
                            match meta.modified() {
                                Ok(mtime) => {
                                    let elapsed = mtime.elapsed().unwrap_or(Duration::ZERO);
                                    if elapsed >= Duration::from_secs(3600) {
                                        info!(path = %path.display(), age = ?elapsed, reason = "orphan (no routing.json) and old", "Pruning orphan agent directory");
                                        if let Err(e) = fs::remove_dir_all(&path).await {
                                            warn!(path = %path.display(), error = %e, "Failed to prune orphan agent directory");
                                        } else {
                                            stats.pruned_orphan += 1;
                                        }
                                    } else {
                                        stats.kept_recent += 1;
                                    }
                                }
                                Err(e) => {
                                    warn!(path = %path.display(), error = %e, "Failed to read mtime for agent directory");
                                }
                            }
                        }
                        Err(e) => {
                            warn!(path = %path.display(), error = %e, "Failed to read metadata for agent directory");
                        }
                    }
                }
            }
        }

        info!(
            scanned = stats.scanned,
            pruned_dead_tmux = stats.pruned_dead_tmux,
            pruned_orphan = stats.pruned_orphan,
            kept_live = stats.kept_live,
            kept_recent = stats.kept_recent,
            "GC stale agents completed"
        );

        Ok(stats)
    }

    /// Clean up an agent by identifier (internal_name or issue_id).
    ///
    /// Kills the tmux window, unregisters from Teams config.json,
    /// and removes per-agent config directory (`.exo/agents/{name}/`).
    #[tracing::instrument(skip(self))]
    pub async fn cleanup_agent(&self, identifier: &str) -> Result<()> {
        // Try to find agent in list (for metadata and window matching).
        // Failure here is non-fatal to allow cleaning up worker panes (invisible to list_agents).
        let agents = self.list_agents().await.unwrap_or_default();
        let agent = agents
            .iter()
            .find(|a| a.internal_name.as_str() == identifier);

        info!(
            identifier,
            found = agent.is_some(),
            "Initiating cleanup_agent"
        );

        // Parse identifier into AgentIdentity to get consistent slug/internal_name/display_name.
        // Try resolver first for authoritative identity, then fall back to derivation.
        let identity = {
            let resolver = self.agent_resolver();
            let agent_name_key = AgentName::from(identifier);
            if let Some(record) = resolver.get(&agent_name_key).await {
                let slug = record.slug.as_str().to_string();
                AgentIdentity::new(slug, record.agent_type)
            } else {
                AgentIdentity::from_internal_name(identifier)
            }
        };

        // Remove synthetic team member registration (non-fatal if not registered).
        // Synthetic members are registered under internal_name (e.g., "beta-claude").
        {
            let team_reg = self.team_registry();
            let birth_branch_str = self.birth_branch.as_str();
            let team_info = if let Some(info) = team_reg.get(birth_branch_str).await {
                Some(info)
            } else if let Some(parent) = self.birth_branch.parent() {
                team_reg.get(parent.as_str()).await
            } else {
                None
            };
            let member_name = identity.internal_name();
            if let Some(info) = team_info {
                let team_name = TeamName::from(info.team_name.as_str());
                if let Err(e) = crate::services::synthetic_members::remove_synthetic_member(
                    &team_name,
                    &member_name,
                ) {
                    warn!(team = %team_name, member = %member_name, error = %e, "Failed to remove synthetic team member (non-fatal)");
                }
            } else {
                debug!(member = %member_name, "No team found in registry — skipping synthetic member removal");
            }
        }

        let internal_name = identity.internal_name();
        let display_name = Some(identity.display_name());

        // Remove per-agent config directory (.exo/agents/{name}/)
        let agent_config_dir = self
            .project_dir()
            .join(".exo")
            .join("agents")
            .join(internal_name.as_str());

        // Try direct cleanup via stored window_id (O(1), no listing needed)
        let mut window_closed = false;
        if let Ok(routing) = RoutingInfo::read_from_dir(&agent_config_dir).await {
            if let Some(wid) = routing.window_id {
                let tmux = self.tmux()?;
                match tmux.kill_window(&wid).await {
                    Ok(()) => {
                        info!(identifier, "Closed tmux window via stored window_id");
                        window_closed = true;
                    }
                    Err(e) => {
                        warn!(identifier, error = %e, "kill_window by stored ID failed, falling back to name match");
                    }
                }
            }
        }

        // Close tmux window if found in list
        if !window_closed {
            if let Some(target_window) = display_name {
                let windows = self.get_tmux_windows().await.unwrap_or_default();
                for window in &windows {
                    if window == &target_window {
                        if let Err(e) = self.close_tmux_window(window).await {
                            warn!(window_name = %window, error = %e, "Failed to close tmux window (may not exist)");
                        }
                        break;
                    }
                }
            }
        }

        if agent_config_dir.exists() {
            if let Err(e) = fs::remove_dir_all(&agent_config_dir).await {
                warn!(
                    path = %agent_config_dir.display(),
                    error = %e,
                    "Failed to remove per-agent config dir (non-fatal)"
                );
            } else {
                info!(path = %agent_config_dir.display(), "Removed per-agent config dir");
            }
        }

        // Remove git worktree if it exists.
        // spawn_subtree/spawn_leaf_subtree use bare slug as dir name,
        // spawn_agent/spawn_gemini_teammate use internal_name ({id}-{type}).
        let worktree_path = {
            let slug_path = self.worktree_base.join(identity.slug());
            if slug_path.exists() {
                slug_path
            } else {
                self.worktree_base.join(internal_name.as_str())
            }
        };
        if worktree_path.exists() {
            let git_wt = self.git_wt().clone();
            let path = worktree_path.clone();
            let join_result =
                tokio::task::spawn_blocking(move || git_wt.remove_workspace(&path)).await;
            match join_result {
                Ok(Ok(())) => {
                    // Successfully removed workspace
                }
                Ok(Err(e)) => {
                    warn!(
                        path = %worktree_path.display(),
                        error = %e,
                        "Failed to remove git worktree (non-fatal)"
                    );
                }
                Err(join_err) => {
                    warn!(
                        path = %worktree_path.display(),
                        error = %join_err,
                        "Blocking task for git worktree removal panicked or was cancelled (non-fatal)"
                    );
                }
            }
        }

        // Deregister identity from resolver
        {
            let resolver = self.agent_resolver();
            if let Err(e) = resolver.deregister(&internal_name).await {
                warn!(agent = %internal_name, error = %e, "Failed to deregister agent identity (non-fatal)");
            }
        }

        // Emit agent:stopped event
        if let Some(ref session) = self.tmux_session {
            if let Ok(agent_id) = crate::ui_protocol::AgentId::try_from(identifier.to_string()) {
                let event = crate::ui_protocol::AgentEvent::AgentStopped {
                    agent_id,
                    timestamp: tmux_events::now_iso8601(),
                };
                if let Err(e) = tmux_events::emit_event(session, &event) {
                    warn!("Failed to emit agent:stopped event: {}", e);
                }
            }
        }

        Ok(())
    }

    /// Clean up multiple agents.
    #[tracing::instrument(skip(self))]
    pub async fn cleanup_agents(
        &self,
        issue_ids: &[String],
        _subrepo: Option<&str>,
    ) -> BatchCleanupResult {
        let mut result = BatchCleanupResult {
            cleaned: Vec::new(),
            failed: Vec::new(),
        };

        for issue_id in issue_ids {
            match self.cleanup_agent(issue_id).await {
                Ok(()) => result.cleaned.push(issue_id.clone()),
                Err(e) => {
                    warn!(issue_id, error = %e, "Failed to cleanup agent");
                    result.failed.push((issue_id.clone(), e.to_string()));
                }
            }
        }

        result
    }

    /// Clean up agents whose work is complete.
    ///
    /// Without worktrees, there are no per-agent branches to check for merge status.
    /// This now simply cleans up stopped agents matching the given issue filter.
    #[tracing::instrument(skip(self))]
    pub async fn cleanup_merged_agents(
        &self,
        issues: &[String],
        subrepo: Option<&str>,
    ) -> Result<BatchCleanupResult> {
        let agents = self.list_agents().await?;
        let mut to_cleanup = Vec::new();

        let issue_filter: Option<HashSet<&str>> = if issues.is_empty() {
            None
        } else {
            Some(issues.iter().map(|s| s.as_str()).collect())
        };

        for agent in agents {
            if let Some(ref filter) = issue_filter {
                if !filter.contains(agent.internal_name.as_str()) {
                    continue;
                }
            }

            // Skip SharedDir (worker pane) agents — their liveness can't be
            // reliably detected via tab queries, so "Stopped" may be wrong.
            if agent.topology == Topology::SharedDir {
                continue;
            }

            // Only clean up stopped agents (no running tab)
            if !agent.has_tab {
                info!(agent = %agent.internal_name, "Agent is stopped, marking for cleanup");
                to_cleanup.push(agent.internal_name.to_string());
            }
        }

        if to_cleanup.is_empty() {
            return Ok(BatchCleanupResult {
                cleaned: Vec::new(),
                failed: Vec::new(),
            });
        }

        Ok(self.cleanup_agents(&to_cleanup, subrepo).await)
    }

    /// List all active agents by scanning the filesystem and verifying with tmux.
    ///
    /// Discovery process:
    /// 1. Scan {worktree_base}/ for subtree agents (isolated worktrees)
    /// 2. Scan {project_dir}/.exo/agents/ for worker agents (shared worktree)
    /// 3. Verify liveness by checking tmux windows/panes
    #[tracing::instrument(skip(self))]
    pub async fn list_agents(&self) -> Result<Vec<AgentInfo>> {
        let mut agents = Vec::new();

        // Get all tmux windows for liveness check
        let windows = self.get_tmux_windows().await.unwrap_or_default();

        // 1. Scan worktree_base for subtree agents
        if self.worktree_base.exists() {
            let mut entries = fs::read_dir(&self.worktree_base).await?;
            while let Some(entry) = entries.next_entry().await? {
                if entry.file_type().await?.is_dir() {
                    let path = entry.path();
                    let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

                    // Check for .mcp.json (Claude) or .gemini/settings.json (Gemini)
                    let is_claude = path.join(".mcp.json").exists();
                    let is_gemini = path.join(".gemini/settings.json").exists();

                    if is_claude || is_gemini {
                        let agent_type = if is_claude {
                            AgentType::Claude
                        } else {
                            AgentType::Gemini
                        };
                        let suffix = format!("-{}", agent_type.suffix());
                        let slug_str = name.strip_suffix(&suffix).unwrap_or(name);
                        let display_name = format!("{} {}", agent_type.emoji(), slug_str);

                        let has_tab = windows.iter().any(|t| t == &display_name);

                        agents.push(AgentInfo {
                            internal_name: AgentName::from(name),
                            has_tab,
                            topology: Topology::WorktreePerAgent,
                            agent_dir: Some(path.clone()),
                            slug: Some(AgentName::from(slug_str)),
                            agent_type: Some(agent_type),
                            pr: None,
                        });

                        // 2. Scan subtree's .exo/agents for workers
                        let subtree_agents_dir = path.join(".exo/agents");
                        if subtree_agents_dir.exists() {
                            self.scan_workers(&subtree_agents_dir, &windows, &mut agents)
                                .await?;
                        }
                    }
                }
            }
        }

        // 3. Scan root .exo/agents for workers
        let root_agents_dir = self.project_dir().join(".exo/agents");
        if root_agents_dir.exists() {
            self.scan_workers(&root_agents_dir, &windows, &mut agents)
                .await?;
        }

        Ok(agents)
    }

    /// Helper to scan a directory for worker agents.
    pub(crate) async fn scan_workers(
        &self,
        dir: &Path,
        windows: &[String],
        agents: &mut Vec<AgentInfo>,
    ) -> Result<()> {
        let mut entries = fs::read_dir(dir).await?;
        while let Some(entry) = entries.next_entry().await? {
            if entry.file_type().await?.is_dir() {
                let path = entry.path();
                let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

                // Workers are currently Gemini-only
                if name.ends_with("-gemini") {
                    let base_name = name.strip_suffix("-gemini").unwrap_or(name);

                    // Skip if this is actually a worktree-based agent (leaf subtree or teammate)
                    // found by the worktree scan.
                    if agents
                        .iter()
                        .any(|a| a.slug.as_ref().map(|s| s.as_str()) == Some(base_name))
                    {
                        continue;
                    }

                    let display_name = format!("{} {}", AgentType::Gemini.emoji(), base_name);

                    // Liveness: for workers, they might be panes in a window.
                    // Currently list_agents only sees windows.
                    let has_tab = windows.iter().any(|t| t == &display_name);

                    agents.push(AgentInfo {
                        internal_name: AgentName::from(name),
                        has_tab,
                        topology: Topology::SharedDir,
                        agent_dir: Some(path.clone()),
                        slug: Some(AgentName::from(base_name)),
                        agent_type: Some(AgentType::Gemini),
                        pr: None,
                    });
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::tmux_ipc::IsolatedTmux;
    use crate::services::HasProjectDir;
    use std::fs::File;
    use std::time::SystemTime;

    #[tokio::test]
    async fn test_gc_stale_agents_empty() {
        let temp = tempfile::tempdir().unwrap();
        let project_dir = temp.path().to_path_buf();
        let services = crate::services::Services::test_with_project_dir(project_dir);
        let service = AgentControlService::new(Arc::new(services));

        // Missing .exo/agents/
        let stats = service.gc_stale_agents().await.unwrap();
        assert_eq!(stats.scanned, 0);

        // Empty .exo/agents/
        let agents_dir = service.ctx.project_dir().join(".exo/agents");
        tokio::fs::create_dir_all(&agents_dir).await.unwrap();
        let stats = service.gc_stale_agents().await.unwrap();
        assert_eq!(stats.scanned, 0);
    }

    #[tokio::test]
    async fn test_gc_stale_agents_scenarios() {
        if !IsolatedTmux::is_available().await {
            return;
        }
        let isolated = IsolatedTmux::new().await.unwrap();
        let temp = tempfile::tempdir().unwrap();
        let project_dir = temp.path().to_path_buf();
        let agents_dir = project_dir.join(".exo/agents");
        fs::create_dir_all(&agents_dir).await.unwrap();

        // Use ServicesBuilder directly to ensure project_dir is set to our temp dir
        let services = crate::services::ServicesBuilder::new(
            project_dir.clone(),
            project_dir.join(".exo/tasks"),
            Arc::new(crate::services::GitWorktreeService::new(project_dir.clone())),
            Arc::new(isolated.ipc.clone()),
        ).build();
        let service = AgentControlService::new(Arc::new(services.clone()))
            .with_tmux_session(isolated.session.clone());

        // 1. Live agent (window)
        let live_window_dir = agents_dir.join("live-window");
        fs::create_dir_all(&live_window_dir).await.unwrap();
        let windows = isolated.ipc.list_windows().await.unwrap();
        let live_window_id = &windows[0].window_id;
        let routing = RoutingInfo {
            window_id: Some(live_window_id.clone()),
            pane_id: None,
            parent_tab: None,
        };
        fs::write(live_window_dir.join("routing.json"), serde_json::to_string(&routing).unwrap()).await.unwrap();

        // 2. Dead agent (window)
        let dead_window_dir = agents_dir.join("dead-window");
        fs::create_dir_all(&dead_window_dir).await.unwrap();
        let routing = RoutingInfo {
            window_id: Some(crate::services::tmux_ipc::WindowId::parse("@99999").unwrap()),
            pane_id: None,
            parent_tab: None,
        };
        fs::write(dead_window_dir.join("routing.json"), serde_json::to_string(&routing).unwrap()).await.unwrap();

        // 3. Orphan (old, no routing.json)
        let old_orphan_dir = agents_dir.join("old-orphan");
        fs::create_dir_all(&old_orphan_dir).await.unwrap();
        let f = File::create(old_orphan_dir.join("somefile")).unwrap();
        let past = SystemTime::now() - Duration::from_secs(4000);
        f.set_times(std::fs::FileTimes::new().set_modified(past)).unwrap();
        // Also set mtime on the directory itself
        let d = File::open(&old_orphan_dir).unwrap();
        d.set_times(std::fs::FileTimes::new().set_modified(past)).unwrap();

        // 4. Recent orphan (new, no routing.json)
        let recent_orphan_dir = agents_dir.join("recent-orphan");
        fs::create_dir_all(&recent_orphan_dir).await.unwrap();

        // 5. Malformed (routing.json exists but empty)
        let malformed_dir = agents_dir.join("malformed");
        fs::create_dir_all(&malformed_dir).await.unwrap();
        fs::write(malformed_dir.join("routing.json"), "{}").await.unwrap();

        // 6. Tmux unavailable (should keep the directory)
        let no_tmux_dir = agents_dir.join("no-tmux");
        fs::create_dir_all(&no_tmux_dir).await.unwrap();
        let routing = RoutingInfo {
            window_id: Some(crate::services::tmux_ipc::WindowId::parse("@123").unwrap()),
            pane_id: None,
            parent_tab: None,
        };
        fs::write(no_tmux_dir.join("routing.json"), serde_json::to_string(&routing).unwrap()).await.unwrap();

        let stats = service.gc_stale_agents().await.unwrap();
        assert_eq!(stats.scanned, 6);
        assert_eq!(stats.kept_live, 1); // only live-window
        assert_eq!(stats.pruned_dead_tmux, 2); // dead-window + no-tmux (now dead)
        assert_eq!(stats.pruned_orphan, 2); // old-orphan + malformed
        assert_eq!(stats.kept_recent, 1); // recent-orphan

        assert!(live_window_dir.exists());
        assert!(!dead_window_dir.exists());
        assert!(!old_orphan_dir.exists());
        assert!(recent_orphan_dir.exists());
        assert!(!malformed_dir.exists());
        assert!(!no_tmux_dir.exists());

        // 7. Re-create a directory for testing fail-open without tmux session
        let fail_open_dir = agents_dir.join("fail-open");
        fs::create_dir_all(&fail_open_dir).await.unwrap();
        fs::write(fail_open_dir.join("routing.json"), serde_json::to_string(&routing).unwrap()).await.unwrap();

        // Test with no tmux session set (should fail open for all routing.json cases)
        let service_no_tmux = AgentControlService::new(Arc::new(services.clone()));
        let stats = service_no_tmux.gc_stale_agents().await.unwrap();
        // Remaining: live-window, recent-orphan, fail-open
        assert_eq!(stats.scanned, 3);
        assert_eq!(stats.kept_live, 2); // live-window, fail-open (both fail-open because no session)
        assert_eq!(stats.kept_recent, 1); // recent-orphan
        assert_eq!(stats.pruned_dead_tmux, 0);
        assert_eq!(stats.pruned_orphan, 0);
        assert!(fail_open_dir.exists());
    }
}
