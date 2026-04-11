use super::*;
use std::sync::Arc;

/// RAII-style rollback guard for agent spawn flows.
///
/// When dropped while armed, it triggers a detached tokio task to clean up
/// any resources that were partially created (worktrees, tmux windows/panes,
/// prompt files, etc.).
pub(crate) struct SpawnRollback {
    worktree_path: Option<PathBuf>,
    tmux_window_id: Option<super::tmux_ipc::WindowId>,
    tmux_pane_id: Option<super::tmux_ipc::PaneId>,
    prompt_file: Option<PathBuf>,
    agent_config_dir: Option<PathBuf>,
    tmux: super::tmux_ipc::TmuxIpc,
    git_wt: Arc<GitWorktreeService>,
    armed: bool,
}

impl SpawnRollback {
    pub fn new(tmux: super::tmux_ipc::TmuxIpc, git_wt: Arc<GitWorktreeService>) -> Self {
        Self {
            worktree_path: None,
            tmux_window_id: None,
            tmux_pane_id: None,
            prompt_file: None,
            agent_config_dir: None,
            tmux,
            git_wt,
            armed: true,
        }
    }

    pub fn set_worktree(&mut self, path: PathBuf) {
        self.worktree_path = Some(path);
    }

    pub fn set_window(&mut self, id: super::tmux_ipc::WindowId) {
        self.tmux_window_id = Some(id);
    }

    pub fn set_pane(&mut self, id: super::tmux_ipc::PaneId) {
        self.tmux_pane_id = Some(id);
    }

    pub fn set_prompt_file(&mut self, path: PathBuf) {
        self.prompt_file = Some(path);
    }

    pub fn set_agent_config_dir(&mut self, path: PathBuf) {
        self.agent_config_dir = Some(path);
    }

    pub fn disarm(&mut self) {
        self.armed = false;
    }
}

impl Drop for SpawnRollback {
    fn drop(&mut self) {
        if !self.armed {
            return;
        }

        let wt_path = self.worktree_path.take();
        let window_id = self.tmux_window_id.take();
        let pane_id = self.tmux_pane_id.take();
        let prompt = self.prompt_file.take();
        let config_dir = self.agent_config_dir.take();

        if wt_path.is_none()
            && window_id.is_none()
            && pane_id.is_none()
            && prompt.is_none()
            && config_dir.is_none()
        {
            return;
        }

        let tmux = self.tmux.clone();
        let git_wt = self.git_wt.clone();

        if let Ok(handle) = tokio::runtime::Handle::try_current() {
            handle.spawn(async move {
                if let Some(id) = window_id {
                    let _ = tmux.kill_window(&id).await;
                }
                if let Some(id) = pane_id {
                    let _ = tmux.kill_pane(&id).await;
                }
                if let Some(path) = wt_path {
                    let _ =
                        tokio::task::spawn_blocking(move || git_wt.remove_workspace(&path)).await;
                }
                if let Some(p) = prompt {
                    let _ = tokio::fs::remove_file(p).await;
                }
                if let Some(d) = config_dir {
                    let _ = tokio::fs::remove_dir_all(d).await;
                }
                tracing::info!("SpawnRollback: cleaned up partial spawn state");
            });
        } else {
            tracing::error!("SpawnRollback: no tokio runtime available for cleanup");
        }
    }
}

impl<
        C: super::super::HasGitHubClient
            + super::super::HasAcpRegistry
            + super::super::HasTeamRegistry
            + super::super::HasAgentResolver
            + super::super::HasProjectDir
            + super::super::HasGitWorktreeService
            + 'static,
    > AgentControlService<C>
{
    pub(crate) fn resolve_tmux_session(&self) -> Result<String> {
        self.tmux_session
            .clone()
            .ok_or_else(|| anyhow!("No tmux session configured (call with_tmux_session)"))
    }

    /// Get the direct tmux IPC client, falling back to creating one from config or env.
    pub(crate) fn tmux(&self) -> Result<super::tmux_ipc::TmuxIpc> {
        if let Some(ref ipc) = self.tmux_ipc {
            return Ok(ipc.clone());
        }
        let session = self.resolve_tmux_session()?;
        Ok(super::tmux_ipc::TmuxIpc::new(&session))
    }

    /// Clean up an existing worktree (if present) and create a fresh one.
    ///
    /// Consolidates the idempotent cleanup + spawn_blocking + catch_unwind boilerplate
    /// shared across spawn_agent, spawn_subtree, spawn_leaf_subtree, and spawn_gemini_teammate.
    pub(crate) async fn create_worktree_checked(
        &self,
        worktree_path: &Path,
        branch_name: &BranchName,
        base_branch: &BranchName,
    ) -> Result<()> {
        if worktree_path.exists() {
            info!(path = %worktree_path.display(), "Removing existing workspace for idempotency");
            let git_wt = self.git_wt().clone();
            let path = worktree_path.to_path_buf();
            match tokio::task::spawn_blocking(move || git_wt.remove_workspace(&path)).await {
                Err(join_err) => {
                    warn!(error = %join_err, "Join error while removing existing workspace (non-fatal)");
                }
                Ok(Err(e)) => {
                    warn!(error = %e, "Failed to remove existing workspace (non-fatal)");
                }
                Ok(Ok(_)) => {}
            }
        }

        info!(
            base_branch = %base_branch,
            branch_name = %branch_name,
            worktree_path = %worktree_path.display(),
            "Creating git worktree"
        );

        let git_wt = self.git_wt().clone();
        let path = worktree_path.to_path_buf();
        let bookmark = branch_name.clone();
        let base = base_branch.clone();
        let result = tokio::task::spawn_blocking(move || {
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                git_wt.create_workspace(&path, &bookmark, &base)
            }))
        })
        .await
        .context("tokio task join error while creating git worktree")?;

        match result {
            Ok(Ok(())) => {}
            Ok(Err(e)) => {
                return Err(anyhow::Error::from(EffectError::from(e)))
                    .context("Failed to create git worktree")
            }
            Err(panic_val) => {
                let msg = panic_val
                    .downcast_ref::<String>()
                    .map(|s| s.as_str())
                    .or_else(|| panic_val.downcast_ref::<&str>().copied())
                    .unwrap_or("unknown panic");
                return Err(anyhow!("git worktree creation panicked: {}", msg));
            }
        }

        // Verify the worktree is on the expected branch
        let git_wt = self.git_wt().clone();
        let verify_path = worktree_path.to_path_buf();
        let expected = branch_name.to_string();
        let actual =
            tokio::task::spawn_blocking(move || git_wt.get_workspace_bookmark(&verify_path))
                .await
                .context("spawn_blocking failed during branch verification")?
                .map_err(|e| anyhow!("Failed to verify worktree branch: {}", e))?;

        if actual.as_deref() != Some(&expected) {
            return Err(anyhow!(
                "Worktree branch mismatch: expected '{}', got {:?}",
                expected,
                actual
            ));
        }

        Ok(())
    }

    /// Build the common env vars shared by all spawn functions.
    ///
    /// `session_id` is the agent's birth-branch: for worktree agents this is the child's
    /// branch name; for inline workers it's the parent's birth-branch (they share context).
    pub(crate) fn common_spawn_env(
        &self,
        agent_name: &AgentName,
        session_id: &BranchName,
        role: &crate::domain::Role,
    ) -> HashMap<String, String> {
        let mut env_vars = HashMap::new();
        env_vars.insert("EXOMONAD_AGENT_ID".to_string(), agent_name.to_string());
        env_vars.insert("EXOMONAD_SESSION_ID".to_string(), session_id.to_string());
        env_vars.insert("EXOMONAD_ROLE".to_string(), role.as_str().to_string());
        if let Some(ref session) = self.tmux_session {
            env_vars.insert("EXOMONAD_TMUX_SESSION".to_string(), session.clone());
        }

        // Propagate swarm run_id and parent agent identity for OTel resource attributes
        if let Ok(v) = std::env::var("EXOMONAD_SWARM_RUN_ID") {
            env_vars.insert("EXOMONAD_SWARM_RUN_ID".to_string(), v);
        }
        env_vars.insert(
            "EXOMONAD_PARENT_AGENT".to_string(),
            self.effective_birth_branch(None).to_string(),
        );

        // Propagate W3C traceparent for cross-agent trace correlation
        {
            use tracing_opentelemetry::OpenTelemetrySpanExt;
            let cx = tracing::Span::current().context();
            let mut injector = std::collections::HashMap::new();
            opentelemetry::global::get_text_map_propagator(|propagator| {
                propagator.inject_context(&cx, &mut injector);
            });
            if let Some(traceparent) = injector.get("traceparent") {
                env_vars.insert("TRACEPARENT".to_string(), traceparent.clone());
            }
        }

        env_vars
    }

    /// Emit an agent:started event if tmux_session is configured.
    pub(crate) fn emit_agent_started(&self, agent_name: &AgentName) -> Result<()> {
        if let Some(ref session) = self.tmux_session {
            let agent_id = crate::ui_protocol::AgentId::try_from(agent_name.to_string())
                .map_err(|e| anyhow!("Invalid agent_id: {}", e))?;
            let event = crate::ui_protocol::AgentEvent::AgentStarted {
                agent_id,
                timestamp: tmux_events::now_iso8601(),
            };
            if let Err(e) = tmux_events::emit_event(session, &event) {
                warn!("Failed to emit agent:started event: {}", e);
            }
        }
        Ok(())
    }

    pub(crate) async fn new_tmux_window(
        &self,
        name: &str,
        cwd: &Path,
        agent_type: AgentType,
        prompt_file: Option<PathBuf>,
        env_vars: HashMap<String, String>,
    ) -> Result<super::tmux_ipc::WindowId> {
        self.new_tmux_window_inner(name, cwd, agent_type, prompt_file, env_vars, None, None)
            .await
    }

    /// Build the full shell command string for an agent.
    /// Handles: agent CLI + prompt/flags → env var prefix → nix develop wrapping.
    /// Used by both `new_tmux_window_inner` and `new_tmux_pane`.
    ///
    /// `prompt_file` is an absolute path to a file containing the prompt text.
    /// The prompt is read at runtime via `$(cat ...)` to avoid shell quoting issues
    /// with arbitrary prompt content (apostrophes, backticks, $(), etc.).
    pub(crate) fn build_agent_command(
        agent_type: AgentType,
        prompt_file: Option<&Path>,
        fork_session_id: Option<&str>,
        env_vars: &HashMap<String, String>,
        cwd: &Path,
        claude_flags: Option<&ClaudeSpawnFlags>,
        yolo: bool,
    ) -> String {
        let cmd = agent_type.command();

        // Build permission flags for Claude agents
        let perms_flags = match agent_type {
            AgentType::Claude => {
                let mut flags = String::new();
                let mode = claude_flags.and_then(|f| f.permission_mode.as_ref());
                match mode {
                    Some(m) => {
                        flags.push_str(" --permission-mode ");
                        flags.push_str(m.as_str());
                    }
                    None => flags.push_str(" --dangerously-skip-permissions"),
                }
                if let Some(f) = claude_flags {
                    for tool in &f.allowed_tools {
                        flags.push_str(" --allowedTools ");
                        flags.push_str(&shell_escape::escape(tool.into()));
                    }
                    for tool in &f.disallowed_tools {
                        flags.push_str(" --disallowedTools ");
                        flags.push_str(&shell_escape::escape(tool.into()));
                    }
                }
                flags
            }
            AgentType::Gemini => {
                if yolo {
                    " --yolo".to_string()
                } else {
                    String::new()
                }
            }
            AgentType::Shoal | AgentType::Process => String::new(),
        };

        let agent_command = match (prompt_file, fork_session_id) {
            (Some(pf), Some(session_id)) => {
                let escaped_session = Self::escape_for_shell_command(session_id);
                let escaped_path = Self::escape_for_shell_command(&pf.display().to_string());
                format!(
                    "{}{} --resume {} --fork-session \"$(cat {})\"",
                    cmd, perms_flags, escaped_session, escaped_path
                )
            }
            (Some(pf), None) => {
                let escaped_path = Self::escape_for_shell_command(&pf.display().to_string());
                let flag = agent_type.prompt_flag();
                if flag.is_empty() {
                    format!("{}{} \"$(cat {})\"", cmd, perms_flags, escaped_path)
                } else {
                    format!(
                        "{}{} {} \"$(cat {})\"",
                        cmd, perms_flags, flag, escaped_path
                    )
                }
            }
            _ => format!("{}{}", cmd, perms_flags),
        };

        // Prepend env vars
        let env_prefix = env_vars
            .iter()
            .map(|(k, v)| format!("{}={}", k, shell_escape::escape(v.into())))
            .collect::<Vec<_>>()
            .join(" ");
        let full_command = if env_prefix.is_empty() {
            agent_command
        } else {
            format!("{} {}", env_prefix, agent_command)
        };

        // Wrap in nix develop shell if flake.nix exists in cwd
        if cwd.join("flake.nix").exists() {
            info!("Wrapping agent command in nix develop shell");
            let escaped = full_command.replace('\'', "'\\''");
            format!("nix develop -c sh -c '{}'", escaped)
        } else {
            full_command
        }
    }

    /// Write a prompt to a temp file and return the absolute path.
    /// Files are written to `.exo/tmp/` in the project directory.
    /// Uses UUID filenames to avoid races when multiple agents spawn concurrently.
    pub(crate) async fn write_prompt_file(
        project_dir: &Path,
        agent_name: &str,
        prompt: &str,
    ) -> Result<PathBuf> {
        let tmp_dir = project_dir.join(".exo/tmp");
        tokio::fs::create_dir_all(&tmp_dir)
            .await
            .context("Failed to create .exo/tmp/")?;
        let ts = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        let path = tmp_dir.join(format!("prompt-{}-{}.txt", ts, std::process::id()));
        tokio::fs::write(&path, prompt)
            .await
            .context("Failed to write prompt file")?;
        info!(path = %path.display(), agent = %agent_name, "Wrote prompt to temp file");
        Ok(path)
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) async fn new_tmux_window_inner(
        &self,
        name: &str,
        cwd: &Path,
        agent_type: AgentType,
        prompt_file: Option<PathBuf>,
        env_vars: HashMap<String, String>,
        fork_session_id: Option<&str>,
        claude_flags: Option<&ClaudeSpawnFlags>,
    ) -> Result<super::tmux_ipc::WindowId> {
        info!(name, cwd = %cwd.display(), agent_type = ?agent_type, fork = fork_session_id.is_some(), "Creating tmux window");

        let full_command = Self::build_agent_command(
            agent_type,
            prompt_file.as_deref(),
            fork_session_id,
            &env_vars,
            cwd,
            claude_flags,
            self.yolo,
        );
        let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/sh".to_string());
        let tmux = self.tmux()?;
        let window_name = name.to_string();
        let window_cwd = cwd.to_path_buf();

        let window_id = tmux
            .new_window(&window_name, &window_cwd, &shell, &full_command)
            .await
            .context("Failed to create tmux window")?;

        Ok(window_id)
    }

    pub(crate) async fn get_tmux_windows(&self) -> Result<Vec<String>> {
        debug!("Querying tmux window names via direct IPC");
        let tmux = match self.tmux() {
            Ok(t) => t,
            Err(e) => {
                warn!("Failed to get tmux IPC client for list-windows: {}", e);
                return Ok(Vec::new());
            }
        };

        let result = timeout(TMUX_TIMEOUT, tmux.list_windows())
            .await
            .map_err(|_| {
                anyhow!(
                    "tmux list-windows timed out after {}s",
                    TMUX_TIMEOUT.as_secs()
                )
            })?;

        match result {
            Ok(windows) => Ok(windows.into_iter().map(|w| w.window_name).collect()),
            Err(e) => {
                warn!("tmux list-windows IPC failed, assuming no windows: {}", e);
                Ok(Vec::new())
            }
        }
    }

    /// Check if a tmux window with the given display name exists.
    pub(crate) async fn is_tmux_window_alive(&self, display_name: &str) -> bool {
        self.get_tmux_windows()
            .await
            .unwrap_or_default()
            .iter()
            .any(|window| window == display_name)
    }

    pub(crate) async fn close_tmux_window(&self, name: &str) -> Result<()> {
        info!(name, "Closing tmux window");

        let tmux = self.tmux()?;
        let window_name = name.to_string();

        let window_id = {
            let windows = tmux.list_windows().await?;
            windows
                .into_iter()
                .find(|w| w.window_name == window_name)
                .map(|w| w.window_id)
                .ok_or_else(|| anyhow!("Window not found: {}", window_name))?
        };

        let tmux = self.tmux()?;
        timeout(TMUX_TIMEOUT, tmux.kill_window(&window_id))
            .await
            .map_err(|_| {
                anyhow::Error::new(TimeoutError {
                    message: format!(
                        "tmux kill-window timed out after {}s",
                        TMUX_TIMEOUT.as_secs()
                    ),
                })
            })??;

        info!(name, "tmux kill-window successful");
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) async fn new_tmux_pane(
        &self,
        name: &str,
        cwd: &Path,
        agent_type: AgentType,
        prompt_file: Option<PathBuf>,
        env_vars: HashMap<String, String>,
        parent_window_name: Option<&str>,
        claude_flags: Option<&ClaudeSpawnFlags>,
    ) -> Result<super::tmux_ipc::PaneId> {
        info!(name, cwd = %cwd.display(), agent_type = ?agent_type, parent = ?parent_window_name, "Creating tmux pane");

        let full_command = Self::build_agent_command(
            agent_type,
            prompt_file.as_deref(),
            None,
            &env_vars,
            cwd,
            claude_flags,
            self.yolo,
        );
        let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/zsh".to_string());
        let tmux = self.tmux()?;

        // Find parent window ID by name
        let target_window = if let Some(wname) = parent_window_name {
            let wname = wname.to_string();
            let windows = tmux
                .list_windows()
                .await
                .context("Failed to list tmux windows")?;
            windows
                .iter()
                .find(|w| w.window_name == wname)
                .map(|w| w.window_id.clone())
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "No tmux window found matching '{}' — cannot create pane",
                        wname
                    )
                })?
        } else {
            // Default to first window if no name provided
            let windows = tmux
                .list_windows()
                .await
                .context("Failed to list tmux windows")?;
            windows
                .first()
                .map(|w| w.window_id.clone())
                .ok_or_else(|| {
                    anyhow!(
                        "No windows found in session {} — cannot create pane",
                        tmux.session_name()
                    )
                })?
        };

        let pane_cwd = cwd.to_path_buf();
        let pane_id = tmux
            .split_window(&target_window, &pane_cwd, &shell, &full_command)
            .await
            .context("Failed to create tmux pane")?;

        // Rebalance panes into a grid after each split to prevent
        // exponential height decay (60 → 29 → 14 → 6 → 2 → 1 lines).
        if let Err(e) = tmux
            .select_layout(&target_window, crate::domain::TmuxLayout::Tiled)
            .await
        {
            tracing::warn!(error = %e, "Failed to apply tiled layout (non-fatal)");
        }

        info!(name, pane_id = %pane_id, "Successfully created tmux pane");
        Ok(pane_id)
    }

    /// Write MCP config for the agent directory.
    ///
    /// Claude agents get `.mcp.json`. Gemini agents get `.gemini/settings.json`.
    /// Uses stdio transport via `exomonad mcp-stdio`.
    pub(crate) async fn write_agent_mcp_config(
        &self,
        _effective_dir: &Path,
        agent_dir: &Path,
        agent_type: AgentType,
        role: &crate::domain::Role,
    ) -> Result<()> {
        let agent_name = agent_dir
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown");

        let mcp_content = Self::generate_mcp_config(
            agent_name,
            agent_type,
            role.as_str(),
            &self.wasm_name,
            &self.extra_mcp_servers,
        );

        match agent_type {
            AgentType::Claude => {
                fs::write(agent_dir.join(".mcp.json"), mcp_content).await?;
                info!(agent_dir = %agent_dir.display(), role = %role.as_str(), "Wrote .mcp.json for Claude agent");
            }
            AgentType::Gemini => {
                let gemini_dir = agent_dir.join(".gemini");
                fs::create_dir_all(&gemini_dir).await?;
                fs::write(gemini_dir.join("settings.json"), mcp_content).await?;
                info!(agent_dir = %agent_dir.display(), role = %role.as_str(), "Wrote .gemini/settings.json for Gemini agent");
            }
            AgentType::Process => {} // No MCP config for process companions
            AgentType::Shoal => {
                let exo_dir = agent_dir.join(".exo");
                fs::create_dir_all(&exo_dir).await?;
                fs::write(exo_dir.join("mcp.json"), mcp_content).await?;
                info!(agent_dir = %agent_dir.display(), role = %role.as_str(), "Wrote .exo/mcp.json for Shoal agent");
            }
        }
        Ok(())
    }

    /// Pre-trust a directory for Gemini CLI by adding it to `~/.gemini/trustedFolders.json`.
    ///
    /// This prevents the interactive "Trust this folder?" dialog that blocks Gemini agents.
    pub async fn gemini_trust_folder(path: &Path) {
        let Some(home) = dirs::home_dir() else {
            warn!("Could not determine home directory for Gemini trust");
            return;
        };
        let gemini_home = home.join(".gemini");
        if let Err(e) = tokio::fs::create_dir_all(&gemini_home).await {
            warn!(error = %e, dir = %gemini_home.display(), "Failed to create Gemini config directory");
            return;
        }
        let trust_file = gemini_home.join("trustedFolders.json");
        let abs_path = match path.canonicalize() {
            Ok(p) => p.to_string_lossy().to_string(),
            Err(_) => path.to_string_lossy().to_string(),
        };

        let mut trust_map: serde_json::Map<String, serde_json::Value> = if trust_file.exists() {
            match tokio::fs::read_to_string(&trust_file).await {
                Ok(content) => serde_json::from_str(&content).unwrap_or_default(),
                Err(_) => serde_json::Map::new(),
            }
        } else {
            serde_json::Map::new()
        };

        if trust_map.contains_key(&abs_path) {
            return;
        }

        trust_map.insert(
            abs_path.clone(),
            serde_json::Value::String("TRUST_FOLDER".to_string()),
        );

        if let Ok(content) = serde_json::to_string_pretty(&trust_map) {
            // Atomic write: temp file + rename to avoid partial writes from concurrent spawns
            let tmp_file = trust_file.with_extension("tmp");
            if let Err(e) = tokio::fs::write(&tmp_file, &content).await {
                warn!(path = %abs_path, error = %e, "Failed to write Gemini trustedFolders.json tmp");
            } else if let Err(e) = tokio::fs::rename(&tmp_file, &trust_file).await {
                warn!(path = %abs_path, error = %e, "Failed to rename Gemini trustedFolders.json");
                let _ = tokio::fs::remove_file(&tmp_file).await;
            } else {
                info!(path = %abs_path, "Pre-trusted folder for Gemini CLI");
            }
        }
    }

    /// Symlink server socket into worktree so agents find it without walk-up.
    pub(crate) async fn create_socket_symlink(&self, worktree_path: &Path) {
        let source = self.project_dir().join(".exo/server.sock");
        let target_dir = worktree_path.join(".exo");
        let target = target_dir.join("server.sock");

        if let Err(e) = tokio::fs::create_dir_all(&target_dir).await {
            warn!(path = %target_dir.display(), error = %e, "Failed to create .exo/ in worktree");
            return;
        }

        // Ensure worktree .exo/ has a .gitignore so runtime artifacts don't cause
        // untracked file warnings (which force `git worktree remove --force`).
        let gitignore = target_dir.join(".gitignore");
        if !gitignore.exists() {
            if let Err(e) =
                tokio::fs::write(&gitignore, "# Runtime artifacts\nserver.sock\nserver.pid\n").await
            {
                tracing::warn!(path = %gitignore.display(), error = %e, "Failed to write .gitignore");
            }
        }

        if let Err(e) = tokio::fs::remove_file(&target).await {
            tracing::debug!(path = %target.display(), error = %e, "Could not remove old socket symlink");
        }

        match tokio::fs::symlink(&source, &target).await {
            Ok(()) => info!(
                source = %source.display(),
                target = %target.display(),
                "Symlinked server socket into worktree"
            ),
            Err(e) => warn!(
                source = %source.display(),
                target = %target.display(),
                error = %e,
                "Failed to symlink server socket"
            ),
        }
    }

    /// Resolve role context file with two-tier fallback: project-local > global.
    pub(crate) fn resolve_role_context(&self, role: &crate::domain::Role) -> Option<PathBuf> {
        resolve_role_context_path(self.project_dir(), &self.wasm_name, role.as_str())
    }

    /// Generate MCP configuration JSON for an agent using stdio transport.
    ///
    /// `extra_mcp_servers` are merged into the `mcpServers` object alongside the
    /// core exomonad entry, giving spawned agents access to the same extra servers
    /// (e.g. metacog, notebooklm) configured in the project's `config.toml`.
    pub(crate) fn generate_mcp_config(
        name: &str,
        agent_type: AgentType,
        role: &str,
        wasm_name: &str,
        extra_mcp_servers: &HashMap<String, serde_json::Value>,
    ) -> String {
        match agent_type {
            AgentType::Claude => {
                let mut config = serde_json::json!({
                    "mcpServers": {
                        "exomonad": {
                            "type": "stdio",
                            "command": "exomonad",
                            "args": ["mcp-stdio", "--role", role, "--name", name]
                        }
                    }
                });
                if let Some(servers) = config["mcpServers"].as_object_mut() {
                    for (k, v) in extra_mcp_servers {
                        servers.insert(k.clone(), v.clone());
                    }
                }
                serde_json::to_string_pretty(&config).unwrap()
            }
            AgentType::Gemini => {
                let mut config = serde_json::json!({
                    "mcpServers": {
                        "exomonad": {
                            "type": "stdio",
                            "command": "exomonad",
                            "args": ["mcp-stdio", "--role", role, "--name", name]
                        }
                    },
                    "context": {
                        "fileName": ["GEMINI.md", format!(".exo/roles/{}/context/{}.md", wasm_name, role)]
                    },
                    "hooks": {
                        "BeforeTool": [
                            {
                                "matcher": "*",
                                "hooks": [
                                    {
                                        "type": "command",
                                        "command": "exomonad hook before-tool --runtime gemini"
                                    }
                                ]
                            }
                        ],
                        "BeforeModel": [
                            {
                                "matcher": "*",
                                "hooks": [
                                    {
                                        "type": "command",
                                        "command": "exomonad hook before-model --runtime gemini"
                                    }
                                ]
                            }
                        ],
                        "AfterModel": [
                            {
                                "matcher": "*",
                                "hooks": [
                                    {
                                        "type": "command",
                                        "command": "exomonad hook after-model --runtime gemini"
                                    }
                                ]
                            }
                        ],
                        "AfterAgent": [
                            {
                                "matcher": "*",
                                "hooks": [
                                    {
                                        "type": "command",
                                        "command": "exomonad hook after-agent --runtime gemini"
                                    }
                                ]
                            }
                        ]
                    }
                });
                if let Some(servers) = config["mcpServers"].as_object_mut() {
                    for (k, v) in extra_mcp_servers {
                        servers.insert(k.clone(), v.clone());
                    }
                }
                serde_json::to_string_pretty(&config).unwrap()
            }
            AgentType::Shoal => serde_json::to_string_pretty(&serde_json::json!({
                "command": "exomonad",
                "args": ["mcp-stdio", "--role", role, "--name", name]
            }))
            .unwrap(),
            AgentType::Process => String::new(),
        }
    }

    /// Build the initial prompt for a spawned agent.
    pub(crate) fn build_initial_prompt(
        issue_id: &str,
        title: &str,
        body: &str,
        labels: &[String],
        issue_url: &str,
    ) -> String {
        let labels_str = if labels.is_empty() {
            "None".to_string()
        } else {
            labels
                .iter()
                .map(|l| format!("`{}`", l))
                .collect::<Vec<_>>()
                .join(", ")
        };

        format!(
            r###"# Issue #{issue_id}: {title}

**Issue URL:** {issue_url}
**Labels:** {labels_str}

## Description

{body}"###,
            issue_id = issue_id,
            title = title,
            issue_url = issue_url,
            labels_str = labels_str,
            body = body,
        )
    }

    /// Escape a string for safe use in shell command with single quotes.
    ///
    /// Wraps the string in single quotes and escapes any embedded single quotes.
    /// Used for fork_session_id (branch names). Prompts use file-based passing instead.
    ///
    /// Example: "user's issue" -> "'user'\''s issue'"
    pub(crate) fn escape_for_shell_command(s: &str) -> String {
        // Replace ' with '\'' (end quote, escaped quote, start quote)
        let escaped = s.replace('\'', r"'\''");
        format!("'{}'", escaped)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::HasGitWorktreeService;
    type ACS = AgentControlService<crate::services::Services>;

    #[test]
    fn test_escape_for_shell_command_simple() {
        assert_eq!(
            ACS::escape_for_shell_command("hello world"),
            "'hello world'"
        );
    }

    #[test]
    fn test_escape_for_shell_command_with_quote() {
        // Standard shell escaping: end quote, escaped quote, start quote
        // 'user'\''s issue' = 'user' + \' + 's issue'
        assert_eq!(
            ACS::escape_for_shell_command("user's issue"),
            r"'user'\''s issue'"
        );
    }

    #[test]
    fn test_escape_for_shell_command_shell_chars() {
        let result = ACS::escape_for_shell_command("Test $VAR and `code`");
        assert!(result.contains("$VAR"));
        assert!(result.contains("`code`"));
        assert_eq!(result, "'Test $VAR and `code`'");
    }

    #[test]
    fn test_build_initial_prompt_format() {
        let prompt = ACS::build_initial_prompt(
            "123",
            "Fix the bug",
            "Description",
            &["bug".to_string(), "priority".to_string()],
            "https://github.com/owner/repo/issues/123",
        );

        assert!(prompt.contains("# Issue #123: Fix the bug"));
        assert!(prompt.contains("Description"));
        assert!(prompt.contains("https://github.com/owner/repo/issues/123"));
        assert!(prompt.contains("**Labels:** `bug`, `priority`"));
    }

    #[test]
    fn test_build_initial_prompt_no_labels() {
        let prompt = ACS::build_initial_prompt(
            "123",
            "Fix the bug",
            "Description",
            &[],
            "https://github.com/owner/repo/issues/123",
        );

        assert!(prompt.contains("**Labels:** None"));
    }

    #[test]
    fn test_claude_mcp_config_format() {
        let config = ACS::generate_mcp_config(
            "test-claude",
            AgentType::Claude,
            "tl",
            "devswarm",
            &HashMap::new(),
        );
        let parsed: serde_json::Value = serde_json::from_str(&config).unwrap();
        assert_eq!(parsed["mcpServers"]["exomonad"]["type"], "stdio");
        assert_eq!(parsed["mcpServers"]["exomonad"]["command"], "exomonad");
        let args = parsed["mcpServers"]["exomonad"]["args"].as_array().unwrap();
        assert_eq!(
            args,
            &["mcp-stdio", "--role", "tl", "--name", "test-claude"]
        );
    }

    #[test]
    fn test_gemini_mcp_config_format() {
        let config = ACS::generate_mcp_config(
            "test-gemini",
            AgentType::Gemini,
            "dev",
            "devswarm",
            &HashMap::new(),
        );
        let parsed: serde_json::Value = serde_json::from_str(&config).unwrap();
        assert_eq!(parsed["mcpServers"]["exomonad"]["command"], "exomonad");
        let args = parsed["mcpServers"]["exomonad"]["args"].as_array().unwrap();
        assert_eq!(
            args,
            &["mcp-stdio", "--role", "dev", "--name", "test-gemini"]
        );
        assert_eq!(parsed["mcpServers"]["exomonad"]["type"], "stdio");

        // Check hooks
        let before_tool = &parsed["hooks"]["BeforeTool"];
        assert!(before_tool.is_array());
        let bt_hooks = &before_tool[0]["hooks"];
        assert_eq!(
            bt_hooks[0]["command"],
            "exomonad hook before-tool --runtime gemini"
        );

        let before_model = &parsed["hooks"]["BeforeModel"];
        assert!(before_model.is_array());
        let bm_hooks = &before_model[0]["hooks"];
        assert_eq!(
            bm_hooks[0]["command"],
            "exomonad hook before-model --runtime gemini"
        );

        let after_model = &parsed["hooks"]["AfterModel"];
        assert!(after_model.is_array());
        let am_hooks = &after_model[0]["hooks"];
        assert_eq!(
            am_hooks[0]["command"],
            "exomonad hook after-model --runtime gemini"
        );

        let after_agent = &parsed["hooks"]["AfterAgent"];
        assert!(after_agent.is_array());
        let hooks_list = &after_agent[0]["hooks"];
        assert_eq!(
            hooks_list[0]["command"],
            "exomonad hook after-agent --runtime gemini"
        );
    }

    #[test]
    fn test_gemini_worker_settings_schema_compliance() {
        let settings = ACS::generate_gemini_worker_settings("test-worker", None, &HashMap::new());

        // 1. MCP config uses stdio transport
        assert_eq!(settings["mcpServers"]["exomonad"]["type"], "stdio");
        assert_eq!(settings["mcpServers"]["exomonad"]["command"], "exomonad");
        let args = settings["mcpServers"]["exomonad"]["args"]
            .as_array()
            .unwrap();
        assert_eq!(
            args,
            &["mcp-stdio", "--role", "worker", "--name", "test-worker"]
        );

        // 2. Hooks must strictly use PascalCase
        assert!(
            settings["hooks"].get("AfterAgent").is_some(),
            "hooks.AfterAgent is missing"
        );
        assert!(
            settings["hooks"].get("BeforeTool").is_some(),
            "hooks.BeforeTool is missing"
        );
        assert!(
            settings["hooks"].get("BeforeModel").is_some(),
            "hooks.BeforeModel is missing"
        );
        assert!(
            settings["hooks"].get("AfterModel").is_some(),
            "hooks.AfterModel is missing"
        );
        assert!(
            settings["hooks"].get("after-agent").is_none(),
            "Found invalid kebab-case 'after-agent'"
        );

        // 3. The hook structure must match the array of matcher/hooks objects
        let after_agent = &settings["hooks"]["AfterAgent"];
        assert!(after_agent.is_array(), "hooks.AfterAgent must be an array");

        let first_rule = &after_agent[0];
        assert_eq!(first_rule["matcher"], "*");

        let hooks_list = &first_rule["hooks"];
        assert!(hooks_list.is_array());

        let command_hook = &hooks_list[0];
        assert_eq!(command_hook["type"], "command");
        assert_eq!(
            command_hook["command"], "exomonad hook worker-exit --runtime gemini",
            "Hook command mismatch"
        );
    }

    fn test_services(project_dir: PathBuf) -> Arc<crate::services::Services> {
        Arc::new(crate::services::Services::test_with_project_dir(
            project_dir,
        ))
    }

    #[tokio::test]
    async fn test_create_socket_symlink() {
        let temp_dir = tempfile::tempdir().unwrap();
        let project_dir = temp_dir.path().to_path_buf();
        let exo_dir = project_dir.join(".exo");
        tokio::fs::create_dir_all(&exo_dir).await.unwrap();
        tokio::fs::write(exo_dir.join("server.sock"), "placeholder")
            .await
            .unwrap();

        let services = test_services(project_dir.clone());
        let service = AgentControlService::new(services);

        let worktree = temp_dir.path().join("child-wt");
        tokio::fs::create_dir_all(&worktree).await.unwrap();

        service.create_socket_symlink(&worktree).await;

        let link = worktree.join(".exo/server.sock");
        assert!(link.exists(), "Symlink should exist");
        let target = tokio::fs::read_link(&link).await.unwrap();
        assert_eq!(target, project_dir.join(".exo/server.sock"));
    }

    #[test]
    fn test_common_spawn_env_core_vars() {
        let services = test_services(PathBuf::from("."));
        let service =
            AgentControlService::new(services).with_birth_branch(BirthBranch::from("main.tl-auth"));

        let agent = AgentName::from("fix-oauth-gemini");
        let session_id = BranchName::from("main.tl-auth.fix-oauth-gemini");
        let role = crate::domain::Role::dev();

        let env = service.common_spawn_env(&agent, &session_id, &role);

        assert_eq!(env.get("EXOMONAD_AGENT_ID").unwrap(), "fix-oauth-gemini");
        assert_eq!(
            env.get("EXOMONAD_SESSION_ID").unwrap(),
            "main.tl-auth.fix-oauth-gemini"
        );
        assert_eq!(env.get("EXOMONAD_ROLE").unwrap(), "dev");
        assert_eq!(
            env.get("EXOMONAD_PARENT_AGENT").unwrap(),
            "main.tl-auth",
            "Parent agent should be the service's own birth branch"
        );
    }

    #[test]
    fn test_common_spawn_env_tmux_session() {
        let services = test_services(PathBuf::from("."));
        let service = AgentControlService::new(services)
            .with_birth_branch(BirthBranch::from("main"))
            .with_tmux_session("exo-test-session".to_string());

        let agent = AgentName::from("worker-1");
        let session_id = BranchName::from("main");
        let role = crate::domain::Role::worker();

        let env = service.common_spawn_env(&agent, &session_id, &role);

        assert_eq!(
            env.get("EXOMONAD_TMUX_SESSION").unwrap(),
            "exo-test-session"
        );
    }

    #[test]
    fn test_common_spawn_env_no_tmux_session() {
        let services = test_services(PathBuf::from("."));
        let service =
            AgentControlService::new(services).with_birth_branch(BirthBranch::from("main"));

        let agent = AgentName::from("worker-1");
        let session_id = BranchName::from("main");
        let role = crate::domain::Role::worker();

        let env = service.common_spawn_env(&agent, &session_id, &role);

        assert!(
            env.get("EXOMONAD_TMUX_SESSION").is_none(),
            "No tmux session should be set when not configured"
        );
    }

    #[tokio::test]
    async fn test_spawn_rollback_removes_worktree_on_drop() {
        let temp_dir = tempfile::tempdir().unwrap();
        let project_dir = temp_dir.path().to_path_buf();
        let services = test_services(project_dir.clone());
        let tmux = super::tmux_ipc::TmuxIpc::new("test");
        let git_wt = services.git_worktree_service().clone();

        let wt_path = project_dir.join("test-wt");
        fs::create_dir_all(&wt_path).await.unwrap();
        assert!(wt_path.exists());

        {
            let mut rollback = SpawnRollback::new(tmux, git_wt);
            rollback.set_worktree(wt_path.clone());
            // Drop without disarming
        }

        // Poll for async cleanup
        let mut cleaned = false;
        for _ in 0..10 {
            if !wt_path.exists() {
                cleaned = true;
                break;
            }
            tokio::time::sleep(std::time::Duration::from_millis(20)).await;
        }
        assert!(cleaned, "Worktree should have been removed by rollback");
    }

    #[tokio::test]
    async fn test_spawn_rollback_disarmed_preserves_state() {
        let temp_dir = tempfile::tempdir().unwrap();
        let project_dir = temp_dir.path().to_path_buf();
        let services = test_services(project_dir.clone());
        let tmux = super::tmux_ipc::TmuxIpc::new("test");
        let git_wt = services.git_worktree_service().clone();

        let wt_path = project_dir.join("test-wt-preserved");
        fs::create_dir_all(&wt_path).await.unwrap();
        assert!(wt_path.exists());

        {
            let mut rollback = SpawnRollback::new(tmux, git_wt);
            rollback.set_worktree(wt_path.clone());
            rollback.disarm();
            // Drop while disarmed
        }

        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
        assert!(
            wt_path.exists(),
            "Worktree should NOT have been removed when disarmed"
        );
    }
}
