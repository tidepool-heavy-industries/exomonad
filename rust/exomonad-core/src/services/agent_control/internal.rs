use super::*;

impl AgentControlService {
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
        branch_name: &str,
        base_branch: &str,
    ) -> Result<()> {
        if worktree_path.exists() {
            info!(path = %worktree_path.display(), "Removing existing workspace for idempotency");
            let git_wt = self.git_wt.clone();
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

        let git_wt = self.git_wt.clone();
        let path = worktree_path.to_path_buf();
        let bookmark = crate::domain::BranchName::from(branch_name);
        let base = crate::domain::BranchName::from(base_branch);
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

        Ok(())
    }

    /// Build the common env vars shared by all spawn functions.
    pub(crate) fn common_spawn_env(
        &self,
        internal_name: &str,
        session_id: &str,
        role: &str,
    ) -> HashMap<String, String> {
        let mut env_vars = HashMap::new();
        env_vars.insert("EXOMONAD_AGENT_ID".to_string(), internal_name.to_string());
        env_vars.insert("EXOMONAD_SESSION_ID".to_string(), session_id.to_string());
        env_vars.insert("EXOMONAD_ROLE".to_string(), role.to_string());
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
    pub(crate) fn emit_agent_started(&self, internal_name: &str) -> Result<()> {
        if let Some(ref session) = self.tmux_session {
            let agent_id = crate::ui_protocol::AgentId::try_from(internal_name.to_string())
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
        prompt: Option<&str>,
        env_vars: HashMap<String, String>,
    ) -> Result<super::tmux_ipc::WindowId> {
        self.new_tmux_window_inner(name, cwd, agent_type, prompt, env_vars, None, None)
            .await
    }

    /// Build the full shell command string for an agent.
    /// Handles: agent CLI + prompt/flags → env var prefix → nix develop wrapping.
    /// Used by both `new_tmux_window_inner` and `new_tmux_pane`.
    pub(crate) fn build_agent_command(
        agent_type: AgentType,
        prompt: Option<&str>,
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
                let mode = claude_flags.and_then(|f| f.permission_mode.as_deref());
                match mode {
                    Some(m) => {
                        flags.push_str(" --permission-mode ");
                        flags.push_str(m);
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
            AgentType::Shoal => String::new(),
        };

        let agent_command = match (prompt, fork_session_id) {
            (Some(p), Some(session_id)) => {
                let escaped_prompt = Self::escape_for_shell_command(p);
                let escaped_session = Self::escape_for_shell_command(session_id);
                format!(
                    "{}{} --resume {} --fork-session {}",
                    cmd, perms_flags, escaped_session, escaped_prompt
                )
            }
            (Some(p), None) => {
                let escaped_prompt = Self::escape_for_shell_command(p);
                let flag = agent_type.prompt_flag();
                if flag.is_empty() {
                    format!("{}{} {}", cmd, perms_flags, escaped_prompt)
                } else {
                    format!("{}{} {} {}", cmd, perms_flags, flag, escaped_prompt)
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

    pub(crate) async fn new_tmux_window_inner(
        &self,
        name: &str,
        cwd: &Path,
        agent_type: AgentType,
        prompt: Option<&str>,
        env_vars: HashMap<String, String>,
        fork_session_id: Option<&str>,
        claude_flags: Option<&ClaudeSpawnFlags>,
    ) -> Result<super::tmux_ipc::WindowId> {
        info!(name, cwd = %cwd.display(), agent_type = ?agent_type, fork = fork_session_id.is_some(), "Creating tmux window");

        let full_command = Self::build_agent_command(
            agent_type,
            prompt,
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

        let window_id = tokio::task::spawn_blocking(move || {
            tmux.new_window(&window_name, &window_cwd, &shell, &full_command)
        })
        .await
        .context("tokio task join error")?
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

        let result = timeout(
            TMUX_TIMEOUT,
            tokio::task::spawn_blocking(move || tmux.list_windows()),
        )
        .await
        .map_err(|_| {
            anyhow!(
                "tmux list-windows timed out after {}s",
                TMUX_TIMEOUT.as_secs()
            )
        })?
        .context("tokio task join error")?;

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

        let window_id = tokio::task::spawn_blocking(move || {
            let windows = tmux.list_windows()?;
            windows
                .into_iter()
                .find(|w| w.window_name == window_name)
                .map(|w| w.window_id)
                .ok_or_else(|| anyhow!("Window not found: {}", window_name))
        })
        .await
        .context("tokio task join error")??;

        let tmux = self.tmux()?;
        timeout(
            TMUX_TIMEOUT,
            tokio::task::spawn_blocking(move || tmux.kill_window(&window_id)),
        )
        .await
        .map_err(|_| {
            anyhow::Error::new(TimeoutError {
                message: format!(
                    "tmux kill-window timed out after {}s",
                    TMUX_TIMEOUT.as_secs()
                ),
            })
        })?
        .context("tokio task join error")?
        .context("tmux kill-window failed")?;

        info!(name, "tmux kill-window successful");
        Ok(())
    }

    pub(crate) async fn new_tmux_pane(
        &self,
        name: &str,
        cwd: &Path,
        agent_type: AgentType,
        prompt: Option<&str>,
        env_vars: HashMap<String, String>,
        parent_window_name: Option<&str>,
        claude_flags: Option<&ClaudeSpawnFlags>,
    ) -> Result<super::tmux_ipc::PaneId> {
        info!(name, cwd = %cwd.display(), agent_type = ?agent_type, parent = ?parent_window_name, "Creating tmux pane");

        let full_command = Self::build_agent_command(
            agent_type,
            prompt,
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
            let t = tmux.clone();
            let windows = tokio::task::spawn_blocking(move || t.list_windows())
                .await
                .context("tokio task join error")?
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
            let t = tmux.clone();
            let windows = tokio::task::spawn_blocking(move || t.list_windows())
                .await
                .context("tokio task join error")?
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
        let t = tmux.clone();
        let pane_id = tokio::task::spawn_blocking(move || {
            let pane_id = t.split_window(&target_window, &pane_cwd, &shell, &full_command)?;
            // Rebalance panes into a grid after each split to prevent
            // exponential height decay (60 → 29 → 14 → 6 → 2 → 1 lines).
            if let Err(e) = t.select_layout(&target_window, "tiled") {
                tracing::warn!(error = %e, "Failed to apply tiled layout (non-fatal)");
            }
            Ok::<_, anyhow::Error>(pane_id)
        })
        .await
        .context("tokio task join error")?
        .context("Failed to create tmux pane")?;

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
        role: &str,
    ) -> Result<()> {
        let agent_name = agent_dir
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown");

        let mcp_content = Self::generate_mcp_config(agent_name, agent_type, role);

        match agent_type {
            AgentType::Claude => {
                fs::write(agent_dir.join(".mcp.json"), mcp_content).await?;
                info!(agent_dir = %agent_dir.display(), role = %role, "Wrote .mcp.json for Claude agent");
            }
            AgentType::Gemini => {
                let gemini_dir = agent_dir.join(".gemini");
                fs::create_dir_all(&gemini_dir).await?;
                fs::write(gemini_dir.join("settings.json"), mcp_content).await?;
                info!(agent_dir = %agent_dir.display(), role = %role, "Wrote .gemini/settings.json for Gemini agent");
            }
            AgentType::Shoal => {
                let exo_dir = agent_dir.join(".exo");
                fs::create_dir_all(&exo_dir).await?;
                fs::write(exo_dir.join("mcp.json"), mcp_content).await?;
                info!(agent_dir = %agent_dir.display(), role = %role, "Wrote .exo/mcp.json for Shoal agent");
            }
        }
        Ok(())
    }

    /// Symlink server socket into worktree so agents find it without walk-up.
    pub(crate) async fn create_socket_symlink(&self, worktree_path: &Path) {
        let source = self.project_dir.join(".exo/server.sock");
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

    /// Generate MCP configuration JSON for an agent using stdio transport.
    pub(crate) fn generate_mcp_config(name: &str, agent_type: AgentType, role: &str) -> String {
        match agent_type {
            AgentType::Claude => serde_json::to_string_pretty(&serde_json::json!({
                "mcpServers": {
                    "exomonad": {
                        "type": "stdio",
                        "command": "exomonad",
                        "args": ["mcp-stdio", "--role", role, "--name", name]
                    }
                }
            }))
            .unwrap(),
            AgentType::Gemini => serde_json::to_string_pretty(&serde_json::json!({
                "mcpServers": {
                    "exomonad": {
                        "type": "stdio",
                        "command": "exomonad",
                        "args": ["mcp-stdio", "--role", role, "--name", name]
                    }
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
            }))
            .unwrap(),
            AgentType::Shoal => serde_json::to_string_pretty(&serde_json::json!({
                "command": "exomonad",
                "args": ["mcp-stdio", "--role", role, "--name", name]
            }))
            .unwrap(),
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
    /// This is suitable for: sh -c "claude --prompt '...'"
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

    #[test]
    fn test_escape_for_shell_command_simple() {
        assert_eq!(
            AgentControlService::escape_for_shell_command("hello world"),
            "'hello world'"
        );
    }

    #[test]
    fn test_escape_for_shell_command_with_quote() {
        // Standard shell escaping: end quote, escaped quote, start quote
        // 'user'\''s issue' = 'user' + \' + 's issue'
        assert_eq!(
            AgentControlService::escape_for_shell_command("user's issue"),
            r"'user'\''s issue'"
        );
    }

    #[test]
    fn test_escape_for_shell_command_shell_chars() {
        let result = AgentControlService::escape_for_shell_command("Test $VAR and `code`");
        assert!(result.contains("$VAR"));
        assert!(result.contains("`code`"));
        assert_eq!(result, "'Test $VAR and `code`'");
    }

    #[test]
    fn test_build_initial_prompt_format() {
        let prompt = AgentControlService::build_initial_prompt(
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
        let prompt = AgentControlService::build_initial_prompt(
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
        let config =
            AgentControlService::generate_mcp_config("test-claude", AgentType::Claude, "tl");
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
        let config =
            AgentControlService::generate_mcp_config("test-gemini", AgentType::Gemini, "dev");
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
        let settings = AgentControlService::generate_gemini_worker_settings("test-worker");

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

    #[tokio::test]
    async fn test_create_socket_symlink() {
        let temp_dir = tempfile::tempdir().unwrap();
        let project_dir = temp_dir.path().to_path_buf();
        let exo_dir = project_dir.join(".exo");
        tokio::fs::create_dir_all(&exo_dir).await.unwrap();
        tokio::fs::write(exo_dir.join("server.sock"), "placeholder")
            .await
            .unwrap();

        let git_wt = Arc::new(crate::services::git_worktree::GitWorktreeService::new(
            project_dir.clone(),
        ));
        let service = AgentControlService::new(project_dir.clone(), None, git_wt);

        let worktree = temp_dir.path().join("child-wt");
        tokio::fs::create_dir_all(&worktree).await.unwrap();

        service.create_socket_symlink(&worktree).await;

        let link = worktree.join(".exo/server.sock");
        assert!(link.exists(), "Symlink should exist");
        let target = tokio::fs::read_link(&link).await.unwrap();
        assert_eq!(target, project_dir.join(".exo/server.sock"));
    }
}
