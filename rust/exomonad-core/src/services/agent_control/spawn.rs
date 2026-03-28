use super::*;

impl AgentControlService {
    /// Spawn an agent for a GitHub issue.
    ///
    /// This is the high-level semantic operation that:
    /// 1. Fetches issue from GitHub
    /// 2. Creates agent directory (.exo/agents/{agent_id}/)
    /// 3. Writes .mcp.json pointing to the Unix socket server
    /// 4. Opens tmux window with agent command (cwd = project_dir)
    #[tracing::instrument(skip(self, options), fields(issue_id = %issue_number.as_u64()))]
    pub async fn spawn_agent(
        &self,
        issue_number: IssueNumber,
        options: &SpawnOptions,
        caller_bb: &BirthBranch,
    ) -> Result<SpawnResult> {
        let issue_id_log = issue_number.as_u64().to_string();
        info!(issue_id = %issue_id_log, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_agent");

        let result = timeout(SPAWN_TIMEOUT, async {
            // Validate we're in tmux
            self.resolve_tmux_session()?;

            // Resolve effective project dir.
            let effective_project_dir = self.effective_project_dir(options.subrepo.as_deref())?;

            // Get GitHub service
            let github = self
                .github
                .as_ref()
                .ok_or_else(|| anyhow!("GitHub service not available (GITHUB_TOKEN not set)"))?;

            // Fetch issue from GitHub
            let issue_id = issue_number.as_u64().to_string();
            info!(issue_id, "Fetching issue from GitHub");
            let repo = Repo {
                owner: options.owner.clone(),
                name: options.repo.clone(),
            };
            let issue = github.get_issue(&repo, issue_number).await?;

            // Generate slug and agent name
            let slug = slugify(&issue.title);
            let agent_suffix = options.agent_type.suffix();
            let internal_name = format!("gh-{}-{}-{}", issue_id, slug, agent_suffix);

            // Determine base branch (use birth_branch for root detection)
            let default_base = self.birth_branch.as_parent_branch().to_string();
            let base = options
                .base_branch
                .as_ref()
                .map(|b| b.as_str().to_string())
                .unwrap_or(default_base);
            let branch_name = if self.birth_branch.depth() == 0 {
                format!("gh-{}/{}-{}", issue_id, slug, agent_suffix)
            } else {
                format!("{}/{}-{}", base, slug, agent_suffix)
            };

            // Create worktree
            let worktree_path = self.worktree_base.join(&internal_name);

            self.create_worktree_checked(&worktree_path, &branch_name, &base)
                .await?;

            // Use worktree path as agent_dir
            let agent_dir = worktree_path;

            // Write .mcp.json for the agent
            let role = match options.agent_type {
                AgentType::Claude => "tl",
                AgentType::Gemini => "dev",
                AgentType::Shoal => "shoal",
                AgentType::Process => unreachable!("Process agents are not spawned via effects"),
            };
            self.write_agent_mcp_config(
                &effective_project_dir,
                &agent_dir,
                options.agent_type,
                role,
            )
            .await?;

            // Build initial prompt
            let issue_url = format!(
                "https://github.com/{}/{}/issues/{}",
                options.owner, options.repo, issue_id
            );
            let initial_prompt = Self::build_initial_prompt(
                &issue_id,
                &issue.title,
                &issue.body,
                &issue.labels,
                &issue_url,
            );

            tracing::info!(
                issue_id,
                prompt_length = initial_prompt.len(),
                "Built initial prompt for agent"
            );

            // tmux display name (emoji + short format)
            let display_name = options.agent_type.display_name(&issue_id, &slug);

            let env_vars = self.common_spawn_env(
                &internal_name,
                self.effective_birth_branch(Some(caller_bb)).as_ref(),
                role,
            );

            // Open tmux window with cwd = worktree_path
            let window_id = self
                .new_tmux_window(
                    &display_name,
                    &agent_dir,
                    options.agent_type,
                    Some(&initial_prompt),
                    env_vars,
                )
                .await?;

            // Store window_id for message delivery and cleanup
            let routing = RoutingInfo::window(window_id.as_str());
            self.finalize_spawn(&internal_name, routing).await?;

            self.emit_agent_started(&internal_name)?;

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: agent_dir.clone(),
                tab_name: internal_name,
                issue_title: issue.title,
                agent_type: options.agent_type,
            })
        })
        .await
        .map_err(|_| {
            let msg = format!("spawn_agent timed out after {}s", SPAWN_TIMEOUT.as_secs());
            warn!(issue_id = %issue_id_log, error = %msg, "spawn_agent timed out");
            anyhow::Error::new(TimeoutError { message: msg })
        })??;

        info!(issue_id = %issue_id_log, "spawn_agent completed successfully");
        Ok(result)
    }

    /// Spawn multiple agents.
    #[tracing::instrument(skip(self, options))]
    pub async fn spawn_agents(
        &self,
        issue_ids: &[String],
        options: &SpawnOptions,
        caller_bb: &BirthBranch,
    ) -> BatchSpawnResult {
        let mut result = BatchSpawnResult {
            spawned: Vec::new(),
            failed: Vec::new(),
        };

        for issue_id_str in issue_ids {
            // Parse issue ID
            match IssueNumber::try_from(issue_id_str.clone()) {
                Ok(issue_number) => {
                    match self.spawn_agent(issue_number, options, caller_bb).await {
                        Ok(spawn_result) => result.spawned.push(spawn_result),
                        Err(e) => {
                            warn!(issue_id = issue_id_str, error = %e, "Failed to spawn agent");
                            result.failed.push((issue_id_str.clone(), e.to_string()));
                        }
                    }
                }
                Err(e) => {
                    warn!(issue_id = issue_id_str, error = %e, "Invalid issue number");
                    result.failed.push((issue_id_str.clone(), e.to_string()));
                }
            }
        }

        result
    }

    /// Spawn a named teammate with a direct prompt.
    ///
    /// Idempotent on teammate name: if already running, returns existing info.
    /// If config entry exists but tmux window is dead, cleans stale entry and respawns.
    /// No per-agent directories or MCP configs — agents share the repo's config.
    /// State lives in Teams config.json + tmux window only.
    #[tracing::instrument(skip(self, options), fields(name = %options.name.as_str()))]
    pub async fn spawn_gemini_teammate(
        &self,
        options: &SpawnGeminiTeammateOptions,
        caller_bb: &BirthBranch,
    ) -> Result<SpawnResult> {
        info!(name = %options.name, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_gemini_teammate");

        let result = timeout(SPAWN_TIMEOUT, async {
            self.resolve_tmux_session()?;

            let effective_project_dir = self.effective_project_dir(options.subrepo.as_deref())?;

            // Sanitize name for internal use
            let slug = slugify(options.name.as_str());
            let agent_suffix = options.agent_type.suffix();
            let internal_name = format!("{}-{}", slug, agent_suffix);
            let display_name = format!("{} {}", options.agent_type.emoji(), slug);

            // Idempotency check: if tmux window is alive, return existing info
            let tab_alive = self.is_tmux_window_alive(&display_name).await;

            info!(
                name = %options.name,
                internal_name,
                tab_alive,
                "Idempotency check"
            );

            if tab_alive {
                info!(name = %options.name, "Teammate already running, returning existing");
                // TODO: Return actual worktree path if possible, but for now empty is fine as it's just info
                return Ok(SpawnResult {
                    agent_dir: PathBuf::new(),
                    tab_name: internal_name,
                    issue_title: options.name.to_string(),
                    agent_type: options.agent_type,
                });
            }

            // Determine base branch
            let base_branch = if let Some(ref b) = options.base_branch {
                b.to_string()
            } else {
                // Default to current branch
                let current_branch_output = Command::new("git")
                    .args(["rev-parse", "--abbrev-ref", "HEAD"])
                    .current_dir(&effective_project_dir)
                    .output()
                    .await
                    .context("Failed to get current branch")?;
                String::from_utf8_lossy(&current_branch_output.stdout)
                    .trim()
                    .to_string()
            };

            // Use '.' separator to avoid directory/file conflicts in git refs
            // and avoid ambiguity with '-' word separators in slugs.
            let branch_name = format!("{}.{}", base_branch, slug);
            let worktree_path = self.worktree_base.join(&internal_name);

            self.create_worktree_checked(&worktree_path, &branch_name, &base_branch)
                .await?;

            let mut env_vars = self.common_spawn_env(
                &internal_name,
                self.effective_birth_branch(Some(caller_bb)).as_ref(),
                "dev",
            );

            // Write per-agent MCP config into the worktree
            self.write_agent_mcp_config(
                &effective_project_dir,
                &worktree_path,
                options.agent_type,
                "dev",
            )
            .await?;

            // For Gemini agents, point at worktree settings via env var and pre-trust folder
            if options.agent_type == AgentType::Gemini {
                let settings_path = worktree_path.join(".gemini").join("settings.json");
                env_vars.insert(
                    "GEMINI_CLI_SYSTEM_SETTINGS_PATH".to_string(),
                    settings_path.to_string_lossy().to_string(),
                );
                Self::gemini_trust_folder(&worktree_path).await;
            }

            let window_id = self
                .new_tmux_window(
                    &display_name,
                    &worktree_path,
                    options.agent_type,
                    Some(&options.prompt),
                    env_vars,
                )
                .await?;

            // Store window_id for message delivery and cleanup
            let routing = RoutingInfo::window(window_id.as_str());
            self.finalize_spawn(&internal_name, routing).await?;

            self.emit_agent_started(&internal_name)?;

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: PathBuf::new(),
                tab_name: internal_name,
                issue_title: options.name.to_string(),
                agent_type: options.agent_type,
            })
        })
        .await
        .map_err(|_| {
            let msg = format!(
                "spawn_gemini_teammate timed out after {}s",
                SPAWN_TIMEOUT.as_secs()
            );
            warn!(name = %options.name, error = %msg, "spawn_gemini_teammate timed out");
            anyhow::Error::new(TimeoutError { message: msg })
        })??;

        info!(name = %options.name, "spawn_gemini_teammate completed successfully");
        Ok(result)
    }

    /// Generate settings.json content for a Gemini worker.
    ///
    /// Constructs the JSON configuration including MCP server connection and lifecycle hooks.
    /// Note: Gemini hooks must be PascalCase (e.g. AfterAgent).
    ///
    /// `context_path` is an optional absolute path to the role context file.
    /// Using an absolute path ensures workers spawned from worktrees can find the context.
    pub(crate) fn generate_gemini_worker_settings(
        agent_name: &str,
        context_path: Option<&Path>,
        extra_mcp_servers: &HashMap<String, serde_json::Value>,
    ) -> serde_json::Value {
        let mut context_files = vec![serde_json::Value::String("GEMINI.md".to_string())];
        if let Some(path) = context_path {
            context_files.push(serde_json::Value::String(
                path.to_string_lossy().to_string(),
            ));
        }
        let mut settings = serde_json::json!({
            "mcpServers": {
                "exomonad": {
                    "type": "stdio",
                    "command": "exomonad",
                    "args": ["mcp-stdio", "--role", "worker", "--name", agent_name]
                }
            },
            "context": {
                "fileName": context_files
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
                                "command": "exomonad hook worker-exit --runtime gemini"
                            }
                        ]
                    }
                ]
            }
        });
        if let Some(servers) = settings["mcpServers"].as_object_mut() {
            for (k, v) in extra_mcp_servers {
                servers.insert(k.clone(), v.clone());
            }
        }
        settings
    }

    /// Spawn a Gemini worker agent (Phase 2/3).
    ///
    /// Creates a new git worktree and branch for isolation.
    #[instrument(skip_all, fields(name = %options.name, agent_type = "gemini"))]
    pub async fn spawn_worker(
        &self,
        options: &SpawnWorkerOptions,
        ctx: &crate::effects::EffectContext,
    ) -> Result<SpawnResult> {
        info!(name = %options.name, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_worker");

        let result = timeout(SPAWN_TIMEOUT, async {
            self.resolve_tmux_session()?;

            // Sanitize name for internal use
            let slug = slugify(&options.name);
            let internal_name = format!("{}-gemini", slug);
            let display_name = format!("{} {}", AgentType::Gemini.emoji(), slug);

            // Idempotency: check if agent config dir already exists (workers are panes, not tabs)
            let agent_config_dir = self.project_dir
                .join(".exo")
                .join("agents")
                .join(&internal_name);
            let settings_path = agent_config_dir.join("settings.json");
            if settings_path.exists() {
                // Check tmux pane liveness — settings.json can outlive the pane
                let pane_alive = match RoutingInfo::read_from_dir(&agent_config_dir).await {
                    Ok(routing) => match routing.pane_id {
                        Some(ref pid) => match PaneId::parse(pid) {
                            Ok(pane_id) => self.tmux()?.pane_exists(&pane_id).await.unwrap_or(false),
                            Err(_) => false,
                        },
                        None => false,
                    },
                    Err(_) => false,
                };
                if pane_alive {
                    info!(name = %options.name, "Worker pane still alive, returning existing");
                    return Ok(SpawnResult {
                        agent_dir: PathBuf::new(),
                        tab_name: internal_name,
                        issue_title: options.name.clone(),
                        agent_type: AgentType::Gemini,
                    });
                }
                // Stale: pane is dead but config dir remains. Clean up and respawn.
                info!(name = %options.name, path = %agent_config_dir.display(), "Stale worker detected (pane dead), cleaning up and respawning");
                if let Err(e) = fs::remove_dir_all(&agent_config_dir).await {
                    warn!(name = %options.name, error = %e, "Failed to clean up stale worker config dir");
                }
            }

            let mut env_vars = self.common_spawn_env(&internal_name, self.effective_birth_branch(Some(&ctx.birth_branch)).as_ref(), "worker");

            // Write Gemini settings to worker config dir in project root
            fs::create_dir_all(&agent_config_dir).await?;

            // Write parent's birth_branch so the server can resolve it for notify_parent routing.
            // Workers don't have worktrees, so git-based resolution fails. This file is the fallback.
            let parent_bb = self.effective_birth_branch(Some(&ctx.birth_branch));
            fs::write(agent_config_dir.join(".birth_branch"), parent_bb.as_str()).await?;
            let context_path = self.resolve_role_context("worker");
            let settings = Self::generate_gemini_worker_settings(&internal_name, context_path.as_deref(), &self.extra_mcp_servers);
            fs::write(&settings_path, serde_json::to_string_pretty(&settings)?).await?;
            info!(
                path = %settings_path.display(),
                name = %internal_name,
                "Wrote worker Gemini settings to agent config dir"
            );

            env_vars.insert(
                "GEMINI_CLI_SYSTEM_SETTINGS_PATH".to_string(),
                settings_path.to_string_lossy().to_string(),
            );

            // Pre-trust the caller's worktree for Gemini
            let caller_worktree_for_trust = self.project_dir.join(&ctx.working_dir);
            Self::gemini_trust_folder(&caller_worktree_for_trust).await;

            // Resolve caller's context (tab and worktree) from its context.
            let caller_tab = resolve_own_tab_name(ctx);
            let caller_worktree = ctx.working_dir.clone();
            let absolute_worktree = self.project_dir.join(caller_worktree);

            // Worker role context is loaded via context.fileName in settings.json.
            // Prompt goes through a temp file to avoid shell quoting issues.

            // Write routing info so send_message can target this pane correctly.
            // Workers are panes in the parent's tab — pane_id is the stable identifier
            // Spawn pane in caller's tab, cwd = caller's worktree
            let pane_id = self.new_tmux_pane(
                &display_name,
                &absolute_worktree,
                AgentType::Gemini,
                Some(&options.prompt),
                env_vars,
                Some(&caller_tab),
                Some(&options.claude_flags),
            )
            .await?;

            // Store pane_id for message delivery and cleanup
            let routing = RoutingInfo::pane(pane_id.as_str(), &caller_tab);
            self.finalize_spawn(&internal_name, routing)
                .await?;

            self.emit_agent_started(&internal_name)?;

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: PathBuf::new(),
                tab_name: internal_name,
                issue_title: options.name.clone(),
                agent_type: AgentType::Gemini,
            })
        })
        .await
        .map_err(|_| {
            let msg = format!("spawn_worker timed out after {}s", SPAWN_TIMEOUT.as_secs());
            warn!(name = %options.name, error = %msg, "spawn_worker timed out");
            anyhow::Error::new(TimeoutError { message: msg })
        })??;

        info!(name = %options.name, "spawn_worker completed successfully");
        Ok(result)
    }

    /// Spawn a subtree agent (Claude-only) in a new git worktree.
    #[instrument(skip_all, fields(slug = %options.branch_name, agent_type = "claude"))]
    pub async fn spawn_subtree(
        &self,
        options: &SpawnSubtreeOptions,
        caller_bb: &BirthBranch,
    ) -> Result<SpawnResult> {
        info!(branch_name = %options.branch_name, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_subtree");

        let result = timeout(SPAWN_TIMEOUT, async {
            self.resolve_tmux_session()?;

            let effective_birth = self.effective_birth_branch(Some(caller_bb));

            // Depth check using typed birth-branch.
            let depth = effective_birth.depth();

            if depth >= 3 {
                return Err(anyhow!("Subtree depth limit reached (max 3). Current birth-branch: {}, depth: {}", effective_birth, depth));
            }

            let effective_project_dir = &self.project_dir;

            // Sanitize branch name for internal use
            let slug = slugify(&options.branch_name);
            let agent_type = options.agent_type;
            let agent_suffix = agent_type.suffix();
            let internal_name = format!("{}-{}", slug, agent_suffix);
            let display_name = format!("{} {}", agent_type.emoji(), slug);

            // Idempotency check: if tmux window is alive, return existing info
            let tab_alive = self.is_tmux_window_alive(&display_name).await;
            if tab_alive {
                info!(slug = %slug, "Subtree already running, returning existing");
                return Ok(SpawnResult {
                    agent_dir: self.worktree_base.join(&slug),
                    tab_name: internal_name,
                    issue_title: options.branch_name.clone(),
                    agent_type,
                });
            }

            // Parent branch derived from typed birth-branch.
            let current_branch = effective_birth.as_parent_branch();

            // Push parent branch so child PRs can reference it as base
            ensure_branch_pushed(&self.git_wt, current_branch, effective_project_dir).await;

            // Branch: {current_branch}.{slug}
            let child_birth = effective_birth.child(&slug);
            let branch_name = child_birth.to_string();

            // Path resolution: working_dir overrides the default worktree location.
            // standalone_repo: git init (fresh .git boundary) instead of git worktree add.
            // These are orthogonal: working_dir controls WHERE, standalone_repo controls HOW.
            let (worktree_path, is_custom_dir) = if let Some(ref custom_dir) = options.working_dir {
                (custom_dir.clone(), true)
            } else {
                (self.worktree_base.join(&slug), false)
            };

            if options.standalone_repo {
                self.init_standalone_repo(&worktree_path).await?;
                if !options.allowed_dirs.is_empty() {
                    self.copy_allowed_dirs(&worktree_path, &options.allowed_dirs).await?;
                }
            } else if !is_custom_dir {
                self.create_worktree_checked(&worktree_path, &branch_name, current_branch).await?;
            }

            self.create_socket_symlink(&worktree_path).await;

            let role = options.role.as_deref().unwrap_or("tl");

            // Copy role context into worktree.
            // Must be a copy, not a symlink — symlinks escape the worktree boundary
            // and cause Claude Code to discover parent context files.
            if let Some(context_src) = self.resolve_role_context(role) {
                match agent_type {
                    AgentType::Claude => {
                        // Claude: .claude/rules/exomonad_role.md (loaded as rules file)
                        let rules_dir = worktree_path.join(".claude/rules");
                        let _ = fs::create_dir_all(&rules_dir).await;
                        let dest = rules_dir.join("exomonad_role.md");
                        let _ = fs::remove_file(&dest).await;
                        match fs::copy(&context_src, &dest).await {
                            Ok(_) => info!(role = %role, src = %context_src.display(), dest = %dest.display(), "Copied role context into worktree"),
                            Err(e) => warn!(role = %role, error = %e, "Failed to copy role context (non-fatal)"),
                        }
                    }
                    AgentType::Gemini => {
                        // Gemini: .exo/roles/{wasm}/context/{role}.md (matched by context.fileName in settings)
                        let dest_dir = worktree_path.join(format!(".exo/roles/{}/context", self.wasm_name));
                        let _ = fs::create_dir_all(&dest_dir).await;
                        let dest = dest_dir.join(format!("{}.md", role));
                        let _ = fs::remove_file(&dest).await;
                        match fs::copy(&context_src, &dest).await {
                            Ok(_) => info!(role = %role, src = %context_src.display(), dest = %dest.display(), "Copied role context into Gemini worktree"),
                            Err(e) => warn!(role = %role, error = %e, "Failed to copy Gemini role context (non-fatal)"),
                        }
                    }
                    AgentType::Shoal | AgentType::Process => {}
                }
            }

            let mut env_vars = self.common_spawn_env(&internal_name, &branch_name, role);
            // Enable Claude Code Agent Teams for native inter-agent messaging
            env_vars.insert(
                "CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS".to_string(),
                "1".to_string(),
            );
            self.write_agent_mcp_config(effective_project_dir, &worktree_path, agent_type, role)
                .await?;

            // Write .claude/settings.local.json with hooks (SessionStart registers UUID for --fork-session)
            let binary_path = crate::util::find_exomonad_binary();
            crate::hooks::HookConfig::write_persistent(&worktree_path, &binary_path, options.permissions.as_ref(), Some(&self.project_dir))
                .map_err(|e| anyhow!("Failed to write hook config in worktree: {}", e))?;
            info!(worktree = %worktree_path.display(), "Wrote hook configuration for spawned Claude agent");

            // Symlink Claude project dir so child can discover parent's sessions for --fork-session.
            // Claude Code encodes paths via [^a-zA-Z0-9] → '-' (lossy regex replacement).
            // Without this symlink, --resume --fork-session fails with "no conversation ID found".
            {
                let claude_projects_dir = dirs::home_dir()
                    .unwrap_or_default()
                    .join(".claude")
                    .join("projects");
                let encode_path = |p: &Path| -> String {
                    p.to_string_lossy()
                        .chars()
                        .map(|c| if c.is_ascii_alphanumeric() { c } else { '-' })
                        .collect()
                };
                let canonical_project_dir = self.project_dir.canonicalize().unwrap_or_else(|_| self.project_dir.clone());
                let parent_encoded = encode_path(&canonical_project_dir);
                let worktree_encoded = encode_path(&worktree_path);
                let parent_project = claude_projects_dir.join(&parent_encoded);
                let child_project = claude_projects_dir.join(&worktree_encoded);
                if parent_project.exists() && !child_project.exists() {
                    match std::os::unix::fs::symlink(&parent_project, &child_project) {
                        Ok(()) => info!(
                            parent = %parent_encoded,
                            child = %worktree_encoded,
                            "Symlinked Claude project dir for session inheritance"
                        ),
                        Err(e) => warn!(
                            parent = %parent_encoded,
                            child = %worktree_encoded,
                            error = %e,
                            "Failed to symlink Claude project dir (fork-session may not work)"
                        ),
                    }
                }
            }

            // Build task prompt with worktree context warning
            let mut task_with_context = format!(
                "You are now in worktree {} on branch {}. All file paths from your inherited context are STALE — use relative paths only and re-read files before editing.\n\n{}",
                worktree_path.display(), branch_name, options.task
            );

            if options.standalone_repo && !options.allowed_dirs.is_empty() {
                task_with_context.push_str("\n\nShared technical dependencies are available as read-only reference in `.exo/context/`. Do not modify files in this directory.");
            }

            // Determine fork mode from parent_session_id
            let fork_id = options.parent_session_id.as_ref().map(|id| id.as_str());

            // Open tmux window with cwd = worktree_path
            let agent_config_dir = self.project_dir.join(".exo").join("agents").join(&internal_name);
            let window_id = match self.new_tmux_window_inner(
                &display_name,
                &worktree_path,
                agent_type,
                Some(&task_with_context),
                env_vars,
                fork_id,
                Some(&options.claude_flags),
            )
            .await {
                Ok(wid) => wid,
                Err(e) => {
                    warn!(name = %slug, error = %e, "tmux window creation failed, rolling back");
                    let _ = fs::remove_dir_all(&agent_config_dir).await;
                    // Remove worktree if it was created
                    if worktree_path.exists() {
                        let git_wt = self.git_wt.clone();
                        let path = worktree_path.clone();
                        let _ = tokio::task::spawn_blocking(move || git_wt.remove_workspace(&path)).await;
                    }
                    return Err(e);
                }
            };

            // Store window_id for message delivery and cleanup
            let routing = RoutingInfo::window(window_id.as_str());
            self.finalize_spawn(&internal_name, routing)
                .await?;

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: worktree_path.clone(),
                tab_name: internal_name,
                issue_title: options.branch_name.clone(),
                agent_type,
            })
        })
        .await
        .map_err(|_| {
            let msg = format!("spawn_subtree timed out after {}s", SPAWN_TIMEOUT.as_secs());
            warn!(branch_name = %options.branch_name, error = %msg, "spawn_subtree timed out");
            anyhow::Error::new(TimeoutError { message: msg })
        })??;

        info!(branch_name = %options.branch_name, "spawn_subtree completed successfully");
        Ok(result)
    }

    /// Spawn a Gemini leaf agent in a new git worktree.
    #[instrument(skip_all, fields(slug = %options.branch_name, agent_type = "gemini"))]
    pub async fn spawn_leaf_subtree(
        &self,
        options: &SpawnLeafOptions,
        caller_bb: &BirthBranch,
    ) -> Result<SpawnResult> {
        info!(branch_name = %options.branch_name, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_leaf_subtree");

        let result = timeout(SPAWN_TIMEOUT, async {
            self.resolve_tmux_session()?;

            // No depth check for leaf nodes.

            let effective_birth = self.effective_birth_branch(Some(caller_bb));
            let effective_project_dir = &self.project_dir;

            // Parent branch derived from typed birth-branch.
            let current_branch = effective_birth.as_parent_branch().to_string();

            // Sanitize branch name
            let slug = slugify(&options.branch_name);
            let agent_type = options.agent_type;
            let agent_suffix = agent_type.suffix();
            let internal_name = format!("{}-{}", slug, agent_suffix);
            let display_name = format!("{} {}", agent_type.emoji(), slug);

            // Idempotency check
            let tab_alive = self.is_tmux_window_alive(&display_name).await;
            if tab_alive {
                info!(slug = %slug, "Leaf subtree already running, returning existing");
                return Ok(SpawnResult {
                    agent_dir: self.worktree_base.join(&slug),
                    tab_name: internal_name,
                    issue_title: options.branch_name.clone(),
                    agent_type,
                });
            }

            // Push parent branch so child PRs can reference it as base
            ensure_branch_pushed(&self.git_wt, &current_branch, effective_project_dir).await;

            let child_birth = effective_birth.child(&slug);
            let branch_name = child_birth.to_string();

            let worktree_path = self.worktree_base.join(&slug);

            if options.standalone_repo {
                self.init_standalone_repo(&worktree_path).await?;
                if !options.allowed_dirs.is_empty() {
                    self.copy_allowed_dirs(&worktree_path, &options.allowed_dirs).await?;
                }
            } else {
                self.create_worktree_checked(&worktree_path, &branch_name, &current_branch).await?;
            }

            self.create_socket_symlink(&worktree_path).await;

            let role = options.role.as_deref().unwrap_or("dev");
            let mut env_vars = self.common_spawn_env(&internal_name, &branch_name, role);
            self.write_agent_mcp_config(effective_project_dir, &worktree_path, agent_type, role)
                .await?;

            // Set GEMINI_CLI_SYSTEM_SETTINGS_PATH and pre-trust folder
            let settings_path = worktree_path.join(".gemini").join("settings.json");
            env_vars.insert(
                "GEMINI_CLI_SYSTEM_SETTINGS_PATH".to_string(),
                settings_path.to_string_lossy().to_string(),
            );
            Self::gemini_trust_folder(&worktree_path).await;

            let mut task = options.task.clone();
            if options.standalone_repo && !options.allowed_dirs.is_empty() {
                task.push_str("\n\nShared technical dependencies are available as read-only reference in `.exo/context/`. Do not modify files in this directory.");
            }

            // Open tmux window (not pane)
            // Task already includes leaf completion protocol — rendered by Haskell Prompt builder.
            let agent_config_dir = self.project_dir.join(".exo").join("agents").join(&internal_name);
            let window_id = match self.new_tmux_window(
                &display_name,
                &worktree_path,
                agent_type,
                Some(&task),
                env_vars,
            )
            .await {
                Ok(wid) => wid,
                Err(e) => {
                    warn!(name = %slug, error = %e, "tmux window creation failed, rolling back");
                    let _ = fs::remove_dir_all(&agent_config_dir).await;
                    // Remove worktree if it was created
                    if worktree_path.exists() {
                        let git_wt = self.git_wt.clone();
                        let path = worktree_path.clone();
                        let _ = tokio::task::spawn_blocking(move || git_wt.remove_workspace(&path)).await;
                    }
                    return Err(e);
                }
            };

            // Store window_id for message delivery and cleanup
            let routing = RoutingInfo::window(window_id.as_str());
            self.finalize_spawn(&internal_name, routing)
                .await?;

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: worktree_path.clone(),
                tab_name: internal_name,
                issue_title: options.branch_name.clone(),
                agent_type,
            })
        })
        .await
        .map_err(|_| {
            let msg = format!("spawn_leaf_subtree timed out after {}s", SPAWN_TIMEOUT.as_secs());
            warn!(branch_name = %options.branch_name, error = %msg, "spawn_leaf_subtree timed out");
            anyhow::Error::new(TimeoutError { message: msg })
        })??;

        info!(branch_name = %options.branch_name, "spawn_leaf_subtree completed successfully");
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_copy_allowed_dirs_validation() {
        let temp_dir = tempfile::tempdir().unwrap();
        let project_dir = temp_dir.path().to_path_buf();

        // Setup source dirs
        let shared_context = project_dir.join("shared-context");
        fs::create_dir_all(&shared_context).await.unwrap();
        fs::write(shared_context.join("ref.txt"), "context data")
            .await
            .unwrap();

        let agent_wt = project_dir.join("agent-wt");
        fs::create_dir_all(&agent_wt).await.unwrap();

        let git_wt = Arc::new(crate::services::git_worktree::GitWorktreeService::new(
            project_dir.clone(),
        ));
        let service = AgentControlService::new(project_dir.clone(), None, git_wt);

        // Test valid copy
        service
            .copy_allowed_dirs(&agent_wt, &["shared-context".to_string()])
            .await
            .unwrap();
        assert!(agent_wt
            .join(".exo/context/shared-context/ref.txt")
            .exists());

        // Test invalid paths (should skip but not fail)
        service
            .copy_allowed_dirs(
                &agent_wt,
                &["/absolute".to_string(), "../outside".to_string()],
            )
            .await
            .unwrap();
        assert!(!agent_wt.join(".exo/context/absolute").exists());
        assert!(!agent_wt.join(".exo/context/outside").exists());
    }

    #[test]
    fn test_claude_project_path_encoding() {
        // Claude Code encodes paths via [^a-zA-Z0-9] → '-'
        // Verified against actual ~/.claude/projects/ directory names.
        let encode = |s: &str| -> String {
            s.chars()
                .map(|c| if c.is_ascii_alphanumeric() { c } else { '-' })
                .collect()
        };

        // Basic path
        assert_eq!(
            encode("/home/inanna/dev/exomonad"),
            "-home-inanna-dev-exomonad"
        );
        // Worktree path (dots and hyphens in segments)
        assert_eq!(
            encode("/home/inanna/dev/exomonad/.exo/worktrees/fork-session"),
            "-home-inanna-dev-exomonad--exo-worktrees-fork-session"
        );
        // Hidden dir (leading dot → double dash after parent separator)
        assert_eq!(
            encode("/home/inanna/.config/home-manager"),
            "-home-inanna--config-home-manager"
        );
        // Deep nested path with hyphens
        assert_eq!(
            encode("/home/inanna/dev/aegis-binder-diagnostic-framework"),
            "-home-inanna-dev-aegis-binder-diagnostic-framework"
        );
        // Path with spaces
        assert_eq!(
            encode("/home/user/My Projects/app"),
            "-home-user-My-Projects-app"
        );
    }
}
