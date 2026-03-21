use crate::uds_client;
use anyhow::{Context, Result};
use exomonad::config::Config;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};
use tracing::{debug, info, warn};

/// Run the init command: create or attach to tmux session.
pub async fn run(session_override: Option<String>, recreate: bool) -> Result<()> {
    use exomonad_core::services::tmux_ipc::TmuxIpc;
    use exomonad_core::services::{resolve_role_context_path, AgentType};
    use std::io::{IsTerminal, Write};
    let cwd = std::env::current_dir()?;
    let config_path = cwd.join(".exo/config.toml");
    if !config_path.exists() {
        anyhow::bail!("No exomonad project found. Run `exomonad new` first.");
    }

    // Resolve config
    let config = Config::discover()?;

    // Check OTel endpoint reachability if configured
    if let Some(ref endpoint) = config.otlp_endpoint {
        if let Some(host_port) = endpoint
            .strip_prefix("http://")
            .or_else(|| endpoint.strip_prefix("https://"))
        {
            let hp = host_port.to_string();
            let reachable = tokio::task::spawn_blocking(move || {
                use std::net::ToSocketAddrs;
                match hp.to_socket_addrs() {
                    Ok(mut addrs) => {
                        if let Some(addr) = addrs.next() {
                            std::net::TcpStream::connect_timeout(
                                &addr,
                                std::time::Duration::from_secs(2),
                            )
                            .is_ok()
                        } else {
                            false
                        }
                    }
                    Err(_) => false,
                }
            })
            .await
            .unwrap_or(false);

            if reachable {
                info!(endpoint = %endpoint, "OTel endpoint reachable");
            } else if config.yolo || !std::io::stdin().is_terminal() {
                warn!(
                    endpoint = %endpoint,
                    "OTel endpoint unreachable — proceeding without tracing (YOLO or headless)"
                );
            } else {
                eprint!(
                    "OTel endpoint {} unreachable — continue without tracing? [y/N] ",
                    endpoint
                );
                std::io::stderr().flush().ok();
                let input = tokio::task::spawn_blocking(|| {
                    let mut buf = String::new();
                    std::io::stdin().read_line(&mut buf).ok();
                    buf
                })
                .await
                .unwrap_or_default();
                if !input.trim().eq_ignore_ascii_case("y") {
                    anyhow::bail!("OTel endpoint unreachable. Start it with:\n  docker compose -f ~/.exo/otel/docker-compose.yml up -d");
                }
            }
        }
    }

    let session = session_override.unwrap_or(config.tmux_session.clone());

    // Auto-build or copy WASM if it doesn't exist yet
    let wasm_filename = format!("wasm-guest-{}.wasm", config.wasm_name);
    let wasm_path = config.wasm_dir.join(&wasm_filename);
    let roles_dir = cwd.join(".exo/roles");
    let has_roles = roles_dir.is_dir();

    if !wasm_path.exists() {
        if has_roles {
            info!(path = %wasm_path.display(), "WASM not found, building...");
            exomonad::recompile::run_recompile(
                &config.wasm_name,
                &cwd,
                config.flake_ref.as_deref(),
            )
            .await?;
        } else if let Ok(home) = std::env::var("HOME") {
            let home = PathBuf::from(home);
            // Fall back to globally installed WASM from ~/.exo/wasm/
            let global_wasm = home.join(".exo/wasm").join(&wasm_filename);
            if global_wasm.exists() {
                info!(
                    src = %global_wasm.display(),
                    dst = %wasm_path.display(),
                    "Copying WASM from global install"
                );
                std::fs::create_dir_all(&config.wasm_dir)?;
                std::fs::copy(&global_wasm, &wasm_path)?;
            } else {
                warn!(
                    path = %wasm_path.display(),
                    "No WASM found locally or at ~/.exo/wasm/. Run 'just install-all' in the exomonad repo, or copy roles: cp -r /path/to/exomonad/.exo/roles .exo/roles"
                );
            }
        } else {
            warn!(
                path = %wasm_path.display(),
                "No WASM found locally or at ~/.exo/wasm/. Run 'just install-all' in the exomonad repo, or copy roles: cp -r /path/to/exomonad/.exo/roles .exo/roles"
            );
        }
    } else if !has_roles {
        // Refresh stale WASM from global install if it's newer
        if let Ok(home) = std::env::var("HOME") {
            let global_wasm = PathBuf::from(home).join(".exo/wasm").join(&wasm_filename);
            if global_wasm.exists() {
                let local_mtime = std::fs::metadata(&wasm_path).and_then(|m| m.modified());
                let global_mtime = std::fs::metadata(&global_wasm).and_then(|m| m.modified());

                match (local_mtime, global_mtime) {
                    (Ok(local), Ok(global)) if global > local => {
                        info!(
                            src = %global_wasm.display(),
                            dst = %wasm_path.display(),
                            local_mtime = ?local,
                            global_mtime = ?global,
                            "Refreshing project WASM from global install (global is newer)"
                        );
                        std::fs::copy(&global_wasm, &wasm_path)?;
                    }
                    (Err(e), _) | (_, Err(e)) => {
                        debug!(error = %e, "Failed to compare WASM mtimes, skipping refresh");
                    }
                    _ => {}
                }
            }
        }
    }

    // Write root agent birth branch so fork_wave resolves the correct parent prefix.
    // Without this, BirthBranch::root() falls back to `git branch --show-current` in the
    // server process CWD, which may differ from the TL's actual branch.
    {
        let root_agent_dir = cwd.join(".exo/agents/root");
        std::fs::create_dir_all(&root_agent_dir)?;
        let current_branch = std::process::Command::new("git")
            .args(["branch", "--show-current"])
            .current_dir(&cwd)
            .output()
            .ok()
            .filter(|o| o.status.success())
            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
            .unwrap_or_else(|| "main".to_string());
        std::fs::write(root_agent_dir.join(".birth_branch"), &current_branch)?;
        info!(branch = %current_branch, "Wrote root agent birth branch");
    }

    // Write hook configuration (SessionStart registers Claude UUID for --fork-session)
    let binary_path = exomonad_core::find_exomonad_binary();
    exomonad_core::hooks::HookConfig::write_persistent(&cwd, &binary_path, None, None)
        .context("Failed to write hook configuration")?;
    info!("Hook configuration written to .claude/settings.local.json");

    // Copy Claude rules template if available and not already present
    {
        let rules_dest = cwd.join(".claude/rules/exomonad.md");
        if !rules_dest.exists() {
            // Resolution: project-local .exo/rules/ → global ~/.exo/rules/
            let local_template = cwd.join(".exo/rules/exomonad.md");
            let global_template = std::env::var("HOME")
                .ok()
                .map(|h| PathBuf::from(h).join(".exo/rules/exomonad.md"));

            let source = if local_template.exists() {
                Some(local_template)
            } else {
                global_template.filter(|p| p.exists())
            };

            if let Some(src) = source {
                std::fs::create_dir_all(cwd.join(".claude/rules"))?;
                std::fs::copy(&src, &rules_dest)?;
                info!(
                    src = %src.display(),
                    "Copied Claude rules to .claude/rules/exomonad.md"
                );
            }
        }
    }

    // Symlink role context for root agent
    {
        let context_source = resolve_role_context_path(&cwd, &config.wasm_name, "root");
        if let Some(src) = context_source {
            let rules_dir = cwd.join(".claude/rules");
            std::fs::create_dir_all(&rules_dir)?;
            let link = rules_dir.join("exomonad_role.md");
            let _ = std::fs::remove_file(&link); // idempotent
            // Compute relative path from .claude/rules/ to the source
            let relative = pathdiff::diff_paths(&src, &rules_dir)
                .unwrap_or(src.clone());
            match std::os::unix::fs::symlink(&relative, &link) {
                Ok(()) => info!(src = %src.display(), link = %link.display(), "Symlinked role context for root"),
                Err(e) => warn!(error = %e, "Failed to symlink role context (non-fatal)"),
            }
        }
    }

    // Write Gemini MCP configuration if root agent is Gemini
    if config.root_agent_type == AgentType::Gemini {
        let gemini_dir = cwd.join(".gemini");
        std::fs::create_dir_all(&gemini_dir)?;
        let settings_path = gemini_dir.join("settings.json");

        let mut mcp_servers = serde_json::Map::new();
        mcp_servers.insert(
            "exomonad".to_string(),
            serde_json::json!({
                "type": "stdio",
                "command": "exomonad",
                "args": ["mcp-stdio", "--role", "root", "--name", "root"]
            }),
        );
        for (name, server) in &config.extra_mcp_servers {
            let entry = match server {
                exomonad::config::McpServerConfig::Http { url, .. } => {
                    serde_json::json!({ "httpUrl": url })
                }
                exomonad::config::McpServerConfig::Stdio { command, args } => {
                    serde_json::json!({"type": "stdio", "command": command, "args": args})
                }
            };
            mcp_servers.insert(name.clone(), entry);
        }

        let settings = serde_json::json!({ "mcpServers": mcp_servers });
        std::fs::write(&settings_path, serde_json::to_string_pretty(&settings)?)?;
        info!("Gemini MCP configuration written to .gemini/settings.json");
    }

    // Validate tmux is available
    let tmux_check = std::process::Command::new("tmux").arg("-V").output();
    match tmux_check {
        Ok(output) if output.status.success() => {
            let version = String::from_utf8_lossy(&output.stdout);
            info!("tmux version: {}", version.trim());
        }
        Ok(output) => {
            anyhow::bail!(
                "tmux -V failed (status {}). Is tmux installed correctly?",
                output.status
            );
        }
        Err(e) => {
            anyhow::bail!(
                "tmux not found: {}. Install tmux before running exomonad init.",
                e
            );
        }
    }

    let session_alive = TmuxIpc::has_session(&session).await?;

    if recreate {
        // Kill the running server process before tearing down the session
        let pid_path = cwd.join(".exo/server.pid");
        if let Ok(content) = std::fs::read_to_string(&pid_path) {
            if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&content) {
                if let Some(pid) = parsed.get("pid").and_then(|v| v.as_u64()) {
                    use nix::sys::signal;
                    use nix::unistd::Pid;
                    let pid = Pid::from_raw(pid as i32);
                    if signal::kill(pid, None).is_ok() {
                        info!(pid = pid.as_raw(), "Stopping server");
                        let _ = signal::kill(pid, signal::Signal::SIGTERM);
                        for _ in 0..10 {
                            if signal::kill(pid, None).is_err() {
                                break;
                            }
                            tokio::time::sleep(Duration::from_millis(200)).await;
                        }
                    }
                }
            }
        }
        // Clean up stale socket
        let sock = cwd.join(".exo/server.sock");
        if sock.exists() {
            info!("Removing stale server socket");
            let _ = std::fs::remove_file(&sock);
        }

        if session_alive {
            info!(session = %session, "Deleting session (--recreate)");
            TmuxIpc::kill_session(&session).await?;
        }
    } else if session_alive {
        // Attach to running session
        info!(session = %session, "Attaching to session");
        return TmuxIpc::attach_session(&session).await;
    }

    // Create fresh session
    info!(session = %session, "Creating session");

    // 1. Write .mcp.json
    let mut mcp_servers = serde_json::Map::new();
    mcp_servers.insert(
        "exomonad".to_string(),
        serde_json::json!({
            "type": "stdio",
            "command": "exomonad",
            "args": ["mcp-stdio", "--role", "root", "--name", "root"]
        }),
    );

    // Add extra MCP servers from config
    for (name, server) in &config.extra_mcp_servers {
        let entry = match server {
            exomonad::config::McpServerConfig::Http { url, headers } => {
                let mut e = serde_json::json!({"type": "http", "url": url});
                if !headers.is_empty() {
                    e["headers"] = serde_json::to_value(headers)?;
                }
                e
            }
            exomonad::config::McpServerConfig::Stdio { command, args } => {
                serde_json::json!({"type": "stdio", "command": command, "args": args})
            }
        };
        mcp_servers.insert(name.clone(), entry);
    }

    let mcp_json = serde_json::json!({ "mcpServers": mcp_servers });
    std::fs::write(
        cwd.join(".mcp.json"),
        serde_json::to_string_pretty(&mcp_json)?,
    )?;
    info!("Wrote .mcp.json with {} MCP server(s)", mcp_servers.len());

    // 2. Create session in background
    let server_window_id = TmuxIpc::new_session(&session, &cwd).await?;

    // Verify session
    if !TmuxIpc::has_session(&session).await? {
        anyhow::bail!(
            "tmux session '{}' was created but is not responding.",
            session
        );
    }

    // Set EXOMONAD_TMUX_SESSION
    let env_output = std::process::Command::new("tmux")
        .args([
            "set-environment",
            "-t",
            &session,
            "EXOMONAD_TMUX_SESSION",
            &session,
        ])
        .output()
        .context("Failed to set EXOMONAD_TMUX_SESSION in tmux session")?;
    if !env_output.status.success() {
        warn!(
            "tmux set-environment failed: {}",
            String::from_utf8_lossy(&env_output.stderr)
        );
    }

    // Set EXOMONAD_ROLE=root so hook CLI passes &role=root to server
    let role_output = std::process::Command::new("tmux")
        .args(["set-environment", "-t", &session, "EXOMONAD_ROLE", "root"])
        .output()
        .context("Failed to set EXOMONAD_ROLE in tmux session")?;
    if !role_output.status.success() {
        warn!(
            "tmux set-environment EXOMONAD_ROLE failed: {}",
            String::from_utf8_lossy(&role_output.stderr)
        );
    }

    // Set terminal window title to project/session name
    let _ = std::process::Command::new("tmux")
        .args(["set-option", "-t", &session, "set-titles", "on"])
        .output();
    let _ = std::process::Command::new("tmux")
        .args([
            "set-option",
            "-t",
            &session,
            "set-titles-string",
            "#{session_name}:#{window_name}",
        ])
        .output();

    // 3. Setup windows
    let ipc = TmuxIpc::new(&session);
    let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/sh".to_string());

    let server_target = server_window_id;
    let rename_status = std::process::Command::new("tmux")
        .args(["rename-window", "-t", server_target.as_str(), "Server"])
        .status()
        .context("Failed to rename server window")?;
    if !rename_status.success() {
        warn!("tmux rename-window failed with status {}", rename_status);
    }
    let serve_cmd = format!("EXOMONAD_TMUX_SESSION={} exomonad serve", &session);
    let send_status = std::process::Command::new("tmux")
        .args([
            "send-keys",
            "-t",
            server_target.as_str(),
            &serve_cmd,
            "Enter",
        ])
        .status()
        .context("Failed to send server start command to tmux")?;
    if !send_status.success() {
        anyhow::bail!(
            "Failed to start server in tmux (send-keys exited with {})",
            send_status
        );
    }

    // Create "TL" window
    let base_command = if let Some(ref cmd) = config.root_command {
        cmd.clone()
    } else {
        match (config.root_agent_type, config.initial_prompt.as_deref()) {
            (AgentType::Claude, _) => "claude --dangerously-skip-permissions -c || claude --dangerously-skip-permissions; echo; echo [Claude Code exited]; exec bash -l".to_string(),
            (AgentType::Gemini, Some(prompt)) => format!("gemini --prompt-interactive '{}'", prompt.replace('\'', "'\\''")),
            (AgentType::Gemini, None) => "gemini".to_string(),
            (AgentType::Shoal, Some(prompt)) => format!("shoal-agent --exo root --prompt '{}'", prompt.replace('\'', "'\\''")),
            (AgentType::Shoal, None) => "shoal-agent --exo root".to_string(),
        }
    };

    let tl_command = match config.shell_command {
        Some(sc) => format!("{} -c \"{}\"", sc, base_command.replace('"', "\\\"")),
        None => base_command,
    };

    let _ = ipc.new_window("TL", &cwd, &shell, &tl_command).await?;

    // 4. Poll for server socket
    wait_for_server_socket(&cwd).await?;

    // 5. Spawn companion agents
    for companion in &config.companions {
        // Validate companion name (alphanumeric, hyphens, underscores only)
        if !companion
            .name
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
        {
            anyhow::bail!(
                "Invalid companion name '{}': must contain only [A-Za-z0-9_-]",
                companion.name
            );
        }

        // Resolve agent_type: explicit or default to Claude with warning
        let agent_type = match companion.agent_type {
            Some(t) => t,
            None => {
                warn!(
                    name = %companion.name,
                    "Companion '{}' missing agent_type, defaulting to claude. Add agent_type = \"claude\" to silence this warning.",
                    companion.name
                );
                AgentType::Claude
            }
        };

        info!(name = %companion.name, role = %companion.role, agent_type = ?agent_type, "Spawning companion agent");

        // Create agent identity directory
        let agent_dir = cwd.join(".exo/agents").join(&companion.name);
        std::fs::create_dir_all(&agent_dir)?;

        // Write birth_branch identity
        std::fs::write(agent_dir.join(".birth_branch"), &companion.name)?;

        // Determine CWD for the companion window
        let companion_cwd = if agent_type == AgentType::Claude {
            // Claude companions get their own git worktree for isolated .mcp.json discovery
            let worktree_path = cwd.join(".exo/companions").join(&companion.name);
            let branch_name = format!("companion/{}", companion.name);

            if !worktree_path.exists() {
                // Ensure HEAD exists — worktree creation needs a valid ref
                let head_valid = std::process::Command::new("git")
                    .args(["rev-parse", "--verify", "HEAD"])
                    .current_dir(&cwd)
                    .output()
                    .map(|o| o.status.success())
                    .unwrap_or(false);

                if !head_valid {
                    info!("No commits in repo, creating initial commit for worktree support");
                    let _ = std::process::Command::new("git")
                        .args(["commit", "--allow-empty", "-m", "initial commit"])
                        .current_dir(&cwd)
                        .output();
                }

                // Create worktree (reuse branch if it already exists)
                let branch_exists = std::process::Command::new("git")
                    .args(["rev-parse", "--verify", &branch_name])
                    .current_dir(&cwd)
                    .output()
                    .map(|o| o.status.success())
                    .unwrap_or(false);

                std::fs::create_dir_all(cwd.join(".exo/companions"))?;

                let worktree_result = if branch_exists {
                    std::process::Command::new("git")
                        .args(["worktree", "add"])
                        .arg(&worktree_path)
                        .arg(&branch_name)
                        .current_dir(&cwd)
                        .output()
                } else {
                    std::process::Command::new("git")
                        .args(["worktree", "add", "-b", &branch_name])
                        .arg(&worktree_path)
                        .arg("HEAD")
                        .current_dir(&cwd)
                        .output()
                };

                match worktree_result {
                    Ok(output) if output.status.success() => {
                        info!(
                            name = %companion.name,
                            path = %worktree_path.display(),
                            branch = %branch_name,
                            "Created companion worktree"
                        );
                    }
                    Ok(output) => {
                        anyhow::bail!(
                            "Failed to create worktree for companion '{}': {}",
                            companion.name,
                            String::from_utf8_lossy(&output.stderr)
                        );
                    }
                    Err(e) => {
                        anyhow::bail!(
                            "Failed to run git worktree add for companion '{}': {}",
                            companion.name,
                            e
                        );
                    }
                }
            } else {
                info!(
                    name = %companion.name,
                    path = %worktree_path.display(),
                    "Reusing existing companion worktree"
                );
            }

            // Write .mcp.json to worktree root — Claude discovers via CWD
            let mut companion_mcp_servers = serde_json::Map::new();
            companion_mcp_servers.insert(
                "exomonad".to_string(),
                serde_json::json!({
                    "type": "stdio",
                    "command": "exomonad",
                    "args": ["mcp-stdio", "--role", &companion.role, "--name", &companion.name]
                }),
            );
            // Include extra MCP servers from config
            for (name, server) in &config.extra_mcp_servers {
                let entry = match server {
                    exomonad::config::McpServerConfig::Http { url, headers } => {
                        let mut e = serde_json::json!({"type": "http", "url": url});
                        if !headers.is_empty() {
                            e["headers"] = serde_json::to_value(headers)?;
                        }
                        e
                    }
                    exomonad::config::McpServerConfig::Stdio { command, args } => {
                        serde_json::json!({"type": "stdio", "command": command, "args": args})
                    }
                };
                companion_mcp_servers.insert(name.clone(), entry);
            }
            let companion_mcp_json = serde_json::json!({ "mcpServers": companion_mcp_servers });
            std::fs::write(
                worktree_path.join(".mcp.json"),
                serde_json::to_string_pretty(&companion_mcp_json)?,
            )?;

            // Write .claude/settings.local.json to worktree root (hooks)
            exomonad_core::hooks::HookConfig::write_persistent(
                &worktree_path,
                &binary_path,
                None,
                Some(&cwd),
            )
            .context("Failed to write companion hook configuration")?;

            // Copy role context into companion's rules dir.
            // Must be a copy, not a symlink — symlinks escape the worktree boundary
            // and cause Claude Code to discover parent context files.
            {
                let context_source = resolve_role_context_path(&cwd, &config.wasm_name, &companion.role);
                if let Some(src) = context_source {
                    let rules_dir = worktree_path.join(".claude/rules");
                    let _ = std::fs::create_dir_all(&rules_dir);
                    let dest = rules_dir.join("exomonad_role.md");
                    let _ = std::fs::remove_file(&dest); // idempotent
                    match std::fs::copy(&src, &dest) {
                        Ok(_) => info!(name = %companion.name, src = %src.display(), dest = %dest.display(), "Copied role context for companion"),
                        Err(e) => warn!(name = %companion.name, error = %e, "Failed to copy role context (non-fatal)"),
                    }
                }
            }


            // Symlink server socket into worktree's .exo/
            let worktree_exo = worktree_path.join(".exo");
            std::fs::create_dir_all(&worktree_exo)?;
            let socket_target = worktree_exo.join("server.sock");
            let _ = std::fs::remove_file(&socket_target);
            let socket_source = cwd.join(".exo/server.sock");
            std::os::unix::fs::symlink(&socket_source, &socket_target)?;
            info!(
                source = %socket_source.display(),
                target = %socket_target.display(),
                "Symlinked server socket into companion worktree"
            );

            worktree_path
        } else {
            // Gemini/Shoal companions use project root CWD
            let companion_mcp = serde_json::json!({
                "mcpServers": {
                    "exomonad": {
                        "type": "stdio",
                        "command": "exomonad",
                        "args": ["mcp-stdio", "--role", &companion.role, "--name", &companion.name]
                    }
                }
            });

            match agent_type {
                AgentType::Gemini => {
                    let settings = serde_json::json!({
                        "mcpServers": companion_mcp["mcpServers"]
                    });
                    std::fs::write(
                        agent_dir.join("settings.json"),
                        serde_json::to_string_pretty(&settings)?,
                    )?;
                }
                AgentType::Shoal => {}
                AgentType::Claude => unreachable!(),
            }

            cwd.clone()
        };

        // Build command per agent type
        let escaped_task = companion
            .task
            .as_deref()
            .map(|t| t.replace('\'', "'\\''"));
        let companion_cmd = match agent_type {
            AgentType::Claude => {
                // Pure CWD discovery — no --mcp-config, no --strict-mcp-config
                let task_part = match &escaped_task {
                    Some(t) => format!(" '{}'", t),
                    None => String::new(),
                };
                format!(
                    "{}{task_part}; echo; echo '[{} exited]'; exec bash -l",
                    companion.command, companion.name
                )
            }
            AgentType::Gemini => {
                let settings = agent_dir.join("settings.json");
                let task_part = match &escaped_task {
                    Some(t) => format!(" '{}'", t),
                    None => String::new(),
                };
                format!(
                    "GEMINI_CLI_SYSTEM_SETTINGS_PATH={} {}{}",
                    settings.display(),
                    companion.command,
                    task_part
                )
            }
            AgentType::Shoal => {
                let task_part = match &escaped_task {
                    Some(t) => format!(" '{}'", t),
                    None => String::new(),
                };
                format!("{}{}", companion.command, task_part)
            }
        };
        let window_id = ipc
            .new_window(&companion.name, &companion_cwd, &shell, &companion_cmd)
            .await?;

        // Write routing.json with window_id
        let routing = serde_json::json!({
            "window_id": window_id.as_str()
        });
        std::fs::write(
            agent_dir.join("routing.json"),
            serde_json::to_string_pretty(&routing)?,
        )?;

        info!(name = %companion.name, window = %window_id.as_str(), "Companion agent spawned");
    }

    // 6. Attach
    info!(session = %session, "Attaching to session");
    TmuxIpc::attach_session(&session).await
}

pub fn ensure_gitignore(project_dir: &Path) -> Result<()> {
    let gitignore_path = project_dir.join(".gitignore");
    let content = if gitignore_path.exists() {
        std::fs::read_to_string(&gitignore_path)?
    } else {
        String::new()
    };

    let has_line = |line: &str| content.lines().any(|l| l.trim() == line);
    let needed: Vec<&str> = [
        ".exo/*",
        "!.exo/config.toml",
        "!.exo/roles/",
        "!.exo/lib/",
        "!.exo/rules/",
    ]
    .into_iter()
    .filter(|line| !has_line(line))
    .collect();

    if needed.is_empty() {
        return Ok(());
    }

    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&gitignore_path)?;
    use std::io::Write;
    if !content.is_empty() && !content.ends_with('\n') {
        writeln!(file)?;
    }
    if !has_line(".exo/*") {
        writeln!(
            file,
            "# ExoMonad - track config and source, ignore runtime artifacts"
        )?;
    }
    for line in &needed {
        writeln!(file, "{}", line)?;
    }
    Ok(())
}

pub async fn wait_for_server_socket(project_dir: &Path) -> Result<()> {
    let socket_path = project_dir.join(".exo/server.sock");
    let start = Instant::now();
    let timeout_dur = Duration::from_secs(30);

    while start.elapsed() < timeout_dur {
        if socket_path.exists() {
            break;
        }
        tokio::time::sleep(Duration::from_millis(500)).await;
    }

    if !socket_path.exists() {
        anyhow::bail!(
            "Server socket not found at {} after 30s.",
            socket_path.display()
        );
    }

    let client = uds_client::ServerClient::new(socket_path.to_path_buf());
    for _ in 0..5 {
        if client.is_healthy().await {
            return Ok(());
        }
        tokio::time::sleep(Duration::from_millis(500)).await;
    }

    anyhow::bail!("Server socket exists but health check failed.")
}

