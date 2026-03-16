use exomonad::config::Config;
use crate::uds_client;
use anyhow::{Context, Result};
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};
use tracing::{info, warn};

/// Run the init command: create or attach to tmux session.
pub async fn run(session_override: Option<String>, recreate: bool) -> Result<()> {
    use exomonad_core::services::tmux_ipc::TmuxIpc;
    use exomonad_core::services::AgentType;

    // Bootstrap: create .exo/config.toml if missing
    let cwd = std::env::current_dir()?;
    let config_path = cwd.join(".exo/config.toml");
    if !config_path.exists() {
        info!("Bootstrapping .exo/config.toml");
        std::fs::create_dir_all(cwd.join(".exo"))?;
        std::fs::write(
            &config_path,
            "# ExoMonad project config\n# All fields are optional — see docs for overrides\n",
        )?;

        // Add gitignore entries
        ensure_gitignore(&cwd)?;
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
            } else {
                use std::io::Write;
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
    if !wasm_path.exists() {
        let roles_dir = cwd.join(".exo/roles");
        if roles_dir.is_dir() {
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
    }

    // Write hook configuration (SessionStart registers Claude UUID for --fork-session)
    let binary_path = exomonad_core::find_exomonad_binary();
    exomonad_core::hooks::HookConfig::write_persistent(&cwd, &binary_path, None)
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
                "args": ["mcp-stdio", "--role", "tl", "--name", "root"]
            }),
        );
        for (name, server) in &config.extra_mcp_servers {
            mcp_servers.insert(name.clone(), serde_json::json!({ "httpUrl": server.url }));
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

    let session_alive = TmuxIpc::has_session(&session)?;

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
                            std::thread::sleep(Duration::from_millis(200));
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
            TmuxIpc::kill_session(&session)?;
        }
    } else if session_alive {
        // Attach to running session
        info!(session = %session, "Attaching to session");
        return TmuxIpc::attach_session(&session);
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
            "args": ["mcp-stdio", "--role", "tl", "--name", "root"]
        }),
    );

    // Add extra MCP servers from config
    for (name, server) in &config.extra_mcp_servers {
        let mut entry = serde_json::json!({
            "type": "http",
            "url": server.url,
        });
        if !server.headers.is_empty() {
            entry["headers"] = serde_json::to_value(&server.headers)?;
        }
        mcp_servers.insert(name.clone(), entry);
    }

    // Auto-register SigNoz MCP server if .env.signoz has an API key
    let signoz_env = cwd.join(".exo/otel/.env.signoz");
    if signoz_env.exists() && !config.extra_mcp_servers.contains_key("signoz") {
        if let Ok(content) = std::fs::read_to_string(&signoz_env) {
            let key = content
                .lines()
                .find_map(|line| line.strip_prefix("SIGNOZ_API_KEY="))
                .unwrap_or("")
                .trim();
            if !key.is_empty() {
                mcp_servers.insert(
                    "signoz".to_string(),
                    serde_json::json!({
                        "type": "http",
                        "url": format!("http://localhost:{}/mcp", config.signoz_mcp_port),
                        "headers": { "Authorization": format!("Bearer {}", key) }
                    }),
                );
                info!("Auto-registered SigNoz MCP server from .exo/otel/.env.signoz");
            }
        }
    }

    let mcp_json = serde_json::json!({ "mcpServers": mcp_servers });
    std::fs::write(
        cwd.join(".mcp.json"),
        serde_json::to_string_pretty(&mcp_json)?,
    )?;
    info!("Wrote .mcp.json with {} MCP server(s)", mcp_servers.len());

    // 2. Create session in background
    let server_window_id = TmuxIpc::new_session(&session, &cwd)?;

    // Verify session
    if !TmuxIpc::has_session(&session)? {
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
    let base_command = match (config.root_agent_type, config.initial_prompt.as_deref()) {
        (AgentType::Claude, _) => "claude --dangerously-skip-permissions -c || claude --dangerously-skip-permissions; echo; echo [Claude Code exited]; exec bash -l".to_string(),
        (AgentType::Gemini, Some(prompt)) => format!("gemini --prompt-interactive '{}'", prompt.replace('\'', "'\\''")),
        (AgentType::Gemini, None) => "gemini".to_string(),
        (AgentType::Shoal, Some(prompt)) => format!("shoal-agent --prompt '{}'", prompt.replace('\'', "'\\''")),
        (AgentType::Shoal, None) => "shoal-agent".to_string(),
    };

    let tl_command = match config.shell_command {
        Some(sc) => format!("{} -c \"{}\"", sc, base_command.replace('"', "\\\"")),
        None => base_command,
    };

    let _ = ipc.new_window("TL", &cwd, &shell, &tl_command)?;

    // 4. Poll for server socket
    wait_for_server_socket(&cwd).await?;

    // 5. Attach
    info!(session = %session, "Attaching to session");
    TmuxIpc::attach_session(&session)
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
        anyhow::bail!("Server socket not found at {} after 30s.", socket_path.display());
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
