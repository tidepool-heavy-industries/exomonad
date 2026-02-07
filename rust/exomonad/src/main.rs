//! exomonad: Rust host with embedded Haskell WASM plugin.
//!
//! This binary runs as a sidecar in each agent container, handling:
//! - Claude Code hooks via WASM plugin
//! - MCP tools via local Rust implementation
//!
//! WASM plugins are embedded at compile time. No file paths to resolve.

use exomonad::config;

use anyhow::{Context, Result};
use std::os::unix::process::CommandExt;
use clap::{Parser, Subcommand};
use exomonad_contrib::services::{git, zellij_events};
use exomonad_contrib::Services;
use exomonad_core::mcp;
use exomonad_core::{PluginManager, Runtime, RuntimeBuilder};
use exomonad_services::otel::OtelService;
use exomonad_services::ExternalService;
use exomonad_shared::protocol::{
    ClaudePreToolUseOutput, HookEventType, HookInput, HookSpecificOutput, InternalStopHookOutput,
    Runtime as HookRuntime, ServiceRequest, StopDecision,
};
use exomonad_shared::ToolPermission;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::io::AsyncWriteExt;
use tokio::net::UnixStream;
use tracing::{debug, info, warn};
use tracing_subscriber::prelude::*;

// ============================================================================
// CLI Types
// ============================================================================

#[derive(Parser)]
#[command(name = "exomonad")]
#[command(about = "ExoMonad: Rust host with embedded Haskell WASM plugin for agent orchestration")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Handle a Claude Code hook event via WASM plugin
    Hook {
        /// The hook event type to handle
        #[arg(value_enum)]
        event: HookEventType,

        /// The runtime environment (Claude or Gemini)
        #[arg(long, default_value = "claude")]
        runtime: HookRuntime,
    },

    /// Run as stdio MCP server (Claude Code spawns this)
    ///
    /// Reads config from .exomonad/config.toml in current directory.
    /// Claude Code should configure this in .mcp.json with type: "stdio".
    McpStdio,

    /// Initialize Zellij session for this project.
    ///
    /// Creates a new session with TL layout if none exists, or attaches to existing.
    /// Session name is read from .exomonad/config.toml zellij_session field.
    Init {
        /// Optionally override session name (default: from config)
        #[arg(long)]
        session: Option<String>,
        /// Delete existing session and create fresh (use after binary/layout updates)
        #[arg(long)]
        recreate: bool,
    },

    /// Reply to a UI request (sent by Zellij plugin)
    Reply {
        /// Request ID
        #[arg(long)]
        id: String,

        /// JSON payload
        #[arg(long)]
        payload: Option<String>,

        /// Cancel the request
        #[arg(long)]
        cancel: bool,
    },
}

// ============================================================================
// Hook Handler
// ============================================================================

async fn emit_hook_span(
    otel: &OtelService,
    trace_id: &str,
    event_type: HookEventType,
    runtime: HookRuntime,
    hook_input: &HookInput,
    decision_str: &str,
    start_ns: u64,
    extra_attributes: HashMap<String, String>,
) {
    let end_ns = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos() as u64;

    let mut attributes = HashMap::new();
    attributes.insert("session.id".to_string(), hook_input.session_id.to_string());
    attributes.insert("jsonl.file".to_string(), hook_input.transcript_path.clone());
    if let Some(tid) = &hook_input.tool_use_id {
        attributes.insert("tool_use_id".to_string(), tid.clone());
    }
    attributes.insert("hook.type".to_string(), format!("{:?}", event_type));
    attributes.insert("hook.runtime".to_string(), runtime.to_string());
    attributes.insert("hook.decision".to_string(), decision_str.to_string());

    for (k, v) in extra_attributes {
        attributes.insert(k, v);
    }

    let req = ServiceRequest::OtelSpan {
        trace_id: trace_id.to_string(),
        span_id: uuid::Uuid::new_v4().simple().to_string()[..16].to_string(),
        name: format!("hook:{:?}", event_type),
        start_ns: Some(start_ns),
        end_ns: Some(end_ns),
        attributes: Some(attributes),
    };

    if let Err(e) = otel.call(req).await {
        warn!("Failed to emit OTel span: {}", e);
    }
}

#[tracing::instrument(skip(plugin, runtime, event_type, zellij_session), fields(event = ?event_type))]
async fn handle_hook(
    plugin: &PluginManager,
    event_type: HookEventType,
    runtime: HookRuntime,
    zellij_session: &str,
) -> Result<()> {
    use std::io::Read;

    let otel = OtelService::from_env().ok();
    let trace_id = uuid::Uuid::new_v4().simple().to_string();
    let start_ns = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos() as u64;

    // Read hook payload from stdin
    let mut stdin_content = String::new();
    std::io::stdin()
        .read_to_string(&mut stdin_content)
        .context("Failed to read stdin")?;

    debug!(
        runtime = ?runtime,
        payload_len = stdin_content.len(),
        "Received hook event"
    );

    // Emit HookReceived event
    if let Ok(branch) = git::get_current_branch() {
        if let Some(agent_id_str) = git::extract_agent_id(&branch) {
            match exomonad_ui_protocol::AgentId::try_from(agent_id_str.clone()) {
                Ok(agent_id) => {
                    let event = exomonad_ui_protocol::AgentEvent::HookReceived {
                        agent_id,
                        hook_type: event_type.to_string(),
                        timestamp: zellij_events::now_iso8601(),
                    };
                    if let Err(e) = zellij_events::emit_event(zellij_session, &event) {
                        warn!("Failed to emit hook:received event: {}", e);
                    }
                }
                Err(e) => warn!("Invalid agent_id in branch '{}': {}", agent_id_str, e),
            }
        } else {
            warn!("Could not extract agent_id from branch: {}", branch);
        }
    } else {
        warn!("Could not determine current git branch for HookReceived event");
    }

    // Parse the hook input and inject runtime
    let mut hook_input: HookInput =
        serde_json::from_str(&stdin_content).context("Failed to parse hook input")?;
    hook_input.runtime = Some(runtime);

    // Normalize CLI-specific hook types to internal abstractions before WASM.
    // Both Claude's Stop and Gemini's AfterAgent represent "main agent stop".
    let is_stop_hook = matches!(
        event_type,
        HookEventType::Stop
            | HookEventType::AfterAgent
            | HookEventType::SubagentStop
            | HookEventType::SessionEnd
    );

    let normalized_event_name = match event_type {
        HookEventType::Stop | HookEventType::AfterAgent => "Stop",
        HookEventType::SubagentStop => "SubagentStop",
        HookEventType::SessionEnd => "SessionEnd",
        HookEventType::PreToolUse => "PreToolUse",
        _ => {
            // Pass through unhandled hooks with allow
            debug!(event = ?event_type, "Hook type not implemented in WASM, allowing");
            let output_json = serde_json::to_string(&ClaudePreToolUseOutput::default())
                .context("Failed to serialize output")?;
            println!("{}", output_json);

            // Emit OTel span for unhandled hook
            if let Some(otel) = &otel {
                emit_hook_span(
                    otel,
                    &trace_id,
                    event_type,
                    runtime,
                    &hook_input,
                    "allow",
                    start_ns,
                    HashMap::new(),
                )
                .await;
            }

            return Ok(());
        }
    };
    hook_input.hook_event_name = normalized_event_name.to_string();

    // Handle stop hooks with runtime-specific output translation
    if is_stop_hook {
        // Call WASM and parse as internal domain type
        let internal_output: InternalStopHookOutput = plugin
            .call("handle_pre_tool_use", &hook_input)
            .await
            .context("WASM handle_pre_tool_use failed")?;

        // Translate to runtime-specific format at the edge
        let output_json = internal_output.to_runtime_json(&runtime);
        println!("{}", output_json);

        let decision_str = match internal_output.decision {
            StopDecision::Allow => "allow",
            StopDecision::Block => "block",
        };

        // Emit OTel span
        if let Some(otel) = &otel {
            let mut extra_attributes = HashMap::new();
            extra_attributes.insert("routing.decision".to_string(), decision_str.to_string());
            if let Some(reason) = &internal_output.reason {
                extra_attributes.insert("routing.reason".to_string(), reason.clone());
            }

            emit_hook_span(
                otel,
                &trace_id,
                event_type,
                runtime,
                &hook_input,
                decision_str,
                start_ns,
                extra_attributes,
            )
            .await;
        }

        // Exit code and event emission based on decision
        if internal_output.decision == exomonad_shared::protocol::StopDecision::Block {
            // Emit StopHookBlocked event for SubagentStop hooks
            if event_type == HookEventType::SubagentStop {
                if let Ok(branch) = git::get_current_branch() {
                    if let Some(agent_id_str) = git::extract_agent_id(&branch) {
                        let reason = internal_output
                            .reason
                            .clone()
                            .unwrap_or_else(|| "Hook blocked agent stop".to_string());

                        match exomonad_ui_protocol::AgentId::try_from(agent_id_str.clone()) {
                            Ok(agent_id) => {
                                let event = exomonad_ui_protocol::AgentEvent::StopHookBlocked {
                                    agent_id,
                                    reason,
                                    timestamp: zellij_events::now_iso8601(),
                                };
                                if let Err(e) = zellij_events::emit_event(zellij_session, &event) {
                                    warn!("Failed to emit stop_hook:blocked event: {}", e);
                                }
                            }
                            Err(e) => warn!("Invalid agent_id in branch '{}': {}", agent_id_str, e),
                        }
                    }
                }
            }
            // Exit 0 for both runtimes - decision is conveyed via JSON response.
            // Refs:
            // - Claude: https://docs.anthropic.com/en/docs/claude-code/hooks#stop-decision-control
            // - Gemini: https://geminicli.com/docs/hooks/reference/#afteragent
            //   (Exit 2 triggers "retry with stderr as feedback prompt" - not what we want)
        }
    } else {
        // Non-stop hooks: use existing ClaudePreToolUseOutput format
        let output: ClaudePreToolUseOutput = plugin
            .call("handle_pre_tool_use", &hook_input)
            .await
            .context("WASM handle_pre_tool_use failed")?;

        let output_json = serde_json::to_string(&output).context("Failed to serialize output")?;
        println!("{}", output_json);

        let decision_str = if !output.continue_ {
            "block"
        } else {
            match output.hook_specific_output {
                Some(HookSpecificOutput::PreToolUse {
                    permission_decision,
                    ..
                }) => match permission_decision {
                    ToolPermission::Allow => "allow",
                    ToolPermission::Deny => "deny",
                    ToolPermission::Ask => "ask",
                },
                _ => "allow",
            }
        };

        // Emit OTel span
        if let Some(otel) = &otel {
            emit_hook_span(
                otel,
                &trace_id,
                event_type,
                runtime,
                &hook_input,
                decision_str,
                start_ns,
                HashMap::new(),
            )
            .await;
        }

        if !output.continue_ {
            std::process::exit(2);
        }
    }

    Ok(())
}

// ============================================================================
// Init Command
// ============================================================================

/// Run the init command: create or attach to Zellij session.
///
/// Three-way session handling:
/// - Session alive → attach
/// - Session EXITED → delete stale state, create fresh from layout
/// - No session → create fresh from layout
///
/// With `--recreate`: delete any existing session (even alive), then create fresh.
fn run_init(session_override: Option<String>, recreate: bool) -> Result<()> {
    // Preflight: warn if XDG_RUNTIME_DIR missing (SSH edge case)
    if std::env::var("XDG_RUNTIME_DIR").is_err() {
        eprintln!("Warning: XDG_RUNTIME_DIR not set. Zellij may fail to find sessions.");
    }

    // Resolve session: override takes priority, then config
    let session = match session_override {
        Some(s) => s,
        None => {
            let config = config::Config::discover()?;
            config.zellij_session
        }
    };

    // Query existing sessions
    let output = std::process::Command::new("zellij")
        .args(["list-sessions", "--short"])
        .output()
        .context("Failed to run zellij list-sessions")?;

    let sessions_str = String::from_utf8_lossy(&output.stdout);

    // `zellij list-sessions --short` outputs lines like:
    //   "my-session" (alive) or "my-session (EXITED ...)" (dead)
    let session_alive = sessions_str
        .lines()
        .any(|l| l.trim() == session);
    let session_exited = sessions_str
        .lines()
        .any(|l| l.starts_with(&session) && l.contains("EXITED"));

    if recreate && (session_alive || session_exited) {
        eprintln!("Deleting session (--recreate): {}", session);
        let status = std::process::Command::new("zellij")
            .args(["delete-session", &session])
            .status()
            .context("Failed to run zellij delete-session")?;
        if !status.success() {
            eprintln!("Warning: zellij delete-session exited with {}", status);
        }
    } else if session_alive {
        // Attach to running session
        eprintln!("Attaching to session: {}", session);
        let err = std::process::Command::new("zellij")
            .args(["attach", &session])
            .exec();
        return Err(err).context("Failed to exec zellij attach");
    } else if session_exited {
        // Kill stale serialized state to prevent resurrection
        eprintln!("Cleaning up exited session: {}", session);
        let status = std::process::Command::new("zellij")
            .args(["delete-session", &session])
            .status()
            .context("Failed to run zellij delete-session")?;
        if !status.success() {
            eprintln!("Warning: zellij delete-session exited with {}", status);
        }
    }

    // Create fresh session from layout
    eprintln!("Creating session: {}", session);
    let layout_path = generate_tl_layout()?;

    let err = std::process::Command::new("zellij")
        .arg("--session")
        .arg(&session)
        .arg("--new-session-with-layout")
        .arg(&layout_path)
        .exec();
    Err(err).context("Failed to exec zellij with layout")
}

/// Generate a TL layout file for the init command.
///
/// Uses `nix develop` to provide the development environment (GHC, Cabal, Rust, etc.).
/// The template wraps this in a login shell (`$SHELL -l -c ...`) so profile-installed
/// tools like `claude` remain available via PATH inheritance.
fn generate_tl_layout() -> Result<std::path::PathBuf> {
    let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/zsh".to_string());
    let cwd = std::env::current_dir()?;

    let params = zellij_gen::AgentTabParams {
        tab_name: "TL",
        pane_name: "Main",
        command: "nix develop",
        cwd: &cwd,
        shell: &shell,
        focus: true,
        close_on_exit: false,
    };

    let layout = zellij_gen::generate_agent_layout(&params)
        .context("Failed to generate TL layout")?;

    let layout_path = std::env::temp_dir().join("exomonad-tl-layout.kdl");
    std::fs::write(&layout_path, layout)?;

    Ok(layout_path)
}

// ============================================================================
// Logging
// ============================================================================

/// Initialize logging based on the command mode.
///
/// Two independent axes composed via Option layers:
/// - **File layer**: Always present if .exomonad/logs/ is writable (rolling daily)
/// - **Stderr layer**: Present for non-MCP commands (MCP uses stdio, stderr would corrupt it)
///
/// JSON formatting controlled by EXOMONAD_LOG_FORMAT=json.
fn init_logging(command: &Commands) -> Option<tracing_appender::non_blocking::WorkerGuard> {
    let use_json = std::env::var("EXOMONAD_LOG_FORMAT")
        .map(|v| v.eq_ignore_ascii_case("json"))
        .unwrap_or(false);
    let is_mcp = matches!(command, Commands::McpStdio);

    let log_dir = PathBuf::from(".exomonad/logs");
    let file_ok = std::fs::create_dir_all(&log_dir).is_ok();
    if !file_ok {
        eprintln!("Failed to create .exomonad/logs/. Falling back to stderr-only logging.");
    }

    let env_filter = tracing_subscriber::EnvFilter::from_default_env()
        .add_directive(tracing::Level::INFO.into());

    // File layer (Option — absent if dir creation failed)
    let (file_layer_plain, file_layer_json, guard) = if file_ok {
        let appender = tracing_appender::rolling::daily(&log_dir, "sidecar.log");
        let (nb, g) = tracing_appender::non_blocking(appender);
        if use_json {
            let layer = tracing_subscriber::fmt::layer()
                .json()
                .with_writer(nb)
                .with_ansi(false);
            (None, Some(layer), Some(g))
        } else {
            let layer = tracing_subscriber::fmt::layer()
                .with_writer(nb)
                .with_ansi(false);
            (Some(layer), None, Some(g))
        }
    } else {
        (None, None, None)
    };

    // Stderr layer (Option — absent for MCP stdio to avoid corrupting JSON-RPC)
    let (stderr_layer_plain, stderr_layer_json) = if is_mcp && file_ok {
        // MCP with file logging: no stderr needed
        (None, None)
    } else if use_json {
        let layer = tracing_subscriber::fmt::layer()
            .json()
            .with_writer(std::io::stderr);
        (None, Some(layer))
    } else {
        let layer = tracing_subscriber::fmt::layer()
            .with_writer(std::io::stderr);
        (Some(layer), None)
    };

    tracing_subscriber::registry()
        .with(env_filter)
        .with(file_layer_plain)
        .with(file_layer_json)
        .with(stderr_layer_plain)
        .with(stderr_layer_json)
        .init();

    if is_mcp && file_ok {
        eprintln!("MCP stdio logging to .exomonad/logs/sidecar.log.YYYY-MM-DD (daily rotation)");
    }

    guard
}

fn get_agent_id_from_env() -> String {
    let branch = git::get_current_branch().unwrap_or_default();
    git::extract_agent_id(&branch).unwrap_or_else(|| {
        if branch.is_empty() {
            "no-branch".to_string()
        } else {
            "unknown".to_string()
        }
    })
}

// ============================================================================
// Main
// ============================================================================

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging based on command type
    let _guard = init_logging(&cli.command);

    // Discover config (or use default)
    let config = config::Config::discover().unwrap_or_else(|e| {
        debug!(error = %e, "No config found, using defaults");
        config::Config::default()
    });

    // Load embedded WASM for the configured role
    let wasm_bytes = exomonad::wasm::get(config.role)?;
    info!(
        role = ?config.role,
        wasm_size = wasm_bytes.len(),
        "Using embedded WASM"
    );

    match cli.command {
        Commands::Hook { event, runtime } => {
            // Initialize and validate services
            let services = Arc::new(
                Services::new()
                    .validate()
                    .context("Failed to validate services")?,
            );

            // Build runtime with all effect handlers
            let rt = build_runtime(wasm_bytes, &services, Some(config.zellij_session.clone())).await?;

            info!("WASM plugin loaded and initialized");

            handle_hook(rt.plugin_manager(), event, runtime, &config.zellij_session).await?;
        }

        Commands::McpStdio => {
            // stdio MCP server - Claude Code spawns this process
            let project_dir = if config.project_dir.is_absolute() {
                config.project_dir.clone()
            } else {
                std::env::current_dir()
                    .context("Failed to get current directory")?
                    .join(&config.project_dir)
            };

            let pid_file = project_dir.join(".exomonad/sidecar.pid");
            let _pid_guard = exomonad::pid::PidGuard::new(&pid_file)?;

            // Initialize and validate services (secrets loaded from ~/.exomonad/secrets)
            let services = Arc::new(
                Services::new()
                    .validate()
                    .context("Failed to validate services")?,
            );

            // Build runtime with all effect handlers + Zellij session for popup support
            let rt = build_runtime(wasm_bytes, &services, Some(config.zellij_session.clone())).await?;
            let state = rt.into_mcp_state(project_dir);

            // Emit AgentStarted
            let agent_id = get_agent_id_from_env();
            match exomonad_ui_protocol::AgentId::try_from(agent_id.clone()) {
                Ok(id) => {
                    let start_event = exomonad_ui_protocol::AgentEvent::AgentStarted {
                        agent_id: id,
                        timestamp: zellij_events::now_iso8601(),
                    };
                    if let Err(e) = zellij_events::emit_event(&config.zellij_session, &start_event) {
                        warn!("Failed to emit agent:started event: {}", e);
                    }
                }
                Err(e) => warn!("Invalid agent_id '{}': {}", agent_id, e),
            }

            mcp::stdio::run_stdio_server(state).await?;

            // Emit AgentStopped
            // Re-fetch branch as it might have changed
            let stop_agent_id = get_agent_id_from_env();
            match exomonad_ui_protocol::AgentId::try_from(stop_agent_id.clone()) {
                Ok(id) => {
                    let stop_event = exomonad_ui_protocol::AgentEvent::AgentStopped {
                        agent_id: id,
                        timestamp: zellij_events::now_iso8601(),
                    };
                    if let Err(e) = zellij_events::emit_event(&config.zellij_session, &stop_event) {
                        warn!("Failed to emit agent:stopped event: {}", e);
                    }
                }
                Err(e) => warn!("Invalid agent_id '{}': {}", stop_agent_id, e),
            }
        }

        Commands::Init { session, recreate } => {
            run_init(session, recreate)?;
        }

        Commands::Reply {
            id,
            payload,
            cancel,
        } => {
            // Socket path env var or default
            let socket_path = std::env::var("EXOMONAD_CONTROL_SOCKET")
                .unwrap_or_else(|_| ".exomonad/sockets/control.sock".to_string());

            debug!(socket = %socket_path, "Connecting to control socket");

            let mut stream = UnixStream::connect(&socket_path).await.context(format!(
                "Failed to connect to control socket at {}",
                socket_path
            ))?;

            let parsed_payload = if let Some(p) = payload {
                Some(serde_json::from_str(&p).context("Invalid JSON payload")?)
            } else {
                None
            };

            let request = ServiceRequest::UserInteraction {
                request_id: id,
                payload: parsed_payload,
                cancel,
            };

            // NDJSON format: JSON + newline
            let mut json = serde_json::to_vec(&request).context("Serialization failed")?;
            json.push(b'\n');

            stream
                .write_all(&json)
                .await
                .context("Failed to write to socket")?;

            // We don't necessarily wait for response here, it's a push notification
            info!("Sent reply to control socket");
        }
    }

    Ok(())
}

/// Build a Runtime with all effect handlers registered.
async fn build_runtime(
    wasm_bytes: &[u8],
    services: &Arc<exomonad_contrib::ValidatedServices>,
    zellij_session: Option<String>,
) -> Result<Runtime> {
    let mut builder = RuntimeBuilder::new().with_wasm_bytes(wasm_bytes.to_vec());

    if let Some(session) = zellij_session {
        builder = builder.with_zellij_session(session);
    }

    builder = exomonad_contrib::register_builtin_handlers(builder, services);

    builder.build().await.context("Failed to build runtime")
}
