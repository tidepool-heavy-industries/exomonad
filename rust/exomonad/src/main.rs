//! exomonad: Rust host with embedded Haskell WASM plugin.
//!
//! This binary runs as a sidecar in each agent container, handling:
//! - Claude Code hooks via WASM plugin
//! - MCP tools via local Rust implementation
//!
//! WASM plugins are embedded at compile time. No file paths to resolve.

use exomonad::config;

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use exomonad_core::mcp::handler::ExomonadHandler;
use exomonad_core::protocol::{Runtime as HookRuntime, ServiceRequest};
use exomonad_core::services::external::otel::OtelService;
use exomonad_core::services::external::ExternalService;
use exomonad_core::services::{git, zellij_events};
use exomonad_core::{
    ClaudePreToolUseOutput, HookEventType, HookInput, HookSpecificOutput, InternalStopHookOutput,
    PluginManager, Runtime, RuntimeBuilder, Services, StopDecision, ToolPermission,
};
use std::collections::HashMap;
use std::os::unix::process::CommandExt;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::io::AsyncWriteExt;
use tokio::net::UnixStream;
use tower_http::trace::TraceLayer;
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

    /// Recompile WASM plugin from Haskell source
    Recompile {
        /// Role to build (default: from config, usually "tl")
        #[arg(long)]
        role: Option<String>,
    },

    /// Run HTTP MCP server (all agents connect to this)
    ///
    /// Loads WASM from file path (not embedded) with hot reload on change.
    /// Listens on TCP (default: localhost:7432).
    Serve {
        /// TCP port to listen on (default: from config, or 7432)
        #[arg(long)]
        port: Option<u16>,
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

#[allow(clippy::too_many_arguments)]
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
            match exomonad_core::ui_protocol::AgentId::try_from(agent_id_str.clone()) {
                Ok(agent_id) => {
                    let event = exomonad_core::ui_protocol::AgentEvent::HookReceived {
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
        if internal_output.decision == StopDecision::Block {
            // Emit StopHookBlocked event for SubagentStop hooks
            if event_type == HookEventType::SubagentStop {
                if let Ok(branch) = git::get_current_branch() {
                    if let Some(agent_id_str) = git::extract_agent_id(&branch) {
                        let reason = internal_output
                            .reason
                            .clone()
                            .unwrap_or_else(|| "Hook blocked agent stop".to_string());

                        match exomonad_core::ui_protocol::AgentId::try_from(agent_id_str.clone()) {
                            Ok(agent_id) => {
                                let event =
                                    exomonad_core::ui_protocol::AgentEvent::StopHookBlocked {
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
///
fn run_init(session_override: Option<String>, recreate: bool, port: u16) -> Result<()> {
    // Preflight: warn if XDG_RUNTIME_DIR missing (SSH edge case)
    if std::env::var("XDG_RUNTIME_DIR").is_err() {
        eprintln!("Warning: XDG_RUNTIME_DIR not set. Zellij may fail to find sessions.");
    }

    // Resolve config
    let config = config::Config::discover()?;
    let session = session_override.unwrap_or(config.zellij_session);

    // Query existing sessions
    let output = std::process::Command::new("zellij")
        .args(["list-sessions", "--short"])
        .output()
        .context("Failed to run zellij list-sessions")?;

    let sessions_str = String::from_utf8_lossy(&output.stdout);

    // `zellij list-sessions --short` outputs lines like:
    //   "my-session" (alive) or "my-session (EXITED ...)" (dead)
    let session_alive = sessions_str.lines().any(|l| l.trim() == session);
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
    let layout_path = generate_tl_layout(port)?;

    let err = std::process::Command::new("zellij")
        .arg("--session")
        .arg(&session)
        .arg("--new-session-with-layout")
        .arg(&layout_path)
        .exec();
    Err(err).context("Failed to exec zellij with layout")
}

/// Generate a two-tab TL layout: Server tab + TL tab.
///
/// Tab 1 "Server": runs `exomonad serve` on TCP port, stays open on exit.
/// Tab 2 "TL": runs `nix develop` for the dev environment, focused by default.
fn generate_tl_layout(port: u16) -> Result<std::path::PathBuf> {
    let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/zsh".to_string());
    let cwd = std::env::current_dir()?;

    let serve_command = format!("exomonad serve --port {}", port);

    let tabs = vec![
        exomonad_core::layout::AgentTabParams {
            tab_name: "Server",
            pane_name: "exomonad-serve",
            command: &serve_command,
            cwd: &cwd,
            shell: &shell,
            focus: false,
            close_on_exit: false,
            env: HashMap::new(),
        },
        exomonad_core::layout::AgentTabParams {
            tab_name: "TL",
            pane_name: "Main",
            command: "nix develop",
            cwd: &cwd,
            shell: &shell,
            focus: true,
            close_on_exit: false,
            env: HashMap::new(),
        },
    ];

    let layout = exomonad_core::layout::generate_main_layout(tabs)
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
/// Two independent layers, both always active when possible:
/// - **File layer**: Rolling daily logs in .exomonad/logs/
/// - **Stderr layer**: Always present (HTTP serve mode, stderr is safe)
///
/// JSON formatting controlled by EXOMONAD_LOG_FORMAT=json.
fn init_logging() -> Option<tracing_appender::non_blocking::WorkerGuard> {
    let use_json = std::env::var("EXOMONAD_LOG_FORMAT")
        .map(|v| v.eq_ignore_ascii_case("json"))
        .unwrap_or(false);

    let log_dir = PathBuf::from(".exomonad/logs");
    let file_ok = std::fs::create_dir_all(&log_dir).is_ok();
    if !file_ok {
        eprintln!("Failed to create .exomonad/logs/. Falling back to stderr-only logging.");
    }

    let env_filter = tracing_subscriber::EnvFilter::from_default_env()
        .add_directive(tracing::Level::INFO.into());

    // File layer (absent if dir creation failed)
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

    // Stderr layer (always present)
    let (stderr_layer_plain, stderr_layer_json) = if use_json {
        let layer = tracing_subscriber::fmt::layer()
            .json()
            .with_writer(std::io::stderr);
        (None, Some(layer))
    } else {
        let layer = tracing_subscriber::fmt::layer().with_writer(std::io::stderr);
        (Some(layer), None)
    };

    tracing_subscriber::registry()
        .with(env_filter)
        .with(file_layer_plain)
        .with(file_layer_json)
        .with(stderr_layer_plain)
        .with(stderr_layer_json)
        .init();

    guard
}

// ============================================================================
// Per-Role MCP Service
// ============================================================================

/// Build a `StreamableHttpService` for a specific role.
///
/// Each role gets its own MCP endpoint with filtered tools:
/// - "tl": all WASM tools + get_agent_messages + answer_question
/// - "dev": WASM tools minus spawn_agents/cleanup_agents/list_agents
fn build_role_mcp_service(
    base_state: &exomonad_core::mcp::McpState,
    role: &str,
    ct: tokio_util::sync::CancellationToken,
) -> rmcp::transport::streamable_http_server::StreamableHttpService<ExomonadHandler> {
    let mut role_state = base_state.clone();
    role_state.role = Some(role.to_string());
    let role_state = Arc::new(role_state);
    let role_str = role.to_string();

    let mcp_config = rmcp::transport::streamable_http_server::StreamableHttpServerConfig {
        cancellation_token: ct,
        ..Default::default()
    };

    rmcp::transport::streamable_http_server::StreamableHttpService::new(
        {
            let state = role_state.clone();
            move || {
                let state = state.clone();
                let role_str = role_str.clone();
                // rmcp's handler factory is sync (FnMut() -> Result<T>), but our
                // ExomonadHandler::build and WASM reload are async. block_in_place
                // moves this closure off the tokio worker thread so block_on is safe.
                let rt = tokio::runtime::Handle::current();
                tokio::task::block_in_place(|| {
                    rt.block_on(async {
                        // Hot reload: check if WASM changed before creating handler
                        match state.plugin.reload_if_changed().await {
                            Ok(true) => {
                                info!(role = %role_str, "WASM hot-reloaded for new session")
                            }
                            Ok(false) => {}
                            Err(e) => {
                                warn!(role = %role_str, error = %e, "WASM reload check failed")
                            }
                        }
                        ExomonadHandler::build((*state).clone()).await
                    })
                })
                .map_err(std::io::Error::other)
            }
        },
        rmcp::transport::streamable_http_server::session::local::LocalSessionManager::default()
            .into(),
        mcp_config,
    )
}

// ============================================================================
// Main
// ============================================================================

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging based on command type
    let _guard = init_logging();

    // Discover config (or use default)
    let config = config::Config::discover().unwrap_or_else(|e| {
        debug!(error = %e, "No config found, using defaults");
        config::Config::default()
    });

    // Handle recompile before WASM loading (recompile builds WASM, doesn't need it)
    if let Commands::Recompile { role } = &cli.command {
        let default_role = config.role.to_string();
        let role_str = role.as_deref().unwrap_or(&default_role);
        let project_dir = if config.project_dir.is_absolute() {
            config.project_dir.clone()
        } else {
            std::env::current_dir()?.join(&config.project_dir)
        };
        return exomonad::recompile::run_recompile(role_str, &project_dir).await;
    }

    // Handle serve before embedded WASM loading (serve uses file-based WASM, not embedded)
    if let Commands::Serve { port } = &cli.command {
        let port = port.unwrap_or(config.port);
        let project_dir = if config.project_dir.is_absolute() {
            config.project_dir.clone()
        } else {
            std::env::current_dir()?.join(&config.project_dir)
        };

        let role_name = config.role.to_string();
        let wasm_path = project_dir.join(format!(".exomonad/wasm/wasm-guest-{}.wasm", role_name));

        if !wasm_path.exists() {
            anyhow::bail!(
                "WASM file not found: {}\nRun `exomonad recompile` first to build it.",
                wasm_path.display()
            );
        }

        let server_pid_path = project_dir.join(".exomonad/server.pid");

        info!(
            wasm_path = %wasm_path.display(),
            port = %port,
            role = %role_name,
            "Starting HTTP MCP server with file-based WASM (hot reload enabled)"
        );

        // Initialize services (with MCP server port for per-agent endpoint URLs)
        let services = Arc::new(
            Services::new()
                .with_zellij_session(config.zellij_session.clone())
                .with_mcp_server_port(port)
                .validate()
                .context("Failed to validate services")?,
        );

        // Build runtime with file-based WASM loading (enables hot reload)
        let builder = RuntimeBuilder::new().with_wasm_path(wasm_path);
        let (builder, question_registry) =
            exomonad_core::register_builtin_handlers(builder, &services);
        let rt = builder.build().await.context("Failed to build runtime")?;

        let mut base_state = rt.into_mcp_state(project_dir.clone());
        base_state.question_registry = Some(question_registry);

        // Write server.pid for client discovery
        let pid_info = serde_json::json!({
            "pid": std::process::id(),
            "port": port,
            "role": role_name,
        });
        if let Some(parent) = server_pid_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&server_pid_path, serde_json::to_string_pretty(&pid_info)?)?;
        info!(path = %server_pid_path.display(), "Wrote server.pid");

        // Build per-role MCP services: /tl/mcp gets orchestration tools, /dev/mcp gets agent tools
        let ct = tokio_util::sync::CancellationToken::new();

        let tl_service = build_role_mcp_service(&base_state, "tl", ct.clone());
        let dev_service = build_role_mcp_service(&base_state, "dev", ct.clone());

        // Health endpoint alongside MCP — captures state for runtime introspection
        let health_plugin = base_state.plugin.clone();
        let health_role = role_name.clone();
        let health_port = port;
        let health_handler = move || {
            let plugin = health_plugin.clone();
            let role = health_role.clone();
            async move {
                let wasm_hash = plugin.content_hash();
                axum::Json(serde_json::json!({
                    "status": "ok",
                    "version": env!("CARGO_PKG_VERSION"),
                    "port": health_port,
                    "role": role,
                    "wasm_hash": wasm_hash,
                }))
            }
        };

        // Per-agent MCP route: /agents/{name}/mcp
        // Each Gemini agent's settings.json points to its own URL.
        // The handler extracts agent name from the path and sets it as
        // the task-local identity before forwarding to the dev MCP service.
        let agent_dev_service = dev_service.clone();
        let agent_handler = move |
            axum::extract::Path(agent_name): axum::extract::Path<String>,
            request: axum::extract::Request,
        | {
            let service = agent_dev_service.clone();
            async move {
                tracing::info!(agent = %agent_name, "Routing agent MCP request");
                exomonad_core::mcp::agent_identity::with_agent_id(
                    agent_name,
                    async move { service.handle(request).await },
                )
                .await
            }
        };

        let app = axum::Router::new()
            .route("/health", axum::routing::get(health_handler))
            .route("/agents/{name}/mcp", axum::routing::any(agent_handler.clone()))
            .route("/agents/{name}/mcp/{*rest}", axum::routing::any(agent_handler))
            .nest_service("/tl/mcp", tl_service)
            .nest_service("/dev/mcp", dev_service)
            .layer(TraceLayer::new_for_http());

        let addr = std::net::SocketAddr::from(([127, 0, 0, 1], port));
        let listener = tokio::net::TcpListener::bind(addr).await.map_err(|e| {
            if e.kind() == std::io::ErrorKind::AddrInUse {
                anyhow::anyhow!(
                    "Port {} is already in use. Is another exomonad serve running?\n\
                     Check with: lsof -i :{}\n\
                     Or use --port to pick a different port.",
                    port,
                    port
                )
            } else {
                anyhow::anyhow!("Failed to bind to port {}: {}", port, e)
            }
        })?;
        info!(port = %port, "HTTP MCP server listening on TCP (rmcp StreamableHttp)");

        // Run with graceful shutdown on SIGINT or SIGTERM
        axum::serve(listener, app)
            .with_graceful_shutdown(async move {
                let ctrl_c = tokio::signal::ctrl_c();
                #[cfg(unix)]
                let terminate = async {
                    tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
                        .expect("failed to install SIGTERM handler")
                        .recv()
                        .await;
                };
                #[cfg(not(unix))]
                let terminate = std::future::pending::<()>();

                tokio::select! {
                    _ = ctrl_c => info!("Received SIGINT, initiating graceful shutdown"),
                    _ = terminate => info!("Received SIGTERM, initiating graceful shutdown"),
                }
                ct.cancel();
            })
            .await?;

        // Clean up server.pid on shutdown
        if server_pid_path.exists() {
            let _ = std::fs::remove_file(&server_pid_path);
            info!("Cleaned up server.pid");
        }

        info!("HTTP MCP server shut down");
        return Ok(());
    }

    // Load embedded WASM for hook command (the only remaining embedded WASM user)
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
                    .with_zellij_session(config.zellij_session.clone())
                    .validate()
                    .context("Failed to validate services")?,
            );

            // Build runtime with all effect handlers
            let rt = build_runtime(wasm_bytes, &services).await?;

            info!("WASM plugin loaded and initialized");

            handle_hook(rt.plugin_manager(), event, runtime, &config.zellij_session).await?;
        }

        Commands::Init { session, recreate } => {
            run_init(session, recreate, config.port)?;
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

        Commands::Recompile { .. } => unreachable!("handled before WASM loading"),
        Commands::Serve { .. } => unreachable!("handled before WASM loading"),
    }

    Ok(())
}

/// Build a Runtime with all effect handlers registered.
async fn build_runtime(
    wasm_bytes: &[u8],
    services: &Arc<exomonad_core::ValidatedServices>,
) -> Result<Runtime> {
    let builder = RuntimeBuilder::new().with_wasm_bytes(wasm_bytes.to_vec());
    let (builder, _question_registry) =
        exomonad_core::register_builtin_handlers(builder, services);

    builder.build().await.context("Failed to build runtime")
}
