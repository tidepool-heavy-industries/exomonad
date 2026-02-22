//! exomonad: Rust host with embedded Haskell WASM plugin.
//!
//! This binary runs as a sidecar in each agent container, handling:
//! - Claude Code hooks via HTTP forwarding to the server
//! - MCP tools via WASM plugin (server-side)
//!
//! WASM plugins are loaded from file (server-side only).

use exomonad::config;
use urlencoding::encode;

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use exomonad_core::mcp::server::McpServer;
use exomonad_core::protocol::{Runtime as HookRuntime, ServiceRequest};
use exomonad_core::services::external::otel::OtelService;
use exomonad_core::services::external::ExternalService;
use exomonad_core::services::{git, zellij_events};

use axum::response::IntoResponse;
use exomonad_core::{
    ClaudePreToolUseOutput, HookEnvelope, HookEventType, HookInput, HookSpecificOutput,
    InternalStopHookOutput, PluginManager, RuntimeBuilder, StopDecision, ToolPermission,
};
use std::collections::HashMap;
use std::os::unix::process::CommandExt;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::io::AsyncWriteExt;
use tokio::net::UnixStream;
use tower_http::cors::{Any, CorsLayer};
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
    /// Handle a Claude Code hook event (thin HTTP client → server)
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
    /// Session name is read from .exo/config.toml zellij_session field.
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
// Per-Agent Plugin Cache
// ============================================================================

/// Get or create a per-agent PluginManager with baked-in identity.
async fn get_or_create_plugin(
    plugins: &tokio::sync::RwLock<HashMap<exomonad_core::AgentName, Arc<PluginManager>>>,
    agent_name: exomonad_core::AgentName,
    birth_branch: exomonad_core::BirthBranch,
    registry: &Arc<exomonad_core::effects::EffectRegistry>,
    wasm_path: &std::path::Path,
) -> anyhow::Result<Arc<PluginManager>> {
    // Fast path: existing plugin
    {
        let cache = plugins.read().await;
        if let Some(p) = cache.get(&agent_name) {
            return Ok(p.clone());
        }
    }

    // Slow path: create new per-agent plugin
    let ctx = exomonad_core::effects::EffectContext {
        agent_name: agent_name.clone(),
        birth_branch,
    };
    let p = Arc::new(
        PluginManager::from_file(wasm_path, registry.clone(), ctx)
            .await
            .with_context(|| format!("Failed to create plugin for agent {}", agent_name))?,
    );
    let mut cache = plugins.write().await;
    // Re-check after acquiring write lock to avoid TOCTOU race
    if let Some(existing) = cache.get(&agent_name) {
        return Ok(existing.clone());
    }
    cache.insert(agent_name, p.clone());
    Ok(p)
}

// ============================================================================
// Server-side Hook Handler
// ============================================================================

/// Query parameters for the `/hook` endpoint.
#[derive(Debug, serde::Deserialize)]
struct HookQueryParams {
    event: HookEventType,
    runtime: HookRuntime,
    role: Option<String>,
    /// Agent identity (forwarded from caller's env).
    agent_id: Option<String>,
    /// TL session ID for event routing (forwarded from caller's env).
    session_id: Option<String>,
}

/// Server-side hook handler state, shared across requests.
#[derive(Clone)]
struct HookState {
    plugins: Arc<tokio::sync::RwLock<HashMap<exomonad_core::AgentName, Arc<PluginManager>>>>,
    registry: Arc<exomonad_core::effects::EffectRegistry>,
    wasm_path: PathBuf,
    otel: Option<Arc<OtelService>>,
    zellij_session: String,
    default_role: exomonad_core::Role,
}

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

/// Handle a hook request server-side. All WASM, OTel, and Zellij logic runs here.
async fn handle_hook_request(
    axum::extract::Query(params): axum::extract::Query<HookQueryParams>,
    axum::extract::State(state): axum::extract::State<HookState>,
    body: String,
) -> axum::Json<HookEnvelope> {
    match handle_hook_inner(&params, &state, &body).await {
        Ok(envelope) => axum::Json(envelope),
        Err(e) => {
            warn!(error = %e, "Hook handler failed, returning allow");
            axum::Json(HookEnvelope {
                stdout: r#"{"continue":true}"#.to_string(),
                exit_code: 0,
            })
        }
    }
}

async fn handle_hook_inner(
    params: &HookQueryParams,
    state: &HookState,
    body: &str,
) -> Result<HookEnvelope> {
    let event_type = params.event;
    let runtime = params.runtime;
    let role = params
        .role
        .as_deref()
        .and_then(|r| match r {
            "tl" | "TL" => Some(exomonad_core::Role::TL),
            "dev" | "Dev" => Some(exomonad_core::Role::Dev),
            "worker" | "Worker" => Some(exomonad_core::Role::Worker),
            _ => None,
        })
        .unwrap_or(state.default_role);

    let trace_id = uuid::Uuid::new_v4().simple().to_string();
    let start_ns = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos() as u64;

    debug!(
        runtime = ?runtime,
        payload_len = body.len(),
        "Received hook event via HTTP"
    );

    // Emit HookReceived Zellij event
    if let Ok(branch) = git::get_current_branch() {
        if let Some(agent_id_str) = git::extract_agent_id(&branch) {
            match exomonad_core::ui_protocol::AgentId::try_from(agent_id_str.clone()) {
                Ok(agent_id) => {
                    let event = exomonad_core::ui_protocol::AgentEvent::HookReceived {
                        agent_id,
                        hook_type: event_type.to_string(),
                        timestamp: zellij_events::now_iso8601(),
                    };
                    if let Err(e) = zellij_events::emit_event(&state.zellij_session, &event) {
                        warn!("Failed to emit hook:received event: {}", e);
                    }
                }
                Err(e) => warn!("Invalid agent_id in branch '{}': {}", agent_id_str, e),
            }
        }
    }

    // Parse and inject runtime
    let mut hook_input: HookInput =
        serde_json::from_str(body).context("Failed to parse hook input")?;
    hook_input.runtime = Some(runtime);

    // Classify hook event into dispatch category. Exhaustive match ensures new
    // event types get handled explicitly rather than silently falling through.
    #[derive(Debug, Clone, Copy)]
    enum HookDispatch {
        /// Stop/lifecycle hooks: WASM returns InternalStopHookOutput (allow/block + reason)
        Stop,
        /// Tool hooks: WASM returns ClaudePreToolUseOutput (allow/deny/ask)
        ToolUse,
        /// Worker exit: WASM handles notifyParent as side effect, returns simple allow
        WorkerExit,
    }

    let dispatch = match event_type {
        HookEventType::Stop | HookEventType::AfterAgent => HookDispatch::Stop,
        HookEventType::SubagentStop => HookDispatch::Stop,
        HookEventType::SessionEnd => HookDispatch::Stop,
        HookEventType::PreToolUse | HookEventType::BeforeTool => HookDispatch::ToolUse,
        HookEventType::PostToolUse => HookDispatch::ToolUse,
        HookEventType::WorkerExit => HookDispatch::WorkerExit,
        HookEventType::SessionStart => HookDispatch::ToolUse,
        HookEventType::Notification
        | HookEventType::SubagentStart
        | HookEventType::PreCompact
        | HookEventType::PermissionRequest
        | HookEventType::UserPromptSubmit => {
            debug!(event = ?event_type, "Hook type not handled by WASM, allowing");
            let output_json = serde_json::to_string(&ClaudePreToolUseOutput::default())
                .context("Failed to serialize output")?;

            if let Some(ref otel) = state.otel {
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

            return Ok(HookEnvelope {
                stdout: output_json,
                exit_code: 0,
            });
        }
    };

    // Normalize event name for WASM dispatch
    let normalized_event_name = match event_type {
        HookEventType::Stop | HookEventType::AfterAgent => "Stop",
        HookEventType::SubagentStop => "SubagentStop",
        HookEventType::SessionEnd => "SessionEnd",
        HookEventType::PreToolUse | HookEventType::BeforeTool => "PreToolUse",
        HookEventType::PostToolUse => "PostToolUse",
        HookEventType::WorkerExit => "WorkerExit",
        HookEventType::SessionStart => "SessionStart",
        _ => unreachable!("passthrough events returned early above"),
    };
    hook_input.hook_event_name = normalized_event_name.to_string();

    // Create role-aware input for unified WASM
    let mut hook_input_value =
        serde_json::to_value(&hook_input).context("Failed to serialize hook input")?;
    if let serde_json::Value::Object(ref mut map) = hook_input_value {
        map.insert("role".to_string(), serde_json::json!(role));
        if let Some(ref agent_id) = params.agent_id {
            map.insert("agent_id".to_string(), serde_json::json!(agent_id));
        }
        if let Some(ref session_id) = params.session_id {
            map.insert(
                "exomonad_session_id".to_string(),
                serde_json::json!(session_id),
            );
        }
    }

    // Resolve agent identity and get per-agent plugin with baked-in EffectContext.
    let agent_name_for_hook =
        exomonad_core::AgentName::from(params.agent_id.as_deref().unwrap_or("root"));
    let birth_branch_for_hook =
        exomonad_core::BirthBranch::from(params.session_id.as_deref().unwrap_or("main"));
    debug!(
        agent_name = %agent_name_for_hook,
        birth_branch = %birth_branch_for_hook,
        agent_id_from_param = ?params.agent_id,
        "Hook identity context"
    );

    let plugin = get_or_create_plugin(
        &state.plugins,
        agent_name_for_hook.clone(),
        birth_branch_for_hook.clone(),
        &state.registry,
        &state.wasm_path,
    )
    .await
    .context("Failed to get plugin for hook")?;

    match dispatch {
        HookDispatch::WorkerExit => {
            // WASM handles notifyParent as a side effect. We call it and return allow.
            let _: serde_json::Value = plugin
                .call("handle_pre_tool_use", &hook_input_value)
                .await
                .context("WASM handleWorkerExit failed")?;

            Ok(HookEnvelope {
                stdout: serde_json::to_string(&ClaudePreToolUseOutput::default())?,
                exit_code: 0,
            })
        }

        HookDispatch::Stop => {
            let internal_output: InternalStopHookOutput = plugin
                .call("handle_pre_tool_use", &hook_input_value)
                .await
                .context("WASM handle_pre_tool_use (stop) failed")?;

            let output_json = internal_output.to_runtime_json(&runtime);

            let decision_str = match internal_output.decision {
                StopDecision::Allow => "allow",
                StopDecision::Block => "block",
            };

            if let Some(ref otel) = state.otel {
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

            // Emit StopHookBlocked Zellij event
            if internal_output.decision == StopDecision::Block
                && event_type == HookEventType::SubagentStop
            {
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
                                if let Err(e) =
                                    zellij_events::emit_event(&state.zellij_session, &event)
                                {
                                    warn!("Failed to emit stop_hook:blocked event: {}", e);
                                }
                            }
                            Err(e) => {
                                warn!("Invalid agent_id in branch '{}': {}", agent_id_str, e)
                            }
                        }
                    }
                }
            }

            Ok(HookEnvelope {
                stdout: output_json,
                exit_code: 0,
            })
        }

        HookDispatch::ToolUse => {
            let output: ClaudePreToolUseOutput = plugin
                .call("handle_pre_tool_use", &hook_input_value)
                .await
                .context("WASM handle_pre_tool_use failed")?;

            let output_json =
                serde_json::to_string(&output).context("Failed to serialize output")?;

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

            if let Some(ref otel) = state.otel {
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

            let exit_code = if output.continue_ { 0 } else { 2 };

            Ok(HookEnvelope {
                stdout: output_json,
                exit_code,
            })
        }
    }
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

    // Bootstrap: create .exo/config.toml if missing
    let cwd = std::env::current_dir()?;
    let config_path = cwd.join(".exo/config.toml");
    if !config_path.exists() {
        let dirname = cwd
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("project");
        eprintln!("Bootstrapping .exo/config.toml");
        std::fs::create_dir_all(cwd.join(".exo"))?;
        std::fs::write(
            &config_path,
            format!("default_role = \"tl\"\nzellij_session = \"{}\"\n", dirname),
        )?;

        // Add gitignore entries
        ensure_gitignore(&cwd)?;
    }

    // Write hook configuration (SessionStart registers Claude UUID for --fork-session)
    let binary_path = exomonad_core::find_exomonad_binary();
    exomonad_core::hooks::HookConfig::write_persistent(&cwd, &binary_path)
        .context("Failed to write hook configuration")?;
    eprintln!("Hook configuration written to .claude/settings.local.json");

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
    let layout_path = generate_tl_layout(port, config.shell_command.as_deref())?;

    let err = std::process::Command::new("zellij")
        .arg("--session")
        .arg(&session)
        .arg("--new-session-with-layout")
        .arg(&layout_path)
        .exec();
    Err(err).context("Failed to exec zellij with layout")
}

/// Ensure .gitignore has entries for .exo/ (track config, ignore the rest).
fn ensure_gitignore(project_dir: &std::path::Path) -> Result<()> {
    let gitignore_path = project_dir.join(".gitignore");
    let content = if gitignore_path.exists() {
        std::fs::read_to_string(&gitignore_path)?
    } else {
        String::new()
    };

    let has_ignore = content.lines().any(|l| l.trim() == ".exo/*");
    let has_negate = content.lines().any(|l| l.trim() == "!.exo/config.toml");
    if has_ignore && has_negate {
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
    writeln!(file, "# ExoMonad - track config, ignore runtime artifacts")?;
    writeln!(file, ".exo/*")?;
    writeln!(file, "!.exo/config.toml")?;

    eprintln!("Updated .gitignore with .exo/ entries");
    Ok(())
}

/// Generate a two-tab TL layout: Server tab + TL tab.
///
/// Tab 1 "Server": runs `exomonad serve` on TCP port, stays open on exit.
/// Tab 2 "TL": runs shell_command (or $SHELL) for the dev environment, focused by default.
fn generate_tl_layout(port: u16, shell_command: Option<&str>) -> Result<std::path::PathBuf> {
    let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/zsh".to_string());
    let cwd = std::env::current_dir()?;

    // Server tab: wrap serve command in shell_command if configured
    let serve_command = match shell_command {
        Some(sc) => format!("{} -c 'exomonad serve --port {}'", sc, port),
        None => format!("exomonad serve --port {}", port),
    };

    // TL tab: shell wrapping claude --resume (user picks session interactively)
    let tl_command = match shell_command {
        Some(sc) => format!("{} -c 'claude --dangerously-skip-permissions --resume'", sc),
        None => "claude --dangerously-skip-permissions --resume".to_string(),
    };

    let tabs = vec![
        exomonad_core::layout::AgentTabParams {
            tab_name: "Server",
            pane_name: "exomonad-serve",
            command: &serve_command,
            cwd: &cwd,
            shell: &shell,
            focus: false,
            close_on_exit: false,
        },
        exomonad_core::layout::AgentTabParams {
            tab_name: "TL",
            pane_name: "Main",
            command: &tl_command,
            cwd: &cwd,
            shell: &shell,
            focus: true,
            close_on_exit: false,
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
/// - **File layer**: Rolling daily logs in .exo/logs/
/// - **Stderr layer**: Always present (HTTP serve mode, stderr is safe)
///
/// JSON formatting controlled by EXOMONAD_LOG_FORMAT=json.
fn init_logging() -> Option<tracing_appender::non_blocking::WorkerGuard> {
    let use_json = std::env::var("EXOMONAD_LOG_FORMAT")
        .map(|v| v.eq_ignore_ascii_case("json"))
        .unwrap_or(false);

    let log_dir = PathBuf::from(".exo/logs");
    let file_ok = std::fs::create_dir_all(&log_dir).is_ok();
    if !file_ok {
        eprintln!("Failed to create .exo/logs/. Falling back to stderr-only logging.");
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
/// Resolve birth branch for a per-agent route by reading the git branch from its worktree.
///
/// Agent names have a type suffix (-claude, -gemini) that must be stripped to get the
/// worktree slug. The worktree's current git branch IS the birth branch.
async fn resolve_agent_birth_branch(
    worktree_base: &std::path::Path,
    agent_name: &str,
) -> exomonad_core::BirthBranch {
    let slug = agent_name
        .trim_end_matches("-claude")
        .trim_end_matches("-gemini");
    let worktree_path = worktree_base.join(slug);
    match tokio::process::Command::new("git")
        .args(["rev-parse", "--abbrev-ref", "HEAD"])
        .current_dir(&worktree_path)
        .output()
        .await
    {
        Ok(output) if output.status.success() => {
            let branch = String::from_utf8_lossy(&output.stdout).trim().to_string();
            tracing::debug!(agent = %agent_name, branch = %branch, "Resolved agent birth branch from worktree");
            exomonad_core::BirthBranch::from(branch.as_str())
        }
        Ok(output) => {
            tracing::warn!(
                agent = %agent_name,
                path = %worktree_path.display(),
                stderr = %String::from_utf8_lossy(&output.stderr),
                "Failed to resolve birth branch, falling back to root"
            );
            exomonad_core::BirthBranch::root()
        }
        Err(e) => {
            tracing::warn!(
                agent = %agent_name,
                path = %worktree_path.display(),
                error = %e,
                "Failed to run git in worktree, falling back to root"
            );
            exomonad_core::BirthBranch::root()
        }
    }
}

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

    match cli.command {
        Commands::Recompile { ref role } => {
            let default_role = config.role.to_string();
            let role_str = role.as_deref().unwrap_or(&default_role);
            let project_dir = if config.project_dir.is_absolute() {
                config.project_dir.clone()
            } else {
                std::env::current_dir()?.join(&config.project_dir)
            };
            return exomonad::recompile::run_recompile(role_str, &project_dir).await;
        }

        Commands::Serve { port } => {
            let port = port.unwrap_or(config.port);
            let project_dir = if config.project_dir.is_absolute() {
                config.project_dir.clone()
            } else {
                std::env::current_dir()?.join(&config.project_dir)
            };

            let role_name = config.role.to_string();
            let wasm_dir = config.wasm_dir.clone();

            // Prefer unified WASM (contains all roles, role selection per-call).
            // Fall back to per-role WASM for backwards compatibility.
            let unified_path = wasm_dir.join("wasm-guest-unified.wasm");
            let wasm_path = if unified_path.exists() {
                info!(dir = %wasm_dir.display(), "Using unified WASM (all roles in one module)");
                unified_path
            } else {
                let fallback = wasm_dir.join(format!("wasm-guest-{}.wasm", role_name));
                info!(role = %role_name, dir = %wasm_dir.display(), "Unified WASM not found, falling back to per-role WASM");
                fallback
            };

            if !wasm_path.exists() {
                anyhow::bail!(
                    "WASM file not found: {}\nRun `exomonad recompile` first to build it.",
                    wasm_path.display()
                );
            }

            let server_pid_path = project_dir.join(".exo/server.pid");

            let remote_port = std::env::var("EXOMONAD_SERVER_PORT")
                .ok()
                .and_then(|s| s.parse().ok());

            // Validate prerequisites
            exomonad_core::services::validate_git().context("Failed to validate git")?;

            let secrets = exomonad_core::services::secrets::Secrets::load();
            let executor: Arc<dyn exomonad_core::services::docker::CommandExecutor> =
                Arc::new(exomonad_core::services::local::LocalExecutor::new());
            let git = Arc::new(exomonad_core::services::git::GitService::new(executor));
            let git_wt = Arc::new(exomonad_core::services::git_worktree::GitWorktreeService::new(
                project_dir.clone(),
            ));
            let github = secrets
                .github_token()
                .and_then(|t| exomonad_core::services::github::GitHubService::new(t).ok());

            if github.is_some() {
                exomonad_core::services::validate_gh_cli().context("Failed to validate gh CLI")?;
            }

            let project_dir_for_services = project_dir.clone();
            let mut agent_control =
                exomonad_core::services::agent_control::AgentControlService::new(
                    project_dir_for_services.clone(),
                    github.clone(),
                    git_wt.clone(),
                );
            let worktree_base = config.worktree_base;
            agent_control = agent_control
                .with_worktree_base(worktree_base.clone())
                .with_mcp_server_port(port);
            agent_control = agent_control.with_birth_branch(exomonad_core::BirthBranch::root());
            agent_control = agent_control.with_zellij_session(config.zellij_session.clone());
            let event_session_id = uuid::Uuid::new_v4().to_string();
            let agent_control = Arc::new(agent_control);

            let event_queue = Arc::new(exomonad_core::services::event_queue::EventQueue::new());
            let claude_session_registry = Arc::new(
                exomonad_core::services::claude_session_registry::ClaudeSessionRegistry::new(),
            );

            info!(
                wasm_path = %wasm_path.display(),
                port = %port,
                role = %role_name,
                event_session_id = %event_session_id,
                "Starting HTTP MCP server with file-based WASM (hot reload enabled)"
            );

            // Build runtime with handler groups
            let mut builder = RuntimeBuilder::new()
                .with_wasm_path(wasm_path.clone())
                .require_namespaces(vec![
                    "log".to_string(),
                    "kv".to_string(),
                    "fs".to_string(),
                    "git".to_string(),
                    "github".to_string(),
                    "agent".to_string(),
                    "popup".to_string(),
                    "events".to_string(),
                    "messaging".to_string(),
                    "coordination".to_string(),
                    "session".to_string(),
                ]);
            // Create structured event log (JSONL)
            let event_log = match exomonad_core::services::EventLog::open(
                project_dir.join(".exo/events.jsonl"),
            ) {
                Ok(el) => {
                    info!(path = %project_dir.join(".exo/events.jsonl").display(), "Event log opened");
                    Some(Arc::new(el))
                }
                Err(e) => {
                    warn!(error = %e, "Failed to open event log, structured events will not be recorded");
                    None
                }
            };

            builder = builder.with_handlers(exomonad_core::core_handlers(
                project_dir.clone(),
                event_log.clone(),
            ));
            builder = builder.with_handlers(exomonad_core::git_handlers(git, github, git_wt));
            let team_registry = Arc::new(exomonad_core::services::team_registry::TeamRegistry::new());
            let (orch_handlers, question_registry) = exomonad_core::orchestration_handlers(
                agent_control.clone(),
                event_queue.clone(),
                Some(config.zellij_session.clone()),
                project_dir.clone(),
                remote_port,
                Some(event_session_id),
                claude_session_registry,
                team_registry.clone(),
            );
            builder = builder.with_handlers(orch_handlers);
            let rt = builder.build().await.context("Failed to build runtime")?;

            // Extract the shared registry for creating per-agent plugins
            let rt_registry = rt.registry.clone();
            let root_plugin = Arc::new(rt.plugin_manager);

            // Per-agent plugin cache — each agent gets its own PluginManager with baked-in identity
            let plugins: Arc<
                tokio::sync::RwLock<HashMap<exomonad_core::AgentName, Arc<PluginManager>>>,
            > = Arc::new(tokio::sync::RwLock::new(HashMap::new()));

            // Pre-populate with the root agent's plugin
            plugins
                .write()
                .await
                .insert(exomonad_core::AgentName::from("root"), root_plugin.clone());

            let base_state = exomonad_core::mcp::McpState {
                project_dir: project_dir.clone(),
                plugin: root_plugin,
                role: None,
                question_registry: Some(question_registry),
            };

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

            // Start GitHub Poller (background service)
            let mut poller = exomonad_core::services::github_poller::GitHubPoller::new(
                event_queue.clone(),
                project_dir.clone(),
            );
            if let Some(ref el) = event_log {
                poller = poller.with_event_log(el.clone());
            }
            poller = poller.with_team_registry(team_registry);
            tokio::spawn(async move {
                poller.run().await;
            });

            // Lazy server cache — one McpServer per role, created on first request.
            // Rust never enumerates roles; WASM AllRoles.hs is the single source of truth.
            let servers: Arc<tokio::sync::RwLock<HashMap<String, McpServer>>> =
                Arc::new(tokio::sync::RwLock::new(HashMap::new()));

            // Health endpoint
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

            // Unified per-agent MCP handler: /agents/{role}/{name}/mcp
            let unified_handler = {
                let servers = servers.clone();
                let plugins = plugins.clone();
                let registry = rt_registry.clone();
                let wasm_path_for_handler = wasm_path.clone();
                let base_state = base_state.clone();
                let wb = worktree_base.clone();
                move |axum::extract::Path((role, name)): axum::extract::Path<(String, String)>,
                      headers: axum::http::HeaderMap,
                      body: axum::extract::Json<serde_json::Value>| {
                    let servers = servers.clone();
                    let plugins = plugins.clone();
                    let registry = registry.clone();
                    let wasm_path_for_handler = wasm_path_for_handler.clone();
                    let base_state = base_state.clone();
                    let wb = wb.clone();
                    async move {
                        // Resolve agent identity
                        let agent_name = exomonad_core::AgentName::from(name.as_str());

                        // Fast path: check plugin cache before resolving birth branch
                        let plugin = {
                            let cache = plugins.read().await;
                            cache.get(&agent_name).cloned()
                        };

                        let plugin = if let Some(p) = plugin {
                            p
                        } else {
                            // Slow path: resolve birth branch only when creating a new plugin
                            let birth_branch = if name == "root" {
                                exomonad_core::BirthBranch::root()
                            } else {
                                resolve_agent_birth_branch(&wb, &name).await
                            };
                            match get_or_create_plugin(
                                &plugins,
                                agent_name.clone(),
                                birth_branch,
                                &registry,
                                &wasm_path_for_handler,
                            )
                            .await
                            {
                                Ok(p) => p,
                                Err(e) => {
                                    tracing::error!(agent = %agent_name, error = %e, "Failed to create plugin");
                                    return axum::http::StatusCode::INTERNAL_SERVER_ERROR
                                        .into_response();
                                }
                            }
                        };

                        // Get or create McpServer for this role (with per-agent plugin)
                        let server = {
                            let cache = servers.read().await;
                            cache.get(&role).cloned()
                        };
                        let server = match server {
                            Some(s) => s,
                            None => {
                                let mut state = base_state.clone();
                                state.role = Some(role.clone());
                                state.plugin = plugin.clone();
                                let s = McpServer::new(state);
                                servers.write().await.insert(role.clone(), s.clone());
                                s
                            }
                        };

                        server.handle(headers, body, Some(plugin)).await
                    }
                }
            };

            // Event notification endpoint
            let events_handler = move |body: axum::body::Bytes| {
                let queue = event_queue.clone();
                async move {
                    use exomonad_proto::effects::events::NotifyEventRequest;
                    use prost::Message;

                    match NotifyEventRequest::decode(body) {
                        Ok(req) => {
                            if let Some(event) = req.event {
                                queue.notify_event(&req.session_id, event).await;
                                (axum::http::StatusCode::OK, "OK")
                            } else {
                                (axum::http::StatusCode::BAD_REQUEST, "Missing event")
                            }
                        }
                        Err(_) => (axum::http::StatusCode::BAD_REQUEST, "Invalid protobuf"),
                    }
                }
            };

            // Hook handler state (shared OtelService created once)
            let hook_state = HookState {
                plugins: plugins.clone(),
                registry: rt_registry.clone(),
                wasm_path: wasm_path.clone(),
                otel: OtelService::from_env().ok().map(Arc::new),
                zellij_session: config.zellij_session.clone(),
                default_role: config.role,
            };

            // Minimal SSE handler for MCP Streamable HTTP protocol compatibility.
            // Gemini CLI sends GET to establish an SSE event stream during initialization.
            // Returns Content-Type: text/event-stream with keep-alive pings.
            let sse_handler = || async {
                let stream = futures_util::stream::pending::<
                    Result<axum::response::sse::Event, std::convert::Infallible>,
                >();
                axum::response::sse::Sse::new(stream).keep_alive(
                    axum::response::sse::KeepAlive::new()
                        .interval(std::time::Duration::from_secs(15))
                        .text("keep-alive"),
                )
            };

            let cors = CorsLayer::new()
                .allow_origin(Any)
                .allow_methods(Any)
                .allow_headers(Any);

            let app = axum::Router::new()
                .route("/health", axum::routing::get(health_handler))
                .route(
                    "/hook",
                    axum::routing::post(handle_hook_request).with_state(hook_state),
                )
                .route(
                    "/agents/{role}/{name}/mcp",
                    axum::routing::post(unified_handler).get(sse_handler),
                )
                .route("/events", axum::routing::post(events_handler))
                .layer(cors)
                .layer(TraceLayer::new_for_http());

            let addr = std::net::SocketAddr::from((std::net::Ipv6Addr::UNSPECIFIED, port));
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
            info!(port = %port, addr = %addr, "HTTP MCP server listening on TCP (dual-stack)");

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
                })
                .await?;

            // Clean up server.pid on shutdown
            if server_pid_path.exists() {
                let _ = std::fs::remove_file(&server_pid_path);
                info!("Cleaned up server.pid");
            }

            info!("HTTP MCP server shut down");
        }

        Commands::Hook { event, runtime } => {
            use std::io::Read;
            use std::time::Duration;

            let port = config.port;
            let mut url = format!(
                "http://127.0.0.1:{}/hook?event={}&runtime={}",
                port, event, runtime
            );
            // Forward agent identity env vars so the server can inject them into WASM calls
            if let Ok(agent_id) = std::env::var("EXOMONAD_AGENT_ID") {
                url.push_str(&format!("&agent_id={}", encode(&agent_id)));
            }
            if let Ok(session_id) = std::env::var("EXOMONAD_SESSION_ID") {
                url.push_str(&format!("&session_id={}", encode(&session_id)));
            }

            let mut body = String::new();
            std::io::stdin()
                .read_to_string(&mut body)
                .context("Failed to read stdin")?;

            let client = reqwest::Client::builder()
                .timeout(Duration::from_secs(30))
                .build()?;

            debug!(url = %url, event = ?event, "Forwarding hook to server");

            let response = client
                .post(&url)
                .header("content-type", "application/json")
                .body(body)
                .send()
                .await;

            let resp = match response {
                Ok(r) => {
                    debug!(status = %r.status(), "Hook forwarding response");
                    if r.status().is_success() {
                        r.json::<HookEnvelope>().await?
                    } else {
                        eprintln!("exomonad hook: server returned {}", r.status());
                        println!(r#"{{"continue":true}}"#);
                        return Ok(());
                    }
                }
                Err(e) => {
                    eprintln!("exomonad hook: server unreachable: {}", e);
                    println!(r#"{{"continue":true}}"#);
                    return Ok(());
                }
            };

            print!("{}", resp.stdout);
            if resp.exit_code != 0 {
                std::process::exit(resp.exit_code);
            }
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
                .unwrap_or_else(|_| ".exo/sockets/control.sock".to_string());

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

            info!("Sent reply to control socket");
        }
    }

    Ok(())
}