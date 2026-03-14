//! exomonad: Rust host with embedded Haskell WASM plugin.
//!
//! This binary runs as a sidecar in each agent container, handling:
//! - Claude Code hooks via HTTP forwarding to the server
//! - MCP tools via WASM plugin (server-side)
//!
//! WASM plugins are loaded from file (server-side only).

mod mcp_stdio;
mod uds_client;

use exomonad::config;
use urlencoding::encode;

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use exomonad_core::protocol::{Runtime as HookRuntime, ServiceRequest};
use exomonad_core::services::external::otel::OtelService;
use exomonad_core::services::external::ExternalService;
use exomonad_core::services::{git, tmux_events, AgentType};
use std::time::{Duration, Instant};

use axum::response::IntoResponse;
use exomonad_core::{
    ClaudePreToolUseOutput, HookEnvelope, HookEventType, HookInput, HookSpecificOutput,
    InternalStopHookOutput, PluginManager, RuntimeBuilder, StopDecision, ToolPermission,
};
use std::collections::HashMap;
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

    /// Initialize tmux session for this project.
    ///
    /// Creates a new session if none exists, or attaches to existing.
    /// Session name is read from .exo/config.toml tmux_session field.
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
        /// WASM package to build (default: from config wasm_name, usually "devswarm")
        #[arg(long)]
        role: Option<String>,
    },

    /// Run MCP server on Unix domain socket (.exo/server.sock)
    ///
    /// Loads WASM from file path (not embedded) with hot reload on change.
    Serve,

    /// Run stdio MCP proxy (stdin/stdout ↔ UDS server)
    McpStdio {
        /// Agent role (e.g., "tl", "dev", "worker")
        #[arg(long)]
        role: String,
        /// Agent name (e.g., "root", "feature-impl")
        #[arg(long)]
        name: String,
    },

    /// Reply to a UI request
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

    /// Reload WASM plugins (clears plugin cache, next call loads fresh from disk)
    Reload,

    /// Gracefully shut down the running server
    Shutdown,
}

// ============================================================================
// Per-Role WASM Resolution
// ============================================================================

/// Resolve the WASM file path for a given role.
/// Convention: if `wasm-guest-{role}.wasm` exists, use it (role-specific WASM).
/// Otherwise fall back to `wasm-guest-{default_name}.wasm` (shared WASM).
fn resolve_wasm_path_for_role(
    wasm_dir: &std::path::Path,
    role: &str,
    default_name: &str,
) -> Option<PathBuf> {
    let role_specific = wasm_dir.join(format!("wasm-guest-{role}.wasm"));
    if role_specific.exists() {
        return Some(role_specific);
    }
    let default_path = wasm_dir.join(format!("wasm-guest-{default_name}.wasm"));
    if default_path.exists() {
        return Some(default_path);
    }
    None
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
// REST API Types
// ============================================================================

/// Request body for POST /agents/{role}/{name}/tools/call.
#[derive(serde::Deserialize)]
struct ToolCallRequest {
    name: String,
    #[serde(default)]
    arguments: serde_json::Value,
}

/// Resolve the per-agent PluginManager (cached, with identity).
async fn resolve_plugin(
    plugins: &tokio::sync::RwLock<HashMap<exomonad_core::AgentName, Arc<PluginManager>>>,
    registry: &Arc<exomonad_core::effects::EffectRegistry>,
    worktree_base: &std::path::Path,
    name: &str,
    wasm_path: &std::path::Path,
) -> anyhow::Result<Arc<PluginManager>> {
    let agent_name = exomonad_core::AgentName::from(name);

    // Fast path: check plugin cache
    {
        let cache = plugins.read().await;
        if let Some(p) = cache.get(&agent_name) {
            return Ok(p.clone());
        }
    }

    // Slow path: resolve birth branch and create plugin
    let birth_branch = if name == "root" {
        exomonad_core::BirthBranch::root()
    } else {
        resolve_agent_birth_branch(worktree_base, name).await
    };

    get_or_create_plugin(plugins, agent_name, birth_branch, registry, wasm_path).await
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
    tmux_session: String,
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

/// Handle a hook request server-side. All WASM, OTel, and tmux logic runs here.
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
        .as_ref()
        .map(|r| exomonad_core::Role::from(r.as_str()))
        .unwrap_or(state.default_role.clone());

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

    // Emit HookReceived tmux event
    if let Ok(branch) = git::get_current_branch() {
        if let Some(agent_id_str) = git::extract_agent_id(&branch) {
            match exomonad_core::ui_protocol::AgentId::try_from(agent_id_str.clone()) {
                Ok(agent_id) => {
                    let event = exomonad_core::ui_protocol::AgentEvent::HookReceived {
                        agent_id,
                        hook_type: event_type.to_string(),
                        timestamp: tmux_events::now_iso8601(),
                    };
                    if let Err(e) = tmux_events::emit_event(&state.tmux_session, &event) {
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

    // Resolve agent identity defaults (used for both injection and PluginManager)
    let agent_name_for_hook =
        exomonad_core::AgentName::from(params.agent_id.as_deref().unwrap_or("root"));
    let birth_branch_for_hook =
        exomonad_core::BirthBranch::from(params.session_id.as_deref().unwrap_or("main"));

    // Always inject identity into WASM input (hooks need it even when env vars aren't set)
    if let serde_json::Value::Object(ref mut map) = hook_input_value {
        map.insert("role".to_string(), serde_json::json!(role));
        map.insert(
            "agent_id".to_string(),
            serde_json::json!(agent_name_for_hook.to_string()),
        );
        map.insert(
            "exomonad_session_id".to_string(),
            serde_json::json!(birth_branch_for_hook.to_string()),
        );
    }

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

            // Emit StopHookBlocked tmux event
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
                                        timestamp: tmux_events::now_iso8601(),
                                    };
                                if let Err(e) =
                                    tmux_events::emit_event(&state.tmux_session, &event)
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

/// Run the init command: create or attach to tmux session.
///
/// Three-way session handling:
/// - Session alive → attach
/// - No session → create fresh
///
/// With `--recreate`: delete any existing session (even alive), then create fresh.
///
async fn run_init(session_override: Option<String>, recreate: bool) -> Result<()> {
    use exomonad_core::services::tmux_ipc::TmuxIpc;

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
    let config = config::Config::discover()?;
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
    let tmux_check = std::process::Command::new("tmux")
        .arg("-V")
        .output();
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
        // Clean up stale socket (unconditional — may exist from a previous session
        // even if tmux isn't running, e.g. after migrating from Zellij)
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
        // Attach to running session (exec replaces process, releasing the binary)
        info!(session = %session, "Attaching to session");
        return TmuxIpc::attach_session(&session);
    }

    // Create fresh session
    info!(session = %session, "Creating session");

    // 1. Write .mcp.json BEFORE session starts (Claude Code reads it at startup)
    let mcp_json = serde_json::json!({
        "mcpServers": {
            "exomonad": {
                "type": "stdio",
                "command": "exomonad",
                "args": ["mcp-stdio", "--role", "tl", "--name", "root"]
            }
        }
    });
    std::fs::write(
        cwd.join(".mcp.json"),
        serde_json::to_string_pretty(&mcp_json)?,
    )?;
    info!("Wrote .mcp.json with stdio MCP config");

    // 2. Create session in background (returns stable @N window ID)
    let server_window_id = TmuxIpc::new_session(&session, &cwd)?;

    // Verify session is usable
    if !TmuxIpc::has_session(&session)? {
        anyhow::bail!(
            "tmux session '{}' was created but is not responding. Check tmux server status.",
            session
        );
    }

    // Set EXOMONAD_TMUX_SESSION in the tmux session environment so all
    // windows/panes inherit it (used by tmux_events, agent_control, etc.)
    let set_env_output = std::process::Command::new("tmux")
        .args(["set-environment", "-t", &session, "EXOMONAD_TMUX_SESSION", &session])
        .output()
        .context("Failed to set EXOMONAD_TMUX_SESSION in tmux environment")?;
    if !set_env_output.status.success() {
        anyhow::bail!(
            "tmux set-environment failed: {}",
            String::from_utf8_lossy(&set_env_output.stderr)
        );
    }

    // 3. Setup windows
    let ipc = TmuxIpc::new(&session);
    let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/zsh".to_string());

    // Rename initial window to "Server" using stable @N ID (base-index immune)
    let server_target = server_window_id;
    let status = std::process::Command::new("tmux")
        .args(["rename-window", "-t", &server_target, "Server"])
        .status()
        .context("Failed to run tmux rename-window")?;
    if !status.success() {
        warn!("tmux rename-window failed with status: {}", status);
    }
    let serve_cmd = format!("EXOMONAD_TMUX_SESSION={} exomonad serve", &session);
    let status = std::process::Command::new("tmux")
        .args(["send-keys", "-t", &server_target, &serve_cmd, "Enter"])
        .status()
        .context("Failed to run tmux send-keys")?;
    if !status.success() {
        warn!("tmux send-keys failed with status: {}", status);
    }

    // Create "TL" window
    let base_command = match (config.root_agent_type, config.initial_prompt.as_deref()) {
        (AgentType::Claude, _) => "claude --dangerously-skip-permissions -c || claude --dangerously-skip-permissions; echo; echo [Claude Code exited]; exec bash -l".to_string(),
        (AgentType::Gemini, Some(prompt)) => {
            format!(
                "gemini --prompt-interactive '{}'",
                prompt.replace('\'', "'\\''")
            )
        }
        (AgentType::Gemini, None) => "gemini".to_string(),
        (AgentType::Shoal, Some(prompt)) => {
            format!("shoal-agent --prompt '{}'", prompt.replace('\'', "'\\''"))
        }
        (AgentType::Shoal, None) => "shoal-agent".to_string(),
    };

    let tl_command = match config.shell_command {
        Some(sc) => format!("{} -c \"{}\"", sc, base_command.replace('"', "\\\"")),
        None => base_command,
    };

    ipc.new_window("TL", &cwd, &shell, &tl_command)?;

    // 4. Poll for server socket
    wait_for_server_socket(&cwd).await?;

    // 5. Attach
    info!(session = %session, "Attaching to session");
    TmuxIpc::attach_session(&session)
}

/// Ensure .gitignore has entries for .exo/ (track config, ignore the rest).
fn ensure_gitignore(project_dir: &std::path::Path) -> Result<()> {
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

    info!("Updated .gitignore with .exo/ entries");
    Ok(())
}

/// Wait for the server socket to appear and respond to health checks.
async fn wait_for_server_socket(project_dir: &std::path::Path) -> Result<()> {
    let socket_path = project_dir.join(".exo/server.sock");
    let start = Instant::now();
    let timeout_dur = Duration::from_secs(30);

    debug!("Waiting for server socket...");

    // Phase 1: Wait for socket file to exist
    while start.elapsed() < timeout_dur {
        if socket_path.exists() {
            break;
        }
        tokio::time::sleep(Duration::from_millis(500)).await;
    }

    if !socket_path.exists() {
        return Err(anyhow::anyhow!(
            "Server socket not found at {} after 30s. Check the Server tab for errors.",
            socket_path.display()
        ));
    }

    // Phase 2: Verify server responds to health check
    let client = uds_client::ServerClient::new(socket_path.to_path_buf());
    for _ in 0..5 {
        if client.is_healthy().await {
            return Ok(());
        }
        tokio::time::sleep(Duration::from_millis(500)).await;
    }

    Err(anyhow::anyhow!(
        "Server socket exists but health check failed after {:.1}s. Check the Server tab for errors.",
        start.elapsed().as_secs_f64()
    ))
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
        warn!("Failed to create .exo/logs/. Falling back to stderr-only logging.");
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

    // 1. Try worktree (subtrees have their own git branch)
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
            return exomonad_core::BirthBranch::from(branch.as_str());
        }
        _ => {}
    }

    // 2. Try agent config dir (workers write .birth_branch at spawn time)
    if let Some(exo_dir) = worktree_base.parent() {
        let bb_file = exo_dir
            .join("agents")
            .join(agent_name)
            .join(".birth_branch");
        if let Ok(contents) = tokio::fs::read_to_string(&bb_file).await {
            let branch = contents.trim().to_string();
            tracing::debug!(agent = %agent_name, branch = %branch, "Resolved agent birth branch from config file");
            return exomonad_core::BirthBranch::from(branch.as_str());
        }
    }

    // 3. Fallback to root
    tracing::warn!(
        agent = %agent_name,
        "Failed to resolve birth branch from worktree or config, falling back to root"
    );
    exomonad_core::BirthBranch::root()
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
        Commands::McpStdio { ref role, ref name } => {
            return mcp_stdio::run(role, name).await;
        }

        Commands::Recompile { ref role } => {
            let role_str = role.as_deref().unwrap_or(&config.wasm_name);
            let project_dir = if config.project_dir.is_absolute() {
                config.project_dir.clone()
            } else {
                std::env::current_dir()?.join(&config.project_dir)
            };
            return exomonad::recompile::run_recompile(
                role_str,
                &project_dir,
                config.flake_ref.as_deref(),
            )
            .await;
        }

        Commands::Serve => {
            let project_dir = if config.project_dir.is_absolute() {
                config.project_dir.clone()
            } else {
                std::env::current_dir()?.join(&config.project_dir)
            };

            let role_name = config.role.to_string();
            let wasm_dir = config.wasm_dir.clone();
            let wasm_name = config.wasm_name.clone();

            // Resolve default WASM (for root TL role and as fallback)
            let wasm_path = resolve_wasm_path_for_role(&wasm_dir, &role_name, &wasm_name)
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "WASM file not found in {}\nRun `exomonad recompile` first to build it.",
                        wasm_dir.display()
                    )
                })?;

            let server_pid_path = project_dir.join(".exo/server.pid");

            // Validate prerequisites
            exomonad_core::services::validate_git().context("Failed to validate git")?;

            let secrets = exomonad_core::services::secrets::Secrets::load();
            let executor: Arc<dyn exomonad_core::services::command::CommandExecutor> =
                Arc::new(exomonad_core::services::local::LocalExecutor::new());
            let git = Arc::new(exomonad_core::services::git::GitService::new(executor));
            let git_wt = Arc::new(
                exomonad_core::services::git_worktree::GitWorktreeService::new(project_dir.clone()),
            );
            let github = secrets
                .github_token()
                .and_then(|t| exomonad_core::services::github::GitHubService::new(t).ok());

            if github.is_some() {
                exomonad_core::services::validate_gh_cli().context("Failed to validate gh CLI")?;
            }

            let team_registry =
                Arc::new(claude_teams_bridge::TeamRegistry::new());
            let acp_registry = Arc::new(exomonad_core::services::acp_registry::AcpRegistry::new());

            let project_dir_for_services = project_dir.clone();
            let mut agent_control =
                exomonad_core::services::agent_control::AgentControlService::new(
                    project_dir_for_services.clone(),
                    github.clone(),
                    git_wt.clone(),
                )
                .with_acp_registry(acp_registry.clone());
            let worktree_base = config.worktree_base;
            agent_control = agent_control.with_worktree_base(worktree_base.clone());
            agent_control = agent_control.with_birth_branch(exomonad_core::BirthBranch::root());
            agent_control = agent_control.with_tmux_session(config.tmux_session.clone());
            agent_control = agent_control.with_yolo(config.yolo);
            let event_session_id = uuid::Uuid::new_v4().to_string();
            let agent_control = Arc::new(agent_control);

            let event_queue = Arc::new(exomonad_core::services::event_queue::EventQueue::new());
            let mutex_registry = Arc::new(exomonad_core::services::MutexRegistry::new());
            mutex_registry.spawn_expiry_task();

            let claude_session_registry = Arc::new(
                exomonad_core::services::claude_session_registry::ClaudeSessionRegistry::new(),
            );

            info!(
                wasm_path = %wasm_path.display(),
                role = %role_name,
                event_session_id = %event_session_id,
                "Starting MCP server on Unix domain socket (hot reload enabled)"
            );

            // Build runtime with handler groups
            let mut builder = RuntimeBuilder::new()
                .with_wasm_path(wasm_path.clone())
                .require_namespaces(vec![
                    "log".to_string(),
                    "kv".to_string(),
                    "fs".to_string(),
                    "git".to_string(),
                    "agent".to_string(),
                    "events".to_string(),
                    "session".to_string(),
                    "coordination".to_string(),
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
            builder = builder.with_handlers(exomonad_core::git_handlers(
                git,
                github,
                git_wt,
                event_log.clone(),
            ));
            let orch_handlers = exomonad_core::orchestration_handlers(
                agent_control.clone(),
                event_queue.clone(),
                Some(config.tmux_session.clone()),
                project_dir.clone(),
                Some(event_session_id),
                claude_session_registry,
                team_registry.clone(),
                acp_registry.clone(),
                event_log.clone(),
                mutex_registry,
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

            // Write server.pid for stale socket detection
            let pid_info = serde_json::json!({
                "pid": std::process::id(),
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
            poller = poller.with_acp_registry(acp_registry.clone());
            poller = poller.with_plugins(plugins.clone());
            tokio::spawn(async move {
                poller.run().await;
            });

            // Health endpoint
            let health_plugin = root_plugin.clone();
            let health_role = role_name.clone();
            let health_handler = move || {
                let plugin = health_plugin.clone();
                let role = health_role.clone();
                async move {
                    let wasm_hash = plugin.content_hash();
                    axum::Json(serde_json::json!({
                        "status": "ok",
                        "version": env!("CARGO_PKG_VERSION"),
                        "role": role,
                        "wasm_hash": wasm_hash,
                    }))
                }
            };

            // REST handler: GET /agents/{role}/{name}/tools — list available tools
            let list_tools_handler = {
                let plugins = plugins.clone();
                let registry = rt_registry.clone();
                let wasm_path_default = wasm_path.clone();
                let wasm_dir_for_handler = wasm_dir.clone();
                let wasm_name_for_handler = wasm_name.clone();
                let wb = worktree_base.clone();
                move |axum::extract::Path((role, name)): axum::extract::Path<(String, String)>| {
                    let plugins = plugins.clone();
                    let registry = registry.clone();
                    let wasm_path_default = wasm_path_default.clone();
                    let wasm_dir_for_handler = wasm_dir_for_handler.clone();
                    let wasm_name_for_handler = wasm_name_for_handler.clone();
                    let wb = wb.clone();
                    async move {
                        let wasm_path_for_handler = resolve_wasm_path_for_role(
                            &wasm_dir_for_handler,
                            &role,
                            &wasm_name_for_handler,
                        )
                        .unwrap_or_else(|| wasm_path_default.clone());

                        let plugin = match resolve_plugin(
                            &plugins,
                            &registry,
                            &wb,
                            &name,
                            &wasm_path_for_handler,
                        )
                        .await
                        {
                            Ok(p) => p,
                            Err(e) => {
                                tracing::error!(role = %role, name = %name, error = %e, "Failed to resolve plugin");
                                return (
                                    axum::http::StatusCode::INTERNAL_SERVER_ERROR,
                                    axum::Json(serde_json::json!({"error": e.to_string()})),
                                )
                                    .into_response();
                            }
                        };

                        // Hot reload WASM if changed
                        let _ = plugin.reload_if_changed().await;

                        match exomonad_core::mcp::tools::get_tool_definitions(&plugin, Some(&role))
                            .await
                        {
                            Ok(tools) => {
                                axum::Json(serde_json::json!({ "tools": tools })).into_response()
                            }
                            Err(e) => {
                                tracing::error!(role = %role, error = %e, "Tool discovery failed");
                                (
                                    axum::http::StatusCode::INTERNAL_SERVER_ERROR,
                                    axum::Json(serde_json::json!({"error": e.to_string()})),
                                )
                                    .into_response()
                            }
                        }
                    }
                }
            };

            // REST handler: POST /agents/{role}/{name}/tools/call — call a tool
            let call_tool_handler = {
                let plugins = plugins.clone();
                let registry = rt_registry.clone();
                let wasm_path_default = wasm_path.clone();
                let wasm_dir_for_handler = wasm_dir.clone();
                let wasm_name_for_handler = wasm_name.clone();
                let wb = worktree_base.clone();
                move |axum::extract::Path((role, name)): axum::extract::Path<(String, String)>,
                      axum::extract::Json(body): axum::extract::Json<ToolCallRequest>| {
                    let plugins = plugins.clone();
                    let registry = registry.clone();
                    let wasm_path_default = wasm_path_default.clone();
                    let wasm_dir_for_handler = wasm_dir_for_handler.clone();
                    let wasm_name_for_handler = wasm_name_for_handler.clone();
                    let wb = wb.clone();
                    async move {
                        let wasm_path_for_handler = resolve_wasm_path_for_role(
                            &wasm_dir_for_handler, &role, &wasm_name_for_handler
                        ).unwrap_or_else(|| wasm_path_default.clone());

                        let plugin = match resolve_plugin(
                            &plugins, &registry, &wb, &name, &wasm_path_for_handler
                        ).await {
                            Ok(p) => p,
                            Err(e) => {
                                tracing::error!(role = %role, name = %name, error = %e, "Failed to resolve plugin");
                                return (
                                    axum::http::StatusCode::INTERNAL_SERVER_ERROR,
                                    axum::Json(serde_json::json!({"error": e.to_string()})),
                                ).into_response();
                            }
                        };

                        tracing::info!(tool = %body.name, role = %role, agent = %name, "Executing tool");

                        let input = exomonad_core::mcp::tools::MCPCallInput::new(
                            role, body.name.clone(), body.arguments,
                        );

                        let output: exomonad_core::mcp::tools::MCPCallOutput = match plugin.call("handle_mcp_call", &input).await {
                            Ok(o) => o,
                            Err(e) => {
                                tracing::error!(tool = %body.name, error = %e, "WASM call failed");
                                return axum::Json(serde_json::json!({
                                    "success": false,
                                    "result": null,
                                    "error": format!("WASM call failed: {}", e)
                                })).into_response();
                            }
                        };

                        axum::Json(serde_json::json!({
                            "success": output.success,
                            "result": output.result,
                            "error": output.error,
                        })).into_response()
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
                tmux_session: config.tmux_session.clone(),
                default_role: config.role,
            };

            // Shutdown signal for graceful shutdown via /shutdown endpoint
            let shutdown_signal = Arc::new(tokio::sync::Notify::new());

            // Reload handler: clears plugin cache, next call loads fresh WASM from disk
            let reload_handler = {
                let plugins = plugins.clone();
                move || {
                    let plugins = plugins.clone();
                    async move {
                        let mut cache = plugins.write().await;
                        let evicted = cache.len();
                        cache.clear();
                        info!(plugins_evicted = evicted, "Plugin cache cleared (reload)");
                        axum::Json(serde_json::json!({
                            "status": "ok",
                            "plugins_evicted": evicted,
                        }))
                    }
                }
            };

            // Shutdown handler: ack then signal graceful shutdown
            let shutdown_handler = {
                let signal = shutdown_signal.clone();
                move || {
                    let signal = signal.clone();
                    async move {
                        info!("Shutdown requested via /shutdown endpoint");
                        signal.notify_one();
                        axum::Json(serde_json::json!({"status": "ok"}))
                    }
                }
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
                    "/agents/{role}/{name}/tools",
                    axum::routing::get(list_tools_handler),
                )
                .route(
                    "/agents/{role}/{name}/tools/call",
                    axum::routing::post(call_tool_handler),
                )
                .route("/events", axum::routing::post(events_handler))
                .route("/reload", axum::routing::post(reload_handler))
                .route("/shutdown", axum::routing::post(shutdown_handler))
                .layer(cors)
                .layer(TraceLayer::new_for_http());

            // Bind Unix domain socket
            let socket_path = project_dir.join(".exo/server.sock");

            // Check for existing server
            if socket_path.exists() {
                let pid_path = project_dir.join(".exo/server.pid");
                if let Ok(content) = std::fs::read_to_string(&pid_path) {
                    if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&content) {
                        if let Some(pid) = parsed.get("pid").and_then(|v| v.as_u64()) {
                            use nix::sys::signal;
                            use nix::unistd::Pid;
                            if signal::kill(Pid::from_raw(pid as i32), None).is_ok() {
                                return Err(anyhow::anyhow!(
                                    "Server already running (PID {}). Stop it first or use a different project directory.",
                                    pid
                                ));
                            }
                        }
                    }
                }
                // Stale socket — clean up
                info!(path = %socket_path.display(), "Removing stale socket");
                std::fs::remove_file(&socket_path)?;
            }

            // Ensure parent directory exists
            if let Some(parent) = socket_path.parent() {
                std::fs::create_dir_all(parent)?;
            }

            let listener = tokio::net::UnixListener::bind(&socket_path).map_err(|e| {
                anyhow::anyhow!(
                    "Failed to bind Unix socket {}: {}",
                    socket_path.display(),
                    e
                )
            })?;

            // Set socket permissions to owner-only (0600)
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                std::fs::set_permissions(&socket_path, std::fs::Permissions::from_mode(0o600))?;
            }

            info!(socket = %socket_path.display(), "MCP server listening on Unix domain socket");

            let socket_path_for_cleanup = socket_path.clone();
            let server_pid_for_cleanup = server_pid_path.clone();

            // Run with graceful shutdown on SIGINT, SIGTERM, or /shutdown endpoint
            let shutdown_signal_for_server = shutdown_signal.clone();
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
                        _ = shutdown_signal_for_server.notified() => info!("Received /shutdown request, initiating graceful shutdown"),
                    }
                })
                .await?;

            // Clean up socket and pid on shutdown
            if socket_path_for_cleanup.exists() {
                let _ = std::fs::remove_file(&socket_path_for_cleanup);
                info!("Cleaned up server socket");
            }
            if server_pid_for_cleanup.exists() {
                let _ = std::fs::remove_file(&server_pid_for_cleanup);
                info!("Cleaned up server.pid");
            }

            info!("MCP server shut down");
        }

        Commands::Hook { event, runtime } => {
            use std::io::Read;
            use std::time::Duration;

            let mut path = format!("/hook?event={}&runtime={}", event, runtime);
            if let Ok(agent_id) = std::env::var("EXOMONAD_AGENT_ID") {
                path.push_str(&format!("&agent_id={}", encode(&agent_id)));
            }
            if let Ok(session_id) = std::env::var("EXOMONAD_SESSION_ID") {
                path.push_str(&format!("&session_id={}", encode(&session_id)));
            }
            if let Ok(role) = std::env::var("EXOMONAD_ROLE") {
                path.push_str(&format!("&role={}", encode(&role)));
            }

            let mut body = String::new();
            std::io::stdin()
                .read_to_string(&mut body)
                .context("Failed to read stdin")?;

            debug!(path = %path, event = ?event, "Forwarding hook to server via UDS");

            // SessionStart on root session: poll for socket to appear (server may still be starting)
            let is_root_session_start =
                event == HookEventType::SessionStart && std::env::var("EXOMONAD_AGENT_ID").is_err();

            let socket = if is_root_session_start {
                let start = std::time::Instant::now();
                let timeout_dur = Duration::from_secs(5);
                let mut found = None;
                while start.elapsed() < timeout_dur {
                    match uds_client::find_server_socket() {
                        Ok(s) => {
                            found = Some(s);
                            break;
                        }
                        Err(_) => {
                            tokio::time::sleep(Duration::from_millis(500)).await;
                        }
                    }
                }
                match found {
                    Some(s) => s,
                    None => {
                        warn!(
                            "exomonad hook: server socket not found after {}s",
                            timeout_dur.as_secs()
                        );
                        println!(r#"{{"continue":true}}"#);
                        return Ok(());
                    }
                }
            } else {
                match uds_client::find_server_socket() {
                    Ok(s) => s,
                    Err(e) => {
                        warn!("exomonad hook: {}", e);
                        println!(r#"{{"continue":true}}"#);
                        return Ok(());
                    }
                }
            };

            let client = uds_client::ServerClient::new(socket);

            // Parse stdin as JSON; fail-open on invalid input
            let json_body: serde_json::Value = match serde_json::from_str(&body) {
                Ok(v) => v,
                Err(e) => {
                    warn!("exomonad hook: invalid JSON: {}", e);
                    println!(r#"{{"continue":true}}"#);
                    return Ok(());
                }
            };

            match client
                .post_json::<serde_json::Value, HookEnvelope>(&path, &json_body)
                .await
            {
                Ok(resp) => {
                    print!("{}", resp.stdout);
                    if resp.exit_code != 0 {
                        std::process::exit(resp.exit_code);
                    }
                }
                Err(e) => {
                    warn!("exomonad hook: {}", e);
                    println!(r#"{{"continue":true}}"#);
                }
            }
        }

        Commands::Init { session, recreate } => {
            run_init(session, recreate).await?;
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

        Commands::Reload => {
            let socket = uds_client::find_server_socket()
                .context("Cannot find server socket. Is exomonad serve running?")?;
            let client = uds_client::ServerClient::new(socket);
            let resp: serde_json::Value = client
                .post_json("/reload", &serde_json::json!({}))
                .await
                .context("Reload request failed")?;
            println!("{}", serde_json::to_string_pretty(&resp)?);
        }

        Commands::Shutdown => {
            let socket = uds_client::find_server_socket()
                .context("Cannot find server socket. Is exomonad serve running?")?;
            let client = uds_client::ServerClient::new(socket);
            match client
                .post_json::<serde_json::Value, serde_json::Value>(
                    "/shutdown",
                    &serde_json::json!({}),
                )
                .await
            {
                Ok(resp) => println!("{}", serde_json::to_string_pretty(&resp)?),
                Err(_) => {
                    // Server may close connection before sending response — that's expected
                    println!("Shutdown signal sent");
                }
            }
        }
    }

    Ok(())
}
