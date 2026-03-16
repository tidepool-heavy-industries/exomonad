use crate::app_state::AppState;
use exomonad::config::Config;

use anyhow::{Context, Result};
use axum::{
    body::Bytes,
    extract::{Path, Query, State},
    response::IntoResponse,
    routing::{get, post},
    Json, Router,
};
use exomonad_core::protocol::Runtime as HookRuntime;
use exomonad_core::services::{git, tmux_events};
use exomonad_core::{
    AgentName, BirthBranch, ClaudePreToolUseOutput, HookEnvelope, HookEventType, HookInput,
    InternalStopHookOutput, PluginManager, Role, RuntimeBuilder, StopDecision,
};
use std::collections::HashMap;
use std::path::{Path as StdPath, PathBuf};
use std::sync::Arc;
use tower_http::cors::{Any, CorsLayer};
use tower_http::trace::TraceLayer;
use tracing::{debug, info, instrument, warn};

// ============================================================================
// REST API Types
// ============================================================================

/// Request body for POST /agents/{role}/{name}/tools/call.
#[derive(serde::Deserialize)]
pub struct ToolCallRequest {
    pub name: String,
    #[serde(default)]
    pub arguments: serde_json::Value,
}

/// Query parameters for the `/hook` endpoint.
#[derive(Debug, serde::Deserialize)]
pub struct HookQueryParams {
    pub event: HookEventType,
    pub runtime: HookRuntime,
    pub role: Option<String>,
    /// Agent identity (forwarded from caller's env).
    pub agent_id: Option<String>,
    /// TL session ID for event routing (forwarded from caller's env).
    pub session_id: Option<String>,
}

/// Server-side hook handler state, shared across requests.
#[derive(Clone)]
pub struct HookState {
    pub plugins: Arc<tokio::sync::RwLock<HashMap<AgentName, Arc<PluginManager>>>>,
    pub registry: Arc<exomonad_core::effects::EffectRegistry>,
    pub wasm_path: PathBuf,
    pub tmux_session: String,
    pub default_role: Role,
    pub event_log: Option<Arc<exomonad_core::services::EventLog>>,
}

// ============================================================================
// Per-Role WASM Resolution
// ============================================================================

pub fn resolve_wasm_path_for_role(
    wasm_dir: &StdPath,
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

pub async fn get_or_create_plugin(
    plugins: &tokio::sync::RwLock<HashMap<AgentName, Arc<PluginManager>>>,
    agent_name: AgentName,
    birth_branch: BirthBranch,
    registry: &Arc<exomonad_core::effects::EffectRegistry>,
    wasm_path: &StdPath,
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

pub async fn resolve_plugin(
    plugins: &tokio::sync::RwLock<HashMap<AgentName, Arc<PluginManager>>>,
    registry: &Arc<exomonad_core::effects::EffectRegistry>,
    worktree_base: &StdPath,
    name: &str,
    wasm_path: &StdPath,
) -> anyhow::Result<Arc<PluginManager>> {
    let agent_name = AgentName::from(name);

    // Fast path: check plugin cache
    {
        let cache = plugins.read().await;
        if let Some(p) = cache.get(&agent_name) {
            return Ok(p.clone());
        }
    }

    // Slow path: resolve birth branch and create plugin
    let birth_branch = if name == "root" {
        BirthBranch::root()?
    } else {
        resolve_agent_birth_branch(worktree_base, name).await?
    };

    get_or_create_plugin(plugins, agent_name, birth_branch, registry, wasm_path).await
}

pub async fn resolve_agent_birth_branch(
    worktree_base: &StdPath,
    agent_name: &str,
) -> anyhow::Result<BirthBranch> {
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
            return Ok(BirthBranch::from(branch.as_str()));
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
            return Ok(BirthBranch::from(branch.as_str()));
        }
    }

    // 3. Fallback to root
    tracing::warn!(
        agent = %agent_name,
        "Failed to resolve birth branch from worktree or config, falling back to root"
    );
    Ok(BirthBranch::root()
        .context("Failed to resolve root birth branch")?)
}

// ============================================================================
// Server-side Hook Handler Helpers
// ============================================================================

pub async fn handle_hook_inner(
    params: &HookQueryParams,
    state: &HookState,
    body: &str,
) -> Result<HookEnvelope> {
    let event_type = params.event;
    let runtime = params.runtime;
    let role = params
        .role
        .as_ref()
        .map(|r| Role::from(r.as_str()))
        .unwrap_or(state.default_role.clone());

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
        AgentName::from(params.agent_id.as_deref().unwrap_or("root"));
    let birth_branch_for_hook =
        BirthBranch::from(params.session_id.as_deref().unwrap_or("main"));

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

            if let Some(ref log) = state.event_log {
                let _ = log.append(
                    "hook.stop",
                    agent_name_for_hook.as_str(),
                    &serde_json::json!({
                        "event_type": event_type,
                        "decision": decision_str,
                        "reason": internal_output.reason,
                    }),
                );
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
                                if let Err(e) = tmux_events::emit_event(&state.tmux_session, &event)
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

            let exit_code = if output.continue_ { 0 } else { 2 };

            Ok(HookEnvelope {
                stdout: output_json,
                exit_code,
            })
        }
    }
}

// ============================================================================
// Axum Handlers
// ============================================================================

pub async fn health(State(state): State<AppState>) -> impl IntoResponse {
    // We need to resolve the root plugin for health
    let cache = state.plugins.read().await;
    let plugin = cache.get(&AgentName::from("root")).cloned();
    
    let wasm_hash = if let Some(p) = plugin {
        p.content_hash()
    } else {
        "unknown".to_string()
    };

    Json(serde_json::json!({
        "status": "ok",
        "version": env!("CARGO_PKG_VERSION"),
        "role": state.default_role.as_str(),
        "wasm_hash": wasm_hash,
    }))
}

#[instrument(skip_all, fields(hook = ?params.event, agent_id = ?params.agent_id))]
pub async fn handle_hook_request(
    Query(params): Query<HookQueryParams>,
    State(state): State<HookState>,
    body: String,
) -> Json<HookEnvelope> {
    match handle_hook_inner(&params, &state, &body).await {
        Ok(envelope) => Json(envelope),
        Err(e) => {
            warn!(error = %e, "Hook handler failed, returning allow");
            Json(HookEnvelope {
                stdout: r#"{"continue":true}"#.to_string(),
                exit_code: 0,
            })
        }
    }
}

pub async fn list_tools(
    Path((role, name)): Path<(String, String)>,
    State(state): State<AppState>,
) -> impl IntoResponse {
    let wasm_path_for_handler = resolve_wasm_path_for_role(
        &state.wasm_dir,
        &role,
        &state.wasm_name,
    )
    .unwrap_or_else(|| state.wasm_path.clone());

    let plugin = match resolve_plugin(
        &state.plugins,
        &state.registry,
        &state.worktree_base,
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
                Json(serde_json::json!({"error": e.to_string()})),
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
            Json(serde_json::json!({ "tools": tools })).into_response()
        }
        Err(e) => {
            tracing::error!(role = %role, error = %e, "Tool discovery failed");
            (
                axum::http::StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({"error": e.to_string()})),
            )
                .into_response()
        }
    }
}

#[instrument(skip_all, fields(agent_id = %name, role = %role, tool = %body.name))]
pub async fn call_tool(
    Path((role, name)): Path<(String, String)>,
    State(state): State<AppState>,
    Json(body): Json<ToolCallRequest>,
) -> impl IntoResponse {
    let wasm_path_for_handler = resolve_wasm_path_for_role(
        &state.wasm_dir,
        &role,
        &state.wasm_name,
    )
    .unwrap_or_else(|| state.wasm_path.clone());

    let plugin = match resolve_plugin(
        &state.plugins,
        &state.registry,
        &state.worktree_base,
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
                Json(serde_json::json!({"error": e.to_string()})),
            )
                .into_response();
        }
    };

    tracing::info!(tool = %body.name, role = %role, agent = %name, "Executing tool");

    let input = exomonad_core::mcp::tools::MCPCallInput::new(
        role.clone(),
        body.name.clone(),
        body.arguments,
    );

    let start = std::time::Instant::now();
    let result: Result<exomonad_core::mcp::tools::MCPCallOutput, anyhow::Error> = plugin.call("handle_mcp_call", &input).await;
    let duration_ms = start.elapsed().as_millis() as u64;

    let output = match result {
        Ok(o) => o,
        Err(e) => {
            tracing::error!(tool = %body.name, error = %e, "WASM call failed");
            if let Some(ref log) = state.event_log {
                let _ = log.append(
                    "tool.called",
                    &name,
                    &serde_json::json!({
                        "tool_name": body.name,
                        "role": role,
                        "duration_ms": duration_ms,
                        "success": false,
                        "error": e.to_string(),
                    }),
                );
            }
            return Json(serde_json::json!({
                "success": false,
                "result": null,
                "error": format!("WASM call failed: {}", e)
            }))
            .into_response();
        }
    };

    if let Some(ref log) = state.event_log {
        let _ = log.append(
            "tool.called",
            &name,
            &serde_json::json!({
                "tool_name": body.name,
                "role": role,
                "duration_ms": duration_ms,
                "success": output.success,
                "error": output.error,
            }),
        );
    }

    Json(serde_json::json!({
        "success": output.success,
        "result": output.result,
        "error": output.error,
    }))
    .into_response()
}

pub async fn handle_events(
    State(queue): State<Arc<exomonad_core::services::event_queue::EventQueue>>,
    body: Bytes,
) -> impl IntoResponse {
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

pub async fn reload(State(state): State<AppState>) -> impl IntoResponse {
    let mut cache = state.plugins.write().await;
    let evicted = cache.len();
    cache.clear();
    info!(plugins_evicted = evicted, "Plugin cache cleared (reload)");
    Json(serde_json::json!({
        "status": "ok",
        "plugins_evicted": evicted,
    }))
}

pub async fn shutdown_endpoint(State(signal): State<Arc<tokio::sync::Notify>>) -> impl IntoResponse {
    info!("Shutdown requested via /shutdown endpoint");
    signal.notify_one();
    Json(serde_json::json!({"status": "ok"}))
}

// ============================================================================
// Serve Command Runner
// ============================================================================

#[tracing::instrument(name = "exomonad.serve", skip_all)]
pub async fn run(config: &Config) -> Result<()> {
    // Extract TRACEPARENT from env if this is a child agent (parent injected it)
    if let Ok(tp) = std::env::var("TRACEPARENT") {
        use opentelemetry::propagation::TextMapPropagator;
        let propagator = opentelemetry_sdk::propagation::TraceContextPropagator::new();
        let mut carrier = std::collections::HashMap::new();
        carrier.insert("traceparent".to_string(), tp.clone());
        let parent_cx = propagator.extract(&carrier);
        use tracing_opentelemetry::OpenTelemetrySpanExt;
        tracing::Span::current().set_parent(parent_cx);
        info!(traceparent = %tp, "Inherited parent trace context");
    }

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
                "WASM file not found in {}
Run `exomonad recompile` first to build it.",
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

    let team_registry = Arc::new(claude_teams_bridge::TeamRegistry::new());
    let acp_registry = Arc::new(exomonad_core::services::acp_registry::AcpRegistry::new());

    let project_dir_for_services = project_dir.clone();
    let mut agent_control =
        exomonad_core::services::agent_control::AgentControlService::new(
            project_dir_for_services.clone(),
            github.clone(),
            git_wt.clone(),
        )
        .with_acp_registry(acp_registry.clone());
    let worktree_base = config.worktree_base.clone();
    agent_control = agent_control.with_worktree_base(worktree_base.clone());
    agent_control = agent_control.with_birth_branch(BirthBranch::root()?);
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
        project_dir.join(".exo/logs"),
    ) {
        Ok(el) => {
            info!(path = %project_dir.join(".exo/logs").display(), "Event log opened");
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
        tokio::sync::RwLock<HashMap<AgentName, Arc<PluginManager>>>,
    > = Arc::new(tokio::sync::RwLock::new(HashMap::new()));

    // Pre-populate with the root agent's plugin
    plugins
        .write()
        .await
        .insert(AgentName::from("root"), root_plugin.clone());

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

    let app_state = AppState {
        plugins: plugins.clone(),
        registry: rt_registry.clone(),
        wasm_path: wasm_path.clone(),
        wasm_dir: wasm_dir.clone(),
        wasm_name: wasm_name.clone(),
        default_role: config.role.clone(),
        worktree_base: worktree_base.clone(),
        event_log: event_log.clone(),
    };

    let hook_state = HookState {
        plugins: plugins.clone(),
        registry: rt_registry.clone(),
        wasm_path: wasm_path.clone(),
        tmux_session: config.tmux_session.clone(),
        default_role: config.role.clone(),
        event_log: event_log.clone(),
    };

    // Shutdown signal for graceful shutdown via /shutdown endpoint
    let shutdown_signal = Arc::new(tokio::sync::Notify::new());

    let cors = CorsLayer::new()
        .allow_origin(Any)
        .allow_methods(Any)
        .allow_headers(Any);

    let app = Router::new()
        .route("/health", get(health))
        .route(
            "/hook",
            post(handle_hook_request).with_state(hook_state),
        )
        .route(
            "/agents/{role}/{name}/tools",
            get(list_tools),
        )
        .route(
            "/agents/{role}/{name}/tools/call",
            post(call_tool),
        )
        .route("/events", post(handle_events).with_state(event_queue))
        .route("/reload", post(reload))
        .route("/shutdown", post(shutdown_endpoint).with_state(shutdown_signal.clone()))
        .with_state(app_state)
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
    Ok(())
}
