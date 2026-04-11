use crate::app_state::AppState;
use exomonad::config::Config;
use std::time::Duration;

use anyhow::{Context, Result};
use axum::{
    body::Bytes,
    extract::{Extension, Path, Query, State},
    http::Request,
    middleware::Next,
    response::{IntoResponse, Response},
    routing::{get, post},
    Json, Router,
};
use exomonad_core::protocol::Runtime as HookRuntime;
use exomonad_core::services::{git, tmux_events};
use exomonad_core::{
    AgentName, BirthBranch, ClaudePreToolUseOutput, HookEnvelope, HookEventType, HookInput,
    InternalAfterModelOutput, InternalBeforeModelOutput, InternalStopHookOutput, PluginManager,
    Role, RuntimeBuilder, StopDecision,
};
use std::collections::HashMap;
use std::path::{Path as StdPath, PathBuf};
use std::sync::Arc;
use tower_http::cors::{Any, CorsLayer};
use tower_http::trace::TraceLayer;
use tracing::{debug, info, instrument, warn, Instrument};

// ============================================================================
// Config Helpers
// ============================================================================

/// Convert typed `McpServerConfig` entries into pre-serialized JSON values
/// for propagation to spawned agent configs.
fn serialize_extra_mcp_servers(
    servers: &HashMap<String, exomonad::config::McpServerConfig>,
) -> HashMap<String, serde_json::Value> {
    servers
        .iter()
        .map(|(name, server)| {
            let value = match server {
                exomonad::config::McpServerConfig::Http { url, headers } => {
                    let mut e = serde_json::json!({"type": "http", "url": url});
                    if !headers.is_empty() {
                        e["headers"] = serde_json::to_value(headers).unwrap_or_default();
                    }
                    e
                }
                exomonad::config::McpServerConfig::Stdio { command, args } => {
                    serde_json::json!({"type": "stdio", "command": command, "args": args})
                }
            };
            (name.clone(), value)
        })
        .collect()
}

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
    pub agent_resolver: Arc<exomonad_core::services::AgentResolver>,
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
    let working_dir =
        exomonad_core::services::agent_control::resolve_working_dir(birth_branch.as_str());
    let ctx = exomonad_core::effects::EffectContext {
        agent_name: agent_name.clone(),
        birth_branch,
        working_dir,
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
    agent_resolver: Option<&exomonad_core::services::AgentResolver>,
) -> anyhow::Result<Arc<PluginManager>> {
    let agent_name = AgentName::from(name);

    // Root's birth branch is written to .exo/agents/root/.birth_branch by init.
    // Re-resolve on every request to detect branch changes between sessions.
    if name == "root" {
        let birth_branch = resolve_agent_birth_branch(worktree_base, name).await?;
        {
            let cache = plugins.read().await;
            if let Some(p) = cache.get(&agent_name) {
                if p.effect_context().birth_branch == birth_branch {
                    return Ok(p.clone());
                }
                tracing::info!(
                    old = %p.effect_context().birth_branch,
                    new = %birth_branch,
                    "Root birth branch changed, recreating plugin"
                );
            }
        }
        let working_dir =
            exomonad_core::services::agent_control::resolve_working_dir(birth_branch.as_str());
        let ctx = exomonad_core::effects::EffectContext {
            agent_name: agent_name.clone(),
            birth_branch,
            working_dir,
        };
        let p = Arc::new(
            PluginManager::from_file(wasm_path, registry.clone(), ctx)
                .await
                .with_context(|| "Failed to create plugin for root agent")?,
        );
        let mut cache = plugins.write().await;
        cache.insert(agent_name, p.clone());
        return Ok(p);
    }

    // Non-root agents: resolver does in-memory lookup first, then probes disk.
    let (birth_branch, working_dir) = if let Some(resolver) = agent_resolver {
        resolver.resolve_or_probe(worktree_base, name).await?
    } else {
        let bb = resolve_agent_birth_branch(worktree_base, name).await?;
        let wd = exomonad_core::services::agent_control::resolve_working_dir(bb.as_str());
        (bb, wd)
    };

    let ctx = exomonad_core::effects::EffectContext {
        agent_name: agent_name.clone(),
        birth_branch,
        working_dir,
    };
    let p = Arc::new(
        PluginManager::from_file(wasm_path, registry.clone(), ctx)
            .await
            .with_context(|| format!("Failed to create plugin for agent {}", agent_name))?,
    );
    Ok(p)
}

/// Birth branch resolution for the root agent at server startup.
///
/// Only used for the root agent before the resolver is fully initialized.
/// All other agents use `AgentResolver::resolve_or_probe()`.
///
/// Resolution order:
/// 1. identity.json (canonical, written by finalize_spawn)
/// 2. Worktree git branch (subtrees have their own git branch)
/// 3. .birth_branch file (legacy, written by spawn_worker)
/// 4. Fallback to root (with warning)
async fn resolve_agent_birth_branch(
    worktree_base: &StdPath,
    agent_name: &str,
) -> anyhow::Result<BirthBranch> {
    // 1. Try identity.json (canonical source)
    if let Some(exo_dir) = worktree_base.parent() {
        let identity_path = exo_dir
            .join("agents")
            .join(agent_name)
            .join("identity.json");
        if let Ok(contents) = tokio::fs::read_to_string(&identity_path).await {
            if let Ok(record) =
                serde_json::from_str::<exomonad_core::services::AgentIdentityRecord>(&contents)
            {
                tracing::debug!(agent = %agent_name, branch = %record.birth_branch, "Resolved birth branch from identity.json");
                return Ok(record.birth_branch);
            }
        }
    }

    // 2. Try worktree (subtrees have their own git branch)
    // Strip known suffixes to get the slug for worktree dir lookup.
    let slug = agent_name
        .trim_end_matches("-claude")
        .trim_end_matches("-gemini")
        .trim_end_matches("-shoal")
        .trim_end_matches("-process");
    let worktree_path = worktree_base.join(slug);
    match tokio::process::Command::new("git")
        .args(["branch", "--show-current"])
        .current_dir(&worktree_path)
        .output()
        .await
    {
        Ok(output) if output.status.success() => {
            let branch = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if !branch.is_empty() {
                tracing::debug!(agent = %agent_name, branch = %branch, "Resolved agent birth branch from worktree");
                return Ok(BirthBranch::from(branch.as_str()));
            }
        }
        _ => {}
    }

    // 3. Try .birth_branch file (legacy fallback for workers)
    if let Some(exo_dir) = worktree_base.parent() {
        let bb_file = exo_dir
            .join("agents")
            .join(agent_name)
            .join(".birth_branch");
        if let Ok(contents) = tokio::fs::read_to_string(&bb_file).await {
            let branch = contents.trim().to_string();
            tracing::debug!(agent = %agent_name, branch = %branch, "Resolved agent birth branch from .birth_branch file");
            return Ok(BirthBranch::from(branch.as_str()));
        }
    }

    // 4. Fallback to root (with warning — this is a bug if it happens for non-root agents)
    tracing::warn!(
        agent = %agent_name,
        "Failed to resolve birth branch from identity.json, worktree, or .birth_branch — falling back to root"
    );
    BirthBranch::root().context("Failed to resolve root birth branch")
}

// ============================================================================
// Agent Identity Middleware
// ============================================================================

async fn agent_identity_middleware(
    Path((role, name)): Path<(String, String)>,
    State(state): State<AppState>,
    mut request: Request<axum::body::Body>,
    next: Next,
) -> Response {
    let wasm_path = resolve_wasm_path_for_role(&state.wasm_dir, &role, &state.wasm_name)
        .unwrap_or_else(|| state.wasm_path.clone());

    let plugin_result = resolve_plugin(
        &state.plugins,
        &state.registry,
        &state.worktree_base,
        &name,
        &wasm_path,
        Some(&state.agent_resolver),
    )
    .await;

    let parent = plugin_result
        .as_ref()
        .ok()
        .and_then(|p| p.effect_context().birth_branch.parent())
        .map(|p| p.to_string())
        .unwrap_or_default();

    if let Ok(ref plugin) = plugin_result {
        request.extensions_mut().insert(plugin.clone());
    }

    let span = tracing::info_span!(
        "agent_request",
        agent_id = %name,
        agent.role = %role,
        agent.parent = %parent,
        swarm.run_id = %state.run_id,
    );
    next.run(request).instrument(span).await
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
        if let Some(agent_id_str) = git::extract_agent_id(branch.as_str()) {
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
        /// Gemini BeforeModel/AfterModel: passed through to WASM, response serialized as-is
        GeminiBeforeModel,
        GeminiAfterModel,
    }

    let dispatch = match event_type {
        HookEventType::Stop | HookEventType::AfterAgent => HookDispatch::Stop,
        HookEventType::SubagentStop => HookDispatch::Stop,
        HookEventType::SessionEnd => HookDispatch::Stop,
        HookEventType::PreToolUse => HookDispatch::ToolUse,
        HookEventType::BeforeTool => HookDispatch::ToolUse,
        HookEventType::BeforeModel => HookDispatch::GeminiBeforeModel,
        HookEventType::AfterModel => HookDispatch::GeminiAfterModel,
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
        HookEventType::PreToolUse => "PreToolUse",
        HookEventType::BeforeTool => "PreToolUse",
        HookEventType::BeforeModel => "BeforeModel",
        HookEventType::AfterModel => "AfterModel",
        HookEventType::PostToolUse => "PostToolUse",
        HookEventType::WorkerExit => "WorkerExit",
        HookEventType::SessionStart => "SessionStart",
        _ => unreachable!("passthrough events returned early above"),
    };
    hook_input.hook_event_name = normalized_event_name.to_string();

    // Create role-aware input for unified WASM
    let mut hook_input_value =
        serde_json::to_value(&hook_input).context("Failed to serialize hook input")?;

    // Resolve agent identity: try resolver first for authoritative identity,
    // fall back to query params (which may be inaccurate for hooks fired before first tool call).
    let agent_name_for_hook = AgentName::from(params.agent_id.as_deref().unwrap_or("root"));
    let birth_branch_for_hook =
        if let Some(record) = state.agent_resolver.get(&agent_name_for_hook).await {
            record.birth_branch
        } else {
            BirthBranch::from(params.session_id.as_deref().unwrap_or("main"))
        };

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

            tracing::info!(
                otel.name = "hook.stop",
                agent_id = %agent_name_for_hook.as_str(),
                event_type = ?event_type,
                decision = %decision_str,
                reason = ?internal_output.reason,
                "[event] hook.stop"
            );
            if let Some(ref log) = state.event_log {
                let _ = log.append(
                    "hook.stop",
                    agent_name_for_hook.as_str(),
                    &serde_json::json!({
                        "event_type": format!("{:?}", event_type),
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
                    if let Some(agent_id_str) = git::extract_agent_id(branch.as_str()) {
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

        HookDispatch::GeminiBeforeModel => {
            let output: InternalBeforeModelOutput = plugin
                .call("handle_pre_tool_use", &hook_input_value)
                .await
                .context("WASM handle_pre_tool_use (BeforeModel) failed")?;

            let output_json =
                serde_json::to_string(&output).context("Failed to serialize BeforeModel output")?;

            let exit_code = if output.continue_ { 0 } else { 2 };

            Ok(HookEnvelope {
                stdout: output_json,
                exit_code,
            })
        }

        HookDispatch::GeminiAfterModel => {
            let output: InternalAfterModelOutput = plugin
                .call("handle_pre_tool_use", &hook_input_value)
                .await
                .context("WASM handle_pre_tool_use (AfterModel) failed")?;

            let output_json =
                serde_json::to_string(&output).context("Failed to serialize AfterModel output")?;

            let exit_code = if output.continue_ { 0 } else { 2 };

            Ok(HookEnvelope {
                stdout: output_json,
                exit_code,
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

#[instrument(skip_all, fields(hook = ?params.event, hook.type = %params.event, agent_id = tracing::field::Empty, agent.parent = tracing::field::Empty))]
pub async fn handle_hook_request(
    Query(params): Query<HookQueryParams>,
    State(state): State<HookState>,
    body: String,
) -> Json<HookEnvelope> {
    if let Some(ref id) = params.agent_id {
        tracing::Span::current().record("agent_id", id.as_str());
    }
    if let Some(ref sid) = params.session_id {
        tracing::Span::current().record("agent.parent", sid.as_str());
    }
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
    Path((role, _name)): Path<(String, String)>,
    Extension(plugin): Extension<Arc<PluginManager>>,
) -> impl IntoResponse {
    // Hot reload WASM if changed
    let _ = plugin.reload_if_changed().await;

    match exomonad_core::mcp::tools::get_tool_definitions(&plugin, Some(&role)).await {
        Ok(tools) => Json(serde_json::json!({ "tools": tools })).into_response(),
        Err(e) => {
            tracing::error!(error = %e, "Tool discovery failed");
            (
                axum::http::StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({"error": e.to_string()})),
            )
                .into_response()
        }
    }
}

pub async fn call_tool(
    Path((role, name)): Path<(String, String)>,
    State(state): State<AppState>,
    Extension(plugin): Extension<Arc<PluginManager>>,
    Json(body): Json<ToolCallRequest>,
) -> impl IntoResponse {
    tracing::info!(tool = %body.name, "Executing tool");

    let arguments = exomonad_core::mcp::harness_compat::coerce_harness_value(body.arguments);
    let input =
        exomonad_core::mcp::tools::MCPCallInput::new(role.clone(), body.name.clone(), arguments);

    let start = std::time::Instant::now();
    let result: Result<exomonad_core::mcp::tools::MCPCallOutput, anyhow::Error> =
        plugin.call("handle_mcp_call", &input).await;
    let duration_ms = start.elapsed().as_millis() as u64;

    let output = match result {
        Ok(o) => o,
        Err(e) => {
            tracing::error!(tool = %body.name, error = %e, "WASM call failed");
            tracing::info!(
                otel.name = "tool.called",
                tool_name = %body.name,
                duration_ms = duration_ms,
                success = false,
                error = %e,
                "[event] tool.called"
            );
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

    tracing::info!(
        otel.name = "tool.called",
        tool_name = %body.name,
        duration_ms = duration_ms,
        success = output.success,
        error = ?output.error,
        "[event] tool.called"
    );
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

pub async fn shutdown_endpoint(
    State(signal): State<Arc<tokio::sync::Notify>>,
) -> impl IntoResponse {
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

    let project_dir = {
        let raw = if config.project_dir.is_absolute() {
            config.project_dir.clone()
        } else {
            std::env::current_dir()?.join(&config.project_dir)
        };
        raw.canonicalize().unwrap_or(raw)
    };

    // Generate or load swarm run_id (persists across server restarts, resets on init --recreate)
    let run_id_path = project_dir.join(".exo/run_id");
    let run_id: Arc<str> = match std::fs::read_to_string(&run_id_path) {
        Ok(id) if !id.trim().is_empty() => id.trim().into(),
        _ => {
            let id = uuid::Uuid::new_v4().to_string();
            let _ = std::fs::write(&run_id_path, &id);
            info!(run_id = %id, path = %run_id_path.display(), "Generated new swarm run_id");
            id.into()
        }
    };
    // Set env so logging::init() picks it up as an OTel resource attribute in child processes
    std::env::set_var("EXOMONAD_SWARM_RUN_ID", &*run_id);

    let role_name = config.role.to_string();
    let wasm_dir = config.wasm_dir.clone();
    let wasm_name = config.wasm_name.clone();

    // Resolve default WASM (for root TL role and as fallback)
    let wasm_path =
        resolve_wasm_path_for_role(&wasm_dir, &role_name, &wasm_name).ok_or_else(|| {
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
    let github_client = secrets
        .github_token()
        .map(|_| exomonad_core::services::GitHubClient::new(5));

    if github_client.is_some() {
        exomonad_core::services::validate_gh_cli().context("Failed to validate gh CLI")?;
    }

    let team_registry = Arc::new(claude_teams_bridge::TeamRegistry::new());
    let acp_registry = Arc::new(exomonad_core::services::acp_registry::AcpRegistry::new());

    // Load canonical agent identity resolver from disk
    let agent_resolver =
        Arc::new(exomonad_core::services::AgentResolver::load(project_dir.clone()).await);

    // JSONL event log (parallel to OTel span events, queryable via DuckDB/kaizen)
    let event_log = match exomonad_core::services::EventLog::open(project_dir.join(".exo/logs")) {
        Ok(log) => {
            info!(path = %project_dir.join(".exo/logs").display(), "Event log opened");
            Some(Arc::new(log))
        }
        Err(e) => {
            warn!(error = %e, "Failed to open event log, JSONL logging disabled");
            None
        }
    };

    let event_queue = Arc::new(exomonad_core::services::event_queue::EventQueue::new());
    let mutex_registry = Arc::new(exomonad_core::services::MutexRegistry::new());
    mutex_registry.spawn_expiry_task();
    let supervisor_registry = Arc::new(exomonad_core::services::SupervisorRegistry::new());
    let claude_session_registry =
        Arc::new(exomonad_core::services::claude_session_registry::ClaudeSessionRegistry::new());

    let tmux_session = config.tmux_session.clone();
    let tmux_ipc = Arc::new(exomonad_core::services::tmux_ipc::TmuxIpc::new(
        &tmux_session,
    ));

    // Build Services once — all shared registries in one struct
    let tasks_dir = dirs::home_dir().unwrap_or_default().join(".claude/tasks");
    let services = Arc::new(
        exomonad_core::services::ServicesBuilder::new(
            project_dir.clone(),
            tasks_dir,
            git_wt,
            tmux_ipc,
        )
        .github_client_opt(github_client.clone())
        .event_log_opt(event_log.clone())
        .team_registry(team_registry.clone())
        .acp_registry(acp_registry.clone())
        .supervisor_registry(supervisor_registry)
        .claude_session_registry(claude_session_registry)
        .agent_resolver(agent_resolver.clone())
        .event_queue(event_queue.clone())
        .mutex_registry(mutex_registry)
        .build(),
    );

    let mut agent_control =
        exomonad_core::services::agent_control::AgentControlService::new(services.clone())
            .with_wasm_name(wasm_name.clone());
    let worktree_base = config.worktree_base.clone();
    agent_control = agent_control.with_worktree_base(worktree_base.clone());
    agent_control =
        agent_control.with_birth_branch(resolve_agent_birth_branch(&worktree_base, "root").await?);
    agent_control = agent_control.with_tmux_session(config.tmux_session.clone());
    agent_control = agent_control.with_yolo(config.yolo);
    agent_control = agent_control
        .with_extra_mcp_servers(serialize_extra_mcp_servers(&config.extra_mcp_servers));
    let event_session_id = uuid::Uuid::new_v4().to_string();
    let agent_control = Arc::new(agent_control);

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
            "tasks".to_string(),
        ]);

    builder = builder.with_handlers(exomonad_core::core_handlers(services.clone()));
    builder = builder.with_handlers(exomonad_core::git_handlers(services.clone(), git));
    builder = builder.with_handlers(exomonad_core::orchestration_handlers(
        agent_control.clone(),
        services.clone(),
        Some(event_session_id),
    ));
    let rt = builder.build().await.context("Failed to build runtime")?;

    // Extract the shared registry for creating per-agent plugins
    let rt_registry = rt.registry.clone();
    let root_plugin = Arc::new(rt.plugin_manager);

    // Per-agent plugin cache — each agent gets its own PluginManager with baked-in identity
    let plugins: Arc<tokio::sync::RwLock<HashMap<AgentName, Arc<PluginManager>>>> =
        Arc::new(tokio::sync::RwLock::new(HashMap::new()));

    // Pre-populate with the root agent's plugin
    plugins
        .write()
        .await
        .insert(AgentName::from("root"), root_plugin.clone());

    // Check for existing server BEFORE writing our own PID
    let socket_path = project_dir.join(".exo/server.sock");
    if socket_path.exists() {
        if let Ok(content) = std::fs::read_to_string(&server_pid_path) {
            if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&content) {
                if let Some(pid) = parsed.get("pid").and_then(|v| v.as_u64()) {
                    use nix::sys::signal;
                    use nix::unistd::Pid;
                    let pid_i32 = pid as i32;
                    let is_self = pid_i32 == std::process::id() as i32;
                    if !is_self && signal::kill(Pid::from_raw(pid_i32), None).is_ok() {
                        return Err(anyhow::anyhow!(
                            "Server already running (PID {}). Stop it first or use a different project directory.",
                            pid
                        ));
                    }
                }
            }
        }
        info!(path = %socket_path.display(), "Removing stale socket");
        let _ = std::fs::remove_file(&socket_path);
        let _ = std::fs::remove_file(&server_pid_path);
    }

    // Write server.pid
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
    let mut poller = exomonad_core::services::github_poller::GitHubPoller::new(services.clone())
        .with_plugins(plugins.clone());
    if let Some(interval) = config.poll_interval {
        if interval == 0 {
            anyhow::bail!("Invalid configuration: `poll_interval` must be >= 1 second, got 0");
        }
        poller = poller.with_poll_interval(Duration::from_secs(interval));
    }
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
        run_id: run_id.clone(),
        agent_resolver: agent_resolver.clone(),
    };

    let hook_state = HookState {
        plugins: plugins.clone(),
        registry: rt_registry.clone(),
        wasm_path: wasm_path.clone(),
        tmux_session: config.tmux_session.clone(),
        default_role: config.role.clone(),
        event_log: event_log.clone(),
        agent_resolver: agent_resolver.clone(),
    };

    // Shutdown signal for graceful shutdown via /shutdown endpoint
    let shutdown_signal = Arc::new(tokio::sync::Notify::new());

    let cors = CorsLayer::new()
        .allow_origin(Any)
        .allow_methods(Any)
        .allow_headers(Any);

    let agent_routes = Router::new()
        .route("/{role}/{name}/tools", get(list_tools))
        .route("/{role}/{name}/tools/call", post(call_tool))
        .layer(axum::middleware::from_fn_with_state(
            app_state.clone(),
            agent_identity_middleware,
        ))
        .with_state(app_state.clone());

    let app = Router::new()
        .route("/health", get(health))
        .route("/hook", post(handle_hook_request).with_state(hook_state))
        .nest("/agents", agent_routes)
        .route(
            "/events",
            post(handle_events).with_state(event_queue.clone()),
        )
        .route("/reload", post(reload))
        .route(
            "/shutdown",
            post(shutdown_endpoint).with_state(shutdown_signal.clone()),
        )
        .with_state(app_state)
        .layer(cors)
        .layer(TraceLayer::new_for_http());

    // Bind Unix domain socket (stale socket already cleaned up above)
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
