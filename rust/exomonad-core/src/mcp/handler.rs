//! MCP server handler using the official rmcp SDK.
//!
//! `ExomonadHandler` implements `rmcp::ServerHandler`, bridging the rmcp protocol
//! layer to our WASM plugin dispatch. Tools are registered dynamically at startup
//! from two sources:
//! - WASM tools (discovered via `handle_list_tools`, dispatched via `handle_mcp_call`)
//! - Direct Rust tools (TL-only: `get_agent_messages`, `answer_question`)

use super::tools::{self, MCPCallInput, MCPCallOutput};
use super::McpState;
use crate::services::messaging;
use crate::PluginManager;
use anyhow::Result;
use futures_util::FutureExt;
use rmcp::handler::server::router::tool::{ToolRoute, ToolRouter};
use rmcp::handler::server::tool::ToolCallContext;
use rmcp::model::*;
use rmcp::service::RequestContext;
use rmcp::{RoleServer, ServerHandler};
use serde_json::{json, Value};
use std::future::Future;
use std::path::PathBuf;
use std::sync::Arc;
use tracing::{debug, error, info};

// ============================================================================
// Handler
// ============================================================================

/// MCP server handler backed by WASM plugin + direct Rust tools.
///
/// Holds shared state and a pre-built `ToolRouter` for dispatch.
/// The router is populated once at construction from WASM tool discovery
/// plus statically-known Rust tools.
#[derive(Clone)]
pub struct ExomonadHandler {
    project_dir: PathBuf,
    plugin: Arc<PluginManager>,
    tool_router: ToolRouter<Self>,
}

impl ExomonadHandler {
    /// Construct from existing `McpState`.
    ///
    /// Queries WASM for tool definitions and registers all tools in the router.
    /// Blocks on WASM call (async in sync constructor — use `build` for async).
    pub async fn build(state: McpState) -> Result<Self> {
        let tool_router = build_tool_router(&state).await?;

        Ok(Self {
            project_dir: state.project_dir,
            plugin: state.plugin,
            tool_router,
        })
    }

    /// Access the project directory.
    pub fn project_dir(&self) -> &PathBuf {
        &self.project_dir
    }

    /// Access the WASM plugin manager.
    pub fn plugin(&self) -> &Arc<PluginManager> {
        &self.plugin
    }
}

// ============================================================================
// ServerHandler Implementation
// ============================================================================

impl ServerHandler for ExomonadHandler {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            protocol_version: ProtocolVersion::V_2024_11_05,
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            server_info: Implementation {
                name: "exomonad".to_string(),
                title: None,
                version: env!("CARGO_PKG_VERSION").to_string(),
                icons: None,
                website_url: None,
            },
            instructions: Some(
                "ExoMonad MCP server: orchestration tools for LLM agents. \
                 Tools include git operations, GitHub API, agent spawning, \
                 and inter-agent messaging."
                    .to_string(),
            ),
        }
    }

    fn list_tools(
        &self,
        _request: Option<PaginatedRequestParams>,
        _context: RequestContext<RoleServer>,
    ) -> impl Future<Output = Result<ListToolsResult, rmcp::ErrorData>> + Send + '_ {
        let tools = self.tool_router.list_all();
        debug!(count = tools.len(), "Listing tools");
        std::future::ready(Ok(ListToolsResult {
            tools,
            next_cursor: None,
            meta: None,
        }))
    }

    fn call_tool(
        &self,
        request: CallToolRequestParams,
        context: RequestContext<RoleServer>,
    ) -> impl Future<Output = Result<CallToolResult, rmcp::ErrorData>> + Send + '_ {
        let tool_name = request.name.clone();
        info!(tool = %tool_name, "Executing tool");

        async move {
            let ctx = ToolCallContext::new(self, request, context);
            self.tool_router.call(ctx).await
        }
    }
}

// ============================================================================
// Tool Router Construction
// ============================================================================

/// WASM tools that are only available to the TL role.
/// Dev agents should not have access to orchestration tools.
const TL_ONLY_TOOLS: &[&str] = &["spawn_agents", "cleanup_agents", "list_agents"];

/// Build a `ToolRouter` from WASM tools and direct Rust tools.
///
/// WASM tools are discovered dynamically via `handle_list_tools`.
/// Direct Rust tools are registered statically.
/// When `state.role` is set, tools are filtered by role:
/// - "tl": all WASM tools + TL messaging tools (get_agent_messages, answer_question)
/// - "dev": WASM tools minus TL-only orchestration tools, no TL messaging
/// - None: all tools (stdio mode, backwards compatible)
async fn build_tool_router(state: &McpState) -> Result<ToolRouter<ExomonadHandler>> {
    let mut router = ToolRouter::new();
    let is_dev = state.role.as_deref() == Some("dev");

    // 1. WASM tools (dynamic, from Haskell plugin)
    let wasm_tools = tools::get_tool_definitions(state).await?;
    let filtered_count = if is_dev {
        wasm_tools
            .iter()
            .filter(|t| !TL_ONLY_TOOLS.contains(&t.name.as_str()))
            .count()
    } else {
        wasm_tools.len()
    };
    info!(total = wasm_tools.len(), filtered = filtered_count, role = ?state.role, "Registering WASM tools");

    for tool_def in wasm_tools {
        // Skip TL-only tools for dev role
        if is_dev && TL_ONLY_TOOLS.contains(&tool_def.name.as_str()) {
            debug!(tool = %tool_def.name, "Skipping TL-only tool for dev role");
            continue;
        }

        let plugin = state.plugin.clone();
        let tool = Tool::new(
            tool_def.name.clone(),
            tool_def.description,
            to_json_object(tool_def.input_schema),
        );

        let tool_name = tool_def.name;
        router.add_route(ToolRoute::new_dyn(tool, move |ctx| {
            let plugin = plugin.clone();
            let tool_name = tool_name.clone();
            async move {
                let args = ctx.arguments.map(Value::Object).unwrap_or(json!({}));

                let input = MCPCallInput::new(tool_name.clone(), args);

                debug!(tool = %tool_name, "Calling WASM handle_mcp_call");
                let output: MCPCallOutput =
                    plugin.call("handle_mcp_call", &input).await.map_err(|e| {
                        error!(tool = %tool_name, error = %e, "WASM call failed");
                        rmcp::ErrorData::internal_error(
                            format!("WASM call failed for tool '{}': {}", tool_name, e),
                            None,
                        )
                    })?;

                output.into_call_tool_result()
            }
            .boxed()
        }));
    }

    // 2. Direct Rust tools (TL-only: messaging tools for reading agent outboxes)
    if !is_dev {
        register_get_agent_messages(&mut router, state);
        register_answer_question(&mut router, state);
    }

    info!(
        total = router.list_all().len(),
        role = ?state.role,
        "Tool router built"
    );

    Ok(router)
}

// ============================================================================
// Direct Rust Tool Routes
// ============================================================================

fn register_get_agent_messages(router: &mut ToolRouter<ExomonadHandler>, _state: &McpState) {
    let tool = Tool::new(
        "get_agent_messages",
        "Read notes and pending questions from agent outboxes. Scans all agents (or a specific agent) for messages.",
        to_json_object(json!({
            "type": "object",
            "properties": {
                "agent_id": {
                    "type": "string",
                    "description": "Filter to a specific agent directory name. If omitted, reads from all agents."
                },
                "subrepo": {
                    "type": "string",
                    "description": "Subrepo path (e.g. 'egregore/') to scope agent scanning."
                }
            }
        })),
    );

    router.add_route(ToolRoute::new_dyn(tool, move |ctx| {
        async move {
            let agent_id = ctx
                .arguments
                .as_ref()
                .and_then(|a| a.get("agent_id"))
                .and_then(|v| v.as_str())
                .map(String::from);

            // Try Teams inbox first (unified path), fall back to legacy .exomonad/agents/
            let team_name = get_team_name().ok();

            info!(agent_id = ?agent_id, team = ?team_name, "Getting agent messages");

            if let Some(ref team) = team_name {
                // Unified path: read from Teams inboxes
                let result = if let Some(ref agent_id) = agent_id {
                    let messages = messaging::read_agent_inbox(team, agent_id)
                        .await
                        .map_err(|e| {
                            rmcp::ErrorData::internal_error(
                                format!("Failed to read agent inbox: {}", e),
                                None,
                            )
                        })?;

                    info!(agent = %agent_id, count = messages.len(), "Read agent messages from Teams inbox");
                    json!({ "agent_id": agent_id, "messages": messages })
                } else {
                    // Read TL inbox to see all messages from agents
                    let results = messaging::scan_all_agent_messages_teams(team)
                        .await
                        .map_err(|e| {
                            rmcp::ErrorData::internal_error(
                                format!("Failed to scan agent messages: {}", e),
                                None,
                            )
                        })?;

                    let agents: Vec<Value> = results
                        .into_iter()
                        .map(|(id, msgs)| json!({ "agent_id": id, "messages": msgs }))
                        .collect();

                    info!(agent_count = agents.len(), "Scanned all agent messages from Teams inboxes");
                    json!({ "agents": agents })
                };

                Ok(CallToolResult::success(vec![Content::text(
                    serde_json::to_string_pretty(&result).unwrap_or_default(),
                )]))
            } else {
                // No team configured — return empty (legacy .exomonad/agents/ path removed)
                info!("No team name configured, returning empty messages");
                Ok(CallToolResult::success(vec![Content::text(
                    json!({"agents": [], "warning": "No EXOMONAD_TEAM_NAME or CLAUDE_TEAM_NAME set. Configure a team to enable messaging."}).to_string(),
                )]))
            }
        }
        .boxed()
    }));
}

fn register_answer_question(router: &mut ToolRouter<ExomonadHandler>, state: &McpState) {
    let question_registry = state.question_registry.clone();
    let tool = Tool::new(
        "answer_question",
        "Answer a pending question from an agent. Writes the answer to the agent's inbox, unblocking their send_question call.",
        to_json_object(json!({
            "type": "object",
            "properties": {
                "agent_id": {
                    "type": "string",
                    "description": "The agent directory name (e.g. 'gh-42-fix-bug-claude')."
                },
                "question_id": {
                    "type": "string",
                    "description": "The question ID to answer (e.g. 'q-abc123')."
                },
                "answer": {
                    "type": "string",
                    "description": "The answer text."
                },
                "subrepo": {
                    "type": "string",
                    "description": "Subrepo path (e.g. 'egregore/') if agent is in a subrepo."
                }
            },
            "required": ["agent_id", "question_id", "answer"]
        })),
    );

    router.add_route(ToolRoute::new_dyn(tool, move |ctx| {
        let question_registry = question_registry.clone();
        async move {
            let args = ctx.arguments.as_ref();

            let agent_id = args
                .and_then(|a| a.get("agent_id"))
                .and_then(|v| v.as_str())
                .ok_or_else(|| rmcp::ErrorData::invalid_params("agent_id is required", None))?;
            let question_id = args
                .and_then(|a| a.get("question_id"))
                .and_then(|v| v.as_str())
                .ok_or_else(|| rmcp::ErrorData::invalid_params("question_id is required", None))?;
            let answer = args
                .and_then(|a| a.get("answer"))
                .and_then(|v| v.as_str())
                .ok_or_else(|| rmcp::ErrorData::invalid_params("answer is required", None))?;

            let team_name = get_team_name()?;

            info!(agent = %agent_id, question_id = %question_id, team = %team_name, "Answering question via Teams inbox");

            messaging::write_to_agent_inbox(&team_name, agent_id, answer)
                .await
                .map_err(|e| {
                    rmcp::ErrorData::internal_error(
                        format!("Failed to write answer to Teams inbox: {}", e),
                        None,
                    )
                })?;

            // Resolve the oneshot channel so ask_question unblocks immediately.
            if let Some(ref registry) = question_registry {
                let resolved = registry.resolve(question_id, answer.to_string());
                info!(agent = %agent_id, question_id = %question_id, resolved, "Resolved question via QuestionRegistry");
            }

            info!(agent = %agent_id, question_id = %question_id, "Answer written to Teams inbox");

            let result = json!({
                "status": "answered",
                "agent_id": agent_id,
                "question_id": question_id,
            });

            Ok(CallToolResult::success(vec![Content::text(
                serde_json::to_string_pretty(&result).unwrap_or_default(),
            )]))
        }
        .boxed()
    }));
}

/// Get the team name from environment (set by spawn_agents).
fn get_team_name() -> Result<String, rmcp::ErrorData> {
    std::env::var("EXOMONAD_TEAM_NAME")
        .or_else(|_| std::env::var("CLAUDE_TEAM_NAME"))
        .map_err(|_| {
            rmcp::ErrorData::internal_error(
                "No team name found. Set EXOMONAD_TEAM_NAME or CLAUDE_TEAM_NAME.",
                None,
            )
        })
}

// ============================================================================
// Helpers
// ============================================================================

/// Convert a `serde_json::Value` to a `serde_json::Map` for rmcp's `Tool::new`.
fn to_json_object(value: Value) -> serde_json::Map<String, Value> {
    match value {
        Value::Object(map) => map,
        _ => {
            error!("Expected JSON object for input_schema, got: {:?}", value);
            serde_json::Map::new()
        }
    }
}
