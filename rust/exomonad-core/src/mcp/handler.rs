//! MCP server handler using the official rmcp SDK.
//!
//! `ExomonadHandler` implements `rmcp::ServerHandler`, bridging the rmcp protocol
//! layer to our WASM plugin dispatch. Tools are registered dynamically at startup
//! from two sources:
//! - WASM tools (discovered via `handle_list_tools`, dispatched via `handle_mcp_call`)
//! - Direct Rust tools (`get_agent_messages`, `answer_question`)

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

    // 3. Direct Rust tools (dev-only: Teams task list participation)
    if is_dev {
        register_get_tasks(&mut router);
        register_claim_task(&mut router);
        register_complete_task(&mut router);
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

fn register_get_agent_messages(router: &mut ToolRouter<ExomonadHandler>, state: &McpState) {
    let project_dir = state.project_dir.clone();
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
        let project_dir = project_dir.clone();
        async move {
            let agent_id = ctx
                .arguments
                .as_ref()
                .and_then(|a| a.get("agent_id"))
                .and_then(|v| v.as_str())
                .map(String::from);
            let subrepo = ctx
                .arguments
                .as_ref()
                .and_then(|a| a.get("subrepo"))
                .and_then(|v| v.as_str())
                .map(String::from);

            info!(agent_id = ?agent_id, subrepo = ?subrepo, "Getting agent messages");

            let result = if let Some(ref agent_id) = agent_id {
                let agent_dir =
                    tools::resolve_agent_path(&project_dir, subrepo.as_deref(), agent_id);
                let messages = messaging::read_agent_outbox(&agent_dir)
                    .await
                    .map_err(|e| {
                        rmcp::ErrorData::internal_error(
                            format!("Failed to read agent outbox: {}", e),
                            None,
                        )
                    })?;

                info!(agent = %agent_id, count = messages.len(), "Read agent messages");
                json!({ "agent_id": agent_id, "messages": messages })
            } else {
                let results = messaging::scan_all_agent_messages(&project_dir, subrepo.as_deref())
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

                info!(agent_count = agents.len(), "Scanned all agent messages");
                json!({ "agents": agents })
            };

            Ok(CallToolResult::success(vec![Content::text(
                serde_json::to_string_pretty(&result).unwrap_or_default(),
            )]))
        }
        .boxed()
    }));
}

fn register_answer_question(router: &mut ToolRouter<ExomonadHandler>, state: &McpState) {
    let project_dir = state.project_dir.clone();
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
        let project_dir = project_dir.clone();
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
            let subrepo = args.and_then(|a| a.get("subrepo")).and_then(|v| v.as_str());

            let agent_dir = tools::resolve_agent_path(&project_dir, subrepo, agent_id);

            info!(agent = %agent_id, question_id = %question_id, "Answering question");

            messaging::write_agent_answer(&agent_dir, question_id, answer)
                .await
                .map_err(|e| {
                    rmcp::ErrorData::internal_error(format!("Failed to write answer: {}", e), None)
                })?;

            info!(agent = %agent_id, question_id = %question_id, "Answer written");

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

// ============================================================================
// Dev-Role Tools: Teams Task List Participation
// ============================================================================

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

/// Get the tasks directory for a team.
fn get_tasks_dir(team_name: &str) -> Result<std::path::PathBuf, rmcp::ErrorData> {
    let home = dirs::home_dir()
        .ok_or_else(|| rmcp::ErrorData::internal_error("Could not find home directory", None))?;
    Ok(home.join(".claude").join("tasks").join(team_name))
}

fn register_get_tasks(router: &mut ToolRouter<ExomonadHandler>) {
    let tool = Tool::new(
        "get_tasks",
        "List all tasks from the team's task list. Returns task IDs, subjects, status, owner, and dependencies.",
        to_json_object(json!({
            "type": "object",
            "properties": {}
        })),
    );

    router.add_route(ToolRoute::new_dyn(tool, move |_ctx| {
        async move {
            let team_name = get_team_name()?;
            let tasks_dir = get_tasks_dir(&team_name)?;

            if !tasks_dir.exists() {
                return Ok(CallToolResult::success(vec![Content::text(
                    json!({"tasks": []}).to_string(),
                )]));
            }

            let mut tasks = Vec::new();
            let mut entries = tokio::fs::read_dir(&tasks_dir).await.map_err(|e| {
                rmcp::ErrorData::internal_error(format!("Failed to read tasks dir: {}", e), None)
            })?;

            while let Some(entry) = entries.next_entry().await.map_err(|e| {
                rmcp::ErrorData::internal_error(format!("Failed to read dir entry: {}", e), None)
            })? {
                let path = entry.path();
                if path.extension().and_then(|e| e.to_str()) != Some("json") {
                    continue;
                }
                match tokio::fs::read_to_string(&path).await {
                    Ok(content) => match serde_json::from_str::<Value>(&content) {
                        Ok(task) => tasks.push(task),
                        Err(e) => {
                            debug!(path = %path.display(), error = %e, "Skipping malformed task file");
                        }
                    },
                    Err(e) => {
                        debug!(path = %path.display(), error = %e, "Skipping unreadable task file");
                    }
                }
            }

            info!(team = %team_name, count = tasks.len(), "Listed tasks");

            Ok(CallToolResult::success(vec![Content::text(
                serde_json::to_string_pretty(&json!({"tasks": tasks})).unwrap_or_default(),
            )]))
        }
        .boxed()
    }));
}

fn register_claim_task(router: &mut ToolRouter<ExomonadHandler>) {
    let tool = Tool::new(
        "claim_task",
        "Claim a task by setting yourself as the owner. Uses your agent ID from the environment.",
        to_json_object(json!({
            "type": "object",
            "properties": {
                "task_id": {
                    "type": "string",
                    "description": "The task ID to claim (e.g. '1', '2')."
                }
            },
            "required": ["task_id"]
        })),
    );

    router.add_route(ToolRoute::new_dyn(tool, move |ctx| {
        async move {
            let task_id = ctx
                .arguments
                .as_ref()
                .and_then(|a| a.get("task_id"))
                .and_then(|v| v.as_str())
                .ok_or_else(|| rmcp::ErrorData::invalid_params("task_id is required", None))?
                .to_string();

            let agent_id = std::env::var("EXOMONAD_AGENT_ID").unwrap_or_else(|_| "unknown".into());
            let team_name = get_team_name()?;
            let task_path = get_tasks_dir(&team_name)?.join(format!("{}.json", task_id));

            if !task_path.exists() {
                return Err(rmcp::ErrorData::invalid_params(
                    format!("Task {} not found", task_id),
                    None,
                ));
            }

            let content = tokio::fs::read_to_string(&task_path).await.map_err(|e| {
                rmcp::ErrorData::internal_error(format!("Failed to read task: {}", e), None)
            })?;
            let mut task: Value = serde_json::from_str(&content).map_err(|e| {
                rmcp::ErrorData::internal_error(format!("Failed to parse task: {}", e), None)
            })?;

            task["owner"] = json!(agent_id);
            task["status"] = json!("in_progress");

            let updated = serde_json::to_string_pretty(&task).unwrap_or_default();
            tokio::fs::write(&task_path, &updated).await.map_err(|e| {
                rmcp::ErrorData::internal_error(format!("Failed to write task: {}", e), None)
            })?;

            info!(task_id = %task_id, agent = %agent_id, "Task claimed");

            Ok(CallToolResult::success(vec![Content::text(
                json!({"status": "claimed", "task_id": task_id, "owner": agent_id}).to_string(),
            )]))
        }
        .boxed()
    }));
}

fn register_complete_task(router: &mut ToolRouter<ExomonadHandler>) {
    let tool = Tool::new(
        "complete_task",
        "Mark a task as completed.",
        to_json_object(json!({
            "type": "object",
            "properties": {
                "task_id": {
                    "type": "string",
                    "description": "The task ID to complete (e.g. '1', '2')."
                }
            },
            "required": ["task_id"]
        })),
    );

    router.add_route(ToolRoute::new_dyn(tool, move |ctx| {
        async move {
            let task_id = ctx
                .arguments
                .as_ref()
                .and_then(|a| a.get("task_id"))
                .and_then(|v| v.as_str())
                .ok_or_else(|| rmcp::ErrorData::invalid_params("task_id is required", None))?
                .to_string();

            let team_name = get_team_name()?;
            let task_path = get_tasks_dir(&team_name)?.join(format!("{}.json", task_id));

            if !task_path.exists() {
                return Err(rmcp::ErrorData::invalid_params(
                    format!("Task {} not found", task_id),
                    None,
                ));
            }

            let content = tokio::fs::read_to_string(&task_path).await.map_err(|e| {
                rmcp::ErrorData::internal_error(format!("Failed to read task: {}", e), None)
            })?;
            let mut task: Value = serde_json::from_str(&content).map_err(|e| {
                rmcp::ErrorData::internal_error(format!("Failed to parse task: {}", e), None)
            })?;

            task["status"] = json!("completed");

            let updated = serde_json::to_string_pretty(&task).unwrap_or_default();
            tokio::fs::write(&task_path, &updated).await.map_err(|e| {
                rmcp::ErrorData::internal_error(format!("Failed to write task: {}", e), None)
            })?;

            info!(task_id = %task_id, "Task completed");

            Ok(CallToolResult::success(vec![Content::text(
                json!({"status": "completed", "task_id": task_id}).to_string(),
            )]))
        }
        .boxed()
    }));
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
