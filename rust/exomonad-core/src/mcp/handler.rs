//! MCP server handler using the official rmcp SDK.
//!
//! `ExomonadHandler` implements `rmcp::ServerHandler`, bridging the rmcp protocol
//! layer to our WASM plugin dispatch. All tools are discovered and dispatched
//! through WASM (Haskell). Role-based filtering happens in the unified WASM module.

use super::tools::{self, MCPCallInput, MCPCallOutput};
use super::McpState;
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

/// MCP server handler backed by WASM plugin.
///
/// Holds shared state and a pre-built `ToolRouter` for dispatch.
/// The router is populated at construction from WASM tool discovery.
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

/// Build a `ToolRouter` from WASM tools.
///
/// All tools are discovered from WASM via `handle_list_tools` (which receives the role).
/// The unified WASM module handles role-based tool selection natively — Rust does
/// no filtering. Role is passed through in every tool call via `MCPCallInput`.
async fn build_tool_router(state: &McpState) -> Result<ToolRouter<ExomonadHandler>> {
    let mut router = ToolRouter::new();
    let role = state.role.clone().unwrap_or_else(|| "tl".to_string());

    // WASM tools (dynamic, from Haskell plugin — already filtered by role)
    let wasm_tools = tools::get_tool_definitions(state).await?;
    info!(count = wasm_tools.len(), role = %role, "Registering WASM tools");

    for tool_def in wasm_tools {
        let plugin = state.plugin.clone();
        let role = role.clone();
        let tool = Tool::new(
            tool_def.name.clone(),
            tool_def.description,
            to_json_object(tool_def.input_schema),
        );

        let tool_name = tool_def.name;
        router.add_route(ToolRoute::new_dyn(tool, move |ctx| {
            let plugin = plugin.clone();
            let tool_name = tool_name.clone();
            let role = role.clone();
            async move {
                let args = ctx.arguments.map(Value::Object).unwrap_or(json!({}));

                let input = MCPCallInput::new(role, tool_name.clone(), args);

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

    info!(
        total = router.list_all().len(),
        role = %role,
        "Tool router built"
    );

    Ok(router)
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
