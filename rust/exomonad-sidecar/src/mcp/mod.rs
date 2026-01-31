//! MCP (Model Context Protocol) server for Claude Code integration.
//!
//! Supports two transports:
//! - HTTP: Traditional REST endpoints for tools
//! - stdio: JSON-RPC over stdin/stdout for Claude Code's native MCP support
//!
//! All tool logic is in Haskell WASM. This module handles HTTP routing
//! and forwards tool calls to the WASM plugin via handle_mcp_call.

pub mod stdio;
mod tools;

use axum::{
    extract::State,
    http::StatusCode,
    response::IntoResponse,
    routing::{get, post},
    Json, Router,
};
use exomonad_runtime::PluginManager;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::path::PathBuf;
use std::sync::Arc;
use tower_http::cors::{Any, CorsLayer};
use tower_http::trace::TraceLayer;
use tracing::{debug, error, info};

// ============================================================================
// State
// ============================================================================

/// Shared state for MCP server.
#[derive(Clone)]
pub struct McpState {
    /// Working directory for git operations (used for logging).
    pub project_dir: PathBuf,
    /// WASM plugin for routing tool calls through Haskell.
    /// All tool calls are routed through handle_mcp_call in WASM.
    pub plugin: Arc<PluginManager>,
}

// ============================================================================
// MCP Types
// ============================================================================

/// Tool definition for MCP discovery.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolDefinition {
    pub name: String,
    pub description: String,
    #[serde(rename = "inputSchema")]
    pub input_schema: Value,
}

/// MCP tool call request.
#[derive(Debug, Deserialize)]
pub struct ToolCallRequest {
    pub tool_name: String,
    pub arguments: Value,
}

/// MCP tool call response.
#[derive(Debug, Serialize)]
pub struct ToolCallResponse {
    pub result: Option<Value>,
    pub error: Option<McpError>,
}

/// MCP error.
#[derive(Debug, Clone, Serialize)]
pub struct McpError {
    pub code: i32,
    pub message: String,
}

// ============================================================================
// Routes
// ============================================================================

/// Create the MCP router.
pub fn create_router(state: McpState) -> Router {
    let cors = CorsLayer::new()
        .allow_origin(Any)
        .allow_methods(Any)
        .allow_headers(Any);

    Router::new()
        .route("/mcp/tools", get(list_tools))
        .route("/mcp/call", post(call_tool))
        .route("/health", get(health))
        .layer(TraceLayer::new_for_http())
        .layer(cors)
        .with_state(state)
}

/// GET /mcp/tools - List available tools.
async fn list_tools(State(state): State<McpState>) -> impl IntoResponse {
    debug!("Listing MCP tools");
    match tools::get_tool_definitions(&state).await {
        Ok(tools) => Json(tools).into_response(),
        Err(e) => {
            error!(error = %e, "Failed to get tool definitions");
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("Failed to get tool definitions: {}", e),
            )
                .into_response()
        }
    }
}

/// POST /mcp/call - Execute a tool.
async fn call_tool(
    State(state): State<McpState>,
    Json(request): Json<ToolCallRequest>,
) -> impl IntoResponse {
    info!(tool = %request.tool_name, "MCP tool call");
    debug!(arguments = ?request.arguments, "Tool arguments");

    match tools::execute_tool(&state, &request.tool_name, request.arguments).await {
        Ok(result) => {
            debug!(tool = %request.tool_name, "Tool call succeeded");
            (
                StatusCode::OK,
                Json(ToolCallResponse {
                    result: Some(result),
                    error: None,
                }),
            )
        }
        Err(e) => {
            error!(tool = %request.tool_name, error = %e, "Tool call failed");
            (
                StatusCode::OK,
                Json(ToolCallResponse {
                    result: None,
                    error: Some(McpError {
                        code: -1,
                        message: e.to_string(),
                    }),
                }),
            )
        }
    }
}

/// GET /health - Health check.
async fn health() -> &'static str {
    "ok"
}

// ============================================================================
// Server Entry Point
// ============================================================================

/// Run the MCP server on the given port.
pub async fn run_server(state: McpState, port: u16) -> anyhow::Result<()> {
    let addr = std::net::SocketAddr::from(([127, 0, 0, 1], port));
    let listener = tokio::net::TcpListener::bind(addr).await?;

    info!(%addr, "MCP server listening");
    info!(
        project_dir = %state.project_dir.display(),
        "Working directory"
    );

    axum::serve(listener, create_router(state)).await?;
    Ok(())
}
