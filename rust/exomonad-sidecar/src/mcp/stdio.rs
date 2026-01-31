//! stdio MCP transport for Claude Code.
//!
//! Implements JSON-RPC over stdin/stdout for the MCP protocol.
//! Claude Code spawns this process and communicates via stdio.

use super::{tools, McpState, ToolDefinition};
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::io::{BufRead, Write};
use tracing::{debug, error, info};

// ============================================================================
// JSON-RPC Types
// ============================================================================

#[derive(Debug, Deserialize)]
struct JsonRpcRequest {
    #[allow(dead_code)]
    jsonrpc: String,
    id: Option<Value>,
    method: String,
    #[serde(default)]
    params: Value,
}

#[derive(Debug, Serialize)]
struct JsonRpcResponse {
    jsonrpc: &'static str,
    id: Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<JsonRpcError>,
}

#[derive(Debug, Serialize)]
struct JsonRpcError {
    code: i32,
    message: String,
}

impl JsonRpcResponse {
    fn success(id: Value, result: Value) -> Self {
        Self {
            jsonrpc: "2.0",
            id,
            result: Some(result),
            error: None,
        }
    }

    fn error(id: Value, code: i32, message: String) -> Self {
        Self {
            jsonrpc: "2.0",
            id,
            result: None,
            error: Some(JsonRpcError { code, message }),
        }
    }
}

// ============================================================================
// MCP Protocol Types
// ============================================================================

#[derive(Debug, Serialize)]
struct McpServerInfo {
    name: String,
    version: String,
}

#[derive(Debug, Serialize)]
struct McpCapabilities {
    tools: McpToolsCapability,
}

#[derive(Debug, Serialize)]
struct McpToolsCapability {
    #[serde(rename = "listChanged")]
    list_changed: bool,
}

#[derive(Debug, Serialize)]
struct McpInitializeResult {
    #[serde(rename = "protocolVersion")]
    protocol_version: String,
    capabilities: McpCapabilities,
    #[serde(rename = "serverInfo")]
    server_info: McpServerInfo,
}

#[derive(Debug, Serialize)]
struct McpToolsListResult {
    tools: Vec<McpTool>,
}

#[derive(Debug, Serialize)]
struct McpTool {
    name: String,
    description: String,
    #[serde(rename = "inputSchema")]
    input_schema: Value,
}

impl From<ToolDefinition> for McpTool {
    fn from(def: ToolDefinition) -> Self {
        Self {
            name: def.name,
            description: def.description,
            input_schema: def.input_schema,
        }
    }
}

#[derive(Debug, Serialize)]
struct McpToolCallResult {
    content: Vec<McpContent>,
    #[serde(rename = "isError", skip_serializing_if = "Option::is_none")]
    is_error: Option<bool>,
}

#[derive(Debug, Serialize)]
struct McpContent {
    #[serde(rename = "type")]
    content_type: String,
    text: String,
}

// ============================================================================
// Request Handling
// ============================================================================

async fn handle_request(state: &McpState, request: JsonRpcRequest) -> JsonRpcResponse {
    let id = request.id.clone().unwrap_or(Value::Null);

    debug!(method = %request.method, "Handling MCP request");

    match request.method.as_str() {
        "initialize" => {
            let result = McpInitializeResult {
                protocol_version: "2024-11-05".to_string(),
                capabilities: McpCapabilities {
                    tools: McpToolsCapability {
                        list_changed: false,
                    },
                },
                server_info: McpServerInfo {
                    name: "exomonad-sidecar".to_string(),
                    version: env!("CARGO_PKG_VERSION").to_string(),
                },
            };
            JsonRpcResponse::success(id, serde_json::to_value(result).unwrap())
        }

        "notifications/initialized" => {
            // This is a notification, no response needed
            // But we need to return something for the response flow
            JsonRpcResponse::success(id, json!({}))
        }

        "tools/list" => {
            let tool_defs = tools::get_tool_definitions();
            let mcp_tools: Vec<McpTool> = tool_defs.into_iter().map(McpTool::from).collect();
            let result = McpToolsListResult { tools: mcp_tools };
            JsonRpcResponse::success(id, serde_json::to_value(result).unwrap())
        }

        "tools/call" => {
            let tool_name = request
                .params
                .get("name")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let arguments = request
                .params
                .get("arguments")
                .cloned()
                .unwrap_or(json!({}));

            info!(tool = %tool_name, "Executing tool");

            match tools::execute_tool(state, tool_name, arguments).await {
                Ok(result) => {
                    let text = serde_json::to_string_pretty(&result).unwrap_or_default();
                    let mcp_result = McpToolCallResult {
                        content: vec![McpContent {
                            content_type: "text".to_string(),
                            text,
                        }],
                        is_error: None,
                    };
                    JsonRpcResponse::success(id, serde_json::to_value(mcp_result).unwrap())
                }
                Err(e) => {
                    error!(tool = %tool_name, error = %e, "Tool execution failed");
                    let mcp_result = McpToolCallResult {
                        content: vec![McpContent {
                            content_type: "text".to_string(),
                            text: format!("Error: {}", e),
                        }],
                        is_error: Some(true),
                    };
                    JsonRpcResponse::success(id, serde_json::to_value(mcp_result).unwrap())
                }
            }
        }

        _ => {
            debug!(method = %request.method, "Unknown method");
            JsonRpcResponse::error(id, -32601, format!("Method not found: {}", request.method))
        }
    }
}

// ============================================================================
// Main Loop
// ============================================================================

/// Run the stdio MCP server.
///
/// Reads JSON-RPC requests from stdin (one per line), processes them,
/// and writes responses to stdout.
pub async fn run_stdio_server(state: McpState) -> Result<()> {
    info!("Starting stdio MCP server");

    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();

    for line in stdin.lock().lines() {
        let line = line.context("Failed to read from stdin")?;

        if line.trim().is_empty() {
            continue;
        }

        debug!(line_len = line.len(), "Received request");

        // Parse JSON-RPC request
        let request: JsonRpcRequest = match serde_json::from_str(&line) {
            Ok(req) => req,
            Err(e) => {
                error!(error = %e, "Failed to parse JSON-RPC request");
                let response =
                    JsonRpcResponse::error(Value::Null, -32700, format!("Parse error: {}", e));
                let response_json = serde_json::to_string(&response)?;
                writeln!(stdout, "{}", response_json)?;
                stdout.flush()?;
                continue;
            }
        };

        // Skip notifications (no id means notification)
        let is_notification = request.id.is_none();

        // Handle request
        let response = handle_request(&state, request).await;

        // Only send response for requests (not notifications)
        if !is_notification {
            let response_json = serde_json::to_string(&response)?;
            debug!(response_len = response_json.len(), "Sending response");
            writeln!(stdout, "{}", response_json)?;
            stdout.flush()?;
        }
    }

    info!("stdin closed, shutting down");
    Ok(())
}
