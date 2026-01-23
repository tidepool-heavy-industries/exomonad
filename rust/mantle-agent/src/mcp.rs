//! MCP stdio server for MCP tools.
//!
//! Implements the Model Context Protocol (MCP) for serving MCP tools
//! to Claude Code. Tool definitions are queried from the control server
//! at startup and served via JSON-RPC 2.0 over stdio.
//!
//! ## Protocol Flow
//!
//! ```text
//! Claude Code                    mantle-agent mcp
//!     |                                |
//!     | initialize request            |
//!     |------------------------------>|
//!     |<------------------------------|
//!     | initialize response           |
//!     |                                |
//!     | tools/list request            |
//!     |------------------------------>|
//!     |<------------------------------|
//!     | tools/list response           |
//!     |                                |
//!     | tools/call request            |
//!     |------------------------------>|
//!     |   (forwards to control socket) |
//!     |<------------------------------|
//!     | tools/call response           |
//! ```
//!
//! ## Environment Variables
//!
//! - `TIDEPOOL_CONTROL_SOCKET`: Path to control server Unix socket (optional, defaults to .tidepool/sockets/control.sock)

use mantle_shared::protocol::{ControlMessage, ControlResponse, McpError};
use mantle_shared::socket::control_socket_path;
use mantle_shared::ControlSocket;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};
use tracing::{debug, error, info};

// ============================================================================
// JSON-RPC 2.0 Types
// ============================================================================

/// JSON-RPC 2.0 request (or notification).
///
/// MCP notifications have no `id` field - they're fire-and-forget.
/// Requests have an `id` and expect a response.
#[derive(Debug, Deserialize)]
struct JsonRpcRequest {
    #[allow(dead_code)]
    jsonrpc: String,
    /// Request ID. None for notifications (no response expected).
    id: Option<Value>,
    method: String,
    #[serde(default)]
    params: Value,
}

/// JSON-RPC 2.0 response.
#[derive(Debug, Serialize)]
struct JsonRpcResponse {
    jsonrpc: String,
    id: Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<JsonRpcError>,
}

/// JSON-RPC 2.0 error object.
#[derive(Debug, Serialize)]
struct JsonRpcError {
    code: i32,
    message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    data: Option<Value>,
}

impl JsonRpcResponse {
    fn success(id: Value, result: Value) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(result),
            error: None,
        }
    }

    fn error(id: Value, code: i32, message: String) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            result: None,
            error: Some(JsonRpcError {
                code,
                message,
                data: None,
            }),
        }
    }

    fn method_not_found(id: Value, method: &str) -> Self {
        Self::error(id, -32601, format!("Method not found: {}", method))
    }

    fn invalid_params(id: Value, message: String) -> Self {
        Self::error(id, -32602, message)
    }
}

// ============================================================================
// MCP Protocol Types
// ============================================================================

/// Tool definition matching MCP spec and Haskell DecisionTool.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ToolDefinition {
    pub name: String,
    pub description: String,
    #[serde(rename = "inputSchema")]
    pub input_schema: Value,
}

/// MCP server capabilities.
#[derive(Debug, Serialize)]
struct ServerCapabilities {
    tools: ToolsCapability,
}

#[derive(Debug, Serialize)]
struct ToolsCapability {
    #[serde(rename = "listChanged")]
    list_changed: bool,
}

/// MCP initialize result.
#[derive(Debug, Serialize)]
struct InitializeResult {
    #[serde(rename = "protocolVersion")]
    protocol_version: String,
    capabilities: ServerCapabilities,
    #[serde(rename = "serverInfo")]
    server_info: ServerInfo,
}

#[derive(Debug, Serialize)]
struct ServerInfo {
    name: String,
    version: String,
}

/// MCP tools/list result.
#[derive(Debug, Serialize)]
struct ToolsListResult {
    tools: Vec<ToolDefinition>,
}

/// MCP tools/call params.
#[derive(Debug, Deserialize)]
struct ToolCallParams {
    name: String,
    #[serde(default)]
    arguments: Value,
}

/// MCP tools/call result content.
#[derive(Debug, Serialize)]
struct ToolCallResult {
    content: Vec<ToolResultContent>,
    #[serde(rename = "isError", skip_serializing_if = "Option::is_none")]
    is_error: Option<bool>,
}

#[derive(Debug, Serialize)]
struct ToolResultContent {
    #[serde(rename = "type")]
    content_type: String,
    text: String,
}

// ============================================================================
// MCP Server
// ============================================================================

/// Query control server for available MCP tools.
///
/// Returns an error if:
/// - Connection to control server fails
/// - Protocol error (invalid response)
///
/// This implements fail-fast behavior - if the control server is unreachable,
/// mantle-agent exits with an error rather than serving an empty tool list.
fn query_control_server_tools() -> Result<Vec<ToolDefinition>, String> {
    // Get control server socket path
    let path = control_socket_path().map_err(|e| e.to_string())?;

    info!(path = %path.display(), "Querying control server for tool definitions");

    // Connect to control server
    let mut socket = ControlSocket::connect(&path)
        .map_err(|e| format!("Failed to connect to control server at {}: {}", path.display(), e))?;

    // Send ToolsListRequest
    let message = ControlMessage::ToolsListRequest;
    let response = socket.send(&message)
        .map_err(|e| format!("Failed to query tools from control server: {}", e))?;

    // Parse response
    match response {
        ControlResponse::ToolsListResponse { tools } => {
            info!(count = tools.len(), "Discovered tools from control server");

            // Convert protocol::ToolDefinition -> mcp::ToolDefinition
            let mcp_tools = tools.into_iter().map(|proto_tool| {
                ToolDefinition {
                    name: proto_tool.name,
                    description: proto_tool.description,
                    input_schema: proto_tool.input_schema,
                }
            }).collect();

            Ok(mcp_tools)
        }
        _ => Err(format!("Unexpected response type from control server (expected ToolsListResponse)")),
    }
}

/// MCP server state.
pub struct McpServer {
    tools: Vec<ToolDefinition>,
    /// Control server socket path for forwarding tool calls.
    control_path: Option<PathBuf>,
    /// Optional allowlist of tool names to expose (if None, all tools exposed).
    tools_allowlist: Option<Vec<String>>,
}

impl McpServer {
    /// Create a new MCP server with explicit tools and optional allowlist.
    pub fn new(
        control_path: Option<PathBuf>,
        tools: Vec<ToolDefinition>,
        tools_allowlist: Option<Vec<String>>,
    ) -> Self {
        Self {
            tools,
            control_path,
            tools_allowlist,
        }
    }

    /// Create a new MCP server by querying the control server for tools.
    ///
    /// Queries the control server for all available MCP tools at startup.
    /// This implements fail-fast behavior: if the control server is unreachable,
    /// this function panics rather than serving an empty tool list.
    ///
    /// Control server address is read from `TIDEPOOL_CONTROL_SOCKET` or default.
    /// If `tools_allowlist` is provided, only tools in the allowlist will be exposed.
    pub fn new_from_env(tools_allowlist: Option<Vec<String>>) -> Self {
        // Query control server for tools (REQUIRED - fail fast if unavailable)
        let tools = query_control_server_tools()
            .unwrap_or_else(|e| {
                panic!("Failed to discover tools from control server: {}", e);
            });

        // Get control server socket path
        let control_path = control_socket_path().ok();

        Self::new(control_path, tools, tools_allowlist)
    }

    /// Run the MCP server on stdio.
    pub fn run(&mut self) -> io::Result<()> {
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        let reader = stdin.lock();

        info!(
            control_path = ?self.control_path,
            tool_count = self.tools.len(),
            "MCP server starting on stdio"
        );

        for line in reader.lines() {
            let line = line?;
            if line.trim().is_empty() {
                continue;
            }

            debug!(request = %line, "Received JSON-RPC message");

            let response = match serde_json::from_str::<JsonRpcRequest>(&line) {
                Ok(request) => self.handle_request(request),
                Err(e) => {
                    error!(error = %e, "Failed to parse JSON-RPC message");
                    // Parse errors always get a response (with null id)
                    Some(JsonRpcResponse::error(
                        Value::Null,
                        -32700,
                        format!("Parse error: {}", e),
                    ))
                }
            };

            // Only send response for requests (not notifications)
            if let Some(response) = response {
                let response_json = serde_json::to_string(&response)
                    .expect("Failed to serialize response");
                debug!(response = %response_json, "Sending JSON-RPC response");

                writeln!(stdout, "{}", response_json)?;
                stdout.flush()?;
            }
        }

        info!("MCP server shutting down");
        Ok(())
    }

    /// Handle a JSON-RPC request or notification.
    ///
    /// Returns `None` for notifications (no response expected).
    /// Returns `Some(response)` for requests.
    fn handle_request(&mut self, request: JsonRpcRequest) -> Option<JsonRpcResponse> {
        // Notifications have no id - they don't expect a response
        if let Some(id) = request.id {
            // This is a request - needs a response
            let response = match request.method.as_str() {
                "initialize" => self.handle_initialize(id),
                "initialized" => {
                    // This shouldn't happen (initialized is a notification), but handle gracefully
                    JsonRpcResponse::success(id, json!({}))
                }
                "tools/list" => self.handle_tools_list(id),
                "tools/call" => self.handle_tools_call(id, request.params),
                "notifications/cancelled" => {
                    // This shouldn't happen (notifications have no id), but handle gracefully
                    JsonRpcResponse::success(id, json!({}))
                }
                _ => JsonRpcResponse::method_not_found(id, &request.method),
            };
            Some(response)
        } else {
            // This is a notification - handle silently, no response
            debug!(method = %request.method, "Received notification (no response)");
            None
        }
    }

    /// Handle initialize request.
    fn handle_initialize(&self, id: Value) -> JsonRpcResponse {
        let result = InitializeResult {
            protocol_version: "2024-11-05".to_string(),
            capabilities: ServerCapabilities {
                tools: ToolsCapability {
                    list_changed: false,
                },
            },
            server_info: ServerInfo {
                name: "mantle-agent".to_string(),
                version: env!("CARGO_PKG_VERSION").to_string(),
            },
        };

        JsonRpcResponse::success(id, serde_json::to_value(result).unwrap())
    }

    /// Handle tools/list request.
    ///
    /// Filters tools based on the allowlist if provided. If no allowlist is set,
    /// returns all tools from the control server.
    fn handle_tools_list(&self, id: Value) -> JsonRpcResponse {
        let filtered_tools = if let Some(ref allowlist) = self.tools_allowlist {
            // Filter to only tools in the allowlist
            self.tools
                .iter()
                .filter(|tool| allowlist.contains(&tool.name))
                .cloned()
                .collect()
        } else {
            // No allowlist - expose all tools
            self.tools.clone()
        };

        let result = ToolsListResult {
            tools: filtered_tools,
        };

        JsonRpcResponse::success(id, serde_json::to_value(result).unwrap())
    }

    /// Handle tools/call request.
    ///
    /// Rejects calls to tools not in the allowlist (if allowlist is set).
    fn handle_tools_call(&mut self, id: Value, params: Value) -> JsonRpcResponse {
        // Parse tool call params
        let call_params: ToolCallParams = match serde_json::from_value(params) {
            Ok(p) => p,
            Err(e) => {
                return JsonRpcResponse::invalid_params(
                    id,
                    format!("Invalid tool call params: {}", e),
                );
            }
        };

        info!(
            tool = %call_params.name,
            "Processing MCP tool call"
        );

        // Check allowlist first if present
        if let Some(ref allowlist) = self.tools_allowlist {
            if !allowlist.contains(&call_params.name) {
                return JsonRpcResponse::error(
                    id,
                    -32602,
                    format!(
                        "Tool '{}' is not in the allowlist. Available tools: {}",
                        call_params.name,
                        allowlist.join(", ")
                    ),
                );
            }
        }

        // Verify tool exists
        if !self.tools.iter().any(|t| t.name == call_params.name) {
            return JsonRpcResponse::error(
                id,
                -32602,
                format!("Unknown tool: {}", call_params.name),
            );
        }

        // Forward to control server - REQUIRED for MCP tools
        let path = match &self.control_path {
            Some(p) => p,
            None => {
                error!("No control server configured - cannot forward MCP tool call");
                return JsonRpcResponse::error(
                    id,
                    -32603,
                    "Internal error: control server not configured".to_string(),
                );
            }
        };

        info!(path = %path.display(), tool = %call_params.name, "Forwarding tool call to control server");

        match self.forward_tool_call(path, &call_params) {
            Ok(Err(err)) => {
                // MCP error from control socket
                JsonRpcResponse::error(id, err.code, err.message)
            }
            Ok(Ok(result_value)) => {
                // Wrap the raw result JSON in MCP content format
                // The Haskell side returns domain-specific JSON; we format it for MCP here
                let text_content = serde_json::to_string_pretty(&result_value)
                    .unwrap_or_else(|_| result_value.to_string());
                let result = ToolCallResult {
                    content: vec![ToolResultContent {
                        content_type: "text".to_string(),
                        text: text_content,
                    }],
                    is_error: None,
                };
                JsonRpcResponse::success(id, serde_json::to_value(result).unwrap())
            }
            Err(e) => {
                // Socket error is a HARD FAILURE - do not silently succeed
                error!(error = %e, "Failed to forward tool call to control socket");
                JsonRpcResponse::error(
                    id,
                    -32603,
                    format!("Control socket error: {}", e),
                )
            }
        }
    }

    /// Forward a tool call to the control server via Unix socket.
    ///
    /// Returns the host's result (if successful) or an MCP error.
    /// The result Value contains the host's response text which may include
    /// termination instructions for MCP tools.
    fn forward_tool_call(
        &self,
        path: &Path,
        params: &ToolCallParams,
    ) -> Result<Result<Value, McpError>, String> {
        let mut socket = ControlSocket::connect(path)
            .map_err(|e| format!("Unix socket connect failed: {}", e))?;

        let message = ControlMessage::McpToolCall {
            id: uuid::Uuid::new_v4().to_string(),
            tool_name: params.name.clone(),
            arguments: params.arguments.clone(),
        };

        let response = socket
            .send(&message)
            .map_err(|e| format!("Socket send failed: {}", e))?;

        match response {
            ControlResponse::McpToolResponse { result, error, .. } => {
                error.map_or_else(|| {
                    // Use the host's result or create a default response
                    let result_value = result.unwrap_or_else(|| {
                        serde_json::json!({
                            "content": [{
                                "type": "text",
                                "text": format!("Decision recorded: {}", params.name.replace("decision::", ""))
                            }]
                        })
                    });
                    Ok(Ok(result_value))
                }, |err| Ok(Err(err)))
            }
            ControlResponse::HookResponse { .. } => {
                Err("Unexpected HookResponse for MCP call".to_string())
            }
            ControlResponse::ToolsListResponse { .. } => {
                Err("Unexpected ToolsListResponse for MCP tool call".to_string())
            }
            ControlResponse::Pong => {
                Err("Unexpected Pong response for MCP tool call".to_string())
            }
        }
    }
}

/// Run the MCP server.
///
/// This is the main entry point called from the CLI.
///
/// Queries the control server for available MCP tools at startup using:
/// - TIDEPOOL_CONTROL_SOCKET: Control server socket path (optional)
///
/// If `tools_allowlist` is provided, only tools in the allowlist will be exposed
/// to the MCP client. If `None`, all tools from the control server are exposed.
///
/// Implements fail-fast behavior: if the control server is unreachable during
/// initialization, the server exits with an error. This ensures Claude Code sees
/// an accurate tool list and catches configuration issues early.
pub fn run_mcp_server(tools_allowlist: Option<Vec<String>>) -> Result<(), Box<dyn std::error::Error>> {
    let mut server = McpServer::new_from_env(tools_allowlist);
    server.run()?;
    Ok(())
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_rpc_request_parsing() {
        let json = r#"{"jsonrpc":"2.0","id":"1","method":"initialize","params":{}}"#;
        let request: JsonRpcRequest = serde_json::from_str(json).unwrap();
        assert_eq!(request.method, "initialize");
        assert_eq!(request.id, Some(json!("1")));
    }

    #[test]
    fn test_json_rpc_response_success() {
        let response = JsonRpcResponse::success(json!("1"), json!({"key": "value"}));
        let json = serde_json::to_string(&response).unwrap();
        assert!(json.contains("\"result\""));
        assert!(!json.contains("\"error\""));
    }

    #[test]
    fn test_json_rpc_response_error() {
        let response = JsonRpcResponse::error(json!("1"), -32600, "Invalid Request".to_string());
        let json = serde_json::to_string(&response).unwrap();
        assert!(!json.contains("\"result\""));
        assert!(json.contains("\"error\""));
        assert!(json.contains("-32600"));
    }

    #[test]
    fn test_tool_definition_parsing() {
        let json = r#"{
            "name": "decision::approve",
            "description": "Approve the request",
            "inputSchema": {"type": "object", "properties": {"notes": {"type": "string"}}}
        }"#;
        let tool: ToolDefinition = serde_json::from_str(json).unwrap();
        assert_eq!(tool.name, "decision::approve");
    }

    #[test]
    fn test_initialize_result() {
        let result = InitializeResult {
            protocol_version: "2024-11-05".to_string(),
            capabilities: ServerCapabilities {
                tools: ToolsCapability { list_changed: false },
            },
            server_info: ServerInfo {
                name: "mantle-agent".to_string(),
                version: "0.1.0".to_string(),
            },
        };
        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("protocolVersion"));
        assert!(json.contains("2024-11-05"));
    }

    #[test]
    fn test_tools_list_result() {
        let result = ToolsListResult {
            tools: vec![ToolDefinition {
                name: "decision::approve".to_string(),
                description: "Approve".to_string(),
                input_schema: json!({"type": "object"}),
            }],
        };
        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("decision::approve"));
    }

    #[test]
    fn test_tool_call_result() {
        let result = ToolCallResult {
            content: vec![ToolResultContent {
                content_type: "text".to_string(),
                text: "Decision recorded: approve".to_string(),
            }],
            is_error: None,
        };
        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("Decision recorded"));
        assert!(!json.contains("isError"));
    }

    #[test]
    fn test_server_handles_initialize() {
        let server = McpServer::new(None, vec![], None);
        let response = server.handle_initialize(json!("init-1"));
        assert!(response.result.is_some());
        assert!(response.error.is_none());
    }

    #[test]
    fn test_server_handles_tools_list_empty() {
        let server = McpServer::new(None, vec![], None);
        let response = server.handle_tools_list(json!("list-1"));
        assert!(response.result.is_some());
        let result = response.result.unwrap();
        assert!(result["tools"].as_array().unwrap().is_empty());
    }

    #[test]
    fn test_server_handles_unknown_method() {
        let mut server = McpServer::new(None, vec![], None);
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!("1")),
            method: "unknown/method".to_string(),
            params: json!({}),
        };
        let response = server.handle_request(request);
        assert!(response.is_some());
        let response = response.unwrap();
        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, -32601);
    }

    #[test]
    fn test_tools_list_filters_with_allowlist() {
        let tools = vec![
            ToolDefinition {
                name: "tool_a".to_string(),
                description: "Tool A".to_string(),
                input_schema: json!({"type": "object"}),
            },
            ToolDefinition {
                name: "tool_b".to_string(),
                description: "Tool B".to_string(),
                input_schema: json!({"type": "object"}),
            },
            ToolDefinition {
                name: "tool_c".to_string(),
                description: "Tool C".to_string(),
                input_schema: json!({"type": "object"}),
            },
        ];
        let allowlist = Some(vec!["tool_a".to_string(), "tool_c".to_string()]);
        let server = McpServer::new(None, tools, allowlist);

        let response = server.handle_tools_list(json!("list-1"));
        assert!(response.result.is_some());

        let result = response.result.unwrap();
        let tools_array = result["tools"].as_array().unwrap();
        assert_eq!(tools_array.len(), 2);

        let tool_names: Vec<String> = tools_array
            .iter()
            .map(|t| t["name"].as_str().unwrap().to_string())
            .collect();
        assert!(tool_names.contains(&"tool_a".to_string()));
        assert!(tool_names.contains(&"tool_c".to_string()));
        assert!(!tool_names.contains(&"tool_b".to_string()));
    }

    #[test]
    fn test_tools_list_no_filter_without_allowlist() {
        let tools = vec![
            ToolDefinition {
                name: "tool_a".to_string(),
                description: "Tool A".to_string(),
                input_schema: json!({"type": "object"}),
            },
            ToolDefinition {
                name: "tool_b".to_string(),
                description: "Tool B".to_string(),
                input_schema: json!({"type": "object"}),
            },
        ];
        let server = McpServer::new(None, tools.clone(), None);

        let response = server.handle_tools_list(json!("list-1"));
        assert!(response.result.is_some());

        let result = response.result.unwrap();
        let tools_array = result["tools"].as_array().unwrap();
        assert_eq!(tools_array.len(), 2);
    }

    #[test]
    fn test_tool_call_rejects_non_allowlisted_tool() {
        let tools = vec![
            ToolDefinition {
                name: "tool_a".to_string(),
                description: "Tool A".to_string(),
                input_schema: json!({"type": "object"}),
            },
            ToolDefinition {
                name: "tool_b".to_string(),
                description: "Tool B".to_string(),
                input_schema: json!({"type": "object"}),
            },
        ];
        let allowlist = Some(vec!["tool_a".to_string()]);
        let mut server = McpServer::new(None, tools, allowlist);

        let params = json!({
            "name": "tool_b",
            "arguments": {}
        });
        let response = server.handle_tools_call(json!("call-1"), params);

        assert!(response.error.is_some());
        let error = response.error.unwrap();
        assert_eq!(error.code, -32602);
        assert!(error.message.contains("not in the allowlist"));
    }

    #[test]
    fn test_tool_call_allows_allowlisted_tool() {
        let tools = vec![
            ToolDefinition {
                name: "tool_a".to_string(),
                description: "Tool A".to_string(),
                input_schema: json!({"type": "object"}),
            },
        ];
        let allowlist = Some(vec!["tool_a".to_string()]);
        let mut server = McpServer::new(None, tools, allowlist);

        let params = json!({
            "name": "tool_a",
            "arguments": {}
        });
        let response = server.handle_tools_call(json!("call-1"), params);

        // Should fail because no control server is configured, but NOT because of allowlist
        // The error should be about control server, not allowlist
        assert!(response.error.is_some());
        let error = response.error.unwrap();
        assert_eq!(error.code, -32603); // Internal error (no control server)
        assert!(!error.message.contains("not in the allowlist"));
    }
}