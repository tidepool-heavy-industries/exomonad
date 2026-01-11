//! MCP stdio server for decision tools.
//!
//! Implements the Model Context Protocol (MCP) for serving decision tools
//! to Claude Code. Tool definitions are read from the `MANTLE_DECISION_TOOLS`
//! environment variable and served via JSON-RPC 2.0 over stdio.
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
//! - `MANTLE_DECISION_TOOLS`: JSON array of tool definitions
//! - `MANTLE_HOOK_SOCKET`: Path to control socket for tool call forwarding

use mantle_shared::protocol::{ControlMessage, ControlResponse, McpError};
use mantle_shared::ControlSocket;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::io::{self, BufRead, Write};
use std::path::PathBuf;
use tracing::{debug, error, info, warn};

// ============================================================================
// JSON-RPC 2.0 Types
// ============================================================================

/// JSON-RPC 2.0 request.
#[derive(Debug, Deserialize)]
struct JsonRpcRequest {
    jsonrpc: String,
    id: Value,
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

    fn internal_error(id: Value, message: String) -> Self {
        Self::error(id, -32603, message)
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

/// MCP server state.
pub struct McpServer {
    tools: Vec<ToolDefinition>,
    socket_path: Option<PathBuf>,
}

impl McpServer {
    /// Create a new MCP server.
    ///
    /// Reads tool definitions from `MANTLE_DECISION_TOOLS` environment variable.
    pub fn new(socket_path: Option<PathBuf>) -> Self {
        let tools = match std::env::var("MANTLE_DECISION_TOOLS") {
            Ok(json) => match serde_json::from_str(&json) {
                Ok(tools) => {
                    info!(count = ?Vec::<ToolDefinition>::len(&tools), "Loaded decision tools");
                    tools
                }
                Err(e) => {
                    warn!(error = %e, "Failed to parse MANTLE_DECISION_TOOLS, serving no tools");
                    Vec::new()
                }
            },
            Err(_) => {
                debug!("MANTLE_DECISION_TOOLS not set, serving no tools");
                Vec::new()
            }
        };

        Self { tools, socket_path }
    }

    /// Run the MCP server on stdio.
    pub fn run(&mut self) -> io::Result<()> {
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        let reader = stdin.lock();

        info!("MCP server starting on stdio");

        for line in reader.lines() {
            let line = line?;
            if line.trim().is_empty() {
                continue;
            }

            debug!(request = %line, "Received JSON-RPC request");

            let response = match serde_json::from_str::<JsonRpcRequest>(&line) {
                Ok(request) => self.handle_request(request),
                Err(e) => {
                    error!(error = %e, "Failed to parse JSON-RPC request");
                    JsonRpcResponse::error(
                        Value::Null,
                        -32700,
                        format!("Parse error: {}", e),
                    )
                }
            };

            let response_json = serde_json::to_string(&response)
                .expect("Failed to serialize response");
            debug!(response = %response_json, "Sending JSON-RPC response");

            writeln!(stdout, "{}", response_json)?;
            stdout.flush()?;
        }

        info!("MCP server shutting down");
        Ok(())
    }

    /// Handle a JSON-RPC request.
    fn handle_request(&mut self, request: JsonRpcRequest) -> JsonRpcResponse {
        match request.method.as_str() {
            "initialize" => self.handle_initialize(request.id),
            "initialized" => {
                // Notification, no response needed but we return empty for protocol compliance
                JsonRpcResponse::success(request.id, json!({}))
            }
            "tools/list" => self.handle_tools_list(request.id),
            "tools/call" => self.handle_tools_call(request.id, request.params),
            "notifications/cancelled" => {
                // Cancellation notification, acknowledge
                JsonRpcResponse::success(request.id, json!({}))
            }
            _ => JsonRpcResponse::method_not_found(request.id, &request.method),
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
    fn handle_tools_list(&self, id: Value) -> JsonRpcResponse {
        let result = ToolsListResult {
            tools: self.tools.clone(),
        };

        JsonRpcResponse::success(id, serde_json::to_value(result).unwrap())
    }

    /// Handle tools/call request.
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
            "Processing decision tool call"
        );

        // Verify tool exists
        if !self.tools.iter().any(|t| t.name == call_params.name) {
            return JsonRpcResponse::error(
                id,
                -32602,
                format!("Unknown tool: {}", call_params.name),
            );
        }

        // Forward to control socket if available
        if let Some(ref socket_path) = self.socket_path {
            match self.forward_tool_call(socket_path, &call_params) {
                Ok(Some(err)) => {
                    // MCP error from control socket
                    JsonRpcResponse::error(id, err.code, err.message)
                }
                Ok(None) => {
                    // Success
                    self.tool_success_response(id, &call_params.name)
                }
                Err(e) => {
                    // Socket error - still return success to Claude but log warning
                    warn!(error = %e, "Failed to forward tool call to control socket");
                    self.tool_success_response(id, &call_params.name)
                }
            }
        } else {
            // No socket configured - just acknowledge the tool call
            debug!("No control socket configured, acknowledging tool call locally");
            self.tool_success_response(id, &call_params.name)
        }
    }

    /// Forward a tool call to the control socket.
    fn forward_tool_call(
        &self,
        socket_path: &PathBuf,
        params: &ToolCallParams,
    ) -> Result<Option<McpError>, String> {
        let mut socket = ControlSocket::connect(socket_path)
            .map_err(|e| format!("Socket connect failed: {}", e))?;

        let message = ControlMessage::McpToolCall {
            id: uuid::Uuid::new_v4().to_string(),
            tool_name: params.name.clone(),
            arguments: params.arguments.clone(),
        };

        let response = socket
            .send(&message)
            .map_err(|e| format!("Socket send failed: {}", e))?;

        match response {
            ControlResponse::McpToolResponse { error, .. } => Ok(error),
            ControlResponse::HookResponse { .. } => {
                Err("Unexpected HookResponse for MCP call".to_string())
            }
        }
    }

    /// Create a success response for a tool call.
    fn tool_success_response(&self, id: Value, tool_name: &str) -> JsonRpcResponse {
        let result = ToolCallResult {
            content: vec![ToolResultContent {
                content_type: "text".to_string(),
                text: format!("Decision recorded: {}", tool_name.replace("decision::", "")),
            }],
            is_error: None,
        };

        JsonRpcResponse::success(id, serde_json::to_value(result).unwrap())
    }
}

/// Run the MCP server.
///
/// This is the main entry point called from the CLI.
pub fn run_mcp_server(socket_path: Option<PathBuf>) -> Result<(), Box<dyn std::error::Error>> {
    let mut server = McpServer::new(socket_path);
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
        assert_eq!(request.id, json!("1"));
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
        let server = McpServer::new(None);
        let response = server.handle_initialize(json!("init-1"));
        assert!(response.result.is_some());
        assert!(response.error.is_none());
    }

    #[test]
    fn test_server_handles_tools_list_empty() {
        let server = McpServer::new(None);
        let response = server.handle_tools_list(json!("list-1"));
        assert!(response.result.is_some());
        let result = response.result.unwrap();
        assert!(result["tools"].as_array().unwrap().is_empty());
    }

    #[test]
    fn test_server_handles_unknown_method() {
        let mut server = McpServer::new(None);
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: json!("1"),
            method: "unknown/method".to_string(),
            params: json!({}),
        };
        let response = server.handle_request(request);
        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, -32601);
    }

    #[test]
    fn test_server_with_tools_from_env() {
        // Set env var temporarily
        std::env::set_var(
            "MANTLE_DECISION_TOOLS",
            r#"[{"name":"decision::test","description":"Test tool","inputSchema":{"type":"object"}}]"#,
        );

        let server = McpServer::new(None);
        assert_eq!(server.tools.len(), 1);
        assert_eq!(server.tools[0].name, "decision::test");

        // Clean up
        std::env::remove_var("MANTLE_DECISION_TOOLS");
    }
}
