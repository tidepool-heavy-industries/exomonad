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
use mantle_shared::socket::control_server_addr;
use mantle_shared::ControlSocket;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::io::{self, BufRead, Write};
use tracing::{debug, error, info};

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
    /// Control server address (host, port) for forwarding tool calls.
    control_addr: Option<(String, u16)>,
}

impl McpServer {
    /// Create a new MCP server with explicit tools.
    pub fn new(control_addr: Option<(String, u16)>, tools: Vec<ToolDefinition>) -> Self {
        Self { tools, control_addr }
    }

    /// Create a new MCP server, reading tools from `MANTLE_DECISION_TOOLS_FILE` env var
    /// and control address from `MANTLE_CONTROL_HOST`/`MANTLE_CONTROL_PORT` env vars.
    ///
    /// The file path approach avoids shell escaping issues with passing JSON
    /// through environment variables and command-line arguments.
    pub fn new_from_env() -> Self {
        let tools = match std::env::var("MANTLE_DECISION_TOOLS_FILE") {
            Ok(file_path) => {
                info!(file = %file_path, "Loading decision tools from file");
                match std::fs::read_to_string(&file_path) {
                    Ok(json) => match serde_json::from_str(&json) {
                        Ok(tools) => {
                            info!(count = ?Vec::<ToolDefinition>::len(&tools), "Loaded decision tools");
                            tools
                        }
                        Err(e) => {
                            error!(error = %e, file = %file_path, "Failed to parse decision tools JSON");
                            Vec::new()
                        }
                    },
                    Err(e) => {
                        error!(error = %e, file = %file_path, "Failed to read decision tools file");
                        Vec::new()
                    }
                }
            }
            Err(_) => {
                debug!("MANTLE_DECISION_TOOLS_FILE not set, serving no tools");
                Vec::new()
            }
        };

        // Read control server address from env vars
        let control_addr = control_server_addr();
        if let Some((ref host, port)) = control_addr {
            info!(host = %host, port = port, "Control server configured");
        } else {
            debug!("MANTLE_CONTROL_HOST/PORT not set, tool calls will fail");
        }

        Self::new(control_addr, tools)
    }

    /// Run the MCP server on stdio.
    pub fn run(&mut self) -> io::Result<()> {
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        let reader = stdin.lock();

        info!(
            control_addr = ?self.control_addr,
            tool_count = self.tools.len(),
            "MCP server starting on stdio"
        );

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

        // Forward to control server - REQUIRED for decision tools
        let (host, port) = match &self.control_addr {
            Some(addr) => addr,
            None => {
                error!("No control server configured - cannot forward decision tool call");
                return JsonRpcResponse::error(
                    id,
                    -32603,
                    "Internal error: control server not configured".to_string(),
                );
            }
        };

        info!(host = %host, port = port, tool = %call_params.name, "Forwarding tool call to control server");

        match self.forward_tool_call(host, *port, &call_params) {
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

    /// Forward a tool call to the control server via TCP.
    ///
    /// Returns the host's result (if successful) or an MCP error.
    /// The result Value contains the host's response text which may include
    /// termination instructions for decision tools.
    fn forward_tool_call(
        &self,
        host: &str,
        port: u16,
        params: &ToolCallParams,
    ) -> Result<Result<Value, McpError>, String> {
        let mut socket = ControlSocket::connect(host, port)
            .map_err(|e| format!("TCP connect failed: {}", e))?;

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
                if let Some(err) = error {
                    Ok(Err(err))
                } else {
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
                }
            }
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
/// Reads configuration from environment variables:
/// - MANTLE_DECISION_TOOLS_FILE: Path to JSON file with tool definitions
/// - MANTLE_CONTROL_HOST: Host to connect to for forwarding tool calls
/// - MANTLE_CONTROL_PORT: Port to connect to for forwarding tool calls
///
/// Starts immediately without blocking health checks - connection errors
/// surface when tools are actually called. This ensures Claude can see
/// the available tools even during MCP server initialization.
pub fn run_mcp_server() -> Result<(), Box<dyn std::error::Error>> {
    let mut server = McpServer::new_from_env();
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
        let server = McpServer::new(None, vec![]);
        let response = server.handle_initialize(json!("init-1"));
        assert!(response.result.is_some());
        assert!(response.error.is_none());
    }

    #[test]
    fn test_server_handles_tools_list_empty() {
        let server = McpServer::new(None, vec![]);
        let response = server.handle_tools_list(json!("list-1"));
        assert!(response.result.is_some());
        let result = response.result.unwrap();
        assert!(result["tools"].as_array().unwrap().is_empty());
    }

    #[test]
    fn test_server_handles_unknown_method() {
        let mut server = McpServer::new(None, vec![]);
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

        let server = McpServer::new_from_env(None);
        assert_eq!(server.tools.len(), 1);
        assert_eq!(server.tools[0].name, "decision::test");

        // Clean up
        std::env::remove_var("MANTLE_DECISION_TOOLS");
    }
}
