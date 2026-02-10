//! stdio MCP transport.
//!
//! Implements JSON-RPC over stdin/stdout for the MCP protocol.
//! The MCP client spawns this process and communicates via stdio.

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
pub(crate) struct JsonRpcRequest {
    #[allow(dead_code)]
    jsonrpc: String,
    pub(crate) id: Option<Value>,
    pub(crate) method: String,
    #[serde(default)]
    pub(crate) params: Value,
}

#[derive(Debug, Serialize)]
pub(crate) struct JsonRpcResponse {
    jsonrpc: &'static str,
    id: Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<JsonRpcError>,
}

#[derive(Debug, Serialize)]
pub(crate) struct JsonRpcError {
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
// Helpers
// ============================================================================

fn to_json_value_or_internal_error<T: Serialize>(id: Value, data: T) -> JsonRpcResponse {
    match serde_json::to_value(data) {
        Ok(val) => JsonRpcResponse::success(id, val),
        Err(e) => {
            error!("Failed to serialize result: {}", e);
            JsonRpcResponse::error(id, -32603, "Internal serialization error".into())
        }
    }
}

// ============================================================================
// Request Handling
// ============================================================================

pub(crate) async fn handle_request(state: &McpState, request: JsonRpcRequest) -> JsonRpcResponse {
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
                    name: "exomonad".to_string(),
                    version: env!("CARGO_PKG_VERSION").to_string(),
                },
            };
            to_json_value_or_internal_error(id, result)
        }

        "notifications/initialized" => {
            // This is a notification, no response needed
            // But we need to return something for the response flow
            JsonRpcResponse::success(id, json!({}))
        }

        "tools/list" => match tools::get_tool_definitions(state).await {
            Ok(tool_defs) => {
                let mcp_tools: Vec<McpTool> =
                    tool_defs.into_iter().map(McpTool::from).collect();

                let result = McpToolsListResult { tools: mcp_tools };
                to_json_value_or_internal_error(id, result)
            }
            Err(e) => {
                error!(error = %e, "Failed to get tool definitions");
                JsonRpcResponse::error(id, -32603, format!("Failed to get tool definitions: {}", e))
            }
        },

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

            let tool_result = tools::execute_tool(state, tool_name, arguments).await;

            match tool_result {
                Ok(result) => {
                    let text = serde_json::to_string_pretty(&result).unwrap_or_default();
                    let mcp_result = McpToolCallResult {
                        content: vec![McpContent {
                            content_type: "text".to_string(),
                            text,
                        }],
                        is_error: None,
                    };
                    to_json_value_or_internal_error(id, mcp_result)
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
                    to_json_value_or_internal_error(id, mcp_result)
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
                let truncated = if line.len() > 1000 {
                    format!("{}...", &line[..1000])
                } else {
                    line.clone()
                };
                error!(error = %e, request = %truncated, "Failed to parse JSON-RPC request");
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

#[cfg(test)]
mod tests {
    use super::*;

    // === JSON-RPC parsing tests ===

    #[test]
    fn test_jsonrpc_parse_valid() {
        let line = r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}"#;
        let request: JsonRpcRequest = serde_json::from_str(line).unwrap();
        assert_eq!(request.method, "initialize");
        assert_eq!(request.id, Some(Value::Number(1.into())));
    }

    #[test]
    fn test_jsonrpc_parse_with_string_id() {
        let line = r#"{"jsonrpc":"2.0","id":"abc-123","method":"tools/list"}"#;
        let request: JsonRpcRequest = serde_json::from_str(line).unwrap();
        assert_eq!(request.method, "tools/list");
        assert_eq!(request.id, Some(Value::String("abc-123".to_string())));
    }

    #[test]
    fn test_jsonrpc_parse_notification_no_id() {
        let line = r#"{"jsonrpc":"2.0","method":"notifications/initialized"}"#;
        let request: JsonRpcRequest = serde_json::from_str(line).unwrap();
        assert_eq!(request.method, "notifications/initialized");
        assert!(request.id.is_none());
    }

    #[test]
    fn test_jsonrpc_parse_with_params() {
        let line = r#"{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"git_branch","arguments":{}}}"#;
        let request: JsonRpcRequest = serde_json::from_str(line).unwrap();
        assert_eq!(request.method, "tools/call");
        assert_eq!(request.params["name"], "git_branch");
    }

    #[test]
    fn test_jsonrpc_parse_invalid() {
        let line = "not valid json";
        let result: Result<JsonRpcRequest, _> = serde_json::from_str(line);
        assert!(result.is_err());
    }

    #[test]
    fn test_jsonrpc_parse_missing_method() {
        let line = r#"{"jsonrpc":"2.0","id":1}"#;
        let result: Result<JsonRpcRequest, _> = serde_json::from_str(line);
        assert!(result.is_err());
    }

    // === JSON-RPC response tests ===

    #[test]
    fn test_jsonrpc_response_success() {
        let response = JsonRpcResponse::success(Value::Number(42.into()), json!({"status": "ok"}));
        assert_eq!(response.jsonrpc, "2.0");
        assert_eq!(response.id, Value::Number(42.into()));
        assert!(response.result.is_some());
        assert!(response.error.is_none());
    }

    #[test]
    fn test_jsonrpc_response_error() {
        let response =
            JsonRpcResponse::error(Value::Number(1.into()), -32700, "Parse error".to_string());
        assert_eq!(response.jsonrpc, "2.0");
        assert_eq!(response.id, Value::Number(1.into()));
        assert!(response.result.is_none());
        assert!(response.error.is_some());
        let err = response.error.unwrap();
        assert_eq!(err.code, -32700);
        assert_eq!(err.message, "Parse error");
    }

    // === MCP type serialization tests ===

    #[test]
    fn test_mcp_initialize_result_serialization() {
        let result = McpInitializeResult {
            protocol_version: "2024-11-05".to_string(),
            capabilities: McpCapabilities {
                tools: McpToolsCapability {
                    list_changed: false,
                },
            },
            server_info: McpServerInfo {
                name: "exomonad".to_string(),
                version: "0.1.0".to_string(),
            },
        };

        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"protocolVersion\":\"2024-11-05\""));
        assert!(json.contains("\"serverInfo\""));
        assert!(json.contains("\"capabilities\""));
    }

    #[test]
    fn test_mcp_tool_from_definition() {
        let def = ToolDefinition {
            name: "test_tool".to_string(),
            description: "A test tool".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {}
            }),
        };

        let mcp_tool: McpTool = def.into();
        assert_eq!(mcp_tool.name, "test_tool");
        assert_eq!(mcp_tool.description, "A test tool");
    }

    #[test]
    fn test_mcp_tools_list_result_serialization() {
        let result = McpToolsListResult {
            tools: vec![
                McpTool {
                    name: "tool1".to_string(),
                    description: "First tool".to_string(),
                    input_schema: json!({}),
                },
                McpTool {
                    name: "tool2".to_string(),
                    description: "Second tool".to_string(),
                    input_schema: json!({}),
                },
            ],
        };

        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("tool1"));
        assert!(json.contains("tool2"));
        assert!(json.contains("inputSchema"));
    }

    #[test]
    fn test_mcp_tool_call_result_success() {
        let result = McpToolCallResult {
            content: vec![McpContent {
                content_type: "text".to_string(),
                text: "Success!".to_string(),
            }],
            is_error: None,
        };

        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"type\":\"text\""));
        assert!(json.contains("Success!"));
        assert!(!json.contains("isError")); // Should be skipped when None
    }

    #[test]
    fn test_mcp_tool_call_result_error() {
        let result = McpToolCallResult {
            content: vec![McpContent {
                content_type: "text".to_string(),
                text: "Error occurred".to_string(),
            }],
            is_error: Some(true),
        };

        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"isError\":true"));
    }

    // === Internal helper tests ===

    #[test]
    fn test_to_json_value_or_internal_error_success() {
        let data = json!({"key": "value"});
        let response = to_json_value_or_internal_error(Value::Number(1.into()), data);
        assert!(response.result.is_some());
        assert!(response.error.is_none());
    }
}
