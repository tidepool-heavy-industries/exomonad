//! MCP command implementation.
//!
//! Provides a stdio-based MCP server that forwards requests to the Haskell
//! control server via Unix socket. This allows Gemini CLI and other stdio-only
//! MCP clients to access Tidepool tools.

use crate::error::{MantleError, Result};
use crate::protocol::{ControlMessage, ControlResponse, Role};
use crate::socket::{control_socket_path, ControlSocket};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::io::{BufRead, Write};
use tracing::{debug, error, info, trace};

#[derive(Debug, Deserialize)]
struct JsonRpcRequest {
    jsonrpc: String,
    id: Value,
    method: String,
    params: Option<Value>,
}

#[derive(Debug, Serialize)]
struct JsonRpcResponse {
    jsonrpc: String,
    id: Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<Value>,
}

/// Handle MCP requests from stdio.
pub fn handle_mcp(role: Role) -> Result<()> {
    info!(role = %role, "Starting MCP server (stdio)");

    // Get control server socket path
    let path = control_socket_path()?;
    let mut socket = ControlSocket::connect(&path)?;

    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();

    for line in stdin.lock().lines() {
        let line = line.map_err(MantleError::Io)?;
        if line.trim().is_empty() {
            continue;
        }

        trace!(line = %line, "Received MCP request");

        let request: JsonRpcRequest = match serde_json::from_str(&line) {
            Ok(req) => req,
            Err(e) => {
                error!(error = %e, line = %line, "Failed to parse JSON-RPC request");
                let response = JsonRpcResponse {
                    jsonrpc: "2.0".to_string(),
                    id: Value::Null,
                    result: None,
                    error: Some(json!({
                        "code": -32700,
                        "message": format!("Parse error: {}", e)
                    })),
                };
                send_response(&mut stdout, &response)?;
                continue;
            }
        };

        let response = match request.method.as_str() {
            "initialize" => {
                JsonRpcResponse {
                    jsonrpc: "2.0".to_string(),
                    id: request.id,
                    result: Some(json!({
                        "protocolVersion": "2024-11-05",
                        "capabilities": {
                            "tools": {}
                        },
                        "serverInfo": {
                            "name": "tidepool-mcp",
                            "version": "0.1.0"
                        }
                    })),
                    error: None,
                }
            }
            "tools/list" => {
                let msg = ControlMessage::ToolsListRequest { role: Some(role) };
                match socket.send(&msg) {
                    Ok(ControlResponse::ToolsListResponse { tools }) => {
                        let mcp_tools: Vec<Value> = tools.into_iter().map(|t| json!({ 
                            "name": t.name,
                            "description": t.description,
                            "inputSchema": t.input_schema
                        })).collect();
                        
                        JsonRpcResponse {
                            jsonrpc: "2.0".to_string(),
                            id: request.id,
                            result: Some(json!({
                                "tools": mcp_tools
                            })),
                            error: None,
                        }
                    }
                    Ok(_) => {
                        error!("Unexpected response type from control server for tools/list");
                        JsonRpcResponse {
                            jsonrpc: "2.0".to_string(),
                            id: request.id,
                            result: None,
                            error: Some(json!({
                                "code": -32603,
                                "message": "Internal server error: unexpected response type"
                            })),
                        }
                    }
                    Err(e) => {
                        error!(error = %e, "Failed to communicate with control server");
                        JsonRpcResponse {
                            jsonrpc: "2.0".to_string(),
                            id: request.id,
                            result: None,
                            error: Some(json!({
                                "code": -32603,
                                "message": format!("Control server error: {}", e)
                            })),
                        }
                    }
                }
            }
            "tools/call" => {
                let params = request.params.unwrap_or(Value::Null);
                let tool_name = params["name"].as_str().unwrap_or("").to_string();
                let tool_args = params["arguments"].clone();

                let msg = ControlMessage::McpToolCall {
                    id: request.id.to_string(),
                    tool_name,
                    arguments: tool_args,
                    role: Some(role),
                };

                match socket.send(&msg) {
                    Ok(ControlResponse::McpToolResponse { result, error, .. }) => {
                        if let Some(err) = error {
                            JsonRpcResponse {
                                jsonrpc: "2.0".to_string(),
                                id: request.id,
                                result: None,
                                error: Some(json!({
                                    "code": err.code,
                                    "message": err.message
                                })),
                            }
                        } else {
                            JsonRpcResponse {
                                jsonrpc: "2.0".to_string(),
                                id: request.id,
                                result: Some(json!({
                                    "content": [
                                        {
                                            "type": "text",
                                            "text": serde_json::to_string_pretty(&result.unwrap_or(Value::Null)).unwrap_or_default()
                                        }
                                    ]
                                })),
                                error: None,
                            }
                        }
                    }
                    Ok(_) => {
                        error!("Unexpected response type from control server for tools/call");
                        JsonRpcResponse {
                            jsonrpc: "2.0".to_string(),
                            id: request.id,
                            result: None,
                            error: Some(json!({
                                "code": -32603,
                                "message": "Internal server error: unexpected response type"
                            })),
                        }
                    }
                    Err(e) => {
                        error!(error = %e, "Failed to communicate with control server");
                        JsonRpcResponse {
                            jsonrpc: "2.0".to_string(),
                            id: request.id,
                            result: None,
                            error: Some(json!({
                                "code": -32603,
                                "message": format!("Control server error: {}", e)
                            })),
                        }
                    }
                }
            }
            "notifications/initialized" => {
                continue; // Ignore
            }
            _ => {
                debug!(method = %request.method, "Unknown MCP method");
                JsonRpcResponse {
                    jsonrpc: "2.0".to_string(),
                    id: request.id,
                    result: None,
                    error: Some(json!({
                        "code": -32601,
                        "message": format!("Method not found: {}", request.method)
                    })),
                }
            }
        };

        send_response(&mut stdout, &response)?;
    }

    Ok(())
}

fn send_response<W: Write>(writer: &mut W, response: &JsonRpcResponse) -> Result<()> {
    let json = serde_json::to_string(response).map_err(MantleError::JsonSerialize)?;
    writer.write_all(json.as_bytes()).map_err(MantleError::Io)?;
    writer.write_all(b"\n").map_err(MantleError::Io)?;
    writer.flush().map_err(MantleError::Io)?;
    Ok(())
}
