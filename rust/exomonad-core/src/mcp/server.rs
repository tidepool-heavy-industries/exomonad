use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tokio::time::Instant;
use axum::extract::Json;
use axum::http::{HeaderMap, StatusCode};
use axum::response::{IntoResponse, Response};
use serde_json::{json, Value};
use uuid::Uuid;

use super::McpState;
use super::tools::{self, MCPCallInput, MCPCallOutput};

struct McpSession {
    plugin: Arc<crate::PluginManager>,
    role: String,
    /// Cached tool definitions (name → {description, inputSchema})
    tools: Vec<super::ToolDefinition>,
    last_active: Instant,
}

#[derive(Clone)]
pub struct McpServer {
    sessions: Arc<RwLock<HashMap<String, McpSession>>>,
    state: McpState,
}

impl McpServer {
    pub fn new(state: McpState) -> Self {
        let server = Self {
            sessions: Arc::new(RwLock::new(HashMap::new())),
            state,
        };
        // Spawn background sweeper: every 5 min, evict sessions idle > 30 min
        let sessions = server.sessions.clone();
        tokio::spawn(async move {
            let mut interval = tokio::time::interval(std::time::Duration::from_secs(300));
            loop {
                interval.tick().await;
                let mut map = sessions.write().await;
                let cutoff = Instant::now() - std::time::Duration::from_secs(1800);
                map.retain(|_, s| s.last_active > cutoff);
            }
        });
        server
    }

    pub async fn handle(&self, headers: HeaderMap, Json(body): Json<Value>) -> Response {
        let method = body["method"].as_str();
        let id = body.get("id").cloned();

        // Notifications (no id, or id: null) → 202 Accepted, no body
        if id.is_none() || id.as_ref().map(|v| v.is_null()).unwrap_or(false) {
            return StatusCode::ACCEPTED.into_response();
        }

        let id = id.unwrap(); // guaranteed non-null by above check

        match method {
            Some("initialize") => self.handle_initialize(&id).await,
            Some(m) => {
                // All other methods require a valid session
                let session_id = match headers.get("mcp-session-id").and_then(|v| v.to_str().ok()) {
                    Some(sid) => sid.to_string(),
                    None => return json_rpc_error(&id, -32600, "Missing Mcp-Session-Id header"),
                };

                // Touch session (update last_active)
                {
                    let mut sessions = self.sessions.write().await;
                    if let Some(session) = sessions.get_mut(&session_id) {
                        session.last_active = Instant::now();
                    } else {
                        return json_rpc_error(&id, -32600, "Invalid session");
                    }
                }

                match m {
                    "tools/list" => self.handle_list_tools(&session_id, &id).await,
                    "tools/call" => self.handle_call_tool(&session_id, &id, &body["params"]).await,
                    _ => json_rpc_error(&id, -32601, "Method not found"),
                }
            }
            None => json_rpc_error(&id, -32600, "Invalid Request: missing method"),
        }
    }

    async fn handle_initialize(&self, id: &Value) -> Response {
        // Hot reload WASM if changed
        let _ = self.state.plugin.reload_if_changed().await;

        // Discover tools from WASM
        let tools = match tools::get_tool_definitions(&self.state).await {
            Ok(t) => t,
            Err(e) => {
                tracing::error!(error = %e, "Failed to get tool definitions");
                return json_rpc_error(id, -32603, &format!("Tool discovery failed: {}", e));
            }
        };

        let session_id = Uuid::new_v4().to_string();

        let session = McpSession {
            plugin: self.state.plugin.clone(),
            role: self.state.role.clone().unwrap_or_else(|| "tl".to_string()),
            tools,
            last_active: Instant::now(),
        };

        self.sessions.write().await.insert(session_id.clone(), session);

        let result = json!({
            "protocolVersion": "2024-11-05",
            "capabilities": { "tools": {} },
            "serverInfo": {
                "name": "exomonad",
                "version": env!("CARGO_PKG_VERSION")
            },
            "instructions": "ExoMonad MCP server: orchestration tools for LLM agents. Tools include git operations, GitHub API, agent spawning, and inter-agent messaging."
        });

        json_rpc_response(id, &session_id, result)
    }

    async fn handle_list_tools(&self, session_id: &str, id: &Value) -> Response {
        let sessions = self.sessions.read().await;
        let session = sessions.get(session_id).unwrap(); // validated in handle()

        let tools_json: Vec<Value> = session.tools.iter().map(|t| {
            json!({
                "name": t.name,
                "description": t.description,
                "inputSchema": t.input_schema,
            })
        }).collect();

        json_rpc_response(id, session_id, json!({ "tools": tools_json }))
    }

    async fn handle_call_tool(&self, session_id: &str, id: &Value, params: &Value) -> Response {
        let tool_name = match params["name"].as_str() {
            Some(n) => n.to_string(),
            None => return json_rpc_error(id, -32602, "Missing tool name"),
        };
        let args = params.get("arguments").cloned().unwrap_or(json!({}));

        // Read session to get plugin + role (short lock)
        let (plugin, role) = {
            let sessions = self.sessions.read().await;
            let session = sessions.get(session_id).unwrap();
            (session.plugin.clone(), session.role.clone())
        };

        tracing::info!(tool = %tool_name, "Executing tool");

        let input = MCPCallInput::new(role, tool_name.clone(), args);

        let output: MCPCallOutput = match plugin.call("handle_mcp_call", &input).await {
            Ok(o) => o,
            Err(e) => {
                tracing::error!(tool = %tool_name, error = %e, "WASM call failed");
                let result = json!({"content": [{"type": "text", "text": format!("WASM call failed: {}", e)}], "isError": true});
                return json_rpc_response(id, session_id, result);
            }
        };

        let result = output.into_json_rpc_result();
        json_rpc_response(id, session_id, result)
    }
}

/// JSON-RPC success response with Mcp-Session-Id header.
fn json_rpc_response(id: &Value, session_id: &str, result: Value) -> Response {
    let body = json!({
        "jsonrpc": "2.0",
        "id": id,
        "result": result,
    });
    (
        StatusCode::OK,
        [
            ("content-type", "application/json"),
            ("mcp-session-id", session_id),
        ],
        axum::Json(body),
    ).into_response()
}

/// JSON-RPC error response (no session header needed).
fn json_rpc_error(id: &Value, code: i32, message: &str) -> Response {
    let body = json!({
        "jsonrpc": "2.0",
        "id": id,
        "error": { "code": code, "message": message },
    });
    (StatusCode::OK, axum::Json(body)).into_response()
}
