//! **N1 — Outbound.** Serves the node's `exo-policy` tools over rmcp/stdio and routes
//! communication through the node's ingestion system. This module implements the rmcp
//! stdio server that Claude Code or other agents connect to for tool execution.
//!
//! It exposes tools defined in `exo_policy::role_def` for the node's specific role.
//! Tools like `send_message` and `notify_parent` are routed through the `Bus::deliver`
//! mechanism, which appends `IngestionEntry` objects to target inboxes (e.g., the
//! parent's ingestion inbox), maintaining runtime-agnostic communication where the
//! policy layer doesn't need to know about Teams or tmux.

use std::sync::Arc;

use exo_policy::role_def;
use exo_policy::tool::Tool;
use exo_runtime::Runtime;
use serde_json::{json, Value};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tracing::warn;

use crate::bootstrap::NodeContext;
use crate::error::NodeResult;

/// Serve the policy toolset over rmcp/stdio until the stream closes.
pub async fn serve(ctx: Arc<NodeContext>) -> NodeResult<()> {
    let tools = role_def::<Runtime>(ctx.kind).tools;
    let stdin = tokio::io::stdin();
    let mut reader = BufReader::new(stdin).lines();
    let mut stdout = tokio::io::stdout();

    while let Ok(Some(line)) = reader.next_line().await {
        if line.trim().is_empty() {
            continue;
        }

        let msg: Value = match serde_json::from_str(&line) {
            Ok(v) => v,
            // Malformed JSON-RPC: the body won't parse, so there's no recoverable `id` to reply
            // to (a JSON-RPC parse-error reply would carry `id: null`). Log rather than drop silently.
            Err(e) => {
                warn!("dropping unparseable JSON-RPC line: {e}");
                continue;
            }
        };

        if let Some(response) = handle_rpc(&tools, &ctx.runtime, msg).await {
            let mut bytes = serde_json::to_vec(&response).map_err(std::io::Error::other)?;
            bytes.push(b'\n');
            stdout.write_all(&bytes).await?;
            stdout.flush().await?;
        }
    }

    Ok(())
}

async fn handle_rpc(
    tools: &[Box<dyn Tool<Runtime>>],
    runtime: &Runtime,
    msg: Value,
) -> Option<Value> {
    let req = Request::parse(&msg);
    let id = req.id.clone();
    let is_notification = id.is_none() || id.as_ref().map(|v| v.is_null()).unwrap_or(false);

    let result: Option<Result<Value, (i32, String)>> = match req.method {
        "initialize" => Some(Ok(json!({
            "protocolVersion": "2024-11-05",
            "capabilities": { "tools": {} },
            "serverInfo": { "name": "exomonad-node", "version": env!("CARGO_PKG_VERSION") }
        }))),
        "notifications/initialized" => None,
        "tools/list" => {
            let tool_list: Vec<_> = tools
                .iter()
                .map(|t| {
                    json!({
                        "name": t.name(),
                        "description": t.description(),
                        "inputSchema": t.schema(),
                    })
                })
                .collect();
            Some(Ok(json!({ "tools": tool_list })))
        }
        "tools/call" => Some(call_tool(tools, runtime, req.params).await),
        _ => {
            if is_notification {
                None
            } else {
                Some(Err((-32601, format!("Method not found: {}", req.method))))
            }
        }
    };

    if let (Some(res), Some(id)) = (result, id) {
        match res {
            Ok(val) => Some(ok_response(id, val)),
            Err((code, message)) => Some(error_response(id, code, message)),
        }
    } else {
        None
    }
}

async fn call_tool(
    tools: &[Box<dyn Tool<Runtime>>],
    runtime: &Runtime,
    params: Option<&Value>,
) -> Result<Value, (i32, String)> {
    let name = params.and_then(|p| p.get("name")).and_then(|n| n.as_str());
    let arguments = params
        .and_then(|p| p.get("arguments"))
        .cloned()
        .unwrap_or(json!({}));

    let Some(name) = name else {
        return Err((-32602, "Missing tool name".to_string()));
    };

    let Some(tool) = tools.iter().find(|t| t.name() == name) else {
        return Err((-32601, format!("Tool not found: {name}")));
    };

    match tool.call(runtime, arguments).await {
        Ok(output) => {
            // Map ToolOutput (text, data) to MCP CallToolResult
            let mut content = vec![json!({
                "type": "text",
                "text": output.get("text").and_then(|t| t.as_str()).unwrap_or(""),
            })];

            if let Some(data) = output.get("data") {
                if !data.is_null() {
                    content.push(json!({
                        "type": "text",
                        "text": format!("Data: {}", serde_json::to_string_pretty(data).unwrap_or_default()),
                    }));
                }
            }

            Ok(json!({ "content": content }))
        }
        Err(e) => Err((-32603, e.to_string())),
    }
}

struct Request<'a> {
    id: Option<Value>,
    method: &'a str,
    params: Option<&'a Value>,
}

impl<'a> Request<'a> {
    fn parse(msg: &'a Value) -> Self {
        Self {
            id: msg.get("id").cloned(),
            method: msg.get("method").and_then(|m| m.as_str()).unwrap_or(""),
            params: msg.get("params"),
        }
    }
}

fn ok_response(id: Value, result: Value) -> Value {
    json!({
        "jsonrpc": "2.0",
        "id": id,
        "result": result
    })
}

fn error_response(id: Value, code: i32, message: String) -> Value {
    json!({
        "jsonrpc": "2.0",
        "id": id,
        "error": { "code": code, "message": message }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_caps::{AgentName, Branch, NodeKind, NodePath, PaneId};

    fn test_runtime(temp_path: std::path::PathBuf) -> Runtime {
        Runtime::new(
            NodePath::new(vec![AgentName::new("root".into()).unwrap()]).unwrap(),
            Branch::new("main".into()).unwrap(),
            temp_path,
            None,
            "test-run".into(),
            "test-session".into(),
            PaneId::new("%1".into()).unwrap(),
        )
    }

    #[tokio::test]
    async fn test_handle_rpc_tools_list() {
        // Build a minimal Runtime for testing
        let temp = tempfile::tempdir().unwrap();
        let runtime = test_runtime(temp.path().to_path_buf());
        let tools = role_def::<Runtime>(NodeKind::Dev).tools;

        let request = json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "tools/list"
        });

        let response = handle_rpc(&tools, &runtime, request).await.unwrap();
        assert_eq!(response["id"], 1);
        let tools_out = response["result"]["tools"].as_array().unwrap();
        let names: Vec<_> = tools_out
            .iter()
            .map(|t| t["name"].as_str().unwrap())
            .collect();
        assert!(names.contains(&"submit_branch"));
        assert!(names.contains(&"notify_parent"));
    }

    #[tokio::test]
    async fn test_handle_rpc_initialize() {
        let temp = tempfile::tempdir().unwrap();
        let runtime = test_runtime(temp.path().to_path_buf());
        let tools = vec![];

        let request = json!({
            "jsonrpc": "2.0",
            "id": "init-1",
            "method": "initialize"
        });

        let response = handle_rpc(&tools, &runtime, request).await.unwrap();
        assert_eq!(response["id"], "init-1");
        assert_eq!(response["result"]["serverInfo"]["name"], "exomonad-node");
    }

    #[tokio::test]
    async fn test_handle_rpc_tools_call_happy() {
        use exo_caps::CapResult;
        let temp = tempfile::tempdir().unwrap();
        let runtime = test_runtime(temp.path().to_path_buf());

        // Create a dummy tool that returns a predictable result
        struct TestTool;
        #[async_trait::async_trait]
        impl Tool<Runtime> for TestTool {
            fn name(&self) -> &str {
                "test_tool"
            }
            fn description(&self) -> &str {
                "desc"
            }
            fn schema(&self) -> Value {
                json!({})
            }
            async fn call(&self, _runtime: &Runtime, _args: Value) -> CapResult<Value> {
                Ok(json!({ "text": "hello world", "data": { "foo": "bar" } }))
            }
        }

        let tools: Vec<Box<dyn Tool<Runtime>>> = vec![Box::new(TestTool)];

        let request = json!({
            "jsonrpc": "2.0",
            "id": "call-1",
            "method": "tools/call",
            "params": {
                "name": "test_tool",
                "arguments": {}
            }
        });

        let response = handle_rpc(&tools, &runtime, request).await.unwrap();
        assert_eq!(response["id"], "call-1");
        let content = response["result"]["content"].as_array().unwrap();
        assert_eq!(content[0]["text"], "hello world");
        assert!(content[1]["text"]
            .as_str()
            .unwrap()
            .contains("\"foo\": \"bar\""));
    }

    #[tokio::test]
    async fn test_handle_rpc_unknown_method() {
        let temp = tempfile::tempdir().unwrap();
        let runtime = test_runtime(temp.path().to_path_buf());
        let tools = vec![];

        let request = json!({
            "jsonrpc": "2.0",
            "id": "err-1",
            "method": "unknown/method"
        });

        let response = handle_rpc(&tools, &runtime, request).await.unwrap();
        assert_eq!(response["id"], "err-1");
        assert_eq!(response["error"]["code"], -32601);
        assert!(response["error"]["message"]
            .as_str()
            .unwrap()
            .contains("Method not found"));
    }
}
