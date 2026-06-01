//! **N1 — Outbound.** Serve the node's `exo-policy` tools over rmcp/stdio, and route
//! `send_message`/`notify_parent` through `Bus::deliver` (append to the *target's* ingestion
//! inbox — runtime-agnostic; policy never names Teams or tmux).
//!
//! Refactors the `teams-mcp` outbound server (`rust/teams-mcp/src/main.rs`): instead of
//! writing CC Teams inboxes directly, it exposes `role_def::<Runtime>(kind).tools` and the
//! tools' `Bus::deliver` writes the **ingestion** inbox. The rmcp `tools/list` →
//! `Tool::schema`, `tools/call` → `Tool::call(&*ctx.runtime, args)`.
//!
//! **Status: stub (N1 leaf fills this).** Acceptance: `tools/list` returns the role's tool
//! schemas; `tools/call` dispatches to `Tool::call` against the real `Runtime`; a
//! `notify_parent` call appends one `IngestionEntry` to `ctx.parent_inbox`.

use std::sync::Arc;

use exo_policy::role_def;
use exo_policy::tool::Tool;
use exo_runtime::Runtime;
use serde_json::{json, Value};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};

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
            Err(_) => continue,
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
    let id = msg.get("id").cloned();
    let method = msg.get("method").and_then(|m| m.as_str()).unwrap_or("");
    let is_notification = id.is_none() || id.as_ref().map(|v| v.is_null()).unwrap_or(false);

    let result: Option<Result<Value, (i32, String)>> = match method {
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
                        "inputSchema": t.schema(),
                    })
                })
                .collect();
            Some(Ok(json!({ "tools": tool_list })))
        }
        "tools/call" => {
            let params = msg.get("params");
            let name = params.and_then(|p| p.get("name")).and_then(|n| n.as_str());
            let arguments = params
                .and_then(|p| p.get("arguments"))
                .cloned()
                .unwrap_or(json!({}));

            if let Some(name) = name {
                if let Some(tool) = tools.iter().find(|t| t.name() == name) {
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

                            Some(Ok(json!({ "content": content })))
                        }
                        Err(e) => Some(Err((-32603, e.to_string()))),
                    }
                } else {
                    Some(Err((-32601, format!("Tool not found: {}", name))))
                }
            } else {
                Some(Err((-32602, "Missing tool name".to_string())))
            }
        }
        _ => {
            if is_notification {
                None
            } else {
                Some(Err((-32601, format!("Method not found: {}", method))))
            }
        }
    };

    if let (Some(res), Some(id)) = (result, id) {
        match res {
            Ok(val) => Some(json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": val
            })),
            Err((code, message)) => Some(json!({
                "jsonrpc": "2.0",
                "id": id,
                "error": { "code": code, "message": message }
            })),
        }
    } else {
        None
    }
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
        assert!(names.contains(&"file_pr"));
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
}
