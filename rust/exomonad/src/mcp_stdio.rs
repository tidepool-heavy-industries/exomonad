//! MCP stdio translation layer.
//!
//! Speaks JSON-RPC (MCP protocol) on stdin/stdout, translates to
//! domain-typed REST calls against the UDS server.

use crate::uds_client::{self, ServerClient, ToolCallRequest};
use anyhow::{Context, Result};
use serde_json::{json, Value};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};

/// Run the stdio MCP translation layer.
///
/// Reads JSON-RPC from stdin, translates to REST calls against the server,
/// writes JSON-RPC responses to stdout.
pub async fn run(role: &str, name: &str) -> Result<()> {
    // Retry socket discovery — server may still be starting (race with exomonad init)
    let socket = {
        let mut attempts = 0;
        loop {
            match uds_client::find_server_socket() {
                Ok(s) => break s,
                Err(_) => {
                    attempts += 1;
                    if attempts >= 30 {
                        anyhow::bail!(
                            "Server socket not found after 15s. Is exomonad serve running?"
                        );
                    }
                    tokio::time::sleep(std::time::Duration::from_millis(500)).await;
                }
            }
        }
    };
    let client = ServerClient::new(socket);

    let stdin = tokio::io::stdin();
    let mut reader = BufReader::new(stdin).lines();
    let mut stdout = tokio::io::stdout();

    while let Some(line) = reader.next_line().await? {
        if line.trim().is_empty() {
            continue;
        }

        let msg: Value = match serde_json::from_str(&line) {
            Ok(v) => v,
            Err(e) => {
                tracing::warn!("Invalid JSON from stdin: {}", e);
                continue;
            }
        };

        let id = msg.get("id").cloned();
        let method = msg.get("method").and_then(|m| m.as_str()).unwrap_or("");

        // Notifications (no id) — don't send a response
        let is_notification = id.is_none() || id.as_ref().map(|v| v.is_null()).unwrap_or(false);

        let result: Option<Result<Value>> = match method {
            "initialize" => Some(Ok(json!({
                "protocolVersion": "2024-11-05",
                "capabilities": { "tools": { "listChanged": false } },
                "serverInfo": { "name": "exomonad", "version": env!("CARGO_PKG_VERSION") }
            }))),

            "notifications/initialized" | "notifications/cancelled" => {
                // Notifications — no response
                None
            }

            "tools/list" => Some(
                client
                    .list_tools(role, name)
                    .await
                    .map(|tools| json!({ "tools": tools })),
            ),

            "tools/call" => {
                let params = msg.get("params").cloned().unwrap_or(json!({}));
                let req = ToolCallRequest {
                    name: params["name"].as_str().unwrap_or("").to_string(),
                    arguments: params.get("arguments").cloned().unwrap_or(json!({})),
                };
                Some(
                    client
                        .call_tool(role, name, &req)
                        .await
                        .map(|output| output.to_mcp_result()),
                )
            }

            _ if is_notification => None,

            other => Some(Err(anyhow::anyhow!("Unknown method: {}", other))),
        };

        if let (Some(result), Some(id)) = (result, id) {
            let response = match result {
                Ok(result) => json!({ "jsonrpc": "2.0", "id": id, "result": result }),
                Err(e) => json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "error": { "code": -32603, "message": e.to_string() }
                }),
            };
            stdout.write_all(&serde_json::to_vec(&response)?).await?;
            stdout.write_all(b"\n").await?;
            stdout.flush().await?;
        }
    }

    Ok(())
}
