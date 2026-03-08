//! Stdio-to-UDS MCP proxy.
//!
//! Reads JSON-RPC from stdin, forwards to the exomonad server over Unix domain socket,
//! writes responses to stdout. Used by Claude Code and Gemini CLI as an MCP stdio transport.

use anyhow::{Context, Result};
use std::io::{self, Write};
use tokio::io::{AsyncBufReadExt, BufReader};

use crate::uds_client;

/// Run the stdio MCP proxy.
///
/// Reads JSON-RPC messages line-by-line from stdin, forwards each to the server
/// at `/agents/{role}/{name}/mcp` via UDS, writes responses to stdout.
pub async fn run(role: &str, name: &str) -> Result<()> {
    let socket = uds_client::find_server_socket()
        .context("Cannot start MCP proxy: server socket not found")?;

    let path = format!("/agents/{}/{}/mcp", role, name);
    let mut session_id: Option<String> = None;

    let stdin = tokio::io::stdin();
    let reader = BufReader::new(stdin);
    let mut lines = reader.lines();
    
    let mut stdout = io::stdout().lock();

    while let Ok(Some(line)) = lines.next_line().await {
        if line.trim().is_empty() {
            continue;
        }

        // Parse to check if this is a notification (no "id" field)
        let parsed: serde_json::Value = match serde_json::from_str(&line) {
            Ok(v) => v,
            Err(e) => {
                // Write JSON-RPC parse error to stdout
                let error_resp = serde_json::json!({
                    "jsonrpc": "2.0",
                    "error": {
                        "code": -32700,
                        "message": format!("Parse error: {}", e)
                    },
                    "id": null
                });
                writeln!(stdout, "{}", error_resp)?;
                stdout.flush()?;
                continue;
            }
        };

        let is_notification = parsed.get("id").is_none();

        // Build headers
        let mut headers = vec![
            ("content-type", "application/json"),
            ("accept", "application/json"),
        ];

        // Include session ID if we have one
        // We need to hold the string value in a variable so the borrow lives long enough
        let session_header_value;
        if let Some(ref sid) = session_id {
            session_header_value = sid.clone();
            headers.push(("mcp-session-id", &session_header_value));
        }

        // Forward to server
        match uds_client::uds_post(&socket, &path, headers, line.into_bytes()).await {
            Ok((status, resp_body)) => {
                if is_notification {
                    // Notifications don't produce output
                    continue;
                }

                if (200..300).contains(&status) {
                    // Check if this is an initialize response — extract session ID
                    if let Some(method) = parsed.get("method").and_then(|m| m.as_str()) {
                        if method == "initialize" {
                            // Try to extract session-id from response if present
                            if let Ok(resp_parsed) = serde_json::from_slice::<serde_json::Value>(&resp_body) {
                                if let Some(sid) = resp_parsed.get("_meta")
                                    .and_then(|m| m.get("sessionId"))
                                    .and_then(|s| s.as_str())
                                {
                                    session_id = Some(sid.to_string());
                                }
                            }
                        }
                    }

                    // Write response to stdout
                    stdout.write_all(&resp_body)?;
                    writeln!(stdout)?;
                    stdout.flush()?;
                } else {
                    // Server error — synthesize JSON-RPC error
                    let error_resp = serde_json::json!({
                        "jsonrpc": "2.0",
                        "error": {
                            "code": -32603,
                            "message": format!("Server returned HTTP {}", status)
                        },
                        "id": parsed.get("id").cloned().unwrap_or(serde_json::Value::Null)
                    });
                    writeln!(stdout, "{}", error_resp)?;
                    stdout.flush()?;
                }
            }
            Err(e) => {
                if is_notification {
                    continue;
                }
                // Connection error — synthesize JSON-RPC error
                let error_resp = serde_json::json!({
                    "jsonrpc": "2.0",
                    "error": {
                        "code": -32603,
                        "message": format!("UDS connection error: {}", e)
                    },
                    "id": parsed.get("id").cloned().unwrap_or(serde_json::Value::Null)
                });
                writeln!(stdout, "{}", error_resp)?;
                stdout.flush()?;
            }
        }
    }

    Ok(())
}
