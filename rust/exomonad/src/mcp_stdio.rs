use crate::uds_client;
use anyhow::{Context, Result};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};

/// Run the stdio-to-UDS MCP proxy.
///
/// Reads JSON-RPC messages from stdin line-by-line, forwards them to the UDS server,
/// and writes responses back to stdout.
pub async fn run(role: &str, name: &str) -> Result<()> {
    let socket = uds_client::find_server_socket().context("Failed to find server socket")?;
    let path = format!("/agents/{}/{}/mcp", role, name);

    let stdin = tokio::io::stdin();
    let mut reader = BufReader::new(stdin).lines();
    let mut stdout = tokio::io::stdout();

    while let Some(line) = reader.next_line().await? {
        if line.trim().is_empty() {
            continue;
        }

        // Parse to check if it's a request (has "id") or notification (no "id")
        let msg: serde_json::Value = match serde_json::from_str(&line) {
            Ok(v) => v,
            Err(e) => {
                tracing::warn!("Failed to parse JSON-RPC message from stdin: {}", e);
                continue;
            }
        };

        let is_request = msg.get("id").is_some();

        match uds_client::uds_post(
            &socket,
            &path,
            vec![("content-type", "application/json")],
            line.into_bytes(),
        )
        .await
        {
            Ok((status, resp_body)) => {
                if is_request {
                    if (200..300).contains(&status) {
                        stdout.write_all(&resp_body).await?;
                        stdout.write_all(b"
").await?;
                        stdout.flush().await?;
                    } else {
                        tracing::warn!("Server returned status {} for MCP request", status);
                    }
                }
            }
            Err(e) => {
                tracing::error!("Failed to forward MCP message to server: {}", e);
            }
        }
    }

    Ok(())
}
