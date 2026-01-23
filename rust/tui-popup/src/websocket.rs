//! WebSocket communication with control-server.
//!
//! Connects to control-server's /tui/ws endpoint over Unix socket,
//! receives PopupDefinition, and sends back PopupResult.

use anyhow::{Context, Result};
use futures_util::{SinkExt, StreamExt};
use serde::{Deserialize, Serialize};
use tokio::net::UnixStream;
use tokio_tungstenite::tungstenite::Message;
use tui_sidebar::protocol::{PopupDefinition, PopupResult};
use uuid::Uuid;

/// WebSocket request from control-server containing PopupDefinition.
#[derive(Debug, Deserialize)]
struct PopupRequest {
    #[serde(rename = "type")]
    request_type: String,
    request_id: Uuid,
    definition: PopupDefinition,
}

/// WebSocket response to control-server containing PopupResult.
#[derive(Debug, Serialize)]
struct PopupResponse {
    request_id: Uuid,
    result: PopupResult,
}

/// Connect to control-server WebSocket and receive PopupDefinition.
pub async fn receive_popup_definition(socket_path: &str) -> Result<(Uuid, PopupDefinition)> {
    // Connect to Unix socket
    let unix_stream = UnixStream::connect(socket_path)
        .await
        .context("Failed to connect to control socket")?;

    // Upgrade to WebSocket
    let request = http::Request::builder()
        .uri("/tui/ws")
        .header("Host", "localhost")
        .header("Connection", "Upgrade")
        .header("Upgrade", "websocket")
        .header("Sec-WebSocket-Version", "13")
        .header("Sec-WebSocket-Key", generate_key())
        .body(())
        .context("Failed to build WebSocket request")?;

    let (mut ws_stream, _) = tokio_tungstenite::client_async(request, unix_stream)
        .await
        .context("WebSocket handshake failed")?;

    // Wait for PopupRequest from server
    while let Some(msg) = ws_stream.next().await {
        let msg = msg.context("WebSocket error while receiving")?;

        if let Message::Text(text) = msg {
            let request: PopupRequest = serde_json::from_str(&text)
                .context("Failed to parse PopupRequest")?;

            if request.request_type == "request" {
                return Ok((request.request_id, request.definition));
            }
        }
    }

    anyhow::bail!("WebSocket closed before receiving PopupDefinition")
}

/// Send PopupResult back to control-server via WebSocket.
pub async fn send_popup_result(
    socket_path: &str,
    request_id: Uuid,
    result: PopupResult,
) -> Result<()> {
    // Connect to Unix socket
    let unix_stream = UnixStream::connect(socket_path)
        .await
        .context("Failed to connect to control socket")?;

    // Upgrade to WebSocket
    let request = http::Request::builder()
        .uri("/tui/ws")
        .header("Host", "localhost")
        .header("Connection", "Upgrade")
        .header("Upgrade", "websocket")
        .header("Sec-WebSocket-Version", "13")
        .header("Sec-WebSocket-Key", generate_key())
        .body(())
        .context("Failed to build WebSocket request")?;

    let (mut ws_stream, _) = tokio_tungstenite::client_async(request, unix_stream)
        .await
        .context("WebSocket handshake failed")?;

    // Send PopupResponse
    let response = PopupResponse { request_id, result };
    let json = serde_json::to_string(&response)?;

    ws_stream
        .send(Message::Text(json))
        .await
        .context("Failed to send PopupResponse")?;

    // Close connection gracefully
    ws_stream.close(None).await.ok();

    Ok(())
}

/// Generate a random WebSocket key for handshake.
fn generate_key() -> String {
    use base64::Engine;
    let random_bytes: [u8; 16] = rand::random();
    base64::engine::general_purpose::STANDARD.encode(random_bytes)
}
