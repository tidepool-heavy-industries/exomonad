//! Unix socket listener for container communication.
//!
//! Containers submit node results via this socket.

use std::path::Path;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::UnixListener;
use tracing::{debug, error, info, warn};

use crate::db;
use crate::state::AppState;
use crate::types::{HubEvent, NodeResult};

/// Listen for connections on Unix socket.
pub async fn listen(socket_path: impl AsRef<Path>, state: AppState) -> std::io::Result<()> {
    let socket_path = socket_path.as_ref();

    // Remove stale socket file
    if socket_path.exists() {
        std::fs::remove_file(socket_path)?;
    }

    let listener = UnixListener::bind(socket_path)?;

    // Set permissive permissions for container access
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(socket_path, std::fs::Permissions::from_mode(0o666))?;
    }

    info!(socket = %socket_path.display(), "Unix socket listener started");

    loop {
        match listener.accept().await {
            Ok((stream, _)) => {
                let state = state.clone();
                tokio::spawn(async move {
                    if let Err(e) = handle_connection(stream, state).await {
                        error!(error = %e, "Connection handler error");
                    }
                });
            }
            Err(e) => {
                error!(error = %e, "Failed to accept connection");
            }
        }
    }
}

/// Handle a single connection.
async fn handle_connection(stream: tokio::net::UnixStream, state: AppState) -> std::io::Result<()> {
    let (reader, mut writer) = stream.into_split();
    let mut reader = BufReader::new(reader);
    let mut line = String::new();

    // Read single line (newline-delimited JSON)
    let bytes_read = reader.read_line(&mut line).await?;
    if bytes_read == 0 {
        return Ok(());
    }

    debug!(line = %line.trim(), "Received message");

    // Parse as NodeResult
    match serde_json::from_str::<NodeResult>(line.trim()) {
        Ok(result) => {
            let node_id = result.node_id.clone();

            // Store result in database
            match db::insert_result(&state.pool, &result).await {
                Ok(()) => {
                    info!(node_id = %node_id, "Result stored");

                    // Broadcast event
                    state.broadcast(HubEvent::NodeCompleted {
                        node_id: node_id.clone(),
                        result,
                    });

                    // Send success response
                    let response = serde_json::json!({ "status": "ok", "node_id": node_id });
                    writer
                        .write_all(format!("{}\n", response).as_bytes())
                        .await?;
                }
                Err(e) => {
                    warn!(node_id = %node_id, error = %e, "Failed to store result");

                    // Broadcast failure
                    state.broadcast(HubEvent::NodeFailed {
                        node_id: node_id.clone(),
                        error: e.to_string(),
                    });

                    // Send error response
                    let response =
                        serde_json::json!({ "status": "error", "message": e.to_string() });
                    writer
                        .write_all(format!("{}\n", response).as_bytes())
                        .await?;
                }
            }
        }
        Err(e) => {
            warn!(error = %e, "Failed to parse message");
            let response = serde_json::json!({ "status": "error", "message": e.to_string() });
            writer
                .write_all(format!("{}\n", response).as_bytes())
                .await?;
        }
    }

    Ok(())
}
