//! Unix socket listener for container communication.

use std::path::Path;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::UnixListener;
use tracing::{debug, error, info, warn};

use crate::db;
use crate::state::AppState;
use crate::types::{HubEvent, SessionResult};

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
async fn handle_connection(
    stream: tokio::net::UnixStream,
    state: AppState,
) -> std::io::Result<()> {
    let (reader, mut writer) = stream.into_split();
    let mut reader = BufReader::new(reader);
    let mut line = String::new();

    // Read single line (newline-delimited JSON)
    let bytes_read = reader.read_line(&mut line).await?;
    if bytes_read == 0 {
        return Ok(());
    }

    debug!(line = %line.trim(), "Received message");

    // Parse as SessionResult
    match serde_json::from_str::<SessionResult>(line.trim()) {
        Ok(result) => {
            let session_id = result.session_id.clone();

            // Store result in database
            match db::insert_result(&state.pool, &result).await {
                Ok(()) => {
                    info!(session_id = %session_id, "Result stored");

                    // Broadcast event
                    state.broadcast(HubEvent::SessionCompleted {
                        session_id: session_id.clone(),
                        result,
                    });

                    // Send success response
                    let response = serde_json::json!({ "status": "ok", "session_id": session_id });
                    writer
                        .write_all(format!("{}\n", response).as_bytes())
                        .await?;
                }
                Err(e) => {
                    warn!(session_id = %session_id, error = %e, "Failed to store result");

                    // Broadcast failure
                    state.broadcast(HubEvent::SessionFailed {
                        session_id: session_id.clone(),
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
