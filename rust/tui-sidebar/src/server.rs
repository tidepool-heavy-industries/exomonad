use anyhow::{Context, Result};
use std::fs;
use std::path::Path;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::{UnixListener, UnixStream};
use tokio::sync::mpsc;
use tracing::{debug, error, warn};

use crate::protocol::{Interaction, UISpec};

/// Connect to control-server via Unix socket.
///
/// Connects to the socket path and returns the stream.
pub async fn connect_to_control_server(path: &Path) -> Result<UnixStream> {
    debug!(path = %path.display(), "TUI sidebar connecting to control-server");

    let stream = UnixStream::connect(path)
        .await
        .context(format!("Failed to connect to {}", path.display()))?;

    debug!("Connected to control-server via Unix socket");

    Ok(stream)
}

/// Spawn I/O tasks for reading UISpec and writing Interaction.
///
/// Returns (rx, tx):
/// - rx: Receiver for UISpec messages from Haskell
/// - tx: Sender for Interaction events to Haskell
///
/// The I/O tasks run in the background and handle NDJSON framing:
/// - Reader: UnixStream → BufReader → read_line → parse JSON → send to rx
/// - Writer: receive from tx → serialize JSON → writeln → UnixStream
pub fn spawn_io_tasks(
    stream: UnixStream,
) -> (mpsc::Receiver<UISpec>, mpsc::Sender<Interaction>) {
    let (read_half, write_half) = stream.into_split();

    let (msg_tx, msg_rx) = mpsc::channel::<UISpec>(32);
    let (int_tx, mut int_rx) = mpsc::channel::<Interaction>(32);

    // Reader task: UnixStream → UISpec
    tokio::spawn(async move {
        let mut reader = BufReader::new(read_half);
        let mut line = String::new();

        loop {
            line.clear();

            match reader.read_line(&mut line).await {
                Ok(0) => {
                    debug!("Connection closed by peer");
                    break;
                }
                Ok(_) => {
                    let trimmed = line.trim();
                    if trimmed.is_empty() {
                        continue;
                    }

                    match serde_json::from_str::<UISpec>(trimmed) {
                        Ok(spec) => {
                            debug!(ui_id = %spec.id, "Received UISpec");
                            if msg_tx.send(spec).await.is_err() {
                                debug!("Receiver dropped, stopping reader");
                                break;
                            }
                        }
                        Err(e) => {
                            warn!(error = %e, json = %trimmed, "Failed to parse UISpec");
                            // Continue reading, don't stop on malformed JSON
                        }
                    }
                }
                Err(e) => {
                    error!(error = %e, "Failed to read from Unix stream");
                    break;
                }
            }
        }

        debug!("Reader task exiting");
    });

    // Writer task: Interaction → UnixStream
    tokio::spawn(async move {
        let mut writer = write_half;

        while let Some(interaction) = int_rx.recv().await {
            match serde_json::to_string(&interaction) {
                Ok(json) => {
                    debug!(interaction = ?interaction, "Sending interaction");

                    if let Err(e) = writer.write_all(json.as_bytes()).await {
                        error!(error = %e, "Failed to write JSON");
                        break;
                    }

                    if let Err(e) = writer.write_all(b"\n").await {
                        error!(error = %e, "Failed to write newline");
                        break;
                    }

                    if let Err(e) = writer.flush().await {
                        error!(error = %e, "Failed to flush");
                        break;
                    }
                }
                Err(e) => {
                    error!(error = %e, "Failed to serialize interaction");
                    // Continue, don't stop on serialization errors
                }
            }
        }

        debug!("Writer task exiting");
    });

    (msg_rx, int_tx)
}

/// Start a simple Unix listener for health checks.
///
/// Listens on the given path and accepts connections, immediately closing them.
/// This allows 'nc -zU' to verify that the process is alive and listening.
pub async fn start_health_listener(path: &Path) -> Result<tokio::task::JoinHandle<()>> {
    // Ensure parent directory exists
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .context(format!("Failed to create health socket directory {}", parent.display()))?;
    }

    // Remove existing socket file if it exists
    if path.exists() {
        fs::remove_file(path).context("Failed to remove existing health socket file")?;
    }

    let listener = UnixListener::bind(path)
        .context(format!("Failed to bind health socket to {}", path.display()))?;

    debug!(path = %path.display(), "TUI sidebar health listener started");

    let handle = tokio::spawn(async move {
        // Track consecutive accept errors to avoid retrying indefinitely on a persistent failure.
        const MAX_CONSECUTIVE_ERRORS: u32 = 10;
        let mut consecutive_errors: u32 = 0;

        loop {
            match listener.accept().await {
                Ok((_stream, _addr)) => {
                    // Successful accept: reset error counter.
                    consecutive_errors = 0;
                    // Just accept and drop, which is enough for nc -zU
                }
                Err(e) => {
                    consecutive_errors = consecutive_errors.saturating_add(1);
                    error!(error = %e, consecutive_errors, "Health listener accept error");
                    // Wait before retrying to avoid tight loop on persistent errors
                    tokio::time::sleep(std::time::Duration::from_millis(500)).await;

                    if consecutive_errors >= MAX_CONSECUTIVE_ERRORS {
                        error!(
                            consecutive_errors,
                            "Health listener stopping after too many consecutive accept errors"
                        );
                        break;
                    }
                }
            }
        }
    });

    Ok(handle)
}
