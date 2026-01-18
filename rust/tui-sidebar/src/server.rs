use anyhow::{Context, Result};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::mpsc;
use tracing::{debug, error, warn};

use crate::protocol::{Interaction, UISpec};

/// Start TCP server and wait for connection.
///
/// Binds to port 7433 and blocks until a client connects (Haskell tui-interpreter).
pub async fn wait_for_connection(port: u16) -> Result<TcpStream> {
    let addr = format!("127.0.0.1:{}", port);
    let listener = TcpListener::bind(&addr)
        .await
        .context(format!("Failed to bind to {}", addr))?;

    debug!(addr = %addr, "TUI sidebar listening");

    // Accept first connection
    let (stream, peer_addr) = listener.accept().await.context("Failed to accept connection")?;

    debug!(peer = %peer_addr, "Client connected");

    Ok(stream)
}

/// Spawn I/O tasks for reading UISpec and writing Interaction.
///
/// Returns (rx, tx):
/// - rx: Receiver for UISpec messages from Haskell
/// - tx: Sender for Interaction events to Haskell
///
/// The I/O tasks run in the background and handle NDJSON framing:
/// - Reader: TCP → BufReader → read_line → parse JSON → send to rx
/// - Writer: receive from tx → serialize JSON → writeln → TCP
pub fn spawn_io_tasks(
    stream: TcpStream,
) -> (mpsc::Receiver<UISpec>, mpsc::Sender<Interaction>) {
    let (read_half, write_half) = stream.into_split();

    let (msg_tx, msg_rx) = mpsc::channel::<UISpec>(32);
    let (int_tx, mut int_rx) = mpsc::channel::<Interaction>(32);

    // Reader task: TCP → UISpec
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
                    error!(error = %e, "Failed to read from TCP");
                    break;
                }
            }
        }

        debug!("Reader task exiting");
    });

    // Writer task: Interaction → TCP
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
