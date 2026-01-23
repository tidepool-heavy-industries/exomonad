use anyhow::{Context, Result};
use std::fs;
use std::path::Path;
use std::pin::Pin;
use std::task::{Context as TaskContext, Poll};
use tokio::io::{AsyncBufReadExt, AsyncRead, AsyncWrite, AsyncWriteExt, BufReader, ReadBuf};
use tokio::net::{TcpStream, UnixListener, UnixStream};
use tokio::sync::mpsc;
use tracing::{debug, error, warn};

use crate::protocol::{PopupDefinition, PopupResult};

/// Unified stream type for TUI communication (Unix or TCP).
pub enum TuiStream {
    Unix(UnixStream),
    Tcp(TcpStream),
}

impl AsyncRead for TuiStream {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut TaskContext<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        match self.get_mut() {
            TuiStream::Unix(s) => Pin::new(s).poll_read(cx, buf),
            TuiStream::Tcp(s) => Pin::new(s).poll_read(cx, buf),
        }
    }
}

impl AsyncWrite for TuiStream {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut TaskContext<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        match self.get_mut() {
            TuiStream::Unix(s) => Pin::new(s).poll_write(cx, buf),
            TuiStream::Tcp(s) => Pin::new(s).poll_write(cx, buf),
        }
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut TaskContext<'_>) -> Poll<std::io::Result<()>> {
        match self.get_mut() {
            TuiStream::Unix(s) => Pin::new(s).poll_flush(cx),
            TuiStream::Tcp(s) => Pin::new(s).poll_flush(cx),
        }
    }

    fn poll_shutdown(self: Pin<&mut Self>, cx: &mut TaskContext<'_>) -> Poll<std::io::Result<()>> {
        match self.get_mut() {
            TuiStream::Unix(s) => Pin::new(s).poll_shutdown(cx),
            TuiStream::Tcp(s) => Pin::new(s).poll_shutdown(cx),
        }
    }
}

/// Connect to control-server via Unix socket.
pub async fn connect_to_control_server_unix(path: &Path) -> Result<TuiStream> {
    debug!(path = %path.display(), "TUI sidebar connecting to control-server via Unix socket");

    let stream = UnixStream::connect(path)
        .await
        .context(format!("Failed to connect to {}", path.display()))?;

    debug!("Connected to control-server via Unix socket");

    Ok(TuiStream::Unix(stream))
}

/// Connect to control-server via TCP.
pub async fn connect_to_control_server_tcp(port: u16) -> Result<TuiStream> {
    let addr = format!("127.0.0.1:{}", port);
    debug!(addr = %addr, "TUI sidebar connecting to control-server via TCP");

    let stream = TcpStream::connect(&addr)
        .await
        .context(format!("Failed to connect to {}", addr))?;

    stream.set_nodelay(true).ok();

    debug!("Connected to control-server via TCP");

    Ok(TuiStream::Tcp(stream))
}

/// Spawn I/O tasks for reading PopupDefinition and writing PopupResult.
///
/// Returns (rx, tx):
/// - rx: Receiver for PopupDefinition messages from Haskell
/// - tx: Sender for PopupResult responses to Haskell
pub fn spawn_io_tasks(
    stream: TuiStream,
) -> (mpsc::Receiver<PopupDefinition>, mpsc::Sender<PopupResult>) {
    let (read_half, mut write_half) = tokio::io::split(stream);

    let (msg_tx, msg_rx) = mpsc::channel::<PopupDefinition>(32);
    let (res_tx, mut res_rx) = mpsc::channel::<PopupResult>(32);

    // Reader task: TuiStream → PopupDefinition
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

                    match serde_json::from_str::<PopupDefinition>(trimmed) {
                        Ok(definition) => {
                            debug!(title = %definition.title, "Received PopupDefinition");
                            if msg_tx.send(definition).await.is_err() {
                                debug!("Receiver dropped, stopping reader");
                                break;
                            }
                        }
                        Err(e) => {
                            warn!(error = %e, json = %trimmed, "Failed to parse PopupDefinition");
                        }
                    }
                }
                Err(e) => {
                    error!(error = %e, "Failed to read from stream");
                    break;
                }
            }
        }

        debug!("Reader task exiting");
    });

    // Writer task: PopupResult → TuiStream
    tokio::spawn(async move {
        while let Some(result) = res_rx.recv().await {
            match serde_json::to_string(&result) {
                Ok(json) => {
                    debug!(button = %result.button, "Sending PopupResult");

                    if let Err(e) = write_half.write_all(json.as_bytes()).await {
                        error!(error = %e, "Failed to write JSON");
                        break;
                    }

                    if let Err(e) = write_half.write_all(b"\n").await {
                        error!(error = %e, "Failed to write newline");
                        break;
                    }

                    if let Err(e) = write_half.flush().await {
                        error!(error = %e, "Failed to flush");
                        break;
                    }
                }
                Err(e) => {
                    error!(error = %e, "Failed to serialize PopupResult");
                }
            }
        }

        debug!("Writer task exiting");
    });

    (msg_rx, res_tx)
}

/// Start a simple Unix listener for health checks.
pub async fn start_health_listener(path: &Path) -> Result<tokio::task::JoinHandle<()>> {
    // Ensure parent directory exists
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).context(format!(
            "Failed to create health socket directory {}",
            parent.display()
        ))?;
    }

    // Remove existing socket file if it exists
    if path.exists() {
        fs::remove_file(path).context("Failed to remove existing health socket file")?;
    }

    let listener = UnixListener::bind(path).context(format!(
        "Failed to bind health socket to {}",
        path.display()
    ))?;

    debug!(path = %path.display(), "TUI sidebar health listener started");

    let handle = tokio::spawn(async move {
        const MAX_CONSECUTIVE_ERRORS: u32 = 10;
        let mut consecutive_errors: u32 = 0;

        loop {
            match listener.accept().await {
                Ok((_stream, _addr)) => {
                    consecutive_errors = 0;
                }
                Err(e) => {
                    consecutive_errors = consecutive_errors.saturating_add(1);
                    error!(error = %e, consecutive_errors, "Health listener accept error");
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
