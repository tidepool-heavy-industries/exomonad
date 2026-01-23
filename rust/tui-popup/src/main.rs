mod websocket;

use anyhow::{Context, Result};
use clap::Parser;
use std::io::{self, Read};
use tui_sidebar::popup::run_popup;
use tui_sidebar::protocol::PopupDefinition;

/// TUI popup binary for Zellij pane popups.
///
/// Connects to control-server via WebSocket, receives PopupDefinition,
/// renders it, captures user input, and sends PopupResult back.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Control socket path (defaults to $TIDEPOOL_CONTROL_SOCKET or .tidepool/sockets/control.sock)
    #[arg(long)]
    socket: Option<String>,

    /// LEGACY: PopupDefinition JSON string (for testing without WebSocket)
    #[arg(long)]
    spec: Option<String>,

    /// LEGACY: Read PopupDefinition from stdin instead of --spec
    #[arg(long)]
    stdin: bool,
}

#[tokio::main]
async fn main() -> Result<()> {
    // Setup minimal logging to stderr (avoid polluting stdout)
    tracing_subscriber::fmt()
        .with_writer(io::stderr)
        .with_max_level(tracing::Level::WARN)
        .init();

    let args = Args::parse();

    // Get socket path once (used for both receive and send)
    let socket_path = args.socket.clone()
        .or_else(|| std::env::var("TIDEPOOL_CONTROL_SOCKET").ok())
        .unwrap_or_else(|| ".tidepool/sockets/control.sock".to_string());

    // Determine mode: WebSocket (default) or legacy CLI
    if args.spec.is_some() || args.stdin {
        // LEGACY MODE: Get PopupDefinition from --spec or stdin
        let json = if args.stdin {
            let mut buffer = String::new();
            io::stdin()
                .read_to_string(&mut buffer)
                .context("Failed to read from stdin")?;
            buffer
        } else if let Some(spec) = args.spec {
            spec
        } else {
            anyhow::bail!("Either --spec or --stdin must be provided");
        };

        let definition: PopupDefinition =
            serde_json::from_str(&json).context("Failed to parse PopupDefinition JSON")?;

        // Run popup (blocking)
        let result = tokio::task::spawn_blocking(move || run_popup(definition))
            .await
            .context("Popup task panicked")?
            .context("Popup failed")?;

        // Output to stdout
        println!("{}", serde_json::to_string(&result)?);
    } else {
        // WEBSOCKET MODE: Maintain single connection throughout
        let (request_id, definition, mut ws_stream) = websocket::run_websocket_popup(&socket_path)
            .await
            .context("Failed to receive popup definition from WebSocket")?;

        // Run popup (blocking in separate task to avoid blocking Tokio runtime)
        let result = tokio::task::spawn_blocking(move || run_popup(definition))
            .await
            .context("Popup task panicked")?
            .context("Popup failed")?;

        // Send result back on the same WebSocket connection
        websocket::send_popup_result(&mut ws_stream, request_id, result)
            .await
            .context("Failed to send popup result via WebSocket")?;
    }

    Ok(())
}
