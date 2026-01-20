mod app;
mod input;
mod protocol;
mod render;
mod server;
mod ui_stack;

use anyhow::Result;
use clap::Parser;
use std::path::PathBuf;
use tracing::{info, Level};
use tracing_subscriber::FmtSubscriber;

/// TUI sidebar for Tidepool graph handlers.
///
/// Listens on Unix socket and renders interactive UIs based on UISpec messages
/// from Haskell tui-interpreter.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Unix socket path to listen on
    #[arg(short, long, default_value = ".tidepool/sockets/tui.sock")]
    socket: PathBuf,

    /// Log level (trace, debug, info, warn, error)
    #[arg(short, long, default_value = "info")]
    log_level: Level,
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();

    // Setup logging
    let subscriber = FmtSubscriber::builder()
        .with_max_level(args.log_level)
        .finish();
    tracing::subscriber::set_global_default(subscriber)?;

    // Resolve socket path
    let socket_path = if args.socket.is_relative() {
        if let Ok(project_dir) = std::env::var("TIDEPOOL_PROJECT_DIR") {
            PathBuf::from(project_dir).join(&args.socket)
        } else {
            args.socket
        }
    } else {
        args.socket
    };

    info!("TUI sidebar starting on socket {:?}", socket_path);

    // Wait for connection
    let stream = server::wait_for_unix_connection(&socket_path).await?;

    // Spawn I/O tasks
    let (msg_rx, int_tx) = server::spawn_io_tasks(stream);

    // Run event loop
    app::run(msg_rx, int_tx).await?;

    info!("TUI sidebar exiting");

    Ok(())
}
