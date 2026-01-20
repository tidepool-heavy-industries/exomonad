mod app;
mod input;
mod protocol;
mod render;
mod server;
mod ui_stack;

use anyhow::{Context, Result};
use clap::Parser;
use std::path::PathBuf;
use tracing::{info, Level};
use tracing_subscriber::FmtSubscriber;

/// TUI sidebar for Tidepool graph handlers.
///
/// Connects to control-server via Unix socket and renders interactive UIs
/// based on UISpec messages from Haskell tui-interpreter.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Unix socket path to connect to
    #[arg(short, long, default_value = ".tidepool/sockets/tui.sock")]
    socket: PathBuf,

    /// Unix socket path to listen on for health checks
    #[arg(short = 'H', long, default_value = ".tidepool/sockets/tui-sidebar.sock")]
    health_socket: PathBuf,

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

    info!("TUI sidebar connecting to socket {:?}", socket_path);

    // Resolve health socket path
    let health_socket_path = if args.health_socket.is_relative() {
        if let Ok(project_dir) = std::env::var("TIDEPOOL_PROJECT_DIR") {
            PathBuf::from(project_dir).join(&args.health_socket)
        } else {
            args.health_socket
        }
    } else {
        args.health_socket
    };

    // Start health listener
    let _health_handle = server::start_health_listener(&health_socket_path).await?;

    // Connect to control-server with retry logic
    let mut retry_count = 0;
    let max_retries = 10;
    let stream = loop {
        match server::connect_to_control_server(&socket_path).await {
            Ok(s) => break s,
            Err(e) => {
                retry_count += 1;
                if retry_count >= max_retries {
                    return Err(e).context(format!(
                        "Failed to connect to control-server after {} attempts",
                        max_retries
                    ));
                }
                info!(
                    "Waiting for control-server at {:?} (attempt {}/{})",
                    socket_path, retry_count, max_retries
                );
                tokio::time::sleep(std::time::Duration::from_secs(1)).await;
            }
        }
    };

    // Spawn I/O tasks
    let (msg_rx, int_tx) = server::spawn_io_tasks(stream);

    // Run event loop
    let result = app::run(msg_rx, int_tx).await;

    // Cleanup health socket
    if health_socket_path.exists() {
        let _ = std::fs::remove_file(&health_socket_path);
    }

    info!("TUI sidebar exiting");

    result
}
