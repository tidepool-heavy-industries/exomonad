mod app;
mod protocol;
mod realm;
mod server;

use anyhow::{Context, Result};
use clap::Parser;
use std::path::PathBuf;
use tracing::{error, info, Level};
use tracing_subscriber::FmtSubscriber;

/// TUI sidebar for Tidepool graph handlers.
///
/// Connects to control-server via Unix socket and renders interactive UIs
/// based on UISpec messages from Haskell tui-interpreter.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Unix socket path to connect to (legacy)
    #[arg(short, long)]
    socket: Option<PathBuf>,

    /// TCP port to connect to (cross-boundary Docker/Host)
    #[arg(short, long)]
    port: Option<u16>,

    /// Unix socket path to listen on for health checks
    #[arg( 
        short = 'H',
        long,
        default_value = ".tidepool/sockets/tui-sidebar.sock"
    )]
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

    // Connection Priority:
    // 1. --port arg
    // 2. TIDEPOOL_TUI_PORT env
    // 3. --socket arg
    // 4. TIDEPOOL_TUI_SOCKET env

    let tui_port = args.port.or_else(|| {
        std::env::var("TIDEPOOL_TUI_PORT").ok().and_then(|s| s.parse().ok())
    });

    let socket_path = if tui_port.is_none() {
        let path = match args.socket {
            Some(s) => s,
            None => {
                let env_val = std::env::var("TIDEPOOL_TUI_SOCKET")
                    .context("TIDEPOOL_TUI_SOCKET/TIDEPOOL_TUI_PORT environment variable not set and --socket/--port not provided")?;
                PathBuf::from(env_val)
            }
        };
        Some(resolve_socket_path(path))
    } else {
        None
    };

    if let Some(port) = tui_port {
        info!("TUI sidebar connecting via TCP to localhost:{}", port);
    } else if let Some(ref path) = socket_path {
        info!("TUI sidebar connecting to socket {:?}", path);
    }

    // Resolve health socket path
    let health_socket_path = resolve_socket_path(args.health_socket);

    // Start health listener
    let _health_handle = server::start_health_listener(&health_socket_path).await?;

    // Resolve control socket path if available for TUI-disabled detection
    let control_socket_path = std::env::var("TIDEPOOL_CONTROL_SOCKET")
        .ok()
        .map(|s| resolve_socket_path(PathBuf::from(s)));

    loop {
        // Connect to control-server with retry logic and exponential backoff
        let mut retry_count = 0;
        let max_retries = 15;
        const MIN_RETRIES_BEFORE_WARNING: i32 = 3; // Wait a few attempts before warning about mixed socket state
        let mut retry_delay = std::time::Duration::from_secs(1);

        let stream = loop {
            let conn_res = if let Some(port) = tui_port {
                server::connect_to_control_server_tcp(port).await
            } else {
                server::connect_to_control_server_unix(socket_path.as_ref().unwrap()).await
            };

            match conn_res {
                Ok(s) => break s,
                Err(e) => {
                    retry_count += 1;

                    // Check if control server socket exists but TUI socket is missing.
                    // If control.sock exists but tui.sock doesn't after a few retries,
                    // it's likely that TUI was explicitly disabled.
                    let control_socket_exists = control_socket_path.as_ref().is_some_and(|p| p.exists());

                    if retry_count >= max_retries {
                        if control_socket_exists {
                            eprintln!("\n❌ TUI DISABLED");
                            if let Some(path) = &socket_path {
                                eprintln!("Control socket exists at {:?}, but TUI socket {:?} was never created.", 
                                    control_socket_path.as_ref().expect("Guaranteed by control_socket_exists"), 
                                    path
                                );
                            } else {
                                eprintln!("Control socket exists, but TUI TCP connection failed.");
                            }
                            eprintln!("This usually means --no-tui was passed to control-server (or the server crashed leaving a stale socket).\n");
                            // Exit with 0 to indicate "Giving up as intended"
                            std::process::exit(0);
                        }

                        // Cleanup health socket before returning
                        if health_socket_path.exists() {
                            let _ = std::fs::remove_file(&health_socket_path);
                        }

                        return Err(e).context(format!(
                            "Failed to connect to control-server after {} attempts",
                            max_retries
                        ));
                    }

                    if control_socket_exists && retry_count >= MIN_RETRIES_BEFORE_WARNING {
                        if let Some(path) = &socket_path {
                            info!(
                                "Control socket exists but TUI socket {:?} is missing (attempt {}/{})",
                                path, retry_count, max_retries
                            );
                        }
                    } else {
                        if let Some(path) = &socket_path {
                            info!(
                                "Waiting for control-server at {:?} (attempt {}/{}, next retry in {:?})",
                                path, retry_count, max_retries, retry_delay
                            );
                        } else {
                            info!(
                                "Waiting for control-server at localhost:{} (attempt {}/{}, next retry in {:?})",
                                tui_port.unwrap(), retry_count, max_retries, retry_delay
                            );
                        }
                    }

                    tokio::time::sleep(retry_delay).await;

                    // Exponential backoff: 1s, 2s, 4s, 8s, 10s, 10s...
                    retry_delay =
                        std::cmp::min(retry_delay * 2, std::time::Duration::from_secs(10));
                }
            }
        };

        // Spawn I/O tasks
        let (msg_rx, int_tx) = server::spawn_io_tasks(stream);

        // Run event loop
        match app::run(msg_rx, int_tx).await {
            Ok(_) => info!("Connection closed, reconnecting..."),
            Err(e) => error!(error = %e, "App error, reconnecting..."),
        }

        // Wait before reconnecting
        tokio::time::sleep(std::time::Duration::from_secs(1)).await;
    }
}

/// Helper to resolve socket path relative to project dir if needed
fn resolve_socket_path(path: PathBuf) -> PathBuf {
    if path.is_relative() {
        if let Ok(project_dir) = std::env::var("TIDEPOOL_PROJECT_DIR") {
            PathBuf::from(project_dir).join(&path)
        } else {
            path
        }
    } else {
        path
    }
}