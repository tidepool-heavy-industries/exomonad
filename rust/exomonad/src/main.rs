//! exomonad: Rust host with embedded Haskell WASM plugin.
//!
//! This binary runs as a sidecar in each agent container, handling:
//! - Claude Code hooks via HTTP forwarding to the server
//! - MCP tools via WASM plugin (server-side)
//!
//! WASM plugins are loaded from file (server-side only).

mod app_state;
mod init;
mod logging;
mod mcp_stdio;
mod serve;
mod uds_client;

use exomonad::config;
use urlencoding::encode;

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use exomonad_core::protocol::{Runtime as HookRuntime, ServiceRequest};
use std::time::{Duration, Instant};

use exomonad_core::{HookEnvelope, HookEventType};
use tokio::io::AsyncWriteExt;
use tokio::net::UnixStream;
use tracing::debug;

// ============================================================================
// CLI Types
// ============================================================================

#[derive(Parser)]
#[command(name = "exomonad")]
#[command(about = "ExoMonad: Rust host with embedded Haskell WASM plugin for agent orchestration")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Handle a Claude Code hook event (thin HTTP client → server)
    Hook {
        /// The hook event type to handle
        #[arg(value_enum)]
        event: HookEventType,

        /// The runtime environment (Claude or Gemini)
        #[arg(long, default_value = "claude")]
        runtime: HookRuntime,
    },

    /// Initialize tmux session for this project.
    ///
    /// Creates a new session if none exists, or attaches to existing.
    /// Session name is read from .exo/config.toml tmux_session field.
    Init {
        /// Optionally override session name (default: from config)
        #[arg(long)]
        session: Option<String>,
        /// Delete existing session and create fresh (use after binary/layout updates)
        #[arg(long)]
        recreate: bool,
    },

    /// Recompile WASM plugin from Haskell source
    Recompile {
        /// WASM package to build (default: from config wasm_name, usually "devswarm")
        #[arg(long)]
        role: Option<String>,
    },

    /// Run MCP server on Unix domain socket (.exo/server.sock)
    ///
    /// Loads WASM from file path (not embedded) with hot reload on change.
    Serve,

    /// Run stdio MCP proxy (stdin/stdout ↔ UDS server)
    McpStdio {
        /// Agent role (e.g., "tl", "dev", "worker")
        #[arg(long)]
        role: String,
        /// Agent name (e.g., "root", "feature-impl")
        #[arg(long)]
        name: String,
    },

    /// Reply to a UI request
    Reply {
        /// Request ID
        #[arg(long)]
        id: String,

        /// JSON payload
        #[arg(long)]
        payload: Option<String>,

        /// Cancel the request
        #[arg(long)]
        cancel: bool,
    },

    /// Reload WASM plugins (clears plugin cache, next call loads fresh from disk)
    Reload,

    /// Gracefully shut down the running server
    Shutdown,
}

// Main
// ============================================================================

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();
    let _guard = logging::init();

    let config = config::Config::discover().unwrap_or_else(|e| {
        debug!(error = %e, "No config found, using defaults");
        config::Config::default()
    });

    match cli.command {
        Commands::McpStdio { ref role, ref name } => {
            return mcp_stdio::run(role, name).await;
        }

        Commands::Recompile { ref role } => {
            let role_str = role.as_deref().unwrap_or(&config.wasm_name);
            let project_dir = if config.project_dir.is_absolute() {
                config.project_dir.clone()
            } else {
                std::env::current_dir()?.join(&config.project_dir)
            };
            return exomonad::recompile::run_recompile(role_str, &project_dir, config.flake_ref.as_deref()).await;
        }

        Commands::Serve => {
            return serve::run(&config).await;
        }

        Commands::Hook { event, runtime } => {
            let mut path = format!("/hook?event={}&runtime={}", event, runtime);
            if let Ok(agent_id) = std::env::var("EXOMONAD_AGENT_ID") {
                path.push_str(&format!("&agent_id={}", encode(&agent_id)));
            }
            if let Ok(session_id) = std::env::var("EXOMONAD_SESSION_ID") {
                path.push_str(&format!("&session_id={}", encode(&session_id)));
            }
            if let Ok(role) = std::env::var("EXOMONAD_ROLE") {
                path.push_str(&format!("&role={}", encode(&role)));
            }

            let mut body = String::new();
            use std::io::Read;
            std::io::stdin().read_to_string(&mut body)?;

            let is_root_session_start = event == HookEventType::SessionStart && std::env::var("EXOMONAD_AGENT_ID").is_err();

            let socket = if is_root_session_start {
                let start = Instant::now();
                let timeout_dur = Duration::from_secs(5);
                let mut found = None;
                while start.elapsed() < timeout_dur {
                    if let Ok(s) = uds_client::find_server_socket() {
                        found = Some(s);
                        break;
                    }
                    tokio::time::sleep(Duration::from_millis(500)).await;
                }
                match found {
                    Some(s) => s,
                    None => {
                        println!(r#"{{"continue":true}}"#);
                        return Ok(());
                    }
                }
            } else {
                match uds_client::find_server_socket() {
                    Ok(s) => s,
                    Err(_) => {
                        println!(r#"{{"continue":true}}"#);
                        return Ok(());
                    }
                }
            };

            let client = uds_client::ServerClient::new(socket);
            let json_body: serde_json::Value = serde_json::from_str(&body).unwrap_or(serde_json::json!({}));

            match client.post_json::<serde_json::Value, HookEnvelope>(&path, &json_body).await {
                Ok(resp) => {
                    print!("{}", resp.stdout);
                    if resp.exit_code != 0 {
                        std::process::exit(resp.exit_code);
                    }
                }
                Err(_) => println!(r#"{{"continue":true}}"#),
            }
        }

        Commands::Init { session, recreate } => {
            init::run(session, recreate).await?;
        }

        Commands::Reply { id, payload, cancel } => {
            let socket_path = std::env::var("EXOMONAD_CONTROL_SOCKET").unwrap_or_else(|_| ".exo/sockets/control.sock".to_string());
            let mut stream = UnixStream::connect(&socket_path).await?;

            let parsed_payload = payload.and_then(|p| serde_json::from_str(&p).ok());
            let request = ServiceRequest::UserInteraction {
                request_id: id,
                payload: parsed_payload,
                cancel,
            };

            let mut json = serde_json::to_vec(&request)?;
            json.push(b'\n');
            stream.write_all(&json).await?;
        }

        Commands::Reload => {
            let socket = uds_client::find_server_socket().context("Cannot find server socket.")?;
            let client = uds_client::ServerClient::new(socket);
            let resp: serde_json::Value = client.post_json("/reload", &serde_json::json!({})).await?;
            println!("{}", serde_json::to_string_pretty(&resp)?);
        }

        Commands::Shutdown => {
            let socket = uds_client::find_server_socket().context("Cannot find server socket.")?;
            let client = uds_client::ServerClient::new(socket);
            let _ = client.post_json::<serde_json::Value, serde_json::Value>("/shutdown", &serde_json::json!({})).await;
            println!("Shutdown signal sent");
        }
    }

    Ok(())
}
