//! exomonad-sidecar: Rust host with Haskell WASM plugin.
//!
//! This binary runs as a sidecar in each agent container, handling:
//! - Claude Code hooks via WASM plugin
//! - MCP tools via local Rust implementation
//!
//! All IO is handled by Rust; Haskell WASM yields high-level semantic effects.

mod config;
mod mcp;

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use exomonad_runtime::{PluginManager, Services};
use exomonad_shared::protocol::{HookEventType, HookInput, HookOutput, Runtime, ServiceRequest};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::io::AsyncWriteExt;
use tokio::net::UnixStream;
use tracing::{debug, error, info};

// ============================================================================
// CLI Types
// ============================================================================

#[derive(Parser)]
#[command(name = "exomonad-sidecar")]
#[command(about = "Rust sidecar with Haskell WASM plugin for Claude Code")]
#[command(version)]
struct Cli {
    /// Path to the WASM plugin file (required for all commands)
    #[arg(long, env = "EXOMONAD_WASM_PATH")]
    wasm: PathBuf,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Handle a Claude Code hook event via WASM plugin
    Hook {
        /// The hook event type to handle
        #[arg(value_enum)]
        event: HookEventType,

        /// The runtime environment (Claude or Gemini)
        #[arg(long, default_value = "claude")]
        runtime: Runtime,
    },

    /// Start MCP HTTP server for Claude Code tools
    Mcp {
        /// Port to listen on
        #[arg(long, default_value = "7432")]
        port: u16,

        /// Project directory for git operations (defaults to current directory)
        #[arg(long, env = "EXOMONAD_PROJECT_DIR")]
        project_dir: Option<PathBuf>,
    },

    /// Run as stdio MCP server (Claude Code spawns this)
    ///
    /// Reads config from .exomonad/config.toml in current directory.
    /// Claude Code should configure this in .mcp.json with type: "stdio".
    McpStdio,

    /// Reply to a UI request (sent by Zellij plugin)
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
}

// ============================================================================
// Hook Handler
// ============================================================================

async fn handle_hook(
    plugin: &PluginManager,
    event_type: HookEventType,
    _runtime: Runtime,
) -> Result<()> {
    use std::io::Read;

    // Read hook payload from stdin
    let mut stdin_content = String::new();
    std::io::stdin()
        .read_to_string(&mut stdin_content)
        .context("Failed to read stdin")?;

    debug!(
        event = ?event_type,
        payload_len = stdin_content.len(),
        "Received hook event"
    );

    // Parse the hook input
    let hook_input: HookInput =
        serde_json::from_str(&stdin_content).context("Failed to parse hook input")?;

    // Call WASM plugin
    let output: HookOutput = match event_type {
        HookEventType::PreToolUse => plugin
            .call("handle_pre_tool_use", &hook_input)
            .await
            .context("WASM handle_pre_tool_use failed")?,
        _ => {
            // For now, other hook types pass through with allow
            debug!(event = ?event_type, "Hook type not implemented in WASM, allowing");
            HookOutput::default()
        }
    };

    // Output response JSON to stdout
    let output_json = serde_json::to_string(&output).context("Failed to serialize output")?;
    println!("{}", output_json);

    // Exit code based on output
    if !output.continue_ {
        std::process::exit(2);
    }

    Ok(())
}

// ============================================================================
// Main
// ============================================================================

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .with_writer(std::io::stderr)
        .init();

    let cli = Cli::parse();

    match cli.command {
        Commands::Hook { event, runtime } => {
            // Validate WASM plugin exists for Hook
            let wasm_path = cli.wasm;
            if !wasm_path.exists() {
                error!(path = ?wasm_path, "WASM plugin file not found");
                anyhow::bail!("WASM plugin not found: {}", wasm_path.display());
            }

            info!(wasm = ?wasm_path, "Loading WASM plugin");

            // Initialize services with Docker executor (containerized mode)
            let services = Arc::new(Services::new());

            // Load WASM plugin
            let plugin = PluginManager::new(wasm_path, services)
                .await
                .context("Failed to load WASM plugin")?;

            info!("WASM plugin loaded and initialized");

            handle_hook(&plugin, event, runtime).await?;
        }

        Commands::Mcp { port, project_dir } => {
            let wasm_path = cli.wasm;
            if !wasm_path.exists() {
                anyhow::bail!("WASM plugin not found: {}", wasm_path.display());
            }

            let project_dir = project_dir.unwrap_or_else(|| {
                std::env::current_dir().expect("Failed to get current directory")
            });

            info!(wasm = ?wasm_path, "Loading WASM plugin");

            // Initialize services with local executor
            let services = Arc::new(Services::new_local());

            // Load WASM plugin
            let plugin = PluginManager::new(wasm_path, services.clone())
                .await
                .context("Failed to load WASM plugin")?;

            info!(port, project_dir = %project_dir.display(), "Starting MCP server");

            let state = mcp::McpState {
                project_dir,
                plugin: Arc::new(plugin),
            };

            mcp::run_server(state, port).await?;
        }

        Commands::McpStdio => {
            let wasm_path = cli.wasm;
            if !wasm_path.exists() {
                anyhow::bail!("WASM plugin not found: {}", wasm_path.display());
            }

            // stdio MCP server - Claude Code spawns this process
            // Discover project_dir from config or use cwd
            let cfg = match config::Config::discover() {
                Ok(c) => {
                    info!(project_dir = %c.project_dir.display(), "Config discovered");
                    c
                }
                Err(e) => {
                    debug!(error = %e, "No config file found, using defaults");
                    config::Config::default()
                }
            };

            let project_dir = if cfg.project_dir.is_absolute() {
                cfg.project_dir
            } else {
                std::env::current_dir()
                    .expect("Failed to get current directory")
                    .join(&cfg.project_dir)
            };

            info!(wasm = ?wasm_path, "Loading WASM plugin");

            // Initialize services (secrets loaded from ~/.exomonad/secrets)
            let services = Arc::new(Services::new_local());

            // Load WASM plugin
            let plugin = PluginManager::new(wasm_path, services.clone())
                .await
                .context("Failed to load WASM plugin")?;

            let state = mcp::McpState {
                project_dir,
                plugin: Arc::new(plugin),
            };

            mcp::stdio::run_stdio_server(state).await?;
        }

        Commands::Reply {
            id,
            payload,
            cancel,
        } => {
            // Socket path env var or default
            let socket_path = std::env::var("EXOMONAD_CONTROL_SOCKET")
                .unwrap_or_else(|_| ".exomonad/sockets/control.sock".to_string());

            debug!(socket = %socket_path, "Connecting to control socket");

            let mut stream = UnixStream::connect(&socket_path).await.context(format!(
                "Failed to connect to control socket at {}",
                socket_path
            ))?;

            let parsed_payload = if let Some(p) = payload {
                Some(serde_json::from_str(&p).context("Invalid JSON payload")?)
            } else {
                None
            };

            let request = ServiceRequest::UserInteraction {
                request_id: id,
                payload: parsed_payload,
                cancel,
            };

            // NDJSON format: JSON + newline
            let mut json = serde_json::to_vec(&request).context("Serialization failed")?;
            json.push(b'\n');

            stream
                .write_all(&json)
                .await
                .context("Failed to write to socket")?;

            // We don't necessarily wait for response here, it's a push notification
            info!("Sent reply to control socket");
        }
    }

    Ok(())
}
