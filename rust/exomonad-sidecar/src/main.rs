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
use exomonad_runtime::services::{git, zellij_events};
use exomonad_runtime::{PluginManager, Services};
use exomonad_shared::protocol::{HookEventType, HookInput, HookOutput, Runtime, ServiceRequest};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::io::AsyncWriteExt;
use tokio::net::UnixStream;
use tracing::{debug, info, warn};

// ============================================================================
// CLI Types
// ============================================================================

#[derive(Parser)]
#[command(name = "exomonad-sidecar")]
#[command(about = "Rust sidecar with Haskell WASM plugin for Claude Code")]
#[command(version)]
struct Cli {
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
    runtime: Runtime,
) -> Result<()> {
    use std::io::Read;

    // Read hook payload from stdin
    let mut stdin_content = String::new();
    std::io::stdin()
        .read_to_string(&mut stdin_content)
        .context("Failed to read stdin")?;

    debug!(
        event = ?event_type,
        runtime = ?runtime,
        payload_len = stdin_content.len(),
        "Received hook event"
    );

    // Parse the hook input and inject runtime
    let mut hook_input: HookInput =
        serde_json::from_str(&stdin_content).context("Failed to parse hook input")?;
    hook_input.runtime = Some(runtime);

    // Call WASM plugin
    // hookHandler dispatches based on hiHookEventName for stop hooks
    let output: HookOutput = match event_type {
        HookEventType::PreToolUse | HookEventType::SessionEnd | HookEventType::SubagentStop => {
            plugin
                .call("handle_pre_tool_use", &hook_input)
                .await
                .context("WASM handle_pre_tool_use failed")?
        }
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
        // Emit StopHookBlocked event for SubagentStop hooks
        if event_type == HookEventType::SubagentStop {
            if let Ok(branch) = git::get_current_branch() {
                if let Some(agent_id) = git::extract_agent_id(&branch) {
                    let reason = output
                        .stop_reason
                        .clone()
                        .unwrap_or_else(|| "Hook blocked agent stop".to_string());
                    let event = exomonad_ui_protocol::AgentEvent::StopHookBlocked {
                        agent_id,
                        reason,
                        timestamp: zellij_events::now_iso8601(),
                    };
                    if let Err(e) = zellij_events::emit_event(&event) {
                        warn!("Failed to emit stop_hook:blocked event: {}", e);
                    }
                }
            }
        }
        std::process::exit(2);
    }

    Ok(())
}

// ============================================================================
// Logging
// ============================================================================

/// Initialize logging based on the command mode.
/// - MCP stdio: Logs to ~/.exomonad/logs/sidecar-TIMESTAMP.log
/// - Other modes: Logs to stderr (preserves current behavior)
fn init_logging(command: &Commands) {
    match command {
        Commands::McpStdio => {
            // File-based logging for stdio mode
            let home_dir = std::env::var("HOME").expect("HOME environment variable not set");
            let log_dir = PathBuf::from(home_dir).join(".exomonad").join("logs");

            // Create log directory if it doesn't exist
            std::fs::create_dir_all(&log_dir).expect("Failed to create log directory");

            // Generate timestamped filename
            let timestamp = chrono::Local::now().format("%Y-%m-%d-%H-%M-%S");
            let log_file = log_dir.join(format!("sidecar-{}.log", timestamp));

            let file = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&log_file)
                .expect("Failed to open log file");

            tracing_subscriber::fmt()
                .with_env_filter(
                    tracing_subscriber::EnvFilter::from_default_env()
                        .add_directive(tracing::Level::INFO.into()),
                )
                .with_writer(std::sync::Arc::new(file))
                .with_ansi(false) // No ANSI colors in file
                .init();

            eprintln!("MCP stdio logging to: {}", log_file.display());
        }
        _ => {
            // Stderr logging for other modes (existing behavior)
            tracing_subscriber::fmt()
                .with_env_filter(
                    tracing_subscriber::EnvFilter::from_default_env()
                        .add_directive(tracing::Level::INFO.into()),
                )
                .with_writer(std::io::stderr)
                .init();
        }
    }
}

// ============================================================================
// Main
// ============================================================================

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging based on command type
    init_logging(&cli.command);

    // Discover config (or use default)
    let raw_config = config::Config::discover().unwrap_or_else(|e| {
        debug!(error = %e, "No config found, using defaults");
        config::Config::default()
    });

    // Validate config (exits early with helpful error if invalid)
    let config = raw_config
        .validate()
        .map_err(|e| anyhow::anyhow!("Config validation failed: {}", e))?;

    let wasm_path = config.wasm_path_buf();

    info!(
        role = ?config.role(),
        wasm_path = %wasm_path.display(),
        "Loaded and validated config"
    );

    match cli.command {
        Commands::Hook { event, runtime } => {
            info!(wasm = ?wasm_path, "Loading WASM plugin");

            // Initialize and validate services
            let services = Arc::new(
                Services::new()
                    .validate()
                    .context("Failed to validate services")?,
            );

            // Load WASM plugin
            let plugin = PluginManager::new(wasm_path, services)
                .await
                .context("Failed to load WASM plugin")?;

            info!("WASM plugin loaded and initialized");

            handle_hook(&plugin, event, runtime).await?;
        }

        Commands::Mcp { port, project_dir } => {
            let project_dir = project_dir.unwrap_or_else(|| {
                std::env::current_dir().expect("Failed to get current directory")
            });

            info!(wasm = ?wasm_path, "Loading WASM plugin");

            // Initialize and validate services
            let services = Arc::new(
                Services::new()
                    .validate()
                    .context("Failed to validate services")?,
            );

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
            // stdio MCP server - Claude Code spawns this process
            // Use config already loaded above
            let project_dir_ref = config.project_dir();
            let project_dir = if project_dir_ref.is_absolute() {
                project_dir_ref.clone()
            } else {
                std::env::current_dir()
                    .expect("Failed to get current directory")
                    .join(project_dir_ref)
            };

            info!(wasm = ?wasm_path, "Loading WASM plugin");

            // Initialize and validate services (secrets loaded from ~/.exomonad/secrets)
            let services = Arc::new(
                Services::new()
                    .validate()
                    .context("Failed to validate services")?,
            );

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
