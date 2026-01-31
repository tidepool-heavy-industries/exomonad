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
use exomonad_shared::commands::HookEventType;
use exomonad_shared::protocol::{HookInput, HookOutput, Role, Runtime};
use std::path::PathBuf;
use std::sync::Arc;
use tracing::{debug, error, info};

// ============================================================================
// CLI Types
// ============================================================================

#[derive(Parser)]
#[command(name = "exomonad-sidecar")]
#[command(about = "Rust sidecar with Haskell WASM plugin for Claude Code")]
#[command(version)]
struct Cli {
    /// Path to the WASM plugin file (required for hook command)
    #[arg(long, env = "EXOMONAD_WASM_PATH")]
    wasm: Option<PathBuf>,

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

        /// The role of the agent (dev, tl, pm)
        #[arg(long, env = "EXOMONAD_ROLE", default_value = "dev")]
        role: Role,
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
}

// ============================================================================
// Hook Handler
// ============================================================================

async fn handle_hook(
    plugin: &PluginManager,
    event_type: HookEventType,
    _runtime: Runtime,
    _role: Role,
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
        Commands::Hook {
            event,
            runtime,
            role,
        } => {
            // Hook command requires WASM plugin
            let wasm_path = cli.wasm.ok_or_else(|| {
                anyhow::anyhow!("--wasm or EXOMONAD_WASM_PATH is required for hook command")
            })?;

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

            handle_hook(&plugin, event, runtime, role).await?;
        }
        Commands::Mcp { port, project_dir } => {
            // MCP server uses local execution (no WASM needed)
            let project_dir = project_dir.unwrap_or_else(|| {
                std::env::current_dir().expect("Failed to get current directory")
            });

            info!(port, project_dir = %project_dir.display(), "Starting MCP server");

            // Initialize services with local executor and secrets from .exomonad/secrets
            let services = Arc::new(Services::new_local());

            let state = mcp::McpState {
                services,
                project_dir,
            };

            mcp::run_server(state, port).await?;
        }

        Commands::McpStdio => {
            // stdio MCP server - Claude Code spawns this process
            // Discover project_dir from config or use cwd
            let cfg = match config::Config::discover() {
                Ok(c) => {
                    info!(role = %c.role, project_dir = %c.project_dir.display(), "Config discovered");
                    c
                }
                Err(e) => {
                    debug!(error = %e, "No config file found, using defaults");
                    config::Config::default_for_role("dev")
                }
            };

            let project_dir = if cfg.project_dir.is_absolute() {
                cfg.project_dir
            } else {
                std::env::current_dir()
                    .expect("Failed to get current directory")
                    .join(&cfg.project_dir)
            };

            // Initialize services (secrets loaded from ~/.exomonad/secrets)
            let services = Arc::new(Services::new_local());

            let state = mcp::McpState {
                services,
                project_dir,
            };

            mcp::stdio::run_stdio_server(state).await?;
        }
    }

    Ok(())
}
