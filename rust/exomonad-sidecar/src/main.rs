//! exomonad-sidecar: Rust host with Haskell WASM plugin.
//!
//! This binary runs as a sidecar in each agent container, handling:
//! - Claude Code hooks via WASM plugin
//! - MCP tools via WASM plugin (TODO)
//!
//! All IO is handled by Rust; Haskell WASM yields high-level semantic effects.

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
    /// Path to the WASM plugin file
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

        /// The role of the agent (dev, tl, pm)
        #[arg(long, env = "EXOMONAD_ROLE", default_value = "dev")]
        role: Role,
    },

    /// Start MCP server (TODO)
    Mcp {
        /// Port to listen on
        #[arg(long, default_value = "7432")]
        port: u16,
    },
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

    // Verify WASM file exists
    if !cli.wasm.exists() {
        error!(path = ?cli.wasm, "WASM plugin file not found");
        anyhow::bail!("WASM plugin not found: {}", cli.wasm.display());
    }

    info!(wasm = ?cli.wasm, "Loading WASM plugin");

    // Initialize services (Rust-side IO implementations)
    let services = Arc::new(Services::new());

    // Load WASM plugin
    let plugin = PluginManager::new(cli.wasm.clone(), services)
        .await
        .context("Failed to load WASM plugin")?;

    info!("WASM plugin loaded and initialized");

    match cli.command {
        Commands::Hook {
            event,
            runtime,
            role,
        } => {
            handle_hook(&plugin, event, runtime, role).await?;
        }
        Commands::Mcp { port } => {
            info!(port, "MCP server not yet implemented");
            // TODO: Phase 4 - implement MCP server
            anyhow::bail!("MCP server not yet implemented");
        }
    }

    Ok(())
}
