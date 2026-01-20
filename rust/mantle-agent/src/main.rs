//! mantle-agent: Container-side agent for mantle sessions.
//!
//! This binary runs inside Docker containers and handles Claude Code hooks
//! and serves as an MCP server for decision tools.
//!
//! ## Subcommands
//!
//! - `hook <event>` - Handle a Claude Code hook event
//! - `mcp` - Run as MCP stdio server for decision tools

use clap::{Parser, Subcommand};
use tracing::error;

use mantle_shared::commands::HookEventType;
use mantle_shared::handle_hook;
use mantle_shared::protocol::Runtime;

mod mcp;

// ============================================================================
// CLI Types
// ============================================================================

#[derive(Parser)]
#[command(name = "mantle-agent")]
#[command(about = "Container-side agent for Claude Code hook handling")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Handle a Claude Code hook event (called by generated hook scripts)
    ///
    /// Connects to control server via MANTLE_CONTROL_HOST/PORT env vars.
    Hook {
        /// The hook event type to handle
        #[arg(value_enum)]
        event: HookEventType,

        /// The runtime environment (Claude or Gemini)
        #[arg(long, default_value = "claude")]
        runtime: Runtime,
    },

    /// Run as MCP stdio server for decision tools
    ///
    /// Queries the control server for tool definitions at startup and serves
    /// them via JSON-RPC 2.0 over stdio. Tool calls are forwarded to the
    /// control server via TCP (MANTLE_CONTROL_HOST/PORT).
    Mcp,
}

// ============================================================================
// Main
// ============================================================================

fn main() {
    // Initialize tracing with env filter (RUST_LOG)
    mantle_shared::init_logging();

    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Hook { event, runtime } => handle_hook(event, runtime),
        Commands::Mcp => mcp::run_mcp_server()
            .map_err(|e| mantle_shared::MantleError::McpServer(e.to_string())),
    };

    if let Err(e) = result {
        error!(error = %e, "Command failed");
        std::process::exit(1);
    }
}
