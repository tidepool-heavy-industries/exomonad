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
use std::path::PathBuf;
use tracing::error;
use tracing_subscriber::EnvFilter;

use mantle_shared::commands::HookEventType;
use mantle_shared::handle_hook;

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
    Hook {
        /// The hook event type to handle
        #[arg(value_enum)]
        event: HookEventType,

        /// Control socket path (defaults to MANTLE_HOOK_SOCKET env var)
        #[arg(long, env = "MANTLE_HOOK_SOCKET")]
        socket: Option<PathBuf>,
    },

    /// Run as MCP stdio server for decision tools
    ///
    /// Reads tool definitions from MANTLE_DECISION_TOOLS env var and serves
    /// them via JSON-RPC 2.0 over stdio. Tool calls are forwarded to the
    /// control socket for recording.
    Mcp {
        /// Control socket path for reporting tool calls to mantle
        #[arg(long, env = "MANTLE_HOOK_SOCKET")]
        socket: Option<PathBuf>,
    },
}

// ============================================================================
// Main
// ============================================================================

fn main() {
    // Initialize tracing with env filter (RUST_LOG)
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_target(false)
        .init();

    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Hook { event, socket } => handle_hook(event, socket.as_ref()),
        Commands::Mcp { socket } => mcp::run_mcp_server(socket)
            .map_err(|e| mantle_shared::MantleError::McpServer(e.to_string())),
    };

    if let Err(e) = result {
        error!(error = %e, "Command failed");
        std::process::exit(1);
    }
}
