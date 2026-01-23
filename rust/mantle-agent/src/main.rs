//! mantle-agent: Container-side agent for mantle sessions.
//!
//! This binary runs inside Docker containers and handles Claude Code hooks
//! via HTTP requests to the control server.
//!
//! ## Subcommands
//!
//! - `hook <event>` - Handle a Claude Code hook event
//! - `health` - Check socket health

use clap::{Parser, Subcommand};
use tracing::error;

use mantle_shared::commands::HookEventType;
use mantle_shared::handle_hook;
use mantle_shared::protocol::Runtime;

mod health;

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
    /// Connects to control server via Unix socket (MANTLE_CONTROL_SOCKET).
    Hook {
        /// The hook event type to handle
        #[arg(value_enum)]
        event: HookEventType,

        /// The runtime environment (Claude or Gemini)
        #[arg(long, default_value = "claude")]
        runtime: Runtime,
    },

    /// Check control server health via Ping/Pong on socket.
    Health,
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
        Commands::Health => health::run_health_check(),
    };

    if let Err(e) = result {
        error!(error = %e, "Command failed");
        std::process::exit(1);
    }
}
