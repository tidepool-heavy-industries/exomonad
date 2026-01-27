//! exomonad: Hook handler for Claude Code++ sessions.
//!
//! This binary handles Claude Code hooks via HTTP requests to the control server.
//!
//! ## Subcommands
//!
//! - `hook <event>` - Handle a Claude Code hook event
//! - `health` - Check socket health
//!
//! ## MCP Tools
//!
//! MCP tools are accessed directly via HTTP transport. Claude Code connects to
//! the control-server's HTTP API (no proxy needed). Configure in .mcp.json:
//! ```json
//! {"mcpServers": {"exomonad": {"type": "http", "url": "http://localhost:7432/role/tl/mcp"}}}
//! ```

use anyhow::Result;
use clap::{Parser, Subcommand};

use exomonad_shared::commands::HookEventType;
use exomonad_shared::handle_hook;
use exomonad_shared::protocol::{Role, Runtime};

mod health;

// ============================================================================
// Version Info (embedded at compile time via build.rs)
// ============================================================================

/// Build version string from compile-time environment
const VERSION: &str = concat!(
    env!("CARGO_PKG_VERSION"),
    " (",
    env!("VERGEN_GIT_SHA"),
    ")"
);

// ============================================================================
// CLI Types
// ============================================================================

#[derive(Parser)]
#[command(name = "exomonad")]
#[command(about = "Hook handler for Claude Code++ sessions")]
#[command(version = VERSION)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Handle a Claude Code hook event (called by generated hook scripts)
    ///
    /// Connects to control server via HTTP over Unix socket (EXOMONAD_CONTROL_SOCKET).
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

    /// Check control server health via Ping/Pong on socket.
    Health,
}

// ============================================================================
// Main
// ============================================================================

fn main() -> Result<()> {
    // Initialize tracing with env filter (RUST_LOG)
    exomonad_shared::init_logging();

    let cli = Cli::parse();

    match cli.command {
        Commands::Hook { event, runtime, role } => handle_hook(event, runtime, role)?,
        Commands::Health => health::run_health_check()?,
    };

    Ok(())
}
