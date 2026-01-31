//! exomonad: Hook handler for Claude Code++ sessions.
//!
//! This binary handles Claude Code hooks. In standalone mode (default), hooks
//! are passthrough - they allow all operations. For advanced hook logic,
//! use exomonad-sidecar with WASM plugins.
//!
//! ## Subcommands
//!
//! - `hook <event>` - Handle a Claude Code hook event (passthrough)
//! - `health` - Check control server health (if using sidecar mode)
//!
//! ## MCP Tools
//!
//! MCP tools are provided by exomonad-sidecar via stdio transport.
//! Configure in .mcp.json:
//! ```json
//! {"mcpServers": {"exomonad": {"type": "stdio", "command": "exomonad-sidecar", "args": ["mcp-stdio"]}}}
//! ```

use anyhow::Result;
use clap::{Parser, Subcommand};
use std::io::Read;

use exomonad_shared::commands::HookEventType;
use exomonad_shared::protocol::{Role, Runtime};

mod health;

// ============================================================================
// Version Info (embedded at compile time via build.rs)
// ============================================================================

/// Build version string from compile-time environment
const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), " (", env!("VERGEN_GIT_SHA"), ")");

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
// Passthrough Hook Handler
// ============================================================================

/// Handle hook by reading stdin and outputting passthrough response.
///
/// This is a standalone passthrough - no external server connection needed.
/// For advanced hook logic, use exomonad-sidecar with WASM plugins.
fn handle_hook_passthrough(event: HookEventType) -> Result<()> {
    // Read stdin (Claude Code sends hook input here)
    let mut stdin_content = String::new();
    std::io::stdin().read_to_string(&mut stdin_content)?;

    // Log the event (goes to stderr, not stdout)
    eprintln!("[exomonad] hook {} (passthrough)", event);

    // Output passthrough response - allow all operations
    // Claude Code expects {"continue": true} to proceed
    println!(r#"{{"continue":true}}"#);

    Ok(())
}

// ============================================================================
// Main
// ============================================================================

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing with env filter (RUST_LOG)
    exomonad_shared::init_logging();

    let cli = Cli::parse();

    match cli.command {
        Commands::Hook {
            event,
            runtime: _,
            role: _,
        } => handle_hook_passthrough(event)?,
        Commands::Health => health::run_health_check().await?,
    };

    Ok(())
}
