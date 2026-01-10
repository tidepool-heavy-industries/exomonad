//! mantle-agent: Container-side agent for mantle sessions.
//!
//! This binary runs inside Docker containers and wraps Claude Code,
//! handling process supervision, hook interception, and signal forwarding.
//!
//! ## Commands
//!
//! - `wrap`: Main entry point, supervises Claude Code subprocess
//! - `signal`: Write interrupt signals to FIFO
//! - `hook`: Intercept and forward Claude Code hooks

use clap::{Parser, Subcommand};
use std::path::PathBuf;
use tracing::error;
use tracing_subscriber::EnvFilter;

use mantle_shared::commands::HookEventType;
use mantle_shared::{handle_hook, send_signal, wrap_claude, wrap_claude_with_hub, wrap_claude_to_stdout};

// ============================================================================
// CLI Types
// ============================================================================

#[derive(Parser)]
#[command(name = "mantle-agent")]
#[command(about = "Container-side agent for mantle sessions (wraps Claude Code)")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Wrap claude as subprocess, humanize output
    Wrap {
        /// FIFO to write final JSON result to (legacy mode)
        #[arg(long, conflicts_with_all = ["hub_socket", "stdout"])]
        result_fifo: Option<PathBuf>,

        /// Hub socket to write final JSON result to
        #[arg(long, conflicts_with_all = ["result_fifo", "stdout"])]
        hub_socket: Option<PathBuf>,

        /// Write final JSON result to stdout (attached container mode)
        #[arg(long, conflicts_with_all = ["result_fifo", "hub_socket"])]
        stdout: bool,

        /// Working directory for Claude Code
        #[arg(long)]
        cwd: Option<PathBuf>,

        /// Tag for correlating this session with orchestrator state (required with --hub-socket)
        #[arg(long)]
        session_tag: Option<String>,

        /// Timeout in seconds (0 = no timeout)
        #[arg(long, default_value = "0")]
        timeout: u64,

        /// Control socket path (enables hook interception)
        #[arg(long)]
        control_socket: Option<PathBuf>,

        /// All remaining args passed to claude
        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        claude_args: Vec<String>,
    },

    /// Signal an interrupt (called by Claude via Bash)
    Signal {
        /// Signal type: "transition", "escalate", "fork", "request_review", etc.
        signal_type: String,

        /// Target state for transitions (e.g., child branch name for fork)
        #[arg(long)]
        state: Option<String>,

        /// Reason/payload for the signal (e.g., child prompt for fork)
        #[arg(long)]
        reason: Option<String>,

        /// FIFO to write to (set via TIDEPOOL_SIGNAL_FIFO env var by wrap)
        #[arg(long, env = "TIDEPOOL_SIGNAL_FIFO")]
        fifo: PathBuf,
    },

    /// Handle a Claude Code hook event (called by generated hook scripts)
    Hook {
        /// The hook event type to handle
        #[arg(value_enum)]
        event: HookEventType,

        /// Control socket path (defaults to TIDEPOOL_CONTROL_SOCKET env var)
        #[arg(long, env = "TIDEPOOL_CONTROL_SOCKET")]
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
        Commands::Wrap {
            result_fifo,
            hub_socket,
            stdout,
            cwd,
            session_tag,
            timeout,
            control_socket,
            claude_args,
        } => {
            if stdout {
                // Stdout mode - for attached containers
                wrap_claude_to_stdout(
                    cwd.as_ref(),
                    session_tag.as_deref(),
                    timeout,
                    control_socket.as_ref(),
                    &claude_args,
                )
            } else if let Some(hub_socket) = hub_socket {
                // Hub socket mode - requires session_tag
                let session_id = session_tag.as_deref().unwrap_or_else(|| {
                    error!("--session-tag is required when using --hub-socket");
                    std::process::exit(1);
                });
                wrap_claude_with_hub(
                    &hub_socket,
                    session_id,
                    cwd.as_ref(),
                    timeout,
                    control_socket.as_ref(),
                    &claude_args,
                )
            } else if let Some(result_fifo) = result_fifo {
                // Legacy FIFO mode
                wrap_claude(
                    &result_fifo,
                    cwd.as_ref(),
                    session_tag.as_deref(),
                    timeout,
                    control_socket.as_ref(),
                    &claude_args,
                )
            } else {
                error!("One of --stdout, --result-fifo, or --hub-socket must be provided");
                std::process::exit(1);
            }
        }

        Commands::Signal {
            signal_type,
            state,
            reason,
            fifo,
        } => send_signal(&fifo, &signal_type, state.as_deref(), reason.as_deref()),

        Commands::Hook { event, socket } => handle_hook(event, socket.as_ref()),
    };

    if let Err(e) = result {
        error!(error = %e, "Command failed");
        std::process::exit(1);
    }
}
