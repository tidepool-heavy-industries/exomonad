//! Claude Code wrapper implementation.
//!
//! This module provides the core functionality for wrapping Claude Code as a subprocess:
//! - Process supervision with timeout and signal handling
//! - Hook configuration generation
//! - JSON output parsing and event collection
//!
//! ## Architecture
//!
//! ```text
//! wrap_claude() / wrap_claude_with_hub()
//!     │
//!     └── wrap_claude_internal()
//!             ├── Install signal handlers (SIGINT/SIGTERM forwarding)
//!             ├── Create signal FIFO for interrupt communication
//!             ├── Generate hook configuration (if control socket provided)
//!             ├── Spawn Claude with Supervisor
//!             │
//!             └── wrap_claude_simple_to() - humanized output for terminal
//! ```

mod collector;
mod simple;

pub use collector::{EventCollector, ResultDestination};
pub use simple::{wrap_claude_simple, wrap_claude_simple_to};

use crate::error::Result;
use crate::fifo::SignalFifo;
use crate::hooks::HookConfig;
use crate::supervisor::{build_claude_command, install_signal_handlers, Supervisor};
use crate::util::find_mantle_agent_binary;
use std::path::{Path, PathBuf};
use std::time::Duration;
use tracing::{debug, info};

/// Wrap Claude Code as a subprocess with supervision and output processing.
///
/// Writes final result to the specified FIFO path.
pub fn wrap_claude(
    result_fifo: &Path,
    cwd: Option<&PathBuf>,
    session_tag: Option<&str>,
    timeout_secs: u64,
    control_socket: Option<&PathBuf>,
    claude_args: &[String],
) -> Result<()> {
    let destination = ResultDestination::Fifo(result_fifo);
    wrap_claude_internal(cwd, session_tag, timeout_secs, control_socket, claude_args, destination)
}

/// Wrap Claude Code with result sent to hub socket.
///
/// Writes final result to the hub socket for centralized session tracking.
pub fn wrap_claude_with_hub(
    hub_socket: &Path,
    session_id: &str,
    cwd: Option<&PathBuf>,
    timeout_secs: u64,
    control_socket: Option<&PathBuf>,
    claude_args: &[String],
) -> Result<()> {
    let destination = ResultDestination::HubSocket {
        path: hub_socket,
        session_id,
    };
    wrap_claude_internal(cwd, Some(session_id), timeout_secs, control_socket, claude_args, destination)
}

/// Internal implementation shared by wrap_claude and wrap_claude_with_hub.
fn wrap_claude_internal(
    cwd: Option<&PathBuf>,
    session_tag: Option<&str>,
    timeout_secs: u64,
    control_socket: Option<&PathBuf>,
    claude_args: &[String],
    destination: ResultDestination<'_>,
) -> Result<()> {
    // Install signal handlers for SIGINT/SIGTERM forwarding
    install_signal_handlers();

    // Create signal FIFO for interrupt communication
    let signal_fifo = SignalFifo::new()?;

    // Generate hook configuration if control socket is provided
    let _hook_config = if let Some(socket_path) = control_socket {
        let effective_cwd = cwd
            .cloned()
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

        let agent_path = find_mantle_agent_binary();

        info!(
            socket = %socket_path.display(),
            cwd = %effective_cwd.display(),
            agent = %agent_path.display(),
            "Generating hook configuration"
        );

        Some(HookConfig::generate_with_binary(&effective_cwd, &agent_path)?)
    } else {
        None
    };

    // Build claude command
    let mut cmd = build_claude_command(claude_args, cwd.map(|p| p.as_path()), signal_fifo.path());

    // Set TIDEPOOL_CONTROL_SOCKET env var if provided
    if let Some(socket_path) = control_socket {
        cmd.env("TIDEPOOL_CONTROL_SOCKET", socket_path);
        debug!(socket = %socket_path.display(), "Set TIDEPOOL_CONTROL_SOCKET env");
    }

    // Spawn with timeout if specified (0 = no timeout)
    let timeout = if timeout_secs > 0 {
        Some(Duration::from_secs(timeout_secs))
    } else {
        None
    };
    let mut supervisor = Supervisor::spawn(cmd, timeout)?;
    let stdout = supervisor.take_stdout();

    wrap_claude_simple_to(stdout, &signal_fifo, &mut supervisor, destination, session_tag)
}
