//! Claude Code wrapper implementation.
//!
//! This module provides the core functionality for wrapping Claude Code as a subprocess:
//! - Process supervision with timeout and signal handling
//! - Hook configuration generation
//! - JSONL stream parsing and event collection
//! - TUI or simple output modes
//!
//! ## Architecture
//!
//! ```text
//! wrap_claude()
//!     │
//!     ├── Install signal handlers (SIGINT/SIGTERM forwarding)
//!     ├── Create signal FIFO for interrupt communication
//!     ├── Generate hook configuration (if control socket provided)
//!     ├── Spawn Claude with Supervisor
//!     │
//!     └── Either:
//!         ├── wrap_claude_simple() - println mode for CI
//!         └── wrap_claude_tui() - interactive TUI mode
//! ```

mod collector;
mod simple;
mod tui_mode;

pub use collector::EventCollector;
pub use simple::wrap_claude_simple;
pub use tui_mode::wrap_claude_tui;

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
/// This is the main entry point for running Claude Code. It:
/// 1. Sets up signal handlers for graceful shutdown
/// 2. Creates FIFOs for IPC
/// 3. Generates hook configuration if a control socket is provided
/// 4. Spawns and supervises the Claude process
/// 5. Processes output in TUI or simple mode
/// 6. Writes final result to the result FIFO
///
/// # Arguments
///
/// * `result_fifo` - Path to write the final JSON result
/// * `cwd` - Working directory for Claude Code
/// * `session_tag` - Optional tag for correlating with orchestrator state
/// * `timeout_secs` - Timeout in seconds (0 = no timeout)
/// * `control_socket` - Path to control socket for hook interception
/// * `no_tui` - If true, use simple println mode instead of TUI
/// * `claude_args` - Arguments to pass to Claude
pub fn wrap_claude(
    result_fifo: &Path,
    cwd: Option<&PathBuf>,
    session_tag: Option<&str>,
    timeout_secs: u64,
    control_socket: Option<&PathBuf>,
    no_tui: bool,
    claude_args: &[String],
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

        // Use mantle-agent binary for hooks
        let agent_path = find_mantle_agent_binary();

        info!(
            socket = %socket_path.display(),
            cwd = %effective_cwd.display(),
            agent = %agent_path.display(),
            "Generating hook configuration"
        );

        Some(HookConfig::generate_with_binary(
            &effective_cwd,
            &agent_path,
        )?)
    } else {
        None
    };

    // Build claude command with optional control socket env
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

    // Take stdout for reading
    let stdout = supervisor.take_stdout();

    if no_tui {
        // Simple println mode for CI/headless
        wrap_claude_simple(stdout, &signal_fifo, &mut supervisor, result_fifo, session_tag)
    } else {
        // Full TUI mode
        wrap_claude_tui(stdout, &signal_fifo, &mut supervisor, result_fifo, session_tag)
    }
}
