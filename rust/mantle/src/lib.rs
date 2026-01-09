//! mantle: Spawn and manage Claude Code sessions in zellij panes.
//!
//! This library provides the core functionality for running Claude Code
//! sessions in isolated zellij panes with proper supervision, timeout
//! handling, and inter-process communication.
//!
//! ## Modules
//!
//! - [`error`]: Typed error types for all failure modes
//! - [`events`]: Stream event types for parsing Claude Code output
//! - [`fifo`]: FIFO abstractions for IPC (result and signal pipes)
//! - [`humanize`]: Human-readable output formatting for terminal display
//! - [`supervisor`]: Process lifecycle management with timeout/signals
//! - [`protocol`]: Control envelope protocol types (hook events, MCP calls)
//! - [`socket`]: Unix socket client for control envelope communication
//! - [`hooks`]: Hook configuration generation for Claude Code
//!
//! ## Example
//!
//! ```ignore
//! use std::time::Duration;
//! use mantle::{supervisor::Supervisor, fifo::SignalFifo};
//!
//! // Create signal FIFO for interrupt communication
//! let signal_fifo = SignalFifo::new()?;
//!
//! // Build command
//! let cmd = supervisor::build_claude_command(
//!     &["--model", "haiku", "-p", "Hello"],
//!     None,
//!     signal_fifo.path(),
//! );
//!
//! // Spawn with 5 minute timeout
//! let mut sup = Supervisor::spawn(cmd, Some(Duration::from_secs(300)))?;
//!
//! // Process stdout...
//!
//! // Wait with timeout (will kill child if exceeded)
//! let status = sup.wait_with_timeout()?;
//! ```

pub mod error;
pub mod events;
pub mod fifo;
pub mod hooks;
pub mod humanize;
pub mod protocol;
pub mod socket;
pub mod supervisor;
pub mod tui;

// Re-export commonly used types at crate root
pub use error::{Result, ZellijCcError};
pub use events::{InterruptSignal, RunResult, StreamEvent};
pub use fifo::{ResultFifo, SignalFifo};
pub use hooks::{find_mantle_binary, HookConfig};
pub use protocol::{ControlMessage, ControlResponse, HookInput, HookOutput};
pub use socket::ControlSocket;
pub use supervisor::Supervisor;
