//! mantle-shared: Shared types and utilities for mantle and mantle-agent.
//!
//! This library provides the core functionality shared between the mantle
//! orchestrator (host-side) and mantle-agent (container-side).
//!
//! ## Modules
//!
//! ### Core Types
//! - [`error`]: Typed error types for all failure modes
//! - [`events`]: Stream event types for parsing Claude Code output
//! - [`protocol`]: Control envelope protocol types (hook events, MCP calls)
//!
//! ### IPC
//! - [`fifo`]: FIFO abstractions for IPC (result and signal pipes)
//! - [`hub`]: Hub client for session coordination and visualization
//! - [`socket`]: Unix socket client for control envelope communication
//!
//! ### Process Management
//! - [`supervisor`]: Process lifecycle management with timeout/signals
//! - [`hooks`]: Hook configuration generation for Claude Code
//!
//! ### Output
//! - [`humanize`]: Human-readable output formatting for terminal display
//!
//! ### Commands
//! - [`commands`]: CLI command implementations (hook handling)
//! - [`util`]: Shared utilities (shell quoting, path finding)

// Core types
pub mod error;
pub mod events;
pub mod protocol;

// IPC
pub mod fifo;
pub mod hub;
pub mod socket;

// Process management
pub mod hooks;
pub mod supervisor;

// Output
pub mod humanize;

// Commands
pub mod commands;
pub mod util;

// Re-export commonly used types at crate root
pub use error::{MantleError, Result};
pub use events::{InterruptSignal, RunResult, StreamEvent, ToolCall};
pub use fifo::{ResultFifo, SignalFifo};
pub use hooks::{find_mantle_binary, HookConfig};
pub use protocol::{ControlMessage, ControlResponse, HookInput, HookOutput};
pub use socket::ControlSocket;
pub use supervisor::Supervisor;

// Re-export hub types
pub use hub::{
    HubClient, HubConfig, NodeInfo, NodeRegister, NodeResult, NodeState, SessionCreateResponse,
    SessionInfo, SessionRegister, SessionState,
};

// Re-export command types
pub use commands::{handle_hook, send_signal, HookEventType};
pub use util::{build_prompt, find_mantle_agent_binary, shell_quote};
