//! exomonad-shared: Shared types and utilities.
//!
//! This library provides protocol types shared between exomonad-sidecar
//! and other components.
//!
//! ## Modules
//!
//! ### Core Types
//! - [`error`]: Typed error types for all failure modes
//! - [`protocol`]: Hook event types and MCP response types
//!
//! ### Utilities
//! - [`logging`]: Tracing/logging setup with env filter
//! - [`util`]: Shell quoting, binary path resolution
//! - [`hooks`]: Hook configuration generation for Claude Code

// Core types
pub mod error;
pub mod protocol;

// Utilities
pub mod hooks;
pub mod logging;
pub mod util;

// Legacy modules (preserved for reference, may be removed later)
pub mod events;
pub mod fifo;
pub mod humanize;
pub mod supervisor;

// Re-export commonly used types at crate root
pub use error::{ExoMonadError, Result};
pub use hooks::HookConfig;
pub use logging::{init_logging, init_logging_with_default};
pub use protocol::{
    HookEventType, HookInput, HookOutput, HookSpecificOutput, PermissionDecision, Runtime,
};
pub use util::{build_prompt, find_exomonad_binary, shell_quote};
