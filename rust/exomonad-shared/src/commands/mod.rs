//! CLI command implementations.
//!
//! This module provides the core logic for exomonad CLI commands that can be
//! shared between `exomonad` and `exomonad` binaries.
//!
//! ## Commands
//!
//! - [`signal`]: Send interrupt signals to the wrapper process
//! - [`hook`]: Handle Claude Code hook events via control socket

pub mod hook;
pub mod signal;

pub use hook::{default_allow_response, handle_hook, HookEventType};
pub use signal::send_signal;
