//! Shared agent-control seam: launch-command construction and the launch types
//! (`AgentType`, `ClaudeSpawnFlags`) that both classic mode and node mode need.
//!
//! The high-level `AgentControlService<C>` and its spawn/cleanup machinery stay
//! in classic `exomonad-core`; only the type definitions and the pure
//! command-building free functions live here.

pub mod fork_session;
pub mod launch;
mod types;

pub use launch::ClaudeInvocation;
pub use types::{AgentType, ClaudeSpawnFlags};
