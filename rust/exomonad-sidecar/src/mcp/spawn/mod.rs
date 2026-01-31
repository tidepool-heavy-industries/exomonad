//! Spawn agents module for creating isolated development environments.
//!
//! Provides MCP tools for spawning, cleaning up, and listing agent worktrees.

pub mod context;
pub mod types;
pub mod worktree;
pub mod zellij;

pub use types::*;
