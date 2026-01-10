//! mantle: Spawn and manage Claude Code sessions.
//!
//! This crate provides the orchestrator binary for managing Claude Code
//! sessions in Docker containers or zellij panes.
//!
//! ## Modules
//!
//! - [`session`]: Session lifecycle management (start, continue, fork)
//! - [`docker`]: Docker container management via bollard
//!
//! Core types and utilities are re-exported from `mantle-shared`.

pub mod docker;
pub mod session;

// Re-export everything from mantle-shared for backwards compatibility
pub use mantle_shared::*;
