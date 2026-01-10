//! Docker container management for mantle sessions.
//!
//! Provides isolated execution environments for Claude Code sessions
//! using Docker containers.
//!
//! ## Execution Modes
//!
//! ### Attached Mode (Primary)
//! Container runs in foreground via `docker run`. Dies when parent dies.
//! Result written to stdout, captured by parent.
//!
//! ```text
//! mantle (parent)
//!   └── docker run --rm mantle-agent:latest wrap --stdout ...
//!         └── mantle-agent wrap --stdout
//!               └── claude -p "..."
//! ```
//!
//! ### Detached Mode (via bollard)
//! For cleanup operations, container inspection, and hub socket mode.

pub mod container;

pub use container::{run_attached, ContainerConfig, ContainerManager, DockerError};
