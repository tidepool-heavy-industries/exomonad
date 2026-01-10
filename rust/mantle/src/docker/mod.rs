//! Docker container execution for mantle sessions.
//!
//! Runs Claude Code directly via `docker run -t` with TTY support.
//! Stream-json output is parsed on the host side.

pub mod container;

pub use container::{run_claude_direct, ContainerConfig, DockerError};
