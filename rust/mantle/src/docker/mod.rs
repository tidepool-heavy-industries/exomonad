//! Docker container management for mantle sessions.
//!
//! Provides isolated execution environments for Claude Code sessions
//! using Docker containers managed via the bollard crate.
//!
//! ## Architecture
//!
//! ```text
//! Host                          Container
//! ────                          ─────────
//! .mantle/worktrees/foo/ ──────► /workspace (rw)
//! ~/.claude/ ──────────────────► /root/.claude (ro)
//! /tmp/mantle-fifo/ ───────────► /tmp/mantle (rw)
//!                                   │
//!                                   ▼
//!                               mantle-agent wrap \
//!                                 --result-fifo /tmp/mantle/result.fifo \
//!                                 -- claude -p "..."
//! ```

pub mod container;

pub use container::{
    cleanup_fifo_dir, create_fifo_dir, ContainerConfig, ContainerManager, DockerError, FifoReader,
};
