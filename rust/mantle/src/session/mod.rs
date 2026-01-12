//! Session management for mantle orchestration.
//!
//! This module provides:
//! - Session metadata types
//! - Git worktree management
//! - Session lifecycle operations (start, continue, fork)
//!
//! ## Key Design: Stateless Operation
//!
//! Mantle is stateless - all session data is passed as CLI arguments.
//! The hub tracks session metadata for observability.
//!
//! ## Directory Layout
//!
//! ```text
//! .mantle/
//!   worktrees/
//!     <slug>-<hex>/      # Git worktrees for each session
//! ```

pub mod continue_;
pub mod fork;
pub mod start;
pub mod types;
pub mod worktree;

// Re-export commonly used types
pub use continue_::{continue_session, ContinueConfig, ContinueError};
pub use fork::{fork_session, ForkConfig, ForkError};
pub use start::{prepare_session, start_session, StartConfig, StartError};
pub use types::{
    generate_branch_name, generate_session_id, SessionMetadata, SessionOutput, SessionState,
};
pub use worktree::{WorktreeError, WorktreeManager};
