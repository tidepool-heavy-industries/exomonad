//! Session management for mantle orchestration.
//!
//! This module provides:
//! - Session metadata types and state tracking
//! - Persistent state storage with file locking
//! - Git worktree management
//! - Session lifecycle operations (start, continue, fork, info, list, cleanup)
//!
//! ## Directory Layout
//!
//! ```text
//! .mantle/
//!   sessions.json        # Session metadata store
//!   worktrees/
//!     <slug>-<hex>/      # Git worktrees for each session
//! ```

pub mod cleanup;
pub mod continue_;
pub mod fork;
pub mod info;
pub mod list;
pub mod start;
pub mod state;
pub mod types;
pub mod worktree;

// Re-export commonly used types
pub use cleanup::{cleanup_sessions, CleanedSession, CleanupConfig, CleanupError, CleanupOutput};
pub use continue_::{continue_session, ContinueConfig, ContinueError};
pub use fork::{fork_session, ForkConfig, ForkError};
pub use info::{session_info, InfoError, SessionInfoOutput};
pub use list::{list_sessions, ListConfig, ListError, ListOutput, SessionSummary};
pub use start::{prepare_session, start_session, StartConfig, StartError};
pub use state::{StateError, StateFile, StateManager};
pub use types::{
    generate_branch_name, generate_session_id, SessionMetadata, SessionOutput, SessionState,
};
pub use worktree::{WorktreeError, WorktreeManager};
