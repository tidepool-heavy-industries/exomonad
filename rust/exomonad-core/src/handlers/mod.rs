//! Namespace-based effect handlers for the extensible effects system.
//!
//! Each handler owns a namespace and routes effects to the underlying service.
//!
//! # Architecture
//!
//! ```text
//! yield_effect "git.get_branch" {...}
//!     │
//!     │ EffectRegistry::dispatch
//!     ▼
//! GitHandler (namespace = "git")
//!     │
//!     │ match "get_branch"
//!     ▼
//! GitService::get_branch(dir)
//! ```

pub mod agent;
pub mod coordination;
pub mod copilot;
pub mod events;
pub mod file_pr;
pub mod fs;
pub mod git;
pub mod github;
pub mod groups;
pub mod jj;
pub mod kv;
pub mod log;
pub mod merge_pr;
pub mod messaging;
pub mod popup;
pub mod session;

pub use agent::AgentHandler;
pub use coordination::CoordinationHandler;
pub use copilot::CopilotHandler;
pub use events::EventHandler;
pub use file_pr::FilePRHandler;
pub use fs::FsHandler;

pub use git::GitHandler;
pub use github::GitHubHandler;
pub use groups::{core_handlers, git_handlers, orchestration_handlers};
pub use jj::JjHandler;
pub use kv::KvHandler;
pub use log::LogHandler;
pub use merge_pr::MergePRHandler;
pub use messaging::MessagingHandler;
pub use popup::PopupHandler;
pub use session::SessionHandler;

// ============================================================================
// Proto field helpers — shared across handlers
// ============================================================================

/// Convert an empty proto string to None, non-empty to Some.
///
/// Proto3 uses empty string as the default for string fields.
/// Handlers that pass optional strings to services need this everywhere.
pub fn non_empty(s: String) -> Option<String> {
    if s.is_empty() {
        None
    } else {
        Some(s)
    }
}

/// Default an empty proto working_dir field to ".".
pub fn working_dir_or_default(dir: String) -> String {
    if dir.is_empty() {
        ".".to_string()
    } else {
        dir
    }
}

/// Default an empty proto working_dir field to PathBuf::from(".").
pub fn working_dir_path_or_default(dir: &str) -> std::path::PathBuf {
    if dir.is_empty() {
        std::path::PathBuf::from(".")
    } else {
        std::path::PathBuf::from(dir)
    }
}
