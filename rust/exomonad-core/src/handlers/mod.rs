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
