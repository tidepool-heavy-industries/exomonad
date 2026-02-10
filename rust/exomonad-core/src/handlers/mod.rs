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
pub mod copilot;
pub mod file_pr;
pub mod fs;
pub mod git;
pub mod github;
pub mod log;
pub mod messaging;
pub mod popup;
pub mod teams;

pub use agent::AgentHandler;
pub use copilot::CopilotHandler;
pub use file_pr::FilePRHandler;
pub use fs::FsHandler;
pub use git::GitHandler;
pub use github::GitHubHandler;
pub use log::LogHandler;
pub use messaging::MessagingHandler;
pub use popup::PopupHandler;
pub use teams::TeamsHandler;
