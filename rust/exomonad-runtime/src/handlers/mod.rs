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

pub mod git;
pub mod github;
pub mod log;

pub use git::GitHandler;
pub use github::GitHubHandler;
pub use log::LogHandler;
