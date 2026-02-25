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
pub mod events;
pub mod file_pr;
pub mod fs;
pub mod git;
pub mod github;
pub mod groups;
pub mod kv;
pub mod log;
pub mod merge_pr;
pub mod popup;
pub mod session;
pub use agent::AgentHandler;
pub use copilot::CopilotHandler;
pub use events::EventHandler;
pub use file_pr::FilePRHandler;
pub use fs::FsHandler;

pub use git::GitHandler;
pub use github::GitHubHandler;
pub use groups::{core_handlers, git_handlers, orchestration_handlers};
pub use kv::KvHandler;
pub use log::LogHandler;
pub use merge_pr::MergePRHandler;
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

// ============================================================================
// Macro helpers
// ============================================================================

/// Macro for simple pass-through effect handlers.
///
/// Implements [`EffectHandler`] for a struct by delegating to a dispatch function.
#[macro_export]
macro_rules! impl_pass_through_handler {
    ($handler:ident, $namespace:literal, $dispatch:ident) => {
        #[async_trait::async_trait]
        impl $crate::effects::EffectHandler for $handler {
            fn namespace(&self) -> &str {
                $namespace
            }

            async fn handle(
                &self,
                effect_type: &str,
                payload: &[u8],
                ctx: &$crate::effects::EffectContext,
            ) -> $crate::effects::EffectResult<Vec<u8>> {
                $dispatch(self, effect_type, payload, ctx).await
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_non_empty_empty() {
        assert_eq!(non_empty("".to_string()), None);
    }

    #[test]
    fn test_non_empty_full() {
        assert_eq!(non_empty("hello".to_string()), Some("hello".to_string()));
    }

    #[test]
    fn test_working_dir_or_default_empty() {
        assert_eq!(working_dir_or_default("".to_string()), ".");
    }

    #[test]
    fn test_working_dir_or_default_path() {
        assert_eq!(
            working_dir_or_default("/some/path".to_string()),
            "/some/path"
        );
    }

    #[test]
    fn test_working_dir_path_or_default_empty() {
        assert_eq!(working_dir_path_or_default(""), PathBuf::from("."));
    }

    #[test]
    fn test_working_dir_path_or_default_path() {
        assert_eq!(
            working_dir_path_or_default("/some/path"),
            PathBuf::from("/some/path")
        );
    }
}
