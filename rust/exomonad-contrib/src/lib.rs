//! ExoMonad Contrib: Built-in effect handlers and service implementations.
//!
//! Provides ready-to-use handlers for common operations:
//!
//! - **Git**: Branch, status, commits, worktree operations
//! - **GitHub**: Issues, PRs, reviews via GitHub API
//! - **Log**: Structured logging via tracing
//! - **Agent**: Agent lifecycle (spawn, cleanup, list)
//! - **Filesystem**: File read/write operations
//! - **Popup**: Interactive popup UI via Zellij
//! - **FilePR**: Create/update PRs via gh CLI
//! - **Copilot**: Poll for Copilot review comments
//!
//! # Usage
//!
//! ```rust,ignore
//! use exomonad_core::RuntimeBuilder;
//! use exomonad_contrib::prelude::*;
//!
//! let services = Services::new().validate()?;
//! let runtime = RuntimeBuilder::new()
//!     .with_effect_handler(GitHandler::new(services.git().clone()))
//!     .with_effect_handler(LogHandler::new())
//!     .with_wasm_bytes(wasm_bytes)
//!     .build()
//!     .await?;
//! ```

pub mod handlers;
pub mod services;

pub use handlers::{
    AgentHandler, CopilotHandler, FilePRHandler, FsHandler, GitHandler, GitHubHandler, LogHandler,
    PopupHandler,
};
pub use services::{Services, ValidatedServices};

/// Prelude module for convenient imports.
pub mod prelude {
    pub use crate::handlers::*;
    pub use crate::services::{Services, ValidatedServices};
}

/// Register all built-in handlers with a RuntimeBuilder.
///
/// This registers handlers for the core namespaces:
/// - `git.*` - Git operations
/// - `github.*` - GitHub API
/// - `log.*` - Logging
/// - `agent.*` - Agent lifecycle
/// - `fs.*` - Filesystem operations
/// - `popup.*` - Interactive popup UI
/// - `file_pr.*` - PR creation via gh CLI
/// - `copilot.*` - Copilot review polling
pub fn register_builtin_handlers(
    builder: exomonad_core::RuntimeBuilder,
    services: &std::sync::Arc<ValidatedServices>,
) -> exomonad_core::RuntimeBuilder {
    let mut builder = builder;

    // Git handler
    builder = builder.with_effect_handler(handlers::GitHandler::new(services.git().clone()));

    // GitHub handler (if available)
    if let Some(github) = services.github() {
        builder = builder.with_effect_handler(handlers::GitHubHandler::new(github.clone()));
    }

    // Log handler
    builder = builder.with_effect_handler(handlers::LogHandler::new());

    // Agent handler
    builder = builder.with_effect_handler(handlers::AgentHandler::new(
        services.agent_control().clone(),
    ));

    // Filesystem handler
    builder = builder.with_effect_handler(handlers::FsHandler::new(services.filesystem().clone()));

    // Popup handler (requires Zellij session)
    if let Some(session) = services.zellij_session() {
        builder = builder.with_effect_handler(handlers::PopupHandler::new(session.to_string()));
    }

    // FilePR handler
    builder = builder.with_effect_handler(handlers::FilePRHandler::new());

    // Copilot handler
    builder = builder.with_effect_handler(handlers::CopilotHandler::new());

    builder
}
