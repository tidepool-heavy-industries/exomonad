//! ExoMonad Core: effect system, MCP server, Tidepool backend, shared types.
//!
//! # Architecture
//!
//! ```text
//! McpServer / HookHandler
//!     │
//!     │ Arc<dyn RuntimeBackend>
//!     ▼
//! TidepoolBackend
//! (Cranelift JIT Haskell Core)
//! ├── EffectMachine::run_with_user
//! │   → Haskell Core expr eval
//! │   → Per-tool EffectHandler via HList
//! │     Core ↔ FromCore/ToCore
//! └── Services via EffectHandler impls
//! ```
//!
//! # Features
//!
//! - **`runtime`** (default): Full runtime with Tidepool backend, MCP server,
//!   and all service integrations. This is what the `exomonad` binary uses.
//! - Without `runtime`: Only lightweight UI protocol types (`ui_protocol` module).
//!   Used by `exomonad-plugin` (Zellij WASM target) which can't link heavy native deps.

// === Always available (lightweight types for plugin consumers) ===
pub mod ui_protocol;

// === Framework (requires runtime feature) ===
#[cfg(feature = "runtime")]
pub mod effects;
#[cfg(feature = "runtime")]
pub mod mcp;
#[cfg(feature = "runtime")]
pub mod runtime_backend;
#[cfg(feature = "runtime")]
pub mod tidepool_backend;

// === Shared types and utilities (requires runtime feature) ===
#[cfg(feature = "runtime")]
pub mod common;
#[cfg(feature = "runtime")]
pub mod domain;
#[cfg(feature = "runtime")]
pub mod ffi;
#[cfg(feature = "runtime")]
pub mod error;
#[cfg(feature = "runtime")]
pub mod hooks;
#[cfg(feature = "runtime")]
pub mod logging;
#[cfg(feature = "runtime")]
pub mod protocol;
#[cfg(feature = "runtime")]
pub mod util;

// === Services (requires runtime feature) ===
#[cfg(feature = "runtime")]
pub mod layout;
#[cfg(feature = "runtime")]
pub mod services;

// --- Framework re-exports ---
#[cfg(feature = "runtime")]
pub use effects::EffectContext;
#[cfg(feature = "runtime")]
pub use ffi::FFIBoundary;
#[cfg(feature = "runtime")]
pub use runtime_backend::RuntimeBackend;
#[cfg(feature = "runtime")]
pub use tidepool_backend::TidepoolBackend;

// --- Shared type re-exports ---
#[cfg(feature = "runtime")]
pub use domain::{
    AbsolutePath, AgentName, BirthBranch, BranchName, ClaudeSessionUuid, DomainError, GithubOwner,
    GithubRepo, IssueNumber, PRNumber, PathError, Revision, Role, SessionId, TaskId, ToolName,
    ToolPermission,
};
#[cfg(feature = "runtime")]
pub use error::{ExoMonadError, Result};
#[cfg(feature = "runtime")]
pub use hooks::HookConfig;
#[cfg(feature = "runtime")]
pub use logging::{init_logging, init_logging_with_default};
#[cfg(feature = "runtime")]
pub use protocol::{
    ClaudePreToolUseOutput, ClaudeStopHookOutput, GeminiStopHookOutput, HookEnvelope,
    HookEventType, HookInput, HookSpecificOutput, InternalStopHookOutput, PermissionDecision,
    Runtime as ProtocolRuntime, StopDecision,
};
#[cfg(feature = "runtime")]
pub use util::{build_prompt, find_exomonad_binary, shell_quote};

// --- Service re-exports ---
#[cfg(feature = "runtime")]
pub use services::{validate_gh_cli, validate_git};
