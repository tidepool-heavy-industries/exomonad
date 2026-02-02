//! WASM plugin hosting and host function execution.
//!
//! This crate provides the runtime infrastructure for loading and executing Haskell WASM
//! plugins. All MCP tool logic and hook handling is implemented in Haskell (compiled to
//! wasm32-wasi); this runtime:
//!
//! 1. Loads the WASM module via [`PluginManager`]
//! 2. Registers Rust host functions (git, GitHub, filesystem, etc.)
//! 3. Marshals JSON data in/out across the WASM boundary
//! 4. Executes all I/O via [`Services`]
//!
//! # Architecture
//!
//! ```text
//! Rust Sidecar
//!     ↓
//! PluginManager::call("handle_mcp_call", input)
//!     ↓
//! WASM Guest (Haskell) - pure logic, yields effects
//!     ↓
//! Host Functions (git_get_branch, agent_spawn, etc.) - executes I/O
//!     ↓
//! Services (Git, GitHub, AgentControl, FileSystem, Log)
//!     ↓
//! External systems (git CLI, GitHub API, filesystem)
//! ```
//!
//! # Example
//!
//! ```no_run
//! use exomonad_runtime::{PluginManager, Services};
//! use std::path::PathBuf;
//! use std::sync::Arc;
//!
//! # async fn example() -> anyhow::Result<()> {
//! // 1. Create and validate services
//! let services = Arc::new(Services::new().validate()?);
//!
//! // 2. Load WASM plugin
//! let manager = PluginManager::new(
//!     PathBuf::from("wasm-guest.wasm"),
//!     services
//! ).await?;
//!
//! // 3. Call WASM function
//! let result: serde_json::Value = manager.call(
//!     "handle_mcp_call",
//!     &serde_json::json!({"tool": "git_branch"})
//! ).await?;
//! # Ok(())
//! # }
//! ```
//!
//! # Host Functions
//!
//! The runtime registers 25+ host functions that the WASM guest can call:
//!
//! - **Git (7)**: `git_get_branch`, `git_get_worktree`, `git_get_dirty_files`, etc.
//! - **GitHub (6)**: `github_list_issues`, `github_get_issue`, `github_create_pr`, etc.
//! - **Agent Control (5)**: `agent_spawn`, `agent_cleanup`, `agent_list`, etc.
//! - **Filesystem (2)**: `fs_read_file`, `fs_write_file`
//! - **File PR (1)**: `file_pr` (create/update PRs via gh CLI)
//! - **Copilot Review (1)**: `copilot_poll_review`
//! - **Log (3)**: `log_info`, `log_error`, `emit_event`
//!
//! See the individual service modules under [`services`] for details.

pub mod plugin_manager;
pub mod services;

pub use plugin_manager::PluginManager;
pub use services::Services;
