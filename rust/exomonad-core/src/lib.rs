//! ExoMonad Core: effect system, WASM hosting, MCP server, built-in handlers, shared types.
//!
//! # Architecture
//!
//! ```text
//! WASM Guest (Haskell) - pure logic
//!     │
//!     │ yield_effect(EffectEnvelope)
//!     ▼
//! PluginManager (single host function: yield_effect)
//!     │
//!     │ EffectRegistry::dispatch by namespace
//!     ▼
//! EffectHandler implementations (git, github, agent, fs, ...)
//! ```
//!
//! # Features
//!
//! - **`runtime`** (default): Full runtime with WASM hosting, effect handlers, MCP server,
//!   and all service integrations. This is what the `exomonad` binary uses.
//! - Without `runtime`: Only lightweight UI protocol types (`ui_protocol` module).
//!   Used by `exomonad-plugin` (Zellij WASM target) which can't link heavy native deps.
//!
//! # Usage
//!
//! External consumers pick handler groups and add custom domain handlers:
//!
//! ```rust,ignore
//! use exomonad_core::{RuntimeBuilder, core_handlers, git_handlers, EffectHandler, EffectResult};
//! use exomonad_core::mcp::{McpServer, McpState};
//!
//! // Pick only the handler groups you need
//! let mut builder = RuntimeBuilder::new()
//!     .with_wasm_path(wasm_path)
//!     .with_handlers(core_handlers(project_dir.clone()))
//!     .with_handlers(git_handlers(git, github));
//!
//! // Add custom domain handlers
//! builder = builder.with_effect_handler(MyDomainHandler::new());
//!
//! let rt = builder.build().await?;
//!
//! // Embed MCP server
//! let state = McpState::builder(Arc::new(rt.plugin_manager), project_dir).build();
//! McpServer::new(state).serve(addr).await?;
//! ```

// === Always available (lightweight types for plugin consumers) ===
pub mod ui_protocol;

// === Framework (requires runtime feature) ===
#[cfg(feature = "runtime")]
pub mod common;
#[cfg(feature = "runtime")]
pub mod effects;
#[cfg(feature = "runtime")]
pub mod mcp;
#[cfg(feature = "runtime")]
pub mod plugin_manager;

// === Shared types and utilities (requires runtime feature) ===
#[cfg(feature = "runtime")]
pub mod domain;
#[cfg(feature = "runtime")]
pub mod error;
#[cfg(feature = "runtime")]
pub mod ffi;
#[cfg(feature = "runtime")]
pub mod hooks;
#[cfg(feature = "runtime")]
pub mod logging;
#[cfg(feature = "runtime")]
pub mod protocol;
#[cfg(feature = "runtime")]
pub mod util;

// === Handlers and services (requires runtime feature) ===
#[cfg(feature = "runtime")]
pub mod handlers;
#[cfg(feature = "runtime")]
pub mod layout;
#[cfg(feature = "runtime")]
pub mod services;

// --- Framework re-exports ---
#[cfg(feature = "runtime")]
pub use common::{ErrorCode, ErrorContext, HostError, HostResult};
#[cfg(feature = "runtime")]
pub use effects::{EffectError, EffectHandler, EffectRegistry, EffectResult};
#[cfg(feature = "runtime")]
pub use plugin_manager::PluginManager;

// --- Shared type re-exports ---
#[cfg(feature = "runtime")]
pub use domain::{
    AbsolutePath, AgentName, BirthBranch, ClaudeSessionUuid, DomainError, GithubOwner, GithubRepo,
    IssueNumber, PathError, Role, SessionId, ToolName, ToolPermission,
};
#[cfg(feature = "runtime")]
pub use error::{ExoMonadError, Result};
#[cfg(feature = "runtime")]
pub use ffi::{
    ErrorCode as FFIErrorCode, ErrorContext as FFIErrorContext, FFIBoundary, FFIError, FFIResult,
};
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

// --- Handler re-exports ---
#[cfg(feature = "runtime")]
pub use handlers::groups::{core_handlers, git_handlers, orchestration_handlers};
#[cfg(feature = "runtime")]
pub use handlers::{
    AgentHandler, CoordinationHandler, CopilotHandler, EventHandler, FilePRHandler, FsHandler,
    GitHandler, GitHubHandler, JjHandler, KvHandler, LogHandler, MergePRHandler, MessagingHandler,
    PopupHandler,
};
#[cfg(feature = "runtime")]
pub use services::{validate_gh_cli, validate_git};

/// Prelude module for convenient imports.
#[cfg(feature = "runtime")]
pub mod prelude {
    pub use crate::handlers::*;
}

#[cfg(feature = "runtime")]
use std::path::PathBuf;
#[cfg(feature = "runtime")]
use std::sync::Arc;

/// Builder for constructing a runtime with custom effect handlers.
///
/// # Example
///
/// ```rust,ignore
/// use exomonad_core::{RuntimeBuilder, EffectHandler};
///
/// let runtime = RuntimeBuilder::new()
///     .with_effect_handler(MyCustomHandler::new())
///     .with_wasm_bytes(wasm_bytes)
///     .build()
///     .await?;
/// ```
#[cfg(feature = "runtime")]
pub struct RuntimeBuilder {
    registry: EffectRegistry,
    wasm_bytes: Option<Vec<u8>>,
    wasm_path: Option<PathBuf>,
    required_namespaces: Option<Vec<String>>,
}

#[cfg(feature = "runtime")]
impl RuntimeBuilder {
    /// Create a new runtime builder with an empty effect registry.
    pub fn new() -> Self {
        Self {
            registry: EffectRegistry::new(),
            wasm_bytes: None,
            wasm_path: None,
            required_namespaces: None,
        }
    }

    /// Register a custom effect handler.
    ///
    /// The handler will be dispatched for effects matching its namespace prefix.
    pub fn with_effect_handler(mut self, handler: impl EffectHandler + 'static) -> Self {
        self.registry.register_owned(handler);
        self
    }

    /// Register multiple effect handlers at once.
    ///
    /// Useful with handler group functions like `core_handlers()`, `git_handlers()`.
    pub fn with_handlers(mut self, handlers: Vec<Box<dyn EffectHandler>>) -> Self {
        for handler in handlers {
            self.registry.register_boxed(handler);
        }
        self
    }

    /// Set the WASM plugin bytes (embedded at compile time).
    pub fn with_wasm_bytes(mut self, bytes: Vec<u8>) -> Self {
        self.wasm_bytes = Some(bytes);
        self
    }

    /// Set a WASM file path for runtime loading with hot reload support.
    ///
    /// When set, the plugin will be loaded from this file instead of embedded bytes,
    /// and `PluginManager::reload_if_changed()` can detect file modifications.
    pub fn with_wasm_path(mut self, path: PathBuf) -> Self {
        self.wasm_path = Some(path);
        self
    }

    /// Require that these effect namespaces have registered handlers.
    ///
    /// If set, `build()` will fail if any namespace is missing from the registry.
    /// This catches configuration errors early (e.g. forgetting to register git handlers).
    pub fn require_namespaces(mut self, namespaces: Vec<String>) -> Self {
        self.required_namespaces = Some(namespaces);
        self
    }

    /// Get a reference to the effect registry.
    pub fn registry(&self) -> &EffectRegistry {
        &self.registry
    }

    /// Build the runtime with all configured handlers.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - WASM bytes are not set
    /// - WASM plugin loading fails
    pub async fn build(self) -> anyhow::Result<Runtime> {
        // Validate required namespaces before loading WASM
        if let Some(ref required) = self.required_namespaces {
            let mut missing: Vec<_> = required
                .iter()
                .filter(|ns| !self.registry.has_handler(ns))
                .collect();
            if !missing.is_empty() {
                missing.sort();
                let mut registered = self.registry.namespaces();
                registered.sort();
                anyhow::bail!(
                    "Missing effect handlers for namespaces: {:?}\nRegistered: {:?}",
                    missing,
                    registered
                );
            }
        }

        let registry = Arc::new(self.registry);

        let plugin_manager = if let Some(path) = self.wasm_path {
            PluginManager::from_file(&path, registry.clone()).await?
        } else {
            let wasm_bytes = self.wasm_bytes.ok_or_else(|| {
                anyhow::anyhow!("WASM bytes not set — provide either wasm_bytes or wasm_path")
            })?;
            PluginManager::new(&wasm_bytes, registry.clone()).await?
        };

        Ok(Runtime {
            plugin_manager,
            registry,
        })
    }
}

#[cfg(feature = "runtime")]
impl Default for RuntimeBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Configured runtime with WASM plugin and effect handlers.
#[cfg(feature = "runtime")]
pub struct Runtime {
    /// WASM plugin manager for calling guest functions.
    pub plugin_manager: PluginManager,

    /// Effect registry for custom effect dispatch.
    pub registry: Arc<EffectRegistry>,
}

#[cfg(feature = "runtime")]
impl Runtime {
    /// Get a reference to the plugin manager.
    pub fn plugin_manager(&self) -> &PluginManager {
        &self.plugin_manager
    }

    /// Get a reference to the effect registry.
    pub fn registry(&self) -> &EffectRegistry {
        &self.registry
    }

    /// Dispatch an effect to the appropriate handler.
    pub async fn dispatch_effect(
        &self,
        effect_type: &str,
        payload: &[u8],
    ) -> EffectResult<Vec<u8>> {
        self.registry.dispatch(effect_type, payload).await
    }

    /// Convert into MCP state for running the MCP server.
    pub fn into_mcp_state(self, project_dir: PathBuf) -> mcp::McpState {
        mcp::McpState {
            project_dir,
            plugin: Arc::new(self.plugin_manager),
            role: None,
            question_registry: None,
        }
    }
}

#[cfg(all(test, feature = "runtime"))]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_namespace_validation_missing() {
        let builder = RuntimeBuilder::new()
            .require_namespaces(vec!["nonexistent".to_string()])
            .with_wasm_bytes(vec![]); // dummy, won't get this far

        let result = builder.build().await;

        match result {
            Ok(_) => panic!("Expected error but got Ok"),
            Err(e) => {
                let err = e.to_string();
                assert!(
                    err.contains("nonexistent"),
                    "Error should mention missing namespace: {}",
                    err
                );
            }
        }
    }
}
