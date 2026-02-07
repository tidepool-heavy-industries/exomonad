//! ExoMonad Core: Type-safe effect system and WASM plugin hosting.
//!
//! This crate provides the framework for building LLM agent runtimes with:
//!
//! - **Effect system**: [`EffectHandler`] trait, [`EffectRegistry`] for dispatch
//! - **WASM hosting**: [`PluginManager`] for Haskell WASM plugins via Extism
//! - **MCP server**: Reusable stdio MCP server for Claude Code integration
//! - **Runtime builder**: [`RuntimeBuilder`] for composing handlers into a runtime
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
//! EffectHandler implementations (git, github, custom, ...)
//! ```
//!
//! # Usage
//!
//! ```rust,ignore
//! use exomonad_core::{RuntimeBuilder, EffectHandler, EffectResult};
//! use async_trait::async_trait;
//!
//! struct MyHandler;
//!
//! #[async_trait]
//! impl EffectHandler for MyHandler {
//!     fn namespace(&self) -> &str { "my_domain" }
//!     async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
//!         todo!()
//!     }
//! }
//!
//! let runtime = RuntimeBuilder::new()
//!     .with_effect_handler(MyHandler)
//!     .with_wasm_bytes(wasm_bytes)
//!     .build()
//!     .await?;
//!
//! // Run as MCP stdio server
//! exomonad_core::mcp::stdio::run_stdio_server(runtime.into_mcp_state()).await?;
//! ```

pub mod common;
pub mod effects;
pub mod mcp;
pub mod plugin_manager;

pub use common::{ErrorCode, ErrorContext, HostError, HostResult};
pub use effects::{EffectError, EffectHandler, EffectRegistry, EffectResult};
pub use plugin_manager::PluginManager;

// Re-export protocol types from shared (part of the framework API)
pub use exomonad_shared::protocol;
pub use exomonad_shared::domain;
pub use exomonad_shared::ffi;

use std::path::PathBuf;
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
pub struct RuntimeBuilder {
    registry: EffectRegistry,
    wasm_bytes: Option<Vec<u8>>,
    zellij_session: Option<String>,
}

impl RuntimeBuilder {
    /// Create a new runtime builder with an empty effect registry.
    pub fn new() -> Self {
        Self {
            registry: EffectRegistry::new(),
            wasm_bytes: None,
            zellij_session: None,
        }
    }

    /// Register a custom effect handler.
    ///
    /// The handler will be dispatched for effects matching its namespace prefix.
    pub fn with_effect_handler(mut self, handler: impl EffectHandler + 'static) -> Self {
        self.registry.register_owned(handler);
        self
    }

    /// Register an Arc-wrapped effect handler.
    pub fn with_effect_handler_arc(mut self, handler: Arc<dyn EffectHandler>) -> Self {
        self.registry.register(handler);
        self
    }

    /// Set the WASM plugin bytes (embedded at compile time).
    pub fn with_wasm_bytes(mut self, bytes: Vec<u8>) -> Self {
        self.wasm_bytes = Some(bytes);
        self
    }

    /// Set the Zellij session name for popup/event operations.
    pub fn with_zellij_session(mut self, session: impl Into<String>) -> Self {
        self.zellij_session = Some(session.into());
        self
    }

    /// Get a reference to the effect registry.
    pub fn registry(&self) -> &EffectRegistry {
        &self.registry
    }

    /// Consume the builder and return the effect registry.
    pub fn into_registry(self) -> EffectRegistry {
        self.registry
    }

    /// Build the runtime with all configured handlers.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - WASM bytes are not set
    /// - WASM plugin loading fails
    pub async fn build(self) -> anyhow::Result<Runtime> {
        let wasm_bytes = self
            .wasm_bytes
            .ok_or_else(|| anyhow::anyhow!("WASM bytes not set"))?;

        let registry = Arc::new(self.registry);
        let plugin_manager =
            PluginManager::new(&wasm_bytes, registry.clone(), self.zellij_session).await?;

        Ok(Runtime {
            plugin_manager,
            registry,
        })
    }
}

impl Default for RuntimeBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Configured runtime with WASM plugin and effect handlers.
pub struct Runtime {
    /// WASM plugin manager for calling guest functions.
    pub plugin_manager: PluginManager,

    /// Effect registry for custom effect dispatch.
    pub registry: Arc<EffectRegistry>,
}

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

    /// Convert into MCP state for running the stdio server.
    pub fn into_mcp_state(self, project_dir: PathBuf) -> mcp::McpState {
        mcp::McpState {
            project_dir,
            plugin: Arc::new(self.plugin_manager),
        }
    }
}
