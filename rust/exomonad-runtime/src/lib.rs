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
//! // 2. Load WASM plugin (None = no extensible effects)
//! let manager = PluginManager::new(
//!     PathBuf::from("wasm-guest.wasm"),
//!     services,
//!     None,
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
//! - **Effects (1)**: `yield_effect` (extensible effects for external consumers)
//!
//! See the individual service modules under [`services`] for details.
//!
//! # Extensible Effects
//!
//! External consumers can define custom effects without forking exomonad:
//!
//! ```rust,ignore
//! use exomonad_runtime::{RuntimeBuilder, effects::{EffectHandler, EffectResult}};
//! use async_trait::async_trait;
//!
//! struct MyHandler;
//!
//! #[async_trait]
//! impl EffectHandler for MyHandler {
//!     fn namespace(&self) -> &str { "my_domain" }
//!     async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
//!         // Decode proto request, handle, encode proto response
//!         todo!()
//!     }
//! }
//!
//! // Build runtime with custom handler
//! let runtime = RuntimeBuilder::new()
//!     .with_effect_handler(MyHandler)
//!     .with_wasm_path("plugin.wasm")
//!     .build();
//! ```

pub mod common;
pub mod effects;
pub mod handlers;
pub mod plugin_manager;
pub mod services;

pub use common::{ErrorCode, ErrorContext, HostError, HostResult};
pub use effects::{EffectError, EffectHandler, EffectRegistry, EffectResult};
pub use handlers::{GitHandler, GitHubHandler, LogHandler};
pub use plugin_manager::PluginManager;
pub use services::Services;

use std::path::PathBuf;
use std::sync::Arc;

/// Builder for constructing a runtime with custom effect handlers.
///
/// The RuntimeBuilder provides a fluent API for configuring the runtime
/// with both builtin effects (git, github, fs) and custom effect handlers
/// from external consumers.
///
/// # Example
///
/// ```rust,ignore
/// use exomonad_runtime::{RuntimeBuilder, EffectHandler};
///
/// let runtime = RuntimeBuilder::new()
///     .with_effect_handler(MyCustomHandler::new())
///     .with_wasm_path("path/to/plugin.wasm")
///     .build()
///     .await?;
/// ```
pub struct RuntimeBuilder {
    registry: EffectRegistry,
    wasm_path: Option<PathBuf>,
    services: Option<Arc<services::ValidatedServices>>,
}

impl RuntimeBuilder {
    /// Create a new runtime builder with an empty effect registry.
    pub fn new() -> Self {
        Self {
            registry: EffectRegistry::new(),
            wasm_path: None,
            services: None,
        }
    }

    /// Register builtin effect handlers (git, github, log).
    ///
    /// This registers handlers for the core namespaces:
    /// - `git.*` - Git operations (get_branch, get_status, etc.)
    /// - `github.*` - GitHub API (list_issues, create_pr, etc.)
    /// - `log.*` - Logging (info, error, emit_event)
    ///
    /// # Arguments
    ///
    /// * `services` - Validated services for the handlers to use
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let services = Arc::new(Services::new().validate()?);
    /// let runtime = RuntimeBuilder::new()
    ///     .with_builtin_handlers(&services)
    ///     .with_wasm_path("plugin.wasm")
    ///     .build()
    ///     .await?;
    /// ```
    pub fn with_builtin_handlers(mut self, services: &Arc<services::ValidatedServices>) -> Self {
        // Register git handler
        self.registry
            .register_owned(handlers::GitHandler::new(services.git().clone()));

        // Register github handler if available
        if let Some(github) = services.github() {
            self.registry
                .register_owned(handlers::GitHubHandler::new(github.clone()));
        }

        // Register log handler
        self.registry.register_owned(handlers::LogHandler::new());

        self
    }

    /// Register a custom effect handler.
    ///
    /// The handler will be dispatched for effects matching its namespace prefix.
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// builder.with_effect_handler(EgregoreHandler::new())
    /// ```
    pub fn with_effect_handler(mut self, handler: impl EffectHandler + 'static) -> Self {
        self.registry.register_owned(handler);
        self
    }

    /// Register an Arc-wrapped effect handler.
    ///
    /// Use this when you need to share the handler with other components.
    pub fn with_effect_handler_arc(mut self, handler: Arc<dyn EffectHandler>) -> Self {
        self.registry.register(handler);
        self
    }

    /// Set the path to the WASM plugin.
    pub fn with_wasm_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.wasm_path = Some(path.into());
        self
    }

    /// Set pre-validated services.
    ///
    /// If not provided, services will be created and validated during build.
    pub fn with_services(mut self, services: Arc<services::ValidatedServices>) -> Self {
        self.services = Some(services);
        self
    }

    /// Get a reference to the effect registry.
    ///
    /// Use this to inspect registered handlers before building.
    pub fn registry(&self) -> &EffectRegistry {
        &self.registry
    }

    /// Consume the builder and return the effect registry.
    ///
    /// This is useful when you want to build the registry separately
    /// from the full runtime (e.g., for testing).
    pub fn into_registry(self) -> EffectRegistry {
        self.registry
    }

    /// Build the runtime with all configured handlers.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - WASM path is not set
    /// - Services validation fails
    /// - WASM plugin loading fails
    pub async fn build(self) -> anyhow::Result<Runtime> {
        let wasm_path = self
            .wasm_path
            .ok_or_else(|| anyhow::anyhow!("WASM path not set"))?;

        let services = match self.services {
            Some(s) => s,
            None => Arc::new(Services::new().validate()?),
        };

        // Convert the owned registry into Arc for sharing with plugin
        let registry = Arc::new(self.registry);
        let plugin_manager =
            PluginManager::new(wasm_path.clone(), services.clone(), Some(registry.clone()))
                .await?;

        Ok(Runtime {
            plugin_manager,
            registry,
            services,
            wasm_path,
        })
    }
}

impl Default for RuntimeBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Configured runtime with WASM plugin and effect handlers.
///
/// The Runtime owns the WASM plugin manager, effect registry, and services.
/// It provides the entry point for MCP tool handling and effect dispatch.
pub struct Runtime {
    /// WASM plugin manager for calling guest functions.
    pub plugin_manager: PluginManager,

    /// Effect registry for custom effect dispatch.
    pub registry: Arc<EffectRegistry>,

    /// Validated services (Git, GitHub, etc.).
    pub services: Arc<services::ValidatedServices>,

    /// Path to the WASM plugin (for reference).
    pub wasm_path: PathBuf,
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

    /// Get a reference to the services.
    pub fn services(&self) -> &services::ValidatedServices {
        &self.services
    }

    /// Dispatch an effect to the appropriate handler.
    ///
    /// Payload and response are protobuf-encoded bytes.
    pub async fn dispatch_effect(
        &self,
        effect_type: &str,
        payload: &[u8],
    ) -> EffectResult<Vec<u8>> {
        self.registry.dispatch(effect_type, payload).await
    }
}
