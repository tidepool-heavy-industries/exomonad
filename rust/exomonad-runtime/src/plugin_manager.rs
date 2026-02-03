use crate::services::agent_control;
use crate::services::copilot_review;
use crate::services::file_pr;
use crate::services::filesystem;
use crate::services::git;
use crate::services::github;
use crate::services::log;
use crate::services::ValidatedServices;
use anyhow::{Context, Result};
use extism::{Manifest, Plugin, PluginBuilder};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::sync::{Arc, RwLock};
use tracing;

/// Manages the lifecycle of a Haskell WASM plugin.
///
/// The PluginManager loads a WASM module compiled from Haskell (via GHC's wasm32-wasi target)
/// and provides the interface for calling exported functions within that module. All MCP tool
/// logic and hook handling is implemented in the Haskell WASM guest; this manager handles:
///
/// - Loading the WASM module via Extism
/// - Registering Rust host functions (git, GitHub, filesystem, etc.) that the WASM can call
/// - Thread-safe access to the plugin via RwLock
/// - Marshalling JSON data in/out across the WASM boundary
///
/// # Architecture
///
/// ```text
/// Rust Sidecar
///     ↓
/// PluginManager::call("handle_mcp_call", input)
///     ↓
/// WASM Guest (Haskell) - pure logic, yields effects
///     ↓
/// Host Functions (git_get_branch, agent_spawn, etc.) - executes I/O
///     ↓
/// Result marshalled back through WASM → Haskell → Rust
/// ```
///
/// # Example
///
/// ```no_run
/// use exomonad_runtime::{PluginManager, services::Services};
/// use std::path::PathBuf;
/// use std::sync::Arc;
///
/// # async fn example() -> anyhow::Result<()> {
/// let services = Arc::new(Services::new_local()?.validate()?);
/// let manager = PluginManager::new(
///     PathBuf::from("wasm-guest.wasm"),
///     services
/// ).await?;
///
/// // Call WASM function with typed input/output
/// let result: serde_json::Value = manager.call(
///     "handle_mcp_call",
///     &serde_json::json!({"tool": "git_branch"})
/// ).await?;
/// # Ok(())
/// # }
/// ```
#[derive(Clone)]
pub struct PluginManager {
    /// The loaded Extism plugin, wrapped for thread-safe access.
    ///
    /// Uses RwLock to allow concurrent reads (though in practice we always need write
    /// access for calls). The Plugin itself is not Send, so we use spawn_blocking.
    plugin: Arc<RwLock<Plugin>>,
    
    /// Path to the WASM module (kept for reloading).
    path: PathBuf,
    
    /// Services (kept for reloading).
    services: Arc<ValidatedServices>,
    
    /// Number of calls made to this plugin.
    call_count: Arc<std::sync::atomic::AtomicUsize>,
}

impl PluginManager {
    /// Load a WASM plugin and register all host functions.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the wasm32-wasi compiled WASM file
    /// * `services` - Validated service container (Git, GitHub, AgentControl, FileSystem, etc.)
    ///
    /// # Host Functions Registered
    ///
    /// - **Git (7)**: `git_get_branch`, `git_get_worktree`, `git_get_dirty_files`, etc.
    /// - **GitHub (6)**: `github_list_issues`, `github_get_issue`, `github_create_pr`, etc.
    /// - **Agent Control (5)**: `agent_spawn`, `agent_cleanup`, `agent_list`, etc.
    /// - **Filesystem (2)**: `fs_read_file`, `fs_write_file`
    /// - **File PR (1)**: `file_pr` (create/update PRs via gh CLI)
    /// - **Copilot Review (1)**: `copilot_poll_review`
    /// - **Log (3)**: `log_info`, `log_error`, `emit_event`
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - WASM file doesn't exist or is invalid
    /// - Extism fails to create the plugin
    /// - Host function registration fails
    ///
    /// # Notes
    ///
    /// GHC RTS initialization (hs_init) is handled automatically by the Extism Haskell PDK
    /// when the module loads. No explicit initialization is needed.
    pub async fn new(path: PathBuf, services: Arc<ValidatedServices>) -> Result<Self> {
        let plugin = Self::load_plugin(&path, &services)?;

        // Note: GHC RTS initialization (hs_init) is handled automatically by the
        // Extism Haskell PDK when the WASM module is loaded. No explicit call needed.

        Ok(Self {
            plugin: Arc::new(RwLock::new(plugin)),
            path,
            services,
            call_count: Arc::new(std::sync::atomic::AtomicUsize::new(0)),
        })
    }

    fn load_plugin(path: &PathBuf, services: &ValidatedServices) -> Result<Plugin> {
        let manifest = Manifest::new([extism::Wasm::file(path)]);

        // NOTE: Docker functions are NOT registered as WASM imports.
        // They are Rust implementation details used internally by Git/GitHub services.
        // Haskell calls high-level effects (GitGetBranch), Rust handles Docker internally.

        let services_arc = Arc::new(services.clone());

        // Git functions (7) + Log functions (3)
        let mut functions = vec![
            git::git_get_branch_host_fn(services.git().clone()),
            git::git_get_worktree_host_fn(services.git().clone()),
            git::git_get_dirty_files_host_fn(services.git().clone()),
            git::git_get_recent_commits_host_fn(services.git().clone()),
            git::git_has_unpushed_commits_host_fn(services.git().clone()),
            git::git_get_remote_url_host_fn(services.git().clone()),
            git::git_get_repo_info_host_fn(services.git().clone()),
            log::log_info_host_fn(services_arc.clone()),
            log::log_error_host_fn(services_arc.clone()),
            log::emit_event_host_fn(services_arc),
        ];

        // GitHub functions (6 functions) - always register, they check GITHUB_TOKEN at runtime
        functions.extend(github::register_host_functions());

        // Agent control functions (5 functions) - high-level agent lifecycle
        functions.extend(agent_control::register_host_functions(
            services.agent_control().clone(),
        ));

        // Filesystem functions (2 functions) - file read/write
        functions.extend(filesystem::register_host_functions(
            services.filesystem().clone(),
        ));

        // File PR functions (1 function) - create/update PRs via gh CLI
        functions.extend(file_pr::register_host_functions());

        // Copilot review functions (1 function) - poll for Copilot review comments
        functions.extend(copilot_review::register_host_functions());

        let mut builder = PluginBuilder::new(manifest)
            .with_functions(functions)
            .with_wasi(true);

        // Try to enable WASM caching if config exists
        match std::env::var("HOME") {
            Ok(home) => {
                let cache_config = PathBuf::from(home).join(".exomonad/wasm-cache.toml");
                if cache_config.exists() {
                    tracing::info!("Using WASM cache config: {:?}", cache_config);
                    builder = builder.with_cache_config(cache_config);
                } else {
                    tracing::debug!(
                        "WASM cache disabled: config file not found at {:?}",
                        cache_config
                    );
                }
            }
            Err(_) => {
                tracing::debug!("WASM cache disabled: HOME environment variable is not set");
            }
        }

        builder.build().context("Failed to create plugin")
    }

    /// Reload the plugin to clear memory/state.
    async fn reload(&self) -> Result<()> {
        let path = self.path.clone();
        let services = self.services.clone();
        
        let new_plugin = tokio::task::spawn_blocking(move || {
            Self::load_plugin(&path, &services)
        }).await??;

        let mut lock = self.plugin.write().map_err(|e| anyhow::anyhow!("Plugin lock poisoned: {}", e))?;
        *lock = new_plugin;
        
        // Reset counter
        self.call_count.store(0, std::sync::atomic::Ordering::SeqCst);
        
        Ok(())
    }

    /// Call a WASM guest function with typed input/output marshalling.
    ///
    /// This is the core method for invoking Haskell logic in the WASM guest. Input is
    /// serialized to JSON, passed to the WASM function, and the result is deserialized
    /// from JSON.
    ///
    /// # Type Parameters
    ///
    /// * `I` - Input type (must be serializable to JSON)
    /// * `O` - Output type (must be deserializable from JSON)
    ///
    /// # Arguments
    ///
    /// * `function` - Name of the exported WASM function (e.g., "handle_mcp_call", "handle_hook")
    /// * `input` - Input data (will be JSON-serialized)
    ///
    /// # Returns
    ///
    /// The deserialized output from the WASM function.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - Input serialization fails
    /// - Lock is poisoned (previous panic while holding lock)
    /// - WASM function doesn't exist or execution fails
    /// - Output deserialization fails
    ///
    /// # Thread Safety
    ///
    /// Uses spawn_blocking to avoid blocking the tokio runtime during WASM execution.
    /// The Extism Plugin is not Send, so we must execute calls on a blocking thread.
    ///
    /// # Example
    ///
    /// ```no_run
    /// # use exomonad_runtime::PluginManager;
    /// # async fn example(manager: &PluginManager) -> anyhow::Result<()> {
    /// use serde_json::json;
    ///
    /// // Call handle_mcp_call function
    /// let result: serde_json::Value = manager.call(
    ///     "handle_mcp_call",
    ///     &json!({
    ///         "id": "call_123",
    ///         "tool_name": "git_branch",
    ///         "arguments": {}
    ///     })
    /// ).await?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// # Notes
    ///
    /// - Empty WASM responses are treated as JSON `null`
    /// - Function names are not validated at compile time (runtime error if function doesn't exist)
    /// - Consider using typed wrapper methods (e.g., `handle_mcp_call()`) for type safety
    pub async fn call<I, O>(&self, function: &str, input: &I) -> Result<O>
    where
        I: Serialize + Send + Sync + 'static,
        O: for<'de> Deserialize<'de> + Send + 'static,
    {
        // Check if we need to reload (every 100 calls) to mitigate memory leaks
        let count = self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        if count >= 100 {
            tracing::info!("Reloading WASM plugin to clear memory (call count: {})", count);
            if let Err(e) = self.reload().await {
                tracing::error!("Failed to reload plugin: {}", e);
                // Continue with existing plugin if reload fails
            }
        }
    
        let plugin_lock = self.plugin.clone();
        let function_name = function.to_string();
        let input_data = serde_json::to_vec(input)?;

        let result_bytes = tokio::task::spawn_blocking(move || -> Result<Vec<u8>> {
            let mut plugin = plugin_lock
                .write()
                .map_err(|e| anyhow::anyhow!("Plugin lock poisoned: {}", e))?;
            plugin.call::<&[u8], Vec<u8>>(&function_name, &input_data)
        })
        .await??;

        if result_bytes.is_empty() {
            // Handle empty response as null
            let null: O = serde_json::from_str("null")?;
            return Ok(null);
        }

        let output: O = serde_json::from_slice(&result_bytes)?;
        Ok(output)
    }
}
