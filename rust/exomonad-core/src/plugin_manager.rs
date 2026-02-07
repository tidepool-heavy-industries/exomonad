//! WASM plugin hosting with single yield_effect host function.
//!
//! All effects flow through one entry point: `yield_effect`. The WASM guest
//! sends an `EffectEnvelope` (protobuf) and receives an `EffectResponse`.
//! The host dispatches to the appropriate handler via `EffectRegistry`.

use crate::effects::{host_fn::yield_effect_host_fn, host_fn::YieldEffectContext, EffectRegistry};
use anyhow::{Context, Result};
use extism::{Manifest, Plugin, PluginBuilder};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

/// Manages the lifecycle of a Haskell WASM plugin.
///
/// Loads a WASM module and registers a single host function (`yield_effect`)
/// that dispatches all effects through the [`EffectRegistry`].
///
/// # Architecture
///
/// ```text
/// PluginManager::call("handle_mcp_call", input)
///     ↓
/// WASM Guest (Haskell) - pure logic, yields effects
///     ↓
/// yield_effect host function → EffectRegistry::dispatch
///     ↓
/// EffectHandler implementations (git, github, custom, ...)
/// ```
#[derive(Clone)]
pub struct PluginManager {
    plugin: Arc<RwLock<Plugin>>,
    path: PathBuf,
    registry: Arc<EffectRegistry>,
    zellij_session: Option<String>,
}

impl PluginManager {
    /// Load a WASM plugin and register the yield_effect host function.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the wasm32-wasi compiled WASM file
    /// * `registry` - Effect registry for dispatching all effects
    /// * `zellij_session` - Optional Zellij session name (stored for handler access)
    pub async fn new(
        path: PathBuf,
        registry: Arc<EffectRegistry>,
        zellij_session: Option<String>,
    ) -> Result<Self> {
        let plugin = Self::load_plugin(&path, &registry)?;

        Ok(Self {
            plugin: Arc::new(RwLock::new(plugin)),
            path,
            registry,
            zellij_session,
        })
    }

    fn load_plugin(path: &PathBuf, registry: &Arc<EffectRegistry>) -> Result<Plugin> {
        let manifest = Manifest::new([extism::Wasm::file(path)]);

        // Single host function: yield_effect dispatches ALL effects via registry
        let ctx = YieldEffectContext {
            registry: registry.clone(),
        };
        let functions = vec![yield_effect_host_fn(ctx)];

        tracing::info!(
            namespaces = ?registry.namespaces(),
            "Registering yield_effect with {} handler namespaces",
            registry.namespaces().len()
        );

        let mut builder = PluginBuilder::new(manifest)
            .with_functions(functions)
            .with_wasi(true);

        // WASM caching
        let cache_config = PathBuf::from(".exomonad/wasm-cache.toml");
        if cache_config.exists() {
            tracing::info!("Using local WASM cache config: {:?}", cache_config);
            builder = builder.with_cache_config(cache_config);
        } else if let Ok(home) = std::env::var("HOME") {
            let home_cache_config = PathBuf::from(home).join(".exomonad/wasm-cache.toml");
            if home_cache_config.exists() {
                tracing::info!("Using home WASM cache config: {:?}", home_cache_config);
                builder = builder.with_cache_config(home_cache_config);
            } else {
                tracing::debug!("WASM cache disabled: no config found");
            }
        } else {
            tracing::debug!("WASM cache disabled: no config found and HOME not set");
        }

        builder.build().context("Failed to create plugin")
    }

    /// Get the Zellij session name (if configured).
    pub fn zellij_session(&self) -> Option<&str> {
        self.zellij_session.as_deref()
    }

    /// Call a WASM guest function with typed input/output marshalling.
    ///
    /// Input is serialized to JSON, passed to the WASM function, and the
    /// result is deserialized from JSON.
    ///
    /// Uses spawn_blocking because Extism Plugin is not Send.
    pub async fn call<I, O>(&self, function: &str, input: &I) -> Result<O>
    where
        I: Serialize + Send + Sync + 'static,
        O: for<'de> Deserialize<'de> + Send + 'static,
    {
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
            let null: O = serde_json::from_str("null")?;
            return Ok(null);
        }

        let output: O = serde_json::from_slice(&result_bytes)?;
        Ok(output)
    }
}
