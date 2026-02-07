//! WASM plugin hosting with single yield_effect host function.
//!
//! All effects flow through one entry point: `yield_effect`. The WASM guest
//! sends an `EffectEnvelope` (protobuf) and receives an `EffectResponse`.
//! The host dispatches to the appropriate handler via `EffectRegistry`.

use crate::effects::{host_fn::yield_effect_host_fn, host_fn::YieldEffectContext, EffectRegistry};
use anyhow::{Context, Result};
use extism::{Manifest, Plugin, PluginBuilder};
use serde::{Deserialize, Serialize};
use std::sync::{Arc, RwLock};

/// Manages the lifecycle of a Haskell WASM plugin.
///
/// Loads a WASM module from embedded bytes and registers a single host function
/// (`yield_effect`) that dispatches all effects through the [`EffectRegistry`].
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
    /// The underlying Extism plugin instance.
    plugin: Arc<RwLock<Plugin>>,
    content_hash: String,
    registry: Arc<EffectRegistry>,
    /// Optional Zellij session name for event emission.
    zellij_session: Option<String>,
}

impl PluginManager {
    /// Load a WASM plugin from bytes and register the yield_effect host function.
    ///
    /// # Arguments
    ///
    /// * `wasm_bytes` - WASM binary content (embedded at compile time)
    /// * `registry` - Effect registry for dispatching all effects
    /// * `zellij_session` - Optional Zellij session name (stored for handler access)
    pub async fn new(
        wasm_bytes: &[u8],
        registry: Arc<EffectRegistry>,
        zellij_session: Option<String>,
    ) -> Result<Self> {
        let hash = sha256_short(wasm_bytes);
        tracing::info!(size = wasm_bytes.len(), hash = %hash, "Loading embedded WASM plugin");

        let manifest = Manifest::new([extism::Wasm::data(wasm_bytes.to_vec())]);

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

        let plugin = PluginBuilder::new(manifest)
            .with_functions(functions)
            .with_wasi(true)
            .build()
            .context("Failed to create plugin")?;

        Ok(Self {
            plugin: Arc::new(RwLock::new(plugin)),
            content_hash: hash,
            registry,
            zellij_session,
        })
    }

    /// Get the SHA256 content hash of the loaded WASM binary (first 12 hex chars).
    pub fn content_hash(&self) -> &str {
        &self.content_hash
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

fn sha256_short(data: &[u8]) -> String {
    use sha2::{Digest, Sha256};
    format!("{:x}", Sha256::digest(data))[..12].to_string()
}