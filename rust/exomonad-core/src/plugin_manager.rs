//! WASM plugin hosting with single yield_effect host function.
//!
//! All effects flow through one entry point: `yield_effect`. The WASM guest
//! sends an `EffectEnvelope` (protobuf) and receives an `EffectResponse`.
//! The host dispatches to the appropriate handler via `EffectRegistry`.
//!
//! Supports two loading modes:
//! - **Embedded**: WASM bytes compiled into binary via `include_bytes!` (stdio mode)
//! - **File-based**: WASM loaded from disk path with hot reload on mtime change (serve mode)

use crate::effects::{
    host_fn::yield_effect_host_fn, host_fn::YieldEffectContext, EffectContext, EffectRegistry,
};
use anyhow::{Context, Result};
use extism::{Manifest, Plugin, PluginBuilder};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use std::time::{Instant, SystemTime};

/// Result from WASM execution, supporting suspension.
#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "tag", rename_all = "snake_case")]
pub enum WasmResult<T> {
    Done {
        result: T,
    },
    Suspend {
        continuation_id: String,
        effect: EffectRequest,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub struct EffectRequest {
    #[serde(rename = "type")]
    pub effect_type: String,
    pub payload: serde_json::Value,
}

/// Manages the lifecycle of a Haskell WASM plugin.
///
/// Loads a WASM module and registers a single host function (`yield_effect`)
/// that dispatches all effects through the [`EffectRegistry`].
///
/// # Loading Modes
///
/// - `new()`: Embedded bytes (compile-time, no reload)
/// - `from_file()`: File path (runtime, hot reload via mtime check)
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
    /// SHA256 content hash (first 12 hex chars) of loaded WASM.
    content_hash: Arc<RwLock<String>>,
    /// File path for runtime-loaded WASM. None for embedded mode.
    wasm_path: Option<PathBuf>,
    /// Modification time of the last loaded WASM file.
    last_mtime: Arc<RwLock<Option<SystemTime>>>,
    /// Effect registry, stored for reload (recreating plugin requires re-registering host fns).
    registry: Arc<EffectRegistry>,
    /// Agent identity, baked in at construction. Always present.
    ctx: EffectContext,
}

impl PluginManager {
    /// Load a WASM plugin from bytes (embedded mode, no hot reload).
    pub async fn new(
        wasm_bytes: &[u8],
        registry: Arc<EffectRegistry>,
        ctx: EffectContext,
    ) -> Result<Self> {
        let hash = sha256_short(wasm_bytes);
        tracing::info!(size = wasm_bytes.len(), hash = %hash, agent = %ctx.agent_name, "Loading embedded WASM plugin");

        let plugin = build_plugin(wasm_bytes, &registry, &ctx)?;

        Ok(Self {
            plugin: Arc::new(RwLock::new(plugin)),
            content_hash: Arc::new(RwLock::new(hash)),
            wasm_path: None,
            last_mtime: Arc::new(RwLock::new(None)),
            registry,
            ctx,
        })
    }

    /// Load a WASM plugin from a file path (runtime mode, hot reload enabled).
    pub async fn from_file(
        path: &Path,
        registry: Arc<EffectRegistry>,
        ctx: EffectContext,
    ) -> Result<Self> {
        let wasm_bytes = std::fs::read(path)
            .with_context(|| format!("Failed to read WASM from {}", path.display()))?;

        let mtime = std::fs::metadata(path)
            .and_then(|m| m.modified())
            .with_context(|| format!("Failed to get mtime for {}", path.display()))?;

        let hash = sha256_short(&wasm_bytes);
        tracing::info!(
            path = %path.display(),
            size = wasm_bytes.len(),
            hash = %hash,
            agent = %ctx.agent_name,
            "Loading WASM plugin from file"
        );

        let plugin = build_plugin(&wasm_bytes, &registry, &ctx)?;

        Ok(Self {
            plugin: Arc::new(RwLock::new(plugin)),
            content_hash: Arc::new(RwLock::new(hash)),
            wasm_path: Some(path.to_path_buf()),
            last_mtime: Arc::new(RwLock::new(Some(mtime))),
            registry,
            ctx,
        })
    }

    /// Check if the WASM file has changed and reload if so.
    ///
    /// Returns `Ok(true)` if reloaded, `Ok(false)` if unchanged or embedded mode.
    /// Fast path: single stat() syscall when unchanged.
    pub async fn reload_if_changed(&self) -> Result<bool> {
        let path = match &self.wasm_path {
            Some(p) => p.clone(),
            None => return Ok(false), // Embedded mode, no reload
        };

        let current_mtime = std::fs::metadata(&path)
            .and_then(|m| m.modified())
            .with_context(|| format!("Failed to stat WASM file: {}", path.display()))?;

        let stored_mtime = {
            let guard = self
                .last_mtime
                .read()
                .map_err(|e| anyhow::anyhow!("mtime lock poisoned: {}", e))?;
            *guard
        };

        if stored_mtime == Some(current_mtime) {
            return Ok(false); // Fast path: unchanged
        }

        // File changed — reload
        let start = Instant::now();
        let old_hash = {
            let guard = self
                .content_hash
                .read()
                .map_err(|e| anyhow::anyhow!("hash lock poisoned: {}", e))?;
            guard.clone()
        };

        tracing::info!(
            path = %path.display(),
            old_hash = %old_hash,
            "WASM file changed, reloading"
        );

        let wasm_bytes = std::fs::read(&path)
            .with_context(|| format!("Failed to read WASM for reload: {}", path.display()))?;

        let new_hash = sha256_short(&wasm_bytes);
        let new_plugin = build_plugin(&wasm_bytes, &self.registry, &self.ctx)?;
        let size = wasm_bytes.len();

        // Atomic swap
        {
            let mut plugin_guard = self
                .plugin
                .write()
                .map_err(|e| anyhow::anyhow!("plugin lock poisoned: {}", e))?;
            *plugin_guard = new_plugin;
        }
        {
            let mut hash_guard = self
                .content_hash
                .write()
                .map_err(|e| anyhow::anyhow!("hash lock poisoned: {}", e))?;
            *hash_guard = new_hash.clone();
        }
        {
            let mut mtime_guard = self
                .last_mtime
                .write()
                .map_err(|e| anyhow::anyhow!("mtime lock poisoned: {}", e))?;
            *mtime_guard = Some(current_mtime);
        }

        let duration = start.elapsed();
        tracing::info!(
            path = %path.display(),
            old_hash = %old_hash,
            new_hash = %new_hash,
            size,
            duration_ms = duration.as_millis(),
            "WASM reloaded successfully"
        );

        Ok(true)
    }

    /// Get the baked-in agent identity context.
    pub fn effect_context(&self) -> &EffectContext {
        &self.ctx
    }

    /// Get the SHA256 content hash of the loaded WASM binary (first 12 hex chars).
    pub fn content_hash(&self) -> String {
        self.content_hash
            .read()
            .map(|g| g.clone())
            .unwrap_or_else(|_| "unknown".to_string())
    }

    /// Call a WASM guest function with trampoline support for suspend/resume.
    ///
    /// All WASM exports return `WasmResult<O>` (Done | Suspend). On Suspend,
    /// the effect is dispatched asynchronously (lock released), then the WASM
    /// `resume` function is called with the result. Loops until Done.
    pub async fn call<I, O>(&self, function: &str, input: &I) -> Result<O>
    where
        I: Serialize + Send + Sync + 'static,
        O: for<'de> Deserialize<'de> + Send + 'static,
    {
        let mut current_input = serde_json::to_vec(input)?;
        let mut current_function = function.to_string();

        let mut round = 0u32;
        loop {
            round += 1;
            // Acquire lock, call WASM, release lock
            let wasm_result: WasmResult<O> = {
                let plugin_lock = self.plugin.clone();
                let func = current_function.clone();
                let data = current_input.clone();
                let r = round;
                let result_bytes = tokio::task::spawn_blocking(move || -> Result<Vec<u8>> {
                    eprintln!("[trampoline] round={r} func={func} calling WASM ({} bytes input)", data.len());
                    let mut plugin = plugin_lock
                        .write()
                        .map_err(|e| anyhow::anyhow!("Plugin lock poisoned: {}", e))?;
                    let result = plugin.call::<&[u8], Vec<u8>>(&func, &data);
                    eprintln!("[trampoline] round={r} func={func} WASM returned ({} bytes)", result.as_ref().map(|v| v.len()).unwrap_or(0));
                    result
                })
                .await??;

                if result_bytes.is_empty() {
                    return Err(anyhow::anyhow!("Empty result from WASM in async call"));
                }
                serde_json::from_slice(&result_bytes)?
            };
            // Lock is RELEASED here

            match wasm_result {
                WasmResult::Done { result } => return Ok(result),
                WasmResult::Suspend {
                    continuation_id,
                    effect,
                } => {
                    // Execute the async effect WITHOUT holding the lock

                    // Convert payload (Value) to bytes
                    let payload_bytes: Vec<u8> = serde_json::from_value(effect.payload)
                        .context("Failed to parse effect payload as byte array")?;

                    tracing::info!(
                        effect_type = %effect.effect_type,
                        continuation_id = %continuation_id,
                        payload_len = payload_bytes.len(),
                        "Handling suspended effect"
                    );

                    // Dispatch effect — wrap result as EffectResponse protobuf
                    // (same wire format as yield_effect) so WASM can handle errors gracefully
                    use exomonad_proto::effects::EffectResponse;
                    use exomonad_proto::effects::error::effect_response::Result as ResponseResult;
                    use prost::Message as ProstMessage;

                    let dispatch_result = self
                        .registry
                        .dispatch(&effect.effect_type, &payload_bytes, &self.ctx)
                        .await;

                    let response = match dispatch_result {
                        Ok(payload) => EffectResponse {
                            result: Some(ResponseResult::Payload(payload)),
                        },
                        Err(ref err) => {
                            tracing::warn!(
                                effect_type = %effect.effect_type,
                                error = %err,
                                "Effect dispatch failed, wrapping error for WASM"
                            );
                            EffectResponse {
                                result: Some(ResponseResult::Error(
                                    crate::effects::host_fn::to_proto_error(err),
                                )),
                            }
                        }
                    };
                    let effect_result = response.encode_to_vec();

                    // Set up resume call
                    current_function = "resume".to_string();
                    current_input = serde_json::to_vec(&serde_json::json!({
                        "continuation_id": continuation_id,
                        "result": effect_result,
                    }))?;
                    // Loop back → re-acquire lock → call resume
                }
            }
        }
    }
}

/// Build an Extism plugin from WASM bytes with yield_effect host function.
fn build_plugin(
    wasm_bytes: &[u8],
    registry: &Arc<EffectRegistry>,
    effect_ctx: &EffectContext,
) -> Result<Plugin> {
    let manifest = Manifest::new([extism::Wasm::data(wasm_bytes.to_vec())]);

    tracing::info!(
        namespaces = ?registry.namespaces(),
        agent = %effect_ctx.agent_name,
        "Registering yield_effect with {} handler namespaces",
        registry.namespaces().len()
    );

    let ctx = YieldEffectContext {
        registry: registry.clone(),
        ctx: effect_ctx.clone(),
    };
    let functions = vec![yield_effect_host_fn(ctx)];

    PluginBuilder::new(manifest)
        .with_functions(functions)
        .with_wasi(true)
        .build()
        .context("Failed to create WASM plugin")
}

fn sha256_short(data: &[u8]) -> String {
    use sha2::{Digest, Sha256};
    format!("{:x}", Sha256::digest(data))[..12].to_string()
}
