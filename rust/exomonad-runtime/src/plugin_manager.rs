use crate::host_functions;
use crate::services::Services;
use anyhow::{Context, Result};
use extism::{Function, Manifest, Plugin, UserData, ValType};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

#[derive(Clone)]
pub struct PluginManager {
    plugin: Arc<RwLock<Plugin>>,
    wasm_path: PathBuf,
}

impl PluginManager {
    pub async fn new(path: PathBuf, _services: Arc<Services>) -> Result<Self> {
        let plugin = Self::load_plugin(&path)?;

        let manager = Self {
            plugin: Arc::new(RwLock::new(plugin)),
            wasm_path: path,
        };

        // Initialize RTS
        // We must call hs_init once.
        manager
            .call::<(), ()>("hs_init", &())
            .await
            .context("Failed to call hs_init")?;

        Ok(manager)
    }

    fn load_plugin(path: &PathBuf) -> Result<Plugin> {
        let manifest = Manifest::new([extism::Wasm::file(path)]);

        let f1 = Function::new(
            "git_get_branch",
            [],
            [ValType::I64],
            UserData::new(()),
            host_functions::git_get_branch,
        );

        let f2 = Function::new(
            "log_info",
            [ValType::I64, ValType::I64], // ptr, len
            [],
            UserData::new(()),
            host_functions::log_info,
        );

        Plugin::new(&manifest, [f1, f2], true).context("Failed to create plugin")
    }

    pub async fn reload(&self, _services: Arc<Services>) -> Result<()> {
        let new_plugin = Self::load_plugin(&self.wasm_path)?;

        // Swap it without blocking the async runtime
        let plugin_lock = self.plugin.clone();
        tokio::task::spawn_blocking(move || {
            let mut guard = plugin_lock.write().unwrap();
            *guard = new_plugin;
        })
        .await
        .context("Failed to join spawn_blocking task")?;

        // Init again
        // Note: The plugin is expected to handle re-initialization gracefully or be stateless enough that calling hs_init again is safe.
        self.call::<(), ()>("hs_init", &())
            .await
            .context("Failed to call hs_init after reload")?;
        Ok(())
    }

    pub async fn call<I, O>(&self, function: &str, input: &I) -> Result<O>
    where
        I: Serialize + Send + Sync + 'static,
        O: for<'de> Deserialize<'de> + Send + 'static,
    {
        let plugin_lock = self.plugin.clone();
        let function_name = function.to_string();
        let input_data = serde_json::to_vec(input)?;

        let result_bytes = tokio::task::spawn_blocking(move || {
            let mut plugin = plugin_lock.write().unwrap();
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
