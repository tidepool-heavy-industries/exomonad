use crate::services::agent_control;
use crate::services::copilot_review;
use crate::services::file_pr;
use crate::services::filesystem;
use crate::services::git;
use crate::services::github;
use crate::services::log;
use crate::services::Services;
use anyhow::{Context, Result};
use extism::{Manifest, Plugin};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

#[derive(Clone)]
pub struct PluginManager {
    plugin: Arc<RwLock<Plugin>>,
    wasm_path: PathBuf,
}

impl PluginManager {
    pub async fn new(path: PathBuf, services: Arc<Services>) -> Result<Self> {
        let plugin = Self::load_plugin(&path, &services)?;

        let manager = Self {
            plugin: Arc::new(RwLock::new(plugin)),
            wasm_path: path,
        };

        // Note: GHC RTS initialization (hs_init) is handled automatically by the
        // Extism Haskell PDK when the WASM module is loaded. No explicit call needed.

        Ok(manager)
    }

    fn load_plugin(path: &PathBuf, services: &Services) -> Result<Plugin> {
        let manifest = Manifest::new([extism::Wasm::file(path)]);

        let mut functions = vec![];

        // Git functions (7 functions)
        functions.push(git::git_get_branch_host_fn(services.git.clone()));
        functions.push(git::git_get_worktree_host_fn(services.git.clone()));
        functions.push(git::git_get_dirty_files_host_fn(services.git.clone()));
        functions.push(git::git_get_recent_commits_host_fn(services.git.clone()));
        functions.push(git::git_has_unpushed_commits_host_fn(services.git.clone()));
        functions.push(git::git_get_remote_url_host_fn(services.git.clone()));
        functions.push(git::git_get_repo_info_host_fn(services.git.clone()));

        // NOTE: Docker functions are NOT registered as WASM imports.
        // They are Rust implementation details used internally by Git/GitHub services.
        // Haskell calls high-level effects (GitGetBranch), Rust handles Docker internally.

        // Log functions (3 functions)
        let services_arc = Arc::new(services.clone());
        functions.push(log::log_info_host_fn(services_arc.clone()));
        functions.push(log::log_error_host_fn(services_arc.clone()));
        functions.push(log::emit_event_host_fn(services_arc));

        // GitHub functions (6 functions) - always register, they check GITHUB_TOKEN at runtime
        functions.extend(github::register_host_functions());

        // Agent control functions (5 functions) - high-level agent lifecycle
        functions.extend(agent_control::register_host_functions(
            services.agent_control.clone(),
        ));

        // Filesystem functions (2 functions) - file read/write
        functions.extend(filesystem::register_host_functions(
            services.filesystem.clone(),
        ));

        // File PR functions (1 function) - create/update PRs via gh CLI
        functions.extend(file_pr::register_host_functions());

        // Copilot review functions (1 function) - poll for Copilot review comments
        functions.extend(copilot_review::register_host_functions());

        Plugin::new(&manifest, functions, true).context("Failed to create plugin")
    }

    pub async fn reload(&self, services: Arc<Services>) -> Result<()> {
        let new_plugin = Self::load_plugin(&self.wasm_path, &services)?;

        // Swap it without blocking the async runtime
        let plugin_lock = self.plugin.clone();
        tokio::task::spawn_blocking(move || {
            let mut guard = plugin_lock.write().unwrap();
            *guard = new_plugin;
        })
        .await
        .context("Failed to join spawn_blocking task")?;

        // Note: GHC RTS is automatically initialized when the plugin is loaded.
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
