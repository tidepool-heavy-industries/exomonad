//! WASM backend: wraps PluginManager behind RuntimeBackend.
//!
//! All tool logic is in Haskell WASM. This backend delegates to `PluginManager`
//! for tool discovery, tool execution (with trampoline), and hook dispatch.

use crate::mcp::tools::{MCPCallInput, MCPCallOutput};
use crate::mcp::ToolDefinition;
use crate::plugin_manager::PluginManager;
use crate::runtime_backend::RuntimeBackend;
use anyhow::{Context, Result};
use async_trait::async_trait;
use serde_json::Value;
use tracing::debug;

/// Extism-hosted Haskell WASM backend.
///
/// Wraps a [`PluginManager`] and implements [`RuntimeBackend`] by delegating
/// to WASM guest functions (`handle_list_tools`, `handle_mcp_call`, `handle_pre_tool_use`).
pub struct WasmBackend {
    plugin: PluginManager,
}

impl WasmBackend {
    /// Create a new WasmBackend wrapping a PluginManager.
    pub fn new(plugin: PluginManager) -> Self {
        Self { plugin }
    }

    /// Get a reference to the underlying PluginManager.
    pub fn plugin(&self) -> &PluginManager {
        &self.plugin
    }
}

#[async_trait]
impl RuntimeBackend for WasmBackend {
    async fn list_tools(&self, role: &str) -> Result<Vec<ToolDefinition>> {
        debug!(role = %role, "Fetching tool definitions from WASM");

        let tools: Vec<ToolDefinition> = self
            .plugin
            .call("handle_list_tools", &serde_json::json!({"role": role}))
            .await
            .context("WASM handle_list_tools failed")?;

        debug!(count = tools.len(), role = %role, "Got tool definitions from WASM");
        Ok(tools)
    }

    async fn call_tool(&self, role: &str, tool_name: &str, args: Value) -> Result<MCPCallOutput> {
        let input = MCPCallInput::new(role.to_string(), tool_name.to_string(), args);

        self.plugin
            .call_async("handle_mcp_call", &input)
            .await
            .with_context(|| format!("WASM handle_mcp_call failed for tool '{}'", tool_name))
    }

    async fn handle_hook(&self, input: &Value) -> Result<Value> {
        self.plugin
            .call("handle_pre_tool_use", input)
            .await
            .context("WASM handle_pre_tool_use failed")
    }

    async fn reload_if_changed(&self) -> Result<bool> {
        self.plugin.reload_if_changed().await
    }

    fn content_hash(&self) -> String {
        self.plugin.content_hash()
    }
}
