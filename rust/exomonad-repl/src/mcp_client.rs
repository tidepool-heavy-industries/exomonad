use anyhow::{Context, Result};
use exomonad_runtime::{PluginManager, Services};
use exomonad_sidecar::mcp::{tools, McpState, ToolDefinition};
use std::path::PathBuf;
use std::sync::Arc;

pub struct McpClient {
    state: McpState,
}

impl McpClient {
    pub async fn new(wasm_path: PathBuf) -> Result<Self> {
        let services = Arc::new(
            Services::new()
                .validate()
                .context("Failed to validate services")?,
        );

        let plugin = PluginManager::new(wasm_path, services.clone())
            .await
            .context("Failed to load WASM plugin")?;

        let project_dir = std::env::current_dir().context("Failed to get current directory")?;

        Ok(Self {
            state: McpState {
                project_dir,
                plugin: Arc::new(plugin),
            },
        })
    }

    pub async fn list_tools(&self) -> Result<Vec<ToolDefinition>> {
        tools::get_tool_definitions(&self.state).await
    }

    pub async fn call_tool(
        &self,
        name: &str,
        args: serde_json::Value,
    ) -> Result<serde_json::Value> {
        tools::execute_tool(&self.state, name, args).await
    }
}
