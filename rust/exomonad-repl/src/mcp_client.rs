use anyhow::{Context, Result};
use exomonad_core::mcp::{tools, McpState, ToolDefinition};
use exomonad_core::RuntimeBuilder;
use exomonad_contrib::services::Services;
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

        let builder = RuntimeBuilder::new().with_wasm_path(&wasm_path);
        let builder = exomonad_contrib::register_builtin_handlers(builder, &services);
        let runtime = builder.build().await.context("Failed to build runtime")?;

        let project_dir = std::env::current_dir().context("Failed to get current directory")?;

        Ok(Self {
            state: runtime.into_mcp_state(project_dir),
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
