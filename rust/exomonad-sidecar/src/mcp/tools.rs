//! MCP tool definitions and WASM execution.
//!
//! All tool logic is in Haskell WASM. This module provides:
//! - Tool schema discovery via WASM (handle_list_tools)
//! - WASM routing for tool execution (handle_mcp_call)

use super::{McpState, ToolDefinition};
use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tracing::debug;

// ============================================================================
// WASM MCP Types (matches Haskell Main.hs MCPCallInput/MCPCallOutput)
// ============================================================================

/// Input for WASM handle_mcp_call function.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct MCPCallInput {
    tool_name: String,
    tool_args: Value,
}

/// Output from WASM handle_mcp_call function.
#[derive(Debug, Deserialize)]
struct MCPCallOutput {
    success: bool,
    result: Option<Value>,
    error: Option<String>,
}

// ============================================================================
// Tool Discovery (via WASM)
// ============================================================================

/// Get all available tool definitions from WASM plugin.
///
/// Calls handle_list_tools in Haskell WASM, which returns the tool schemas
/// defined in ExoMonad.Guest.Tools. This is the single source of truth.
pub async fn get_tool_definitions(state: &McpState) -> Result<Vec<ToolDefinition>> {
    debug!("Fetching tool definitions from WASM");

    // handle_list_tools takes no input (empty object)
    let tools: Vec<ToolDefinition> = state
        .plugin
        .call("handle_list_tools", &())
        .await
        .context("WASM handle_list_tools failed")?;

    debug!(count = tools.len(), "Got tool definitions from WASM");
    Ok(tools)
}

// ============================================================================
// WASM Execution
// ============================================================================

/// Execute a tool by name via WASM.
///
/// All tool calls are routed through handle_mcp_call in Haskell WASM.
/// Haskell handles the pure logic, Rust executes I/O via host functions.
pub async fn execute_tool(state: &McpState, name: &str, args: Value) -> Result<Value> {
    debug!(tool = %name, "Routing tool call through WASM");

    let input = MCPCallInput {
        tool_name: name.to_string(),
        tool_args: args,
    };

    let output: MCPCallOutput = state
        .plugin
        .call("handle_mcp_call", &input)
        .await
        .context("WASM handle_mcp_call failed")?;

    if output.success {
        output
            .result
            .ok_or_else(|| anyhow!("WASM returned success but no result"))
    } else {
        Err(anyhow!(
            output.error.unwrap_or_else(|| "Unknown WASM error".to_string())
        ))
    }
}
