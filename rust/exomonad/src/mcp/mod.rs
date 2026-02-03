//! MCP (Model Context Protocol) server for Claude Code integration.
//!
//! Implements JSON-RPC over stdin/stdout for Claude Code's native MCP support.
//! All tool logic is in Haskell WASM. This module handles JSON-RPC transport
//! and forwards tool calls to the WASM plugin via handle_mcp_call.

pub mod stdio;
pub mod tools;

use exomonad_runtime::PluginManager;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::path::PathBuf;
use std::sync::Arc;

// ============================================================================
// State
// ============================================================================

/// Shared state for MCP server.
#[derive(Clone)]
pub struct McpState {
    /// Working directory for git operations (used for logging).
    pub project_dir: PathBuf,
    /// WASM plugin for routing tool calls through Haskell.
    /// All tool calls are routed through handle_mcp_call in WASM.
    pub plugin: Arc<PluginManager>,
}

// ============================================================================
// MCP Types
// ============================================================================

/// Tool definition for MCP discovery.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolDefinition {
    pub name: String,
    pub description: String,
    #[serde(rename = "inputSchema")]
    pub input_schema: Value,
}
