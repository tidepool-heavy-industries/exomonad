//! MCP (Model Context Protocol) types and WASM tool execution.
//!
//! All tool logic is in Haskell WASM. This module provides:
//! - Tool definition types for MCP discovery
//! - WASM call types (MCPCallInput/MCPCallOutput)
//! - Tool schema discovery via WASM

pub mod tools;

use serde::{Deserialize, Serialize};
use serde_json::Value;

// ============================================================================
// MCP Types
// ============================================================================

/// Tool definition for MCP discovery.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolDefinition {
    /// Unique name of the tool (e.g., "git_branch").
    pub name: String,
    /// Human-readable description of what the tool does.
    pub description: String,
    /// JSON Schema for the tool's input arguments.
    #[serde(rename = "inputSchema")]
    pub input_schema: Value,
}
