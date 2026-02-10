//! MCP (Model Context Protocol) server.
//!
//! Implements JSON-RPC over stdin/stdout for MCP client integration.
//! All tool logic is in Haskell WASM. This module handles JSON-RPC transport
//! and forwards tool calls to the WASM plugin via handle_mcp_call.

pub mod stdio;
pub mod tools;

#[cfg(feature = "runtime")]
pub mod agent_identity;

#[cfg(feature = "runtime")]
pub mod handler;

#[cfg(feature = "runtime")]
pub mod http;

use crate::PluginManager;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::path::PathBuf;
use std::sync::Arc;

#[cfg(feature = "runtime")]
use crate::services::questions::QuestionRegistry;

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
    /// Role for this MCP endpoint (e.g. "tl" or "dev").
    /// Controls which tools are exposed. None means all tools.
    pub role: Option<String>,
    /// Shared question registry for resolving ask_question/answer_question bridges.
    #[cfg(feature = "runtime")]
    pub question_registry: Option<Arc<QuestionRegistry>>,
}

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
