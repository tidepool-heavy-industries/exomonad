//! Runtime backend trait for the execution engine.
//!
//! All MCP tool routing, hook handling, and hot reload go through this trait.
//! Currently implemented by `TidepoolBackend` (Cranelift-compiled Haskell Core).

use crate::mcp::{tools::MCPCallOutput, ToolDefinition};
use anyhow::Result;
use async_trait::async_trait;
use serde_json::Value;

/// Backend-agnostic runtime for executing MCP tools and hooks.
///
/// Implementations handle the full lifecycle: tool discovery, tool execution
/// (including trampoline/suspend for async effects), hook dispatch, and
/// source change detection.
///
/// Current backend:
/// - [`TidepoolBackend`](crate::tidepool_backend::TidepoolBackend): Cranelift-compiled
///   Haskell Core via tidepool
#[async_trait]
pub trait RuntimeBackend: Send + Sync {
    /// Discover tool definitions for a role.
    async fn list_tools(&self, role: &str) -> Result<Vec<ToolDefinition>>;

    /// Execute a tool call. Handles the full trampoline lifecycle.
    async fn call_tool(&self, role: &str, tool_name: &str, args: Value) -> Result<MCPCallOutput>;

    /// Handle a hook event. Returns raw JSON — caller deserializes per dispatch type.
    async fn handle_hook(&self, input: &Value) -> Result<Value>;

    /// Check for source changes and reload if needed.
    async fn reload_if_changed(&self) -> Result<bool>;

    /// Content hash for cache invalidation / health checks.
    fn content_hash(&self) -> String;
}
