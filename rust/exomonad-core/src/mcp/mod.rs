//! MCP (Model Context Protocol) server.
//!
//! All tool logic is in Haskell WASM. This module handles MCP transport
//! (plain JSON-RPC over HTTP) and forwards tool calls to the WASM plugin.

pub mod tools;

#[cfg(feature = "runtime")]
pub mod server;

#[cfg(feature = "runtime")]
pub use server::McpServer;

use crate::RuntimeBackend;
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
    /// Runtime backend for executing tool calls and hooks.
    pub backend: Arc<dyn RuntimeBackend>,
    /// Role for this MCP endpoint (e.g. "tl" or "dev").
    /// Controls which tools are exposed. None means all tools.
    pub role: Option<String>,
    /// Shared question registry for resolving ask_question/answer_question bridges.
    #[cfg(feature = "runtime")]
    pub question_registry: Option<Arc<QuestionRegistry>>,
}

/// Builder for constructing McpState with optional fields.
pub struct McpStateBuilder {
    backend: Arc<dyn RuntimeBackend>,
    project_dir: PathBuf,
    role: Option<String>,
    #[cfg(feature = "runtime")]
    question_registry: Option<Arc<QuestionRegistry>>,
}

impl McpStateBuilder {
    /// Set the role for this MCP endpoint.
    pub fn role(mut self, role: impl Into<String>) -> Self {
        self.role = Some(role.into());
        self
    }

    /// Set the question registry for answer_question bridging.
    #[cfg(feature = "runtime")]
    pub fn question_registry(mut self, qr: Arc<QuestionRegistry>) -> Self {
        self.question_registry = Some(qr);
        self
    }

    /// Build the McpState.
    pub fn build(self) -> McpState {
        McpState {
            backend: self.backend,
            project_dir: self.project_dir,
            role: self.role,
            #[cfg(feature = "runtime")]
            question_registry: self.question_registry,
        }
    }
}

impl McpState {
    /// Create a builder for McpState.
    pub fn builder(backend: Arc<dyn RuntimeBackend>, project_dir: PathBuf) -> McpStateBuilder {
        McpStateBuilder {
            backend,
            project_dir,
            role: None,
            #[cfg(feature = "runtime")]
            question_registry: None,
        }
    }
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
