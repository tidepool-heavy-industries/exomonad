//! MCP tool definitions and WASM execution.
//!
//! All tool logic is in Haskell WASM. This module provides:
//! - Tool schema definitions for MCP discovery
//! - WASM routing for tool execution

use super::{McpState, ToolDefinition};
use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
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
// Tool Registry
// ============================================================================

/// Get all available tool definitions.
pub fn get_tool_definitions() -> Vec<ToolDefinition> {
    vec![
        // Git tools
        ToolDefinition {
            name: "git_branch".to_string(),
            description: "Get the current git branch name".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "path": {
                        "type": "string",
                        "description": "Directory path (defaults to project root)"
                    }
                }
            }),
        },
        ToolDefinition {
            name: "git_status".to_string(),
            description: "Get list of modified/untracked files (git status --porcelain)".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "path": {
                        "type": "string",
                        "description": "Directory path (defaults to project root)"
                    }
                }
            }),
        },
        ToolDefinition {
            name: "git_log".to_string(),
            description: "Get recent git commits".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "path": {
                        "type": "string",
                        "description": "Directory path (defaults to project root)"
                    },
                    "limit": {
                        "type": "integer",
                        "description": "Number of commits to show (default: 10)",
                        "default": 10
                    }
                }
            }),
        },
        // File tools
        ToolDefinition {
            name: "read_file".to_string(),
            description: "Read contents of a file".to_string(),
            input_schema: json!({
                "type": "object",
                "required": ["path"],
                "properties": {
                    "path": {
                        "type": "string",
                        "description": "File path (relative to project root or absolute)"
                    },
                    "max_lines": {
                        "type": "integer",
                        "description": "Maximum lines to read (default: unlimited)"
                    }
                }
            }),
        },
        // GitHub tools
        ToolDefinition {
            name: "github_list_issues".to_string(),
            description: "List issues from a GitHub repository".to_string(),
            input_schema: json!({
                "type": "object",
                "required": ["owner", "repo"],
                "properties": {
                    "owner": {
                        "type": "string",
                        "description": "Repository owner (user or org)"
                    },
                    "repo": {
                        "type": "string",
                        "description": "Repository name"
                    },
                    "state": {
                        "type": "string",
                        "enum": ["open", "closed", "all"],
                        "description": "Filter by issue state (default: open)"
                    },
                    "labels": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "Filter by labels"
                    }
                }
            }),
        },
        ToolDefinition {
            name: "github_get_issue".to_string(),
            description: "Get a single GitHub issue with full details".to_string(),
            input_schema: json!({
                "type": "object",
                "required": ["owner", "repo", "number"],
                "properties": {
                    "owner": {
                        "type": "string",
                        "description": "Repository owner (user or org)"
                    },
                    "repo": {
                        "type": "string",
                        "description": "Repository name"
                    },
                    "number": {
                        "type": "integer",
                        "description": "Issue number"
                    }
                }
            }),
        },
        ToolDefinition {
            name: "github_list_prs".to_string(),
            description: "List pull requests from a GitHub repository".to_string(),
            input_schema: json!({
                "type": "object",
                "required": ["owner", "repo"],
                "properties": {
                    "owner": {
                        "type": "string",
                        "description": "Repository owner (user or org)"
                    },
                    "repo": {
                        "type": "string",
                        "description": "Repository name"
                    },
                    "state": {
                        "type": "string",
                        "enum": ["open", "closed", "all"],
                        "description": "Filter by PR state (default: open)"
                    },
                    "limit": {
                        "type": "integer",
                        "description": "Maximum number of PRs to return"
                    }
                }
            }),
        },
        // Spawn tools
        ToolDefinition {
            name: "spawn_agents".to_string(),
            description: "Spawn Claude Code agents for GitHub issues in isolated worktrees".to_string(),
            input_schema: json!({
                "type": "object",
                "required": ["issues", "owner", "repo"],
                "properties": {
                    "issues": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "GitHub issue numbers to spawn agents for"
                    },
                    "owner": {
                        "type": "string",
                        "description": "GitHub repository owner"
                    },
                    "repo": {
                        "type": "string",
                        "description": "GitHub repository name"
                    },
                    "worktree_dir": {
                        "type": "string",
                        "description": "Base directory for worktrees (default: ./worktrees)"
                    }
                }
            }),
        },
        ToolDefinition {
            name: "cleanup_agents".to_string(),
            description: "Clean up agent worktrees and close their Zellij tabs".to_string(),
            input_schema: json!({
                "type": "object",
                "required": ["issues"],
                "properties": {
                    "issues": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "Issue IDs to clean up"
                    },
                    "force": {
                        "type": "boolean",
                        "description": "Force deletion even if worktree has uncommitted changes (default: false)"
                    }
                }
            }),
        },
        ToolDefinition {
            name: "list_agents".to_string(),
            description: "List active agent worktrees".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {}
            }),
        },
    ]
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
