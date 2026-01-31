//! MCP tool implementations.
//!
//! Tools exposed via the MCP server for Claude Code to call.

use super::{McpState, ToolDefinition};
use anyhow::{Context, Result};
use serde_json::{json, Value};
use std::path::Path;
use tokio::fs;
use tracing::{debug, info};

// ============================================================================
// Tool Registry
// ============================================================================

/// Get all available tool definitions.
pub fn get_tool_definitions() -> Vec<ToolDefinition> {
    vec![
        git_branch_def(),
        git_status_def(),
        git_log_def(),
        read_file_def(),
    ]
}

/// Execute a tool by name.
pub async fn execute_tool(state: &McpState, name: &str, args: Value) -> Result<Value> {
    match name {
        "git_branch" => git_branch(state, args).await,
        "git_status" => git_status(state, args).await,
        "git_log" => git_log(state, args).await,
        "read_file" => read_file(state, args).await,
        _ => Err(anyhow::anyhow!("Unknown tool: {}", name)),
    }
}

// ============================================================================
// Git Tools
// ============================================================================

fn git_branch_def() -> ToolDefinition {
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
    }
}

async fn git_branch(state: &McpState, args: Value) -> Result<Value> {
    let path = get_path_arg(&args, &state.project_dir);
    info!(path = %path.display(), "Getting git branch");

    let branch = state
        .services
        .git
        .get_branch("local", path.to_str().unwrap_or("."))
        .await
        .context("Failed to get git branch")?;

    Ok(json!({ "branch": branch }))
}

fn git_status_def() -> ToolDefinition {
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
    }
}

async fn git_status(state: &McpState, args: Value) -> Result<Value> {
    let path = get_path_arg(&args, &state.project_dir);
    info!(path = %path.display(), "Getting git status");

    let files = state
        .services
        .git
        .get_dirty_files("local", path.to_str().unwrap_or("."))
        .await
        .context("Failed to get git status")?;

    Ok(json!({
        "files": files,
        "count": files.len()
    }))
}

fn git_log_def() -> ToolDefinition {
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
    }
}

async fn git_log(state: &McpState, args: Value) -> Result<Value> {
    let path = get_path_arg(&args, &state.project_dir);
    let limit = args.get("limit").and_then(|v| v.as_u64()).unwrap_or(10) as u32;

    info!(path = %path.display(), limit, "Getting git log");

    let commits = state
        .services
        .git
        .get_recent_commits("local", path.to_str().unwrap_or("."), limit)
        .await
        .context("Failed to get git log")?;

    let commit_objs: Vec<Value> = commits
        .into_iter()
        .map(|c| {
            json!({
                "hash": c.hash,
                "message": c.message,
                "author": c.author,
                "date": c.date
            })
        })
        .collect();

    Ok(json!({ "commits": commit_objs }))
}

// ============================================================================
// File Tools
// ============================================================================

fn read_file_def() -> ToolDefinition {
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
    }
}

async fn read_file(state: &McpState, args: Value) -> Result<Value> {
    let file_path = args
        .get("path")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow::anyhow!("Missing required 'path' argument"))?;

    let max_lines = args.get("max_lines").and_then(|v| v.as_u64());

    // Resolve relative paths against project dir
    let full_path = if Path::new(file_path).is_absolute() {
        file_path.to_string()
    } else {
        state
            .project_dir
            .join(file_path)
            .to_string_lossy()
            .to_string()
    };

    info!(path = %full_path, "Reading file");

    let content = fs::read_to_string(&full_path)
        .await
        .with_context(|| format!("Failed to read file: {}", full_path))?;

    let content = if let Some(max) = max_lines {
        content
            .lines()
            .take(max as usize)
            .collect::<Vec<_>>()
            .join("\n")
    } else {
        content
    };

    let line_count = content.lines().count();
    debug!(lines = line_count, "File read complete");

    Ok(json!({
        "content": content,
        "lines": line_count,
        "path": full_path
    }))
}

// ============================================================================
// Helpers
// ============================================================================

/// Get path argument, defaulting to project dir.
fn get_path_arg(args: &Value, default: &Path) -> std::path::PathBuf {
    args.get("path")
        .and_then(|v| v.as_str())
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|| default.to_path_buf())
}
