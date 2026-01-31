//! Types for spawn_agents, cleanup_agents, and list_agents MCP tools.

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

// ============================================================================
// spawn_agents
// ============================================================================

/// Arguments for spawn_agents MCP tool.
#[derive(Debug, Deserialize)]
pub struct SpawnAgentsArgs {
    /// GitHub issue numbers to spawn (e.g., ["123", "456"])
    pub issues: Vec<String>,

    /// GitHub repository owner
    pub owner: String,

    /// GitHub repository name
    pub repo: String,

    /// Base directory for worktrees (default: "./worktrees")
    #[serde(default = "default_worktree_dir")]
    pub worktree_dir: PathBuf,
}

fn default_worktree_dir() -> PathBuf {
    PathBuf::from("./worktrees")
}

/// Result of spawn_agents MCP tool.
#[derive(Debug, Serialize)]
pub struct SpawnAgentsResult {
    /// Successfully spawned agents
    pub spawned: Vec<SpawnedAgent>,

    /// Failed spawns: (issue_id, error_message)
    pub failed: Vec<(String, String)>,
}

/// Information about a successfully spawned agent.
#[derive(Debug, Serialize)]
pub struct SpawnedAgent {
    /// Issue ID (e.g., "123")
    pub issue_id: String,

    /// Path to the worktree
    pub worktree_path: String,

    /// Git branch name (e.g., "gh-123/fix-bug")
    pub branch_name: String,

    /// Zellij tab name
    pub tab_name: String,
}

// ============================================================================
// cleanup_agents
// ============================================================================

/// Arguments for cleanup_agents MCP tool.
#[derive(Debug, Deserialize)]
pub struct CleanupAgentsArgs {
    /// Issue IDs to clean up
    pub issues: Vec<String>,

    /// Force deletion even if worktree has uncommitted changes
    #[serde(default)]
    pub force: bool,
}

/// Result of cleanup_agents MCP tool.
#[derive(Debug, Serialize)]
pub struct CleanupAgentsResult {
    /// Successfully cleaned up issue IDs
    pub cleaned: Vec<String>,

    /// Failed cleanups: (issue_id, error_message)
    pub failed: Vec<(String, String)>,
}

// ============================================================================
// list_agents
// ============================================================================

/// Result of list_agents MCP tool.
#[derive(Debug, Serialize)]
pub struct ListAgentsResult {
    /// Active agent worktrees
    pub agents: Vec<AgentInfo>,
}

/// Information about an active agent worktree.
#[derive(Debug, Serialize)]
pub struct AgentInfo {
    /// Issue ID extracted from worktree name
    pub issue_id: String,

    /// Full worktree path
    pub worktree_path: String,

    /// Git branch name
    pub branch_name: String,

    /// Whether the worktree has uncommitted changes
    pub has_changes: bool,
}
