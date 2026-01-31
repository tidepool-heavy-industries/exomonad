//! MCP tool implementations.
//!
//! Tools exposed via the MCP server for Claude Code to call.

use super::spawn::{
    self, CleanupAgentsArgs, CleanupAgentsResult, ListAgentsResult, SpawnAgentsArgs,
    SpawnAgentsResult, SpawnedAgent,
};
use super::{McpState, ToolDefinition};
use anyhow::{anyhow, Context, Result};
use exomonad_runtime::services::github::{IssueFilter, PRFilter, Repo};
use serde_json::{json, Value};
use std::path::Path;
use tokio::fs;
use tracing::{debug, info, warn};

// ============================================================================
// Tool Registry
// ============================================================================

/// Get all available tool definitions.
pub fn get_tool_definitions() -> Vec<ToolDefinition> {
    vec![
        // Git tools
        git_branch_def(),
        git_status_def(),
        git_log_def(),
        // File tools
        read_file_def(),
        // GitHub tools
        github_list_issues_def(),
        github_get_issue_def(),
        github_list_prs_def(),
        // Spawn tools
        spawn_agents_def(),
        cleanup_agents_def(),
        list_agents_def(),
    ]
}

/// Execute a tool by name.
pub async fn execute_tool(state: &McpState, name: &str, args: Value) -> Result<Value> {
    match name {
        // Git tools
        "git_branch" => git_branch(state, args).await,
        "git_status" => git_status(state, args).await,
        "git_log" => git_log(state, args).await,
        // File tools
        "read_file" => read_file(state, args).await,
        // GitHub tools
        "github_list_issues" => github_list_issues(state, args).await,
        "github_get_issue" => github_get_issue(state, args).await,
        "github_list_prs" => github_list_prs(state, args).await,
        // Spawn tools
        "spawn_agents" => spawn_agents(state, args).await,
        "cleanup_agents" => cleanup_agents(state, args).await,
        "list_agents" => list_agents(state, args).await,
        _ => Err(anyhow!("Unknown tool: {}", name)),
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

// ============================================================================
// GitHub Tools
// ============================================================================

fn github_list_issues_def() -> ToolDefinition {
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
    }
}

async fn github_list_issues(state: &McpState, args: Value) -> Result<Value> {
    let owner = args
        .get("owner")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow!("Missing required 'owner' argument"))?;
    let repo_name = args
        .get("repo")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow!("Missing required 'repo' argument"))?;

    info!(owner, repo = repo_name, "Listing GitHub issues");

    let github = state
        .services
        .github
        .as_ref()
        .ok_or_else(|| anyhow!("GitHub service not available (GITHUB_TOKEN not set)"))?;

    let repo = Repo {
        owner: owner.to_string(),
        name: repo_name.to_string(),
    };

    let filter = {
        let state_filter = args.get("state").and_then(|v| v.as_str()).map(String::from);
        let labels = args.get("labels").and_then(|v| v.as_array()).map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect()
        });

        if state_filter.is_some() || labels.is_some() {
            Some(IssueFilter {
                state: state_filter,
                labels,
            })
        } else {
            None
        }
    };

    let issues = github
        .list_issues(&repo, filter.as_ref())
        .await
        .context("Failed to list GitHub issues")?;

    let issue_objs: Vec<Value> = issues
        .into_iter()
        .map(|i| {
            json!({
                "number": i.number,
                "title": i.title,
                "state": i.state,
                "author": i.author,
                "labels": i.labels,
                "url": i.url
            })
        })
        .collect();

    debug!(count = issue_objs.len(), "Found issues");

    Ok(json!({
        "issues": issue_objs,
        "count": issue_objs.len()
    }))
}

fn github_get_issue_def() -> ToolDefinition {
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
    }
}

async fn github_get_issue(state: &McpState, args: Value) -> Result<Value> {
    let owner = args
        .get("owner")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow!("Missing required 'owner' argument"))?;
    let repo_name = args
        .get("repo")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow!("Missing required 'repo' argument"))?;
    let number = args
        .get("number")
        .and_then(|v| v.as_u64())
        .ok_or_else(|| anyhow!("Missing required 'number' argument"))?;

    info!(owner, repo = repo_name, number, "Getting GitHub issue");

    let github = state
        .services
        .github
        .as_ref()
        .ok_or_else(|| anyhow!("GitHub service not available (GITHUB_TOKEN not set)"))?;

    let repo = Repo {
        owner: owner.to_string(),
        name: repo_name.to_string(),
    };

    let issue = github
        .get_issue(&repo, number)
        .await
        .context("Failed to get GitHub issue")?;

    Ok(json!({
        "number": issue.number,
        "title": issue.title,
        "body": issue.body,
        "state": issue.state,
        "author": issue.author,
        "labels": issue.labels,
        "url": issue.url
    }))
}

fn github_list_prs_def() -> ToolDefinition {
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
    }
}

async fn github_list_prs(state: &McpState, args: Value) -> Result<Value> {
    let owner = args
        .get("owner")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow!("Missing required 'owner' argument"))?;
    let repo_name = args
        .get("repo")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow!("Missing required 'repo' argument"))?;

    info!(owner, repo = repo_name, "Listing GitHub PRs");

    let github = state
        .services
        .github
        .as_ref()
        .ok_or_else(|| anyhow!("GitHub service not available (GITHUB_TOKEN not set)"))?;

    let repo = Repo {
        owner: owner.to_string(),
        name: repo_name.to_string(),
    };

    let filter = {
        let state_filter = args.get("state").and_then(|v| v.as_str()).map(String::from);
        let limit = args.get("limit").and_then(|v| v.as_u64()).map(|v| v as u32);

        if state_filter.is_some() || limit.is_some() {
            Some(PRFilter {
                state: state_filter,
                limit,
            })
        } else {
            None
        }
    };

    let prs = github
        .list_prs(&repo, filter.as_ref())
        .await
        .context("Failed to list GitHub PRs")?;

    let pr_objs: Vec<Value> = prs
        .into_iter()
        .map(|pr| {
            json!({
                "number": pr.number,
                "title": pr.title,
                "state": pr.state,
                "author": pr.author,
                "head_ref": pr.head_ref,
                "base_ref": pr.base_ref,
                "url": pr.url,
                "created_at": pr.created_at,
                "merged_at": pr.merged_at
            })
        })
        .collect();

    debug!(count = pr_objs.len(), "Found PRs");

    Ok(json!({
        "pull_requests": pr_objs,
        "count": pr_objs.len()
    }))
}

// ============================================================================
// Spawn Tools
// ============================================================================

fn spawn_agents_def() -> ToolDefinition {
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
    }
}

async fn spawn_agents(state: &McpState, args: Value) -> Result<Value> {
    // Check Zellij environment
    let session = spawn::zellij::check_zellij_env()
        .context("spawn_agents requires running inside a Zellij session")?;
    info!(session, "Running in Zellij session");

    // Parse arguments
    let spawn_args: SpawnAgentsArgs =
        serde_json::from_value(args).context("Failed to parse spawn_agents arguments")?;

    info!(
        issues = ?spawn_args.issues,
        owner = %spawn_args.owner,
        repo = %spawn_args.repo,
        "Spawning agents"
    );

    // Get GitHub service
    let github = state
        .services
        .github
        .as_ref()
        .ok_or_else(|| anyhow!("GitHub service not available (GITHUB_TOKEN not set)"))?;

    let repo = Repo {
        owner: spawn_args.owner.clone(),
        name: spawn_args.repo.clone(),
    };

    // Create worktree manager
    let worktree_manager = spawn::worktree::WorktreeManager::new(
        state.project_dir.clone(),
        spawn_args.worktree_dir.clone(),
    );

    // Fetch origin/main to ensure we have latest
    worktree_manager.fetch_origin().await?;

    let mut result = SpawnAgentsResult {
        spawned: Vec::new(),
        failed: Vec::new(),
    };

    // Process each issue
    for issue_id in &spawn_args.issues {
        match spawn_single_agent(&worktree_manager, github, &repo, issue_id).await {
            Ok(agent) => result.spawned.push(agent),
            Err(e) => {
                warn!(issue_id, error = %e, "Failed to spawn agent");
                result.failed.push((issue_id.clone(), e.to_string()));
            }
        }
    }

    info!(
        spawned = result.spawned.len(),
        failed = result.failed.len(),
        "Spawn complete"
    );

    Ok(serde_json::to_value(result)?)
}

/// Spawn a single agent for an issue.
async fn spawn_single_agent(
    worktree_manager: &spawn::worktree::WorktreeManager,
    github: &exomonad_runtime::services::github::GitHubService,
    repo: &Repo,
    issue_id: &str,
) -> Result<SpawnedAgent> {
    // Parse issue number
    let issue_num: u64 = issue_id
        .parse()
        .with_context(|| format!("Invalid issue number: {}", issue_id))?;

    // Fetch issue from GitHub
    info!(issue_id, "Fetching issue from GitHub");
    let issue = github.get_issue(repo, issue_num).await?;

    // Generate slug from title
    let slug = spawn::worktree::slugify(&issue.title);
    info!(issue_id, slug = %slug, "Generated slug from title");

    // Create worktree (returns PathBuf)
    let worktree_path = worktree_manager.create_worktree(issue_id, &slug).await?;
    let branch_name = spawn::worktree::WorktreeManager::branch_name(issue_id, &slug);

    // Write context files
    let issue_url = format!(
        "https://github.com/{}/{}/issues/{}",
        repo.owner, repo.name, issue_id
    );

    spawn::context::write_exomonad_config(&worktree_path, "dev").await?;
    spawn::context::write_initial_context(
        &worktree_path,
        issue_id,
        &issue.title,
        &issue.body,
        &branch_name,
        &issue_url,
    )
    .await?;

    // Find sidecar binary path for .mcp.json
    let sidecar_path = std::env::current_exe()
        .ok()
        .and_then(|p| p.to_str().map(String::from))
        .unwrap_or_else(|| "exomonad-sidecar".to_string());
    spawn::context::write_mcp_config(&worktree_path, &sidecar_path).await?;

    // Create Zellij tab
    let tab_name = format!("gh-{}", issue_id);
    spawn::zellij::new_tab(&tab_name, &worktree_path, Some("claude")).await?;

    Ok(SpawnedAgent {
        issue_id: issue_id.to_string(),
        worktree_path: worktree_path.to_string_lossy().to_string(),
        branch_name,
        tab_name,
    })
}

fn cleanup_agents_def() -> ToolDefinition {
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
    }
}

async fn cleanup_agents(state: &McpState, args: Value) -> Result<Value> {
    let cleanup_args: CleanupAgentsArgs =
        serde_json::from_value(args).context("Failed to parse cleanup_agents arguments")?;

    info!(issues = ?cleanup_args.issues, force = cleanup_args.force, "Cleaning up agents");

    let worktree_manager = spawn::worktree::WorktreeManager::new(
        state.project_dir.clone(),
        std::path::PathBuf::from("./worktrees"),
    );

    let mut result = CleanupAgentsResult {
        cleaned: Vec::new(),
        failed: Vec::new(),
    };

    for issue_id in &cleanup_args.issues {
        match cleanup_single_agent(&worktree_manager, issue_id, cleanup_args.force).await {
            Ok(()) => result.cleaned.push(issue_id.clone()),
            Err(e) => {
                warn!(issue_id, error = %e, "Failed to clean up agent");
                result.failed.push((issue_id.clone(), e.to_string()));
            }
        }
    }

    info!(
        cleaned = result.cleaned.len(),
        failed = result.failed.len(),
        "Cleanup complete"
    );

    Ok(serde_json::to_value(result)?)
}

/// Clean up a single agent's worktree and Zellij tab.
async fn cleanup_single_agent(
    worktree_manager: &spawn::worktree::WorktreeManager,
    issue_id: &str,
    force: bool,
) -> Result<()> {
    // Close Zellij tab first
    let tab_name = format!("gh-{}", issue_id);
    if let Err(e) = spawn::zellij::close_tab(&tab_name).await {
        warn!(tab_name, error = %e, "Failed to close Zellij tab (may not exist)");
    }

    // Find and delete worktree
    // We need to find the worktree by issue ID pattern
    let worktrees = worktree_manager.list_worktrees().await?;
    let prefix = format!("gh-{}-", issue_id);

    for wt in worktrees {
        let wt_path = std::path::Path::new(&wt.worktree_path);
        if let Some(name) = wt_path.file_name().and_then(|n| n.to_str()) {
            if name.starts_with(&prefix) {
                // Extract slug from path
                let slug = name
                    .strip_prefix(&format!("gh-{}-", issue_id))
                    .unwrap_or("");
                worktree_manager
                    .delete_worktree(issue_id, slug, force)
                    .await?;
                return Ok(());
            }
        }
    }

    Err(anyhow!("No worktree found for issue {}", issue_id))
}

fn list_agents_def() -> ToolDefinition {
    ToolDefinition {
        name: "list_agents".to_string(),
        description: "List active agent worktrees".to_string(),
        input_schema: json!({
            "type": "object",
            "properties": {}
        }),
    }
}

async fn list_agents(state: &McpState, _args: Value) -> Result<Value> {
    info!("Listing agents");

    let worktree_manager = spawn::worktree::WorktreeManager::new(
        state.project_dir.clone(),
        std::path::PathBuf::from("./worktrees"),
    );

    // list_worktrees already returns Vec<AgentInfo> with gh-* filtering
    let agents = worktree_manager.list_worktrees().await?;

    info!(count = agents.len(), "Found agents");

    Ok(serde_json::to_value(ListAgentsResult { agents })?)
}
