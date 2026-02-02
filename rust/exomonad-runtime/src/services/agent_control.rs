//! High-level agent control service.
//!
//! Provides semantic operations for agent lifecycle management:
//! - SpawnAgent: Create worktree, write context, open Zellij tab
//! - CleanupAgent: Close tab, delete worktree
//! - ListAgents: List active agent worktrees
//!
//! These are high-level effects exposed to Haskell WASM, not granular operations.

use anyhow::{anyhow, Context, Result};
use exomonad_shared::{GithubOwner, GithubRepo, IssueNumber};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::fs;
use tokio::process::Command;
use tracing::{debug, info, warn};

use super::github::{GitHubService, Repo};
use super::zellij_events;

// ============================================================================
// Types
// ============================================================================

/// Agent type for spawned agents.
///
/// Determines which CLI tool to use when spawning an agent in a Zellij tab.
/// Each type has different command names and prompt flags.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
#[derive(Default)]
pub enum AgentType {
    /// Claude Code CLI (spawns with `claude --prompt '...'`).
    Claude,

    /// Gemini CLI (spawns with `gemini --prompt-interactive '...'`).
    ///
    /// Default agent type.
    #[default]
    Gemini,
}

impl AgentType {
    /// Get the CLI command for this agent type.
    ///
    /// Returns "claude" or "gemini".
    fn command(&self) -> &'static str {
        match self {
            AgentType::Claude => "claude",
            AgentType::Gemini => "gemini",
        }
    }

    /// Get the prompt flag for this agent type.
    ///
    /// Claude uses `--prompt`, Gemini uses `--prompt-interactive`.
    fn prompt_flag(&self) -> &'static str {
        match self {
            AgentType::Claude => "--prompt",
            AgentType::Gemini => "--prompt-interactive",
        }
    }

    /// Get the suffix for tab/worktree names (lowercase).
    ///
    /// Used to construct unique tab/worktree names (e.g., "gh-123-title-claude").
    fn suffix(&self) -> &'static str {
        match self {
            AgentType::Claude => "claude",
            AgentType::Gemini => "gemini",
        }
    }
}

/// Options for spawning an agent.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnOptions {
    /// GitHub repository owner
    pub owner: GithubOwner,
    /// GitHub repository name
    pub repo: GithubRepo,
    /// Base directory for worktrees (relative to project)
    pub worktree_dir: Option<String>,
    /// Agent type (Claude or Gemini)
    #[serde(default)]
    pub agent_type: AgentType,
}

/// Result of spawning an agent.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SpawnResult {
    /// Path to the created worktree
    pub worktree_path: String,
    /// Git branch name
    pub branch_name: String,
    /// Zellij tab name
    pub tab_name: String,
    /// Issue title
    pub issue_title: String,
    /// Agent type
    pub agent_type: String,
}

/// Information about an active agent.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AgentInfo {
    /// Issue ID (e.g., "123")
    pub issue_id: String,
    /// Full path to worktree
    pub worktree_path: String,
    /// Git branch name
    pub branch_name: String,
    /// Whether the worktree has uncommitted changes
    pub has_changes: bool,
}

/// Result of batch spawn operation.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct BatchSpawnResult {
    pub spawned: Vec<SpawnResult>,
    pub failed: Vec<(String, String)>, // (issue_id, error)
}

/// Result of batch cleanup operation.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct BatchCleanupResult {
    pub cleaned: Vec<String>,
    pub failed: Vec<(String, String)>, // (issue_id, error)
}

// ============================================================================
// Service
// ============================================================================

/// Agent control service for high-level agent lifecycle management.
pub struct AgentControlService {
    /// Project root directory
    project_dir: PathBuf,
    /// GitHub service for fetching issues
    github: Option<GitHubService>,
}

impl AgentControlService {
    /// Create a new agent control service.
    pub fn new(project_dir: PathBuf, github: Option<GitHubService>) -> Self {
        Self {
            project_dir,
            github,
        }
    }

    /// Create from environment (loads secrets from ~/.exomonad/secrets).
    pub fn from_env() -> Result<Self> {
        let project_dir = std::env::current_dir().context("Failed to get current directory")?;

        // Try to load GitHub token from secrets
        let secrets = super::secrets::Secrets::load();
        let github = secrets
            .github_token()
            .and_then(|t| GitHubService::new(t).ok());

        Ok(Self {
            project_dir,
            github,
        })
    }

    // ========================================================================
    // Spawn Agent
    // ========================================================================

    /// Spawn an agent for a GitHub issue.
    ///
    /// This is the high-level semantic operation that:
    /// 1. Fetches issue from GitHub
    /// 2. Creates git worktree from origin/main
    /// 3. Writes context files (.exomonad/config.toml, INITIAL_CONTEXT.md, .mcp.json)
    /// 4. Opens Zellij tab with claude command
    pub async fn spawn_agent(
        &self,
        issue_number: IssueNumber,
        options: &SpawnOptions,
    ) -> Result<SpawnResult> {
        // Validate we're in Zellij
        self.check_zellij_env()?;

        // Get GitHub service
        let github = self
            .github
            .as_ref()
            .ok_or_else(|| anyhow!("GitHub service not available (GITHUB_TOKEN not set)"))?;

        // Fetch issue from GitHub
        let issue_id = issue_number.as_u64().to_string();
        info!(issue_id, "Fetching issue from GitHub");
        let repo = Repo {
            owner: options.owner.clone(),
            name: options.repo.clone(),
        };
        let issue = github.get_issue(&repo, issue_number.as_u64()).await?;

        // Generate slug from title
        let slug = slugify(&issue.title);
        let agent_suffix = options.agent_type.suffix();
        let worktree_dir = options
            .worktree_dir
            .clone()
            .unwrap_or_else(|| "./worktrees".to_string());
        let worktree_path = self
            .project_dir
            .join(&worktree_dir)
            .join(format!("gh-{}-{}-{}", issue_id, slug, agent_suffix));
        let branch_name = format!("gh-{}/{}-{}", issue_id, slug, agent_suffix);

        // Fetch origin/main
        self.fetch_origin().await?;

        // Create worktree
        self.create_worktree(&worktree_path, &branch_name).await?;

        // Write config files (no INITIAL_CONTEXT.md)
        self.write_context_files(&worktree_path, options.agent_type).await?;

        // Build initial prompt
        let issue_url = format!(
            "https://github.com/{}/{}/issues/{}",
            options.owner, options.repo, issue_id
        );
        let initial_prompt = Self::build_initial_prompt(
            &issue_id,
            &issue.title,
            &issue.body,
            &branch_name,
            &issue_url,
        );

        tracing::info!(
            issue_id,
            prompt_length = initial_prompt.len(),
            "Built initial prompt for agent"
        );

        // Open Zellij tab with agent command and initial prompt
        let tab_name = format!("gh-{}-{}", issue_id, agent_suffix);
        self.new_zellij_tab(
            &tab_name,
            &worktree_path,
            options.agent_type,
            Some(&initial_prompt),
        )
        .await?;

        // Emit agent:started event
        let event = exomonad_ui_protocol::AgentEvent::AgentStarted {
            agent_id: tab_name.clone(),
            timestamp: zellij_events::now_iso8601(),
        };
        if let Err(e) = zellij_events::emit_event(&event) {
            warn!("Failed to emit agent:started event: {}", e);
        }

        Ok(SpawnResult {
            worktree_path: worktree_path.to_string_lossy().to_string(),
            branch_name,
            tab_name,
            issue_title: issue.title,
            agent_type: options.agent_type.suffix().to_string(),
        })
    }

    /// Spawn multiple agents.
    pub async fn spawn_agents(
        &self,
        issue_ids: &[String],
        options: &SpawnOptions,
    ) -> BatchSpawnResult {
        let mut result = BatchSpawnResult {
            spawned: Vec::new(),
            failed: Vec::new(),
        };

        for issue_id_str in issue_ids {
            // Parse issue ID
            match IssueNumber::try_from(issue_id_str.clone()) {
                Ok(issue_number) => match self.spawn_agent(issue_number, options).await {
                    Ok(spawn_result) => result.spawned.push(spawn_result),
                    Err(e) => {
                        warn!(issue_id = issue_id_str, error = %e, "Failed to spawn agent");
                        result.failed.push((issue_id_str.clone(), e.to_string()));
                    }
                },
                Err(e) => {
                    warn!(issue_id = issue_id_str, error = %e, "Invalid issue number");
                    result.failed.push((issue_id_str.clone(), e.to_string()));
                }
            }
        }

        result
    }

    // ========================================================================
    // Cleanup Agent
    // ========================================================================

    /// Clean up an agent (close Zellij tab, delete worktree).
    pub async fn cleanup_agent(&self, issue_id: &str, force: bool) -> Result<()> {
        // Find and delete worktree first (to extract agent type from path)
        let agents = self.list_agents().await?;
        let prefix = format!("gh-{}-", issue_id);

        for agent in agents {
            let path = Path::new(&agent.worktree_path);
            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                if name.starts_with(&prefix) {
                    // Extract agent suffix from worktree name: gh-{issue}-{slug}-{agent}
                    let parts: Vec<&str> = name.rsplitn(2, '-').collect();
                    let agent_suffix = parts.first().unwrap_or(&"");

                    // Close Zellij tab with correct agent suffix
                    let tab_name = format!("gh-{}-{}", issue_id, agent_suffix);
                    if let Err(e) = self.close_zellij_tab(&tab_name).await {
                        warn!(tab_name, error = %e, "Failed to close Zellij tab (may not exist)");
                    }

                    self.delete_worktree(path, force).await?;

                    // Emit agent:stopped event
                    let event = exomonad_ui_protocol::AgentEvent::AgentStopped {
                        agent_id: format!("gh-{}", issue_id),
                        timestamp: zellij_events::now_iso8601(),
                    };
                    if let Err(e) = zellij_events::emit_event(&event) {
                        warn!("Failed to emit agent:stopped event: {}", e);
                    }

                    return Ok(());
                }
            }
        }

        Err(anyhow!("No worktree found for issue {}", issue_id))
    }

    /// Clean up multiple agents.
    pub async fn cleanup_agents(&self, issue_ids: &[String], force: bool) -> BatchCleanupResult {
        let mut result = BatchCleanupResult {
            cleaned: Vec::new(),
            failed: Vec::new(),
        };

        for issue_id in issue_ids {
            match self.cleanup_agent(issue_id, force).await {
                Ok(()) => result.cleaned.push(issue_id.clone()),
                Err(e) => {
                    warn!(issue_id, error = %e, "Failed to cleanup agent");
                    result.failed.push((issue_id.clone(), e.to_string()));
                }
            }
        }

        result
    }

    // ========================================================================
    // List Agents
    // ========================================================================

    /// List all active agent worktrees.
    pub async fn list_agents(&self) -> Result<Vec<AgentInfo>> {
        let output = Command::new("git")
            .args(["worktree", "list", "--porcelain"])
            .current_dir(&self.project_dir)
            .output()
            .await
            .context("Failed to execute git worktree list")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!("git worktree list failed: {}", stderr));
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        let mut agents = Vec::new();

        let mut current_path: Option<PathBuf> = None;
        let mut current_branch: Option<String> = None;

        for line in stdout.lines() {
            if let Some(path) = line.strip_prefix("worktree ") {
                current_path = Some(PathBuf::from(path));
            } else if let Some(branch) = line.strip_prefix("branch refs/heads/") {
                current_branch = Some(branch.to_string());
            } else if line.is_empty() {
                if let (Some(path), Some(branch)) = (current_path.take(), current_branch.take()) {
                    if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                        if name.starts_with("gh-") {
                            // Parse: gh-{issue}-{slug}-{agent}
                            // Extract issue ID (second component after 'gh-')
                            let parts: Vec<&str> = name.splitn(4, '-').collect();
                            let issue_id = parts.get(1).map(|s| s.to_string()).unwrap_or_default();

                            // Note: We don't store agent type in AgentInfo yet,
                            // but could add it if needed

                            let has_changes = self.has_uncommitted_changes(&path).await;

                            agents.push(AgentInfo {
                                issue_id,
                                worktree_path: path.to_string_lossy().to_string(),
                                branch_name: branch,
                                has_changes,
                            });
                        }
                    }
                }
            }
        }

        Ok(agents)
    }

    // ========================================================================
    // Internal: Zellij
    // ========================================================================

    fn check_zellij_env(&self) -> Result<String> {
        std::env::var("ZELLIJ_SESSION_NAME")
            .context("Not running inside a Zellij session (ZELLIJ_SESSION_NAME not set)")
    }

    async fn new_zellij_tab(
        &self,
        name: &str,
        cwd: &Path,
        agent_type: AgentType,
        prompt: Option<&str>,
    ) -> Result<()> {
        info!(name, cwd = %cwd.display(), agent_type = ?agent_type, "Creating Zellij tab");

        {
            let cmd = agent_type.command();
            // Build full command with optional prompt
            let full_command = match prompt {
                Some(p) => {
                    let escaped_prompt = Self::escape_for_shell_command(p);
                    tracing::info!(
                        tab_name = name,
                        agent_type = ?agent_type,
                        prompt_length = p.len(),
                        "Spawning agent with CLI prompt"
                    );
                    format!("{} {} {}", cmd, agent_type.prompt_flag(), escaped_prompt)
                }
                None => cmd.to_string(),
            };

            // Generate KDL layout with full tab template and command pane
            let layout_content = format!(
                r#"layout {{
    default_tab_template {{
        pane size=1 borderless=true {{
            plugin location="zellij:tab-bar"
        }}
        children
        pane size=1 borderless=true {{
            plugin location="zellij:status-bar"
        }}
    }}
    tab name="{name}" {{
        pane command="sh" {{
            args "-c" "{cmd}"
            cwd "{cwd}"
            close_on_exit true
        }}
        pane size=3 borderless=true {{
            plugin location="file:~/.config/zellij/plugins/exomonad-plugin.wasm"
        }}
    }}
}}"#,
                name = name,
                cmd = full_command,
                cwd = cwd.display()
            );

            // Write to temporary file
            let temp_dir = std::env::temp_dir();
            let layout_file = temp_dir.join(format!("zellij-tab-{}.kdl", name));
            tokio::fs::write(&layout_file, &layout_content)
                .await
                .context("Failed to write temporary layout file")?;

            tracing::debug!(
                layout_file = %layout_file.display(),
                "Generated temporary Zellij layout"
            );

            // Create tab with layout (spawn in background, don't wait for agent to finish)
            let mut child = Command::new("zellij")
                .args([
                    "action",
                    "new-tab",
                    "--layout",
                    layout_file.to_str().unwrap(),
                ])
                .spawn()
                .context("Failed to spawn zellij")?;

            // Wait briefly to ensure zellij action completes, but not for the agent inside
            tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

            // Clean up temp file
            let _ = tokio::fs::remove_file(&layout_file).await;

            // Check if zellij command failed immediately
            if let Ok(Some(status)) = child.try_wait() {
                if !status.success() {
                    return Err(anyhow!("zellij new-tab failed with status: {}", status));
                }
            }
            // If still running or completed successfully, we're good
        }

        Ok(())
    }

    async fn close_zellij_tab(&self, name: &str) -> Result<()> {
        debug!(name, "Closing Zellij tab");

        let output = Command::new("zellij")
            .args(["action", "close-tab", "--tab-name", name])
            .output()
            .await
            .context("Failed to execute zellij")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!("zellij close-tab failed: {}", stderr));
        }

        Ok(())
    }

    // ========================================================================
    // Internal: Git Worktree
    // ========================================================================

    async fn fetch_origin(&self) -> Result<()> {
        info!("Fetching origin/main");

        let output = Command::new("git")
            .args(["fetch", "origin", "main"])
            .current_dir(&self.project_dir)
            .output()
            .await
            .context("Failed to execute git fetch")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            warn!(stderr = %stderr, "git fetch warning (continuing anyway)");
        }

        Ok(())
    }

    async fn create_worktree(&self, path: &Path, branch: &str) -> Result<()> {
        if path.exists() {
            info!(path = %path.display(), "Worktree already exists, reusing");
            return Ok(());
        }

        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)
                .await
                .context("Failed to create worktree parent directory")?;
        }

        info!(path = %path.display(), branch, "Creating worktree");

        let output = Command::new("git")
            .args([
                "worktree",
                "add",
                "-b",
                branch,
                path.to_str().unwrap(),
                "origin/main",
            ])
            .current_dir(&self.project_dir)
            .output()
            .await
            .context("Failed to execute git worktree add")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);

            if stderr.contains("already exists") {
                debug!("Branch already exists, creating worktree without -b");
                let output = Command::new("git")
                    .args(["worktree", "add", path.to_str().unwrap(), branch])
                    .current_dir(&self.project_dir)
                    .output()
                    .await?;

                if !output.status.success() {
                    let stderr = String::from_utf8_lossy(&output.stderr);
                    return Err(anyhow!("git worktree add failed: {}", stderr));
                }
            } else {
                return Err(anyhow!("git worktree add failed: {}", stderr));
            }
        }

        Ok(())
    }

    async fn delete_worktree(&self, path: &Path, force: bool) -> Result<()> {
        if !path.exists() {
            debug!(path = %path.display(), "Worktree doesn't exist");
            return Ok(());
        }

        info!(path = %path.display(), force, "Deleting worktree");

        let mut args = vec!["worktree", "remove"];
        if force {
            args.push("--force");
        }
        args.push(path.to_str().unwrap());

        let output = Command::new("git")
            .args(&args)
            .current_dir(&self.project_dir)
            .output()
            .await
            .context("Failed to execute git worktree remove")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!("git worktree remove failed: {}", stderr));
        }

        Ok(())
    }

    async fn has_uncommitted_changes(&self, worktree_path: &Path) -> bool {
        let output = Command::new("git")
            .args(["status", "--porcelain"])
            .current_dir(worktree_path)
            .output()
            .await;

        match output {
            Ok(o) => !o.stdout.is_empty(),
            Err(_) => false,
        }
    }

    // ========================================================================
    // Internal: Context Files
    // ========================================================================

    async fn write_context_files(&self, worktree_path: &Path, agent_type: AgentType) -> Result<()> {
        // Create .exomonad directory
        let exomonad_dir = worktree_path.join(".exomonad");
        fs::create_dir_all(&exomonad_dir).await?;

        // Write config.toml with role = "dev" for spawned agents
        let config_content = r#"# Agent config (auto-generated)
role = "dev"
project_dir = "../.."
"#;
        fs::write(exomonad_dir.join("config.toml"), config_content).await?;
        tracing::info!(
            worktree = %worktree_path.display(),
            "Wrote .exomonad/config.toml with role=dev"
        );

        // Write .mcp.json (no --wasm argument, config file handles WASM path)
        let sidecar_path = std::env::current_exe()
            .ok()
            .and_then(|p| p.to_str().map(String::from))
            .unwrap_or_else(|| "exomonad-sidecar".to_string());

        let mcp_content = format!(
            r#"{{
  "mcpServers": {{
    "exomonad": {{
      "command": "{}",
      "args": ["mcp-stdio"]
    }}
  }}
}}
"#,
            sidecar_path
        );
        fs::write(worktree_path.join(".mcp.json"), mcp_content).await?;

        // Write agent-specific hook configuration
        match agent_type {
            AgentType::Claude => {
                // Create .claude directory and write settings with SubagentStop hook
                let claude_dir = worktree_path.join(".claude");
                fs::create_dir_all(&claude_dir).await?;

                let settings_content = format!(
                    r#"{{
  "enableAllProjectMcpServers": true,
  "hooks": {{
    "SubagentStop": [
      {{
        "hooks": [
          {{
            "type": "command",
            "command": "{} hook subagent-stop"
          }}
        ]
      }}
    ]
  }}
}}
"#,
                    sidecar_path
                );
                fs::write(claude_dir.join("settings.local.json"), settings_content).await?;
                tracing::info!(
                    worktree = %worktree_path.display(),
                    "Wrote .claude/settings.local.json with SubagentStop hook"
                );
            }
            AgentType::Gemini => {
                // Create .gemini directory and write settings with SessionEnd hook
                let gemini_dir = worktree_path.join(".gemini");
                fs::create_dir_all(&gemini_dir).await?;

                let settings_content = format!(
                    r#"{{
  "hooks": {{
    "SessionEnd": [
      {{
        "hooks": [
          {{
            "name": "stop-check",
            "type": "command",
            "command": "{} hook subagent-stop",
            "timeout": 30000
          }}
        ]
      }}
    ]
  }}
}}
"#,
                    sidecar_path
                );
                fs::write(gemini_dir.join("settings.json"), settings_content).await?;
                tracing::info!(
                    worktree = %worktree_path.display(),
                    "Wrote .gemini/settings.json with SessionEnd hook"
                );
            }
        }

        info!(worktree = %worktree_path.display(), "Context files written");
        Ok(())
    }

    // ========================================================================
    // Internal: Prompt Building
    // ========================================================================

    /// Build the initial prompt for a spawned agent.
    fn build_initial_prompt(
        issue_id: &str,
        title: &str,
        body: &str,
        branch: &str,
        issue_url: &str,
    ) -> String {
        format!(
            r#"# Issue #{issue_id}: {title}

**Branch:** `{branch}`
**Issue URL:** {issue_url}

## Description

{body}

## Instructions

You are working on this GitHub issue in an isolated worktree.
When done, commit your changes and create a pull request."#
        )
    }

    /// Escape a string for safe use in shell command with single quotes.
    ///
    /// Wraps the string in single quotes and escapes any embedded single quotes.
    /// This is suitable for: sh -c "claude --prompt '...'"
    ///
    /// Example: "user's issue" -> "'user'\''s issue'"
    fn escape_for_shell_command(s: &str) -> String {
        // Replace ' with '\'' (end quote, escaped quote, start quote)
        let escaped = s.replace('\'', r"'\''");
        format!("'{}'", escaped)
    }
}

// ============================================================================
// Helpers
// ============================================================================

/// Create a URL-safe slug from a title.
fn slugify(title: &str) -> String {
    title
        .to_lowercase()
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '-' })
        .collect::<String>()
        .split('-')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join("-")
        .chars()
        .take(50)
        .collect()
}

// ============================================================================
// Host Functions for WASM
// ============================================================================

use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};

// Input/Output types for host functions

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct SpawnAgentInput {
    pub issue_id: IssueNumber,
    pub owner: GithubOwner,
    pub repo: GithubRepo,
    pub worktree_dir: Option<String>,
    #[serde(default)]
    pub agent_type: AgentType,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct SpawnAgentsInput {
    pub issue_ids: Vec<String>,
    pub owner: GithubOwner,
    pub repo: GithubRepo,
    pub worktree_dir: Option<String>,
    #[serde(default)]
    pub agent_type: AgentType,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct CleanupAgentInput {
    pub issue_id: String,
    pub force: bool,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct CleanupAgentsInput {
    pub issue_ids: Vec<String>,
    pub force: bool,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(tag = "kind", content = "payload")]
pub enum HostResult<T> {
    Success(T),
    Error(HostError),
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct HostError {
    pub message: String,
}

impl<T> From<Result<T>> for HostResult<T> {
    fn from(res: Result<T>) -> Self {
        match res {
            Ok(val) => HostResult::Success(val),
            Err(e) => HostResult::Error(HostError {
                message: e.to_string(),
            }),
        }
    }
}

// Helper functions

fn get_input<T: serde::de::DeserializeOwned>(
    plugin: &mut CurrentPlugin,
    val: Val,
) -> Result<T, Error> {
    let handle = plugin
        .memory_from_val(&val)
        .ok_or_else(|| Error::msg("Invalid memory handle in input"))?;
    let bytes = plugin.memory_bytes(handle)?;
    Ok(serde_json::from_slice(bytes)?)
}

fn set_output<T: Serialize>(plugin: &mut CurrentPlugin, data: &T) -> Result<Val, Error> {
    let json = serde_json::to_vec(data)?;
    let handle = plugin.memory_new(json)?;
    Ok(plugin.memory_to_val(handle))
}

fn block_on<F: std::future::Future>(future: F) -> Result<F::Output, Error> {
    match tokio::runtime::Handle::try_current() {
        Ok(handle) => Ok(handle.block_on(future)),
        Err(_) => Err(Error::msg("No Tokio runtime available")),
    }
}

// Host function factories

pub fn spawn_agent_host_fn(service: Arc<AgentControlService>) -> Function {
    Function::new(
        "agent_spawn",
        [ValType::I64],
        [ValType::I64],
        UserData::new(service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<AgentControlService>>|
         -> Result<(), Error> {
            let input: SpawnAgentInput = get_input(plugin, inputs[0])?;

            let service_arc = user_data.get()?;
            let service = service_arc
                .lock()
                .map_err(|_| Error::msg("Poisoned lock"))?;

            let options = SpawnOptions {
                owner: input.owner,
                repo: input.repo,
                worktree_dir: input.worktree_dir,
                agent_type: input.agent_type,
            };

            let result = block_on(service.spawn_agent(input.issue_id, &options))?;
            let output: HostResult<SpawnResult> = result.into();

            outputs[0] = set_output(plugin, &output)?;
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn spawn_agents_host_fn(service: Arc<AgentControlService>) -> Function {
    Function::new(
        "agent_spawn_batch",
        [ValType::I64],
        [ValType::I64],
        UserData::new(service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<AgentControlService>>|
         -> Result<(), Error> {
            let input: SpawnAgentsInput = get_input(plugin, inputs[0])?;

            let service_arc = user_data.get()?;
            let service = service_arc
                .lock()
                .map_err(|_| Error::msg("Poisoned lock"))?;

            let options = SpawnOptions {
                owner: input.owner,
                repo: input.repo,
                worktree_dir: input.worktree_dir,
                agent_type: input.agent_type,
            };

            let result = block_on(service.spawn_agents(&input.issue_ids, &options))?;
            let output = HostResult::Success(result);

            outputs[0] = set_output(plugin, &output)?;
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn cleanup_agent_host_fn(service: Arc<AgentControlService>) -> Function {
    Function::new(
        "agent_cleanup",
        [ValType::I64],
        [ValType::I64],
        UserData::new(service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<AgentControlService>>|
         -> Result<(), Error> {
            let input: CleanupAgentInput = get_input(plugin, inputs[0])?;

            let service_arc = user_data.get()?;
            let service = service_arc
                .lock()
                .map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(service.cleanup_agent(&input.issue_id, input.force))?;
            let output: HostResult<()> = result.into();

            outputs[0] = set_output(plugin, &output)?;
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn cleanup_agents_host_fn(service: Arc<AgentControlService>) -> Function {
    Function::new(
        "agent_cleanup_batch",
        [ValType::I64],
        [ValType::I64],
        UserData::new(service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<AgentControlService>>|
         -> Result<(), Error> {
            let input: CleanupAgentsInput = get_input(plugin, inputs[0])?;

            let service_arc = user_data.get()?;
            let service = service_arc
                .lock()
                .map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(service.cleanup_agents(&input.issue_ids, input.force))?;
            let output = HostResult::Success(result);

            outputs[0] = set_output(plugin, &output)?;
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn list_agents_host_fn(service: Arc<AgentControlService>) -> Function {
    Function::new(
        "agent_list",
        [ValType::I64],
        [ValType::I64],
        UserData::new(service),
        |plugin: &mut CurrentPlugin,
         _inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<AgentControlService>>|
         -> Result<(), Error> {
            let service_arc = user_data.get()?;
            let service = service_arc
                .lock()
                .map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(service.list_agents())?;
            let output: HostResult<Vec<AgentInfo>> = result.into();

            outputs[0] = set_output(plugin, &output)?;
            Ok(())
        },
    )
    .with_namespace("env")
}

/// Register all agent control host functions.
pub fn register_host_functions(service: Arc<AgentControlService>) -> Vec<Function> {
    vec![
        spawn_agent_host_fn(service.clone()),
        spawn_agents_host_fn(service.clone()),
        cleanup_agent_host_fn(service.clone()),
        cleanup_agents_host_fn(service.clone()),
        list_agents_host_fn(service),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slugify() {
        assert_eq!(slugify("Fix the Bug"), "fix-the-bug");
        assert_eq!(slugify("Add new feature!"), "add-new-feature");
        assert_eq!(slugify("CamelCase"), "camelcase");
    }

    #[test]
    fn test_escape_for_shell_command_simple() {
        assert_eq!(
            AgentControlService::escape_for_shell_command("hello world"),
            "'hello world'"
        );
    }

    #[test]
    fn test_escape_for_shell_command_with_quote() {
        assert_eq!(
            AgentControlService::escape_for_shell_command("user's issue"),
            r"'user'\''s issue'"
        );
    }

    #[test]
    fn test_escape_for_shell_command_shell_chars() {
        let result = AgentControlService::escape_for_shell_command("Test $VAR and `code`");
        assert!(result.contains("$VAR"));
        assert!(result.contains("`code`"));
        assert_eq!(result, "'Test $VAR and `code`'");
    }

    #[test]
    fn test_build_initial_prompt_format() {
        let prompt = AgentControlService::build_initial_prompt(
            "123",
            "Fix the bug",
            "Description",
            "gh-123/fix",
            "https://github.com/owner/repo/issues/123",
        );

        assert!(prompt.contains("# Issue #123: Fix the bug"));
        assert!(prompt.contains("**Branch:** `gh-123/fix`"));
        assert!(prompt.contains("Description"));
        assert!(prompt.contains("https://github.com/owner/repo/issues/123"));
        assert!(prompt.contains("When done, commit your changes and create a pull request."));
    }

    #[test]
    fn test_agent_type_command() {
        assert_eq!(AgentType::Claude.command(), "claude");
        assert_eq!(AgentType::Gemini.command(), "gemini");
    }

    #[test]
    fn test_agent_type_prompt_flag() {
        assert_eq!(AgentType::Claude.prompt_flag(), "--prompt");
        assert_eq!(AgentType::Gemini.prompt_flag(), "--prompt-interactive");
    }

    #[test]
    fn test_agent_type_suffix() {
        assert_eq!(AgentType::Claude.suffix(), "claude");
        assert_eq!(AgentType::Gemini.suffix(), "gemini");
    }

    #[test]
    fn test_agent_type_default() {
        assert_eq!(AgentType::default(), AgentType::Gemini);
    }

    #[test]
    fn test_agent_type_deserialization() {
        use serde_json;

        let claude: AgentType = serde_json::from_str("\"claude\"").unwrap();
        assert_eq!(claude, AgentType::Claude);

        let gemini: AgentType = serde_json::from_str("\"gemini\"").unwrap();
        assert_eq!(gemini, AgentType::Gemini);

        // Invalid agent type should fail at parse boundary
        let invalid = serde_json::from_str::<AgentType>("\"invalid\"");
        assert!(invalid.is_err());
    }
}
