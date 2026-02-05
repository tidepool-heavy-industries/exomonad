//! High-level agent control service.
//!
//! Provides semantic operations for agent lifecycle management:
//! - SpawnAgent: Create worktree, write context, open Zellij tab
//! - CleanupAgent: Close tab, delete worktree
//! - ListAgents: List active agent worktrees
//!
//! These are high-level effects exposed to Haskell WASM, not granular operations.

use crate::common::{FFIBoundary, HostResult, IntoFFIResult, TimeoutError};
use anyhow::{anyhow, Context, Result};
use exomonad_shared::{GithubOwner, GithubRepo, IssueNumber};
use extism_convert::Json;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::fs;
use tokio::process::Command;
use tokio::time::{timeout, Duration};
use tracing::{debug, info, warn};

use super::git::GitService;
use super::github::{GitHubService, Repo};
use super::local::LocalExecutor;
use super::zellij_events;

const SPAWN_TIMEOUT: Duration = Duration::from_secs(60);
const GIT_TIMEOUT: Duration = Duration::from_secs(30);
const ZELLIJ_TIMEOUT: Duration = Duration::from_secs(30);

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

    /// Get the emoji for this agent type.
    ///
    /// Used for visual differentiation in Zellij tabs.
    fn emoji(&self) -> &'static str {
        match self {
            AgentType::Claude => "🤖",
            AgentType::Gemini => "💎",
        }
    }

    /// Generate a display name for Zellij tabs.
    ///
    /// Format: "{emoji} {issue_id}-{short_slug}"
    /// The slug is truncated to 20 chars for readability.
    fn display_name(&self, issue_id: &str, slug: &str) -> String {
        let short_slug: String = slug.chars().take(20).collect();
        format!("{} {}-{}", self.emoji(), issue_id, short_slug)
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

impl FFIBoundary for SpawnResult {}

/// Simplified PR info for agent listing.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AgentPrInfo {
    /// PR number.
    pub number: u64,
    /// PR title.
    pub title: String,
    /// Web URL to the PR.
    pub url: String,
    /// PR state ("open", "closed", "merged").
    pub state: String,
}

impl FFIBoundary for AgentPrInfo {}

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
    /// Slug from worktree name (e.g., "fix-bug-in-parser")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub slug: Option<String>,
    /// Agent type ("claude" or "gemini")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_type: Option<String>,
    /// Associated PR if one exists for this branch
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pr: Option<AgentPrInfo>,
}

impl FFIBoundary for AgentInfo {}

/// Result of batch spawn operation.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct BatchSpawnResult {
    pub spawned: Vec<SpawnResult>,
    pub failed: Vec<(String, String)>, // (issue_id, error)
}

impl FFIBoundary for BatchSpawnResult {}

/// Result of batch cleanup operation.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct BatchCleanupResult {
    pub cleaned: Vec<String>,
    pub failed: Vec<(String, String)>, // (issue_id, error)
}

impl FFIBoundary for BatchCleanupResult {}

// ============================================================================
// Service
// ============================================================================

/// Agent control service for high-level agent lifecycle management.
pub struct AgentControlService {
    /// Project root directory
    project_dir: PathBuf,
    /// GitHub service for fetching issues
    github: Option<GitHubService>,
    /// Git service for repo info
    git: GitService,
}

impl AgentControlService {
    /// Create a new agent control service.
    pub fn new(project_dir: PathBuf, github: Option<GitHubService>, git: GitService) -> Self {
        Self {
            project_dir,
            github,
            git,
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

        // Create git service with local executor
        let git = GitService::new(Arc::new(LocalExecutor::new()));

        Ok(Self {
            project_dir,
            github,
            git,
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
    #[tracing::instrument(skip(self, options), fields(issue_id = %issue_number.as_u64()))]
    pub async fn spawn_agent(
        &self,
        issue_number: IssueNumber,
        options: &SpawnOptions,
    ) -> Result<SpawnResult> {
        let issue_id_log = issue_number.as_u64().to_string();
        info!(issue_id = %issue_id_log, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_agent");

        let result = timeout(SPAWN_TIMEOUT, async {
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
                .unwrap_or_else(|| ".exomonad/worktrees".to_string());
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
            self.write_context_files(&worktree_path, options.agent_type)
                .await?;

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
            // Use display_name for the Zellij tab (emoji + short format)
            // Keep internal_name for worktree/branch consistency
            let internal_name = format!("gh-{}-{}-{}", issue_id, slug, agent_suffix);
            let display_name = options.agent_type.display_name(&issue_id, &slug);
            self.new_zellij_tab(
                &display_name,
                &worktree_path,
                options.agent_type,
                Some(&initial_prompt),
            )
            .await?;

            // Emit agent:started event
            let agent_id = exomonad_ui_protocol::AgentId::try_from(internal_name.clone())
                .map_err(|e| anyhow!("Invalid agent_id: {}", e))?;
            let event = exomonad_ui_protocol::AgentEvent::AgentStarted {
                agent_id,
                timestamp: zellij_events::now_iso8601(),
            };
            if let Err(e) = zellij_events::emit_event(&event) {
                warn!("Failed to emit agent:started event: {}", e);
            }

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                worktree_path: worktree_path.to_string_lossy().to_string(),
                branch_name,
                tab_name: internal_name,
                issue_title: issue.title,
                agent_type: options.agent_type.suffix().to_string(),
            })
        })
        .await
        .map_err(|_| {
            let msg = format!("spawn_agent timed out after {}s", SPAWN_TIMEOUT.as_secs());
            warn!(issue_id = %issue_id_log, error = %msg, "spawn_agent timed out");
            anyhow::Error::new(TimeoutError { message: msg })
        })??;

        info!(issue_id = %issue_id_log, "spawn_agent completed successfully");
        Ok(result)
    }

    /// Spawn multiple agents.
    #[tracing::instrument(skip(self, options))]
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
    #[tracing::instrument(skip(self))]
    pub async fn cleanup_agent(&self, issue_id: &str, force: bool) -> Result<()> {
        // Find and delete worktree first (to extract agent type from path)
        let agents = self.list_agents().await?;
        let prefix = format!("gh-{}-", issue_id);

        for agent in agents {
            let path = Path::new(&agent.worktree_path);
            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                if name.starts_with(&prefix) {
                    // Parse the worktree name using helper function
                    if let Some(parsed) = parse_worktree_name(name) {
                        // Close Zellij tab with the display name (if agent type is known)
                        match parsed.agent_type {
                            Some(agent_type) => {
                                let display_name = agent_type.display_name(issue_id, parsed.slug);
                                if let Err(e) = self.close_zellij_tab(&display_name).await {
                                    warn!(tab_name = %display_name, error = %e, "Failed to close Zellij tab (may not exist)");
                                }
                            }
                            None => {
                                warn!(
                                    worktree_name = %name,
                                    "Unknown agent suffix in worktree name; skipping Zellij tab close"
                                );
                            }
                        }
                    }

                    self.delete_worktree(path, force).await?;

                    // Prune stale worktree references from git
                    self.prune_worktrees().await?;

                    // Emit agent:stopped event
                    let agent_id =
                        exomonad_ui_protocol::AgentId::try_from(format!("gh-{}", issue_id))
                            .map_err(|e| anyhow!("Invalid agent_id: {}", e))?;
                    let event = exomonad_ui_protocol::AgentEvent::AgentStopped {
                        agent_id,
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
    #[tracing::instrument(skip(self))]
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

    /// Clean up agents whose branches have been merged into main.
    #[tracing::instrument(skip(self))]
    pub async fn cleanup_merged_agents(&self) -> Result<BatchCleanupResult> {
        // Fetch origin/main to ensure we have latest state
        self.fetch_origin().await?;

        let agents = self.list_agents().await?;
        let mut to_cleanup = Vec::new();

        for agent in agents {
            // Check if branch is merged into origin/main
            // We ignore errors here (e.g. if branch doesn't exist) and just don't cleanup
            if self
                .is_branch_merged(&agent.branch_name)
                .await
                .unwrap_or(false)
            {
                info!(issue_id = %agent.issue_id, branch = %agent.branch_name, "Branch is merged, marking for cleanup");
                to_cleanup.push(agent.issue_id);
            }
        }

        if to_cleanup.is_empty() {
            return Ok(BatchCleanupResult {
                cleaned: Vec::new(),
                failed: Vec::new(),
            });
        }

        // Clean them up (force=false for safety - don't delete if uncommitted changes in worktree)
        Ok(self.cleanup_agents(&to_cleanup, false).await)
    }

    // ========================================================================
    // List Agents
    // ========================================================================

    /// List all active agent worktrees.
    #[tracing::instrument(skip(self))]
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
                            // Parse worktree name to extract issue_id, slug, agent_type
                            let parsed = parse_worktree_name(name);
                            let issue_id = parsed
                                .as_ref()
                                .map(|p| p.issue_id.to_string())
                                .unwrap_or_else(|| {
                                    // Fallback: extract issue ID manually
                                    let parts: Vec<&str> = name.splitn(4, '-').collect();
                                    parts.get(1).map(|s| s.to_string()).unwrap_or_default()
                                });

                            let has_changes = self.has_uncommitted_changes(&path).await;

                            // Extract slug and agent_type from parsed name
                            let slug = parsed.as_ref().map(|p| p.slug.to_string());
                            let agent_type = parsed
                                .as_ref()
                                .and_then(|p| p.agent_type.as_ref())
                                .map(|t| t.suffix().to_string());

                            agents.push(AgentInfo {
                                issue_id,
                                worktree_path: path.to_string_lossy().to_string(),
                                branch_name: branch,
                                has_changes,
                                slug,
                                agent_type,
                                pr: None, // PR lookup added below
                            });
                        }
                    }
                }
            }
        }

        // Look up PRs for agents if GitHub service is available
        if let Some(ref github) = self.github {
            if let Some(repo) = self.get_repo_from_remote().await {
                for agent in &mut agents {
                    if let Ok(Some(pr)) = github.get_pr_for_branch(&repo, &agent.branch_name).await
                    {
                        agent.pr = Some(AgentPrInfo {
                            number: pr.number,
                            title: pr.title,
                            url: pr.url,
                            state: pr.state,
                        });
                    }
                }
            }
        }

        Ok(agents)
    }

    /// Get repo (owner/name) from project's git remote URL.
    async fn get_repo_from_remote(&self) -> Option<Repo> {
        let dir = self.project_dir.to_string_lossy();
        let repo_info = self.git.get_repo_info("", &dir).await.ok()?;

        let owner = repo_info.owner?;
        let name = repo_info.name?;

        Some(Repo {
            owner: GithubOwner::from(owner.as_str()),
            name: GithubRepo::from(name.as_str()),
        })
    }

    // ========================================================================
    // Internal: Zellij
    // ========================================================================

    fn check_zellij_env(&self) -> Result<String> {
        std::env::var("ZELLIJ_SESSION_NAME")
            .context("Not running inside a Zellij session (ZELLIJ_SESSION_NAME not set)")
    }

    #[tracing::instrument(skip(self, prompt))]
    async fn new_zellij_tab(
        &self,
        name: &str,
        cwd: &Path,
        agent_type: AgentType,
        prompt: Option<&str>,
    ) -> Result<()> {
        info!(name, cwd = %cwd.display(), agent_type = ?agent_type, "Creating Zellij tab");

        let cmd = agent_type.command();
        // Build full command with optional prompt
        let full_command = match prompt {
            Some(p) => {
                let escaped_prompt = Self::escape_for_shell_command(p);
                debug!(
                    tab_name = name,
                    agent_type = ?agent_type,
                    prompt_length = p.len(),
                    "Spawning agent with CLI prompt"
                );
                format!("{} {} {}", cmd, agent_type.prompt_flag(), escaped_prompt)
            }
            None => cmd.to_string(),
        };

        // Escape the command for KDL string literal (escape backslashes, quotes, newlines)
        let kdl_escaped_command = Self::escape_for_kdl(&full_command);

        // Use login shell to ensure PATH is loaded (gemini, claude, etc.)
        let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/zsh".to_string());

        // Generate layout using zellij-gen library (includes zjstatus with Solarized Dark)
        let params = zellij_gen::AgentTabParams {
            tab_name: name,
            pane_name: "Agent",
            command: &kdl_escaped_command,
            cwd,
            shell: &shell,
            focus: true,
        };

        let layout_content = zellij_gen::generate_agent_layout(&params)
            .context("Failed to generate Zellij layout")?;

        // Write to temporary file (sanitize name for filename - no emoji/spaces)
        let temp_dir = std::env::temp_dir();
        let safe_filename: String = name
            .chars()
            .map(|c| {
                if c.is_ascii_alphanumeric() || c == '-' || c == '_' {
                    c
                } else {
                    '_'
                }
            })
            .collect();
        let layout_file = temp_dir.join(format!("zellij-tab-{}.kdl", safe_filename));
        tokio::fs::write(&layout_file, &layout_content)
            .await
            .context("Failed to write temporary layout file")?;

        debug!(
            layout_file = %layout_file.display(),
            "Generated temporary Zellij layout"
        );

        // Log the command we're about to execute
        debug!(
            name,
            layout_file = %layout_file.display(),
            "Executing zellij action new-tab"
        );

        // Create tab with layout - wait for zellij action to complete before deleting temp file
        // Note: zellij action new-tab returns quickly after reading the layout,
        // it doesn't wait for the spawned pane command to finish
        let output = Command::new("zellij")
            .args([
                "action",
                "new-tab",
                "--layout",
                layout_file
                    .to_str()
                    .ok_or_else(|| anyhow!("Invalid layout file path (utf8 error)"))?,
            ])
            .output()
            .await
            .context("Failed to run zellij")?;

        // Clean up temp file after zellij has read it
        let _ = tokio::fs::remove_file(&layout_file).await;

        // Check if zellij command failed
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!(
                "zellij new-tab failed with status: {} (stderr: {}, layout file was: {})",
                output.status,
                stderr,
                layout_file.display()
            ));
        }

        Ok(())
    }

    #[tracing::instrument(skip(self))]
    async fn close_zellij_tab(&self, name: &str) -> Result<()> {
        info!(name, "Running zellij action close-tab");

        let result = timeout(ZELLIJ_TIMEOUT, async {
            Command::new("zellij")
                .args(["action", "close-tab", "--tab-name", name])
                .output()
                .await
                .context("Failed to execute zellij")
        })
        .await
        .map_err(|_| {
            anyhow::Error::new(TimeoutError {
                message: format!(
                    "zellij close-tab timed out after {}s",
                    ZELLIJ_TIMEOUT.as_secs()
                ),
            })
        })??;

        if !result.status.success() {
            let stderr = String::from_utf8_lossy(&result.stderr);
            warn!(stderr = %stderr, exit_code = ?result.status.code(), "zellij close-tab failed");
            return Err(anyhow!("zellij close-tab failed: {}", stderr));
        } else {
            info!(exit_code = ?result.status.code(), "zellij close-tab successful");
        }

        Ok(())
    }

    // ========================================================================
    // Internal: Git Worktree
    // ========================================================================

    #[tracing::instrument(skip(self))]
    async fn fetch_origin(&self) -> Result<()> {
        info!("Running git fetch origin main");

        let result = timeout(GIT_TIMEOUT, async {
            Command::new("git")
                .args(["fetch", "origin", "main"])
                .current_dir(&self.project_dir)
                .output()
                .await
                .context("Failed to execute git fetch")
        })
        .await
        .map_err(|_| {
            anyhow::Error::new(TimeoutError {
                message: format!("git fetch timed out after {}s", GIT_TIMEOUT.as_secs()),
            })
        })??;

        if !result.status.success() {
            let stderr = String::from_utf8_lossy(&result.stderr);
            warn!(stderr = %stderr, exit_code = ?result.status.code(), "git fetch warning (continuing anyway)");
        } else {
            info!(exit_code = ?result.status.code(), "git fetch successful");
        }

        Ok(())
    }

    #[tracing::instrument(skip(self))]
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

        info!(path = %path.display(), branch, "Running git worktree add");

        let result = timeout(GIT_TIMEOUT, async {
            Command::new("git")
                .args([
                    "worktree",
                    "add",
                    "-b",
                    branch,
                    path.to_str()
                        .ok_or_else(|| anyhow!("Invalid worktree path (utf8 error)"))?,
                    "origin/main",
                ])
                .current_dir(&self.project_dir)
                .output()
                .await
                .context("Failed to execute git worktree add")
        })
        .await
        .map_err(|_| {
            anyhow::Error::new(TimeoutError {
                message: format!(
                    "git worktree add timed out after {}s",
                    GIT_TIMEOUT.as_secs()
                ),
            })
        })??;

        if !result.status.success() {
            let stderr = String::from_utf8_lossy(&result.stderr);
            warn!(stderr = %stderr, exit_code = ?result.status.code(), "git worktree add failed, checking if branch exists");

            if stderr.contains("already exists") {
                debug!("Branch already exists, creating worktree without -b");
                let fallback_result = timeout(GIT_TIMEOUT, async {
                    Command::new("git")
                        .args([
                            "worktree",
                            "add",
                            path.to_str()
                                .ok_or_else(|| anyhow!("Invalid worktree path (utf8 error)"))?,
                            branch,
                        ])
                        .current_dir(&self.project_dir)
                        .output()
                        .await
                        .context("Failed to execute git worktree add (fallback)")
                })
                .await
                .map_err(|_| {
                    anyhow::Error::new(TimeoutError {
                        message: format!(
                            "git worktree add (fallback) timed out after {}s",
                            GIT_TIMEOUT.as_secs()
                        ),
                    })
                })??;

                if !fallback_result.status.success() {
                    let stderr = String::from_utf8_lossy(&fallback_result.stderr);
                    warn!(stderr = %stderr, exit_code = ?fallback_result.status.code(), "git worktree add (fallback) failed");
                    return Err(anyhow!("git worktree add failed: {}", stderr));
                }
                info!(exit_code = ?fallback_result.status.code(), "git worktree add (fallback) successful");
            } else {
                return Err(anyhow!("git worktree add failed: {}", stderr));
            }
        } else {
            info!(exit_code = ?result.status.code(), "git worktree add successful");
        }

        Ok(())
    }

    #[tracing::instrument(skip(self))]
    async fn delete_worktree(&self, path: &Path, force: bool) -> Result<()> {
        if !path.exists() {
            debug!(path = %path.display(), "Worktree doesn't exist");
            return Ok(());
        }

        info!(path = %path.display(), force, "Running git worktree remove");

        let mut args = vec!["worktree", "remove"];
        if force {
            args.push("--force");
        }
        args.push(
            path.to_str()
                .ok_or_else(|| anyhow!("Invalid worktree path (utf8 error)"))?,
        );

        let result = timeout(GIT_TIMEOUT, async {
            Command::new("git")
                .args(&args)
                .current_dir(&self.project_dir)
                .output()
                .await
                .context("Failed to execute git worktree remove")
        })
        .await
        .map_err(|_| {
            anyhow::Error::new(TimeoutError {
                message: format!(
                    "git worktree remove timed out after {}s",
                    GIT_TIMEOUT.as_secs()
                ),
            })
        })??;

        if !result.status.success() {
            let stderr = String::from_utf8_lossy(&result.stderr);
            warn!(stderr = %stderr, exit_code = ?result.status.code(), "git worktree remove failed");
            return Err(anyhow!("git worktree remove failed: {}", stderr));
        } else {
            info!(exit_code = ?result.status.code(), "git worktree remove successful");
        }

        Ok(())
    }

    /// Prune stale worktree references from git's internal tracking.
    async fn prune_worktrees(&self) -> Result<()> {
        info!("Pruning stale worktree references");

        let result = Command::new("git")
            .args(["worktree", "prune"])
            .current_dir(&self.project_dir)
            .output()
            .await
            .context("Failed to execute git worktree prune")?;

        if !result.status.success() {
            let stderr = String::from_utf8_lossy(&result.stderr);
            warn!(stderr = %stderr, "git worktree prune failed (non-fatal)");
        } else {
            info!("git worktree prune completed");
        }

        Ok(())
    }

    async fn is_branch_merged(&self, branch: &str) -> Result<bool> {
        let result = Command::new("git")
            .args(["merge-base", "--is-ancestor", branch, "origin/main"])
            .current_dir(&self.project_dir)
            .output()
            .await
            .context("Failed to check if branch is merged")?;

        Ok(result.status.success())
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
        use std::os::unix::fs::symlink;

        // Create .exomonad directory
        let exomonad_dir = worktree_path.join(".exomonad");
        fs::create_dir_all(&exomonad_dir).await?;

        // Write config.toml with agent_role = "dev" for spawned agents
        let config_content = r###"# Agent config (auto-generated)
agent_role = "dev"
project_dir = "../../.."
"###;
        fs::write(exomonad_dir.join("config.toml"), config_content).await?;
        tracing::info!(
            worktree = %worktree_path.display(),
            "Wrote .exomonad/config.toml with agent_role=dev"
        );

        // Create symlinks for shared resources back to root .exomonad
        // Worktree is at: {project}/.exomonad/worktrees/gh-xxx/
        // Symlinks at:    {project}/.exomonad/worktrees/gh-xxx/.exomonad/{name}
        // Target:         {project}/.exomonad/{name}
        // Relative path:  ../../../{name} (up to gh-xxx, up to worktrees, up to .exomonad, then {name})
        let symlinks = [
            ("wasm", "../../../wasm"),
            ("roles", "../../../roles"),
            ("lib", "../../../lib"),
            ("flake.nix", "../../../flake.nix"),
            ("flake.lock", "../../../flake.lock"),
        ];

        for (name, target) in symlinks {
            let link_path = exomonad_dir.join(name);
            let target_path = Path::new(target);

            // Remove existing directory/file if it's not a symlink (cleanup old duplicates)
            if link_path.exists() && !link_path.is_symlink() {
                if link_path.is_dir() {
                    fs::remove_dir_all(&link_path).await.ok();
                    tracing::debug!(name = %name, "Removed existing directory for symlink");
                } else if link_path.is_file() {
                    fs::remove_file(&link_path).await.ok();
                    tracing::debug!(name = %name, "Removed existing file for symlink");
                }
            }

            if !link_path.exists() {
                if let Err(e) = symlink(target_path, &link_path) {
                    tracing::warn!(
                        name = %name,
                        target = %target,
                        error = %e,
                        "Failed to create symlink"
                    );
                } else {
                    tracing::debug!(name = %name, target = %target, "Created symlink");
                }
            }
        }

        // Write .gitignore for worktree-specific ignores
        let gitignore_content = "# Auto-generated for worktree\nresult\n";
        fs::write(exomonad_dir.join(".gitignore"), gitignore_content).await?;

        // Write .mcp.json (no --wasm argument, config file handles WASM path)
        let sidecar_path = std::env::current_exe()
            .ok()
            .and_then(|p| p.to_str().map(String::from))
            .unwrap_or_else(|| "exomonad-sidecar".to_string());

        let mcp_content = format!(
            r###"{{
  "mcpServers": {{
    "exomonad": {{
      "command": "{}",
      "args": ["mcp-stdio"]
    }}
  }}
}} "###,
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
                    r###"{{
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
}} "###,
                    sidecar_path
                );
                fs::write(claude_dir.join("settings.local.json"), settings_content).await?;
                tracing::info!(
                    worktree = %worktree_path.display(),
                    "Wrote .claude/settings.local.json with SubagentStop hook"
                );
            }
            AgentType::Gemini => {
                // Create .gemini directory and write settings with AfterAgent hook
                // AfterAgent fires after each agent turn, SessionEnd only on exit
                let gemini_dir = worktree_path.join(".gemini");
                fs::create_dir_all(&gemini_dir).await?;

                let settings_content = format!(
                    r###"{{
  "hooks": {{
    "AfterAgent": [
      {{
        "hooks": [
          {{
            "name": "stop-check",
            "type": "command",
            "command": "{} hook after-agent --runtime gemini",
            "timeout": 30000
          }}
        ]
      }}
    ]
  }}
}} "###,
                    sidecar_path
                );
                fs::write(gemini_dir.join("settings.json"), settings_content).await?;
                tracing::info!(
                    worktree = %worktree_path.display(),
                    "Wrote .gemini/settings.json with AfterAgent hook"
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
            r###"# Issue #{issue_id}: {title}

**Branch:** `{branch}`
**Issue URL:** {issue_url}

## Description

{body}"###,
            issue_id = issue_id,
            title = title,
            branch = branch,
            issue_url = issue_url,
            body = body,
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

    /// Escape a string for use inside a KDL string literal.
    /// KDL strings use backslash escaping: \n for newline, \\ for backslash, \" for quote.
    fn escape_for_kdl(s: &str) -> String {
        s.replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
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

/// Parsed components of a worktree name.
#[derive(Debug, PartialEq)]
struct ParsedWorktreeName<'a> {
    issue_id: &'a str,
    slug: &'a str,
    agent_type: Option<AgentType>,
}

/// Parse a worktree name into its components.
/// Format: gh-{issue_id}-{slug}-{agent_suffix}
/// Returns None if the name doesn't match the expected format.
fn parse_worktree_name(name: &str) -> Option<ParsedWorktreeName<'_>> {
    // Must start with "gh-"
    let rest = name.strip_prefix("gh-")?;

    // Split to get issue_id
    let (issue_id, rest) = rest.split_once('-')?;

    // Split from right to get agent_suffix (claude/gemini have no hyphens)
    let (slug, agent_suffix) = rest.rsplit_once('-')?;

    // Parse agent type
    let agent_type = match agent_suffix {
        "claude" => Some(AgentType::Claude),
        "gemini" => Some(AgentType::Gemini),
        _ => None,
    };

    Some(ParsedWorktreeName {
        issue_id,
        slug,
        agent_type,
    })
}

// ============================================================================
// Host Functions for WASM
// ============================================================================

use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};

// Input/Output types for host functions

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct SpawnAgentInput {
    pub issue_id: String, // Parsed to IssueNumber in host function
    pub owner: GithubOwner,
    pub repo: GithubRepo,
    pub worktree_dir: Option<String>,
    #[serde(default)]
    pub agent_type: AgentType,
}

impl FFIBoundary for SpawnAgentInput {}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct SpawnAgentsInput {
    pub issue_ids: Vec<String>,
    pub owner: GithubOwner,
    pub repo: GithubRepo,
    pub worktree_dir: Option<String>,
    #[serde(default)]
    pub agent_type: AgentType,
}

impl FFIBoundary for SpawnAgentsInput {}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct CleanupAgentInput {
    pub issue_id: String,
    pub force: bool,
}

impl FFIBoundary for CleanupAgentInput {}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct CleanupAgentsInput {
    pub issue_ids: Vec<String>,
    pub force: bool,
}

impl FFIBoundary for CleanupAgentsInput {}

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
            let _span = tracing::info_span!("host_function", function = "agent_spawn").entered();
            let input: SpawnAgentInput = get_input(plugin, inputs[0])?;
            tracing::info!(issue_id = %input.issue_id, agent_type = ?input.agent_type, "Spawning agent");

            // Parse issue ID at the edge (Haskell sends string, Rust validates)
            let issue_number = IssueNumber::try_from(input.issue_id.clone())
                .map_err(|e| Error::msg(format!("Invalid issue ID '{}': {}", input.issue_id, e)))?;

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

            let result = block_on(service.spawn_agent(issue_number, &options))?;

            match &result {
                Ok(_) => tracing::info!(success = true, "Completed"),
                Err(e) => tracing::warn!(error = %e, "Failed"),
            }

            let output: HostResult<SpawnResult> = result.into_ffi_result();

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
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
            let _span = tracing::info_span!("host_function", function = "agent_spawn_batch").entered();
            let input: SpawnAgentsInput = get_input(plugin, inputs[0])?;
            tracing::info!(count = input.issue_ids.len(), agent_type = ?input.agent_type, "Spawning batch agents");

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

            // BatchSpawnResult is not a Result, it's a struct returned successfully
            tracing::info!(success = true, spawned = result.spawned.len(), failed = result.failed.len(), "Completed");

            let output = HostResult::Success(result);

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
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
            let _span = tracing::info_span!("host_function", function = "agent_cleanup").entered();
            let input: CleanupAgentInput = get_input(plugin, inputs[0])?;
            tracing::info!(issue_id = %input.issue_id, force = input.force, "Cleaning up agent");

            let service_arc = user_data.get()?;
            let service = service_arc
                .lock()
                .map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(service.cleanup_agent(&input.issue_id, input.force))?;

            match &result {
                Ok(_) => tracing::info!(success = true, "Completed"),
                Err(e) => tracing::warn!(error = %e, "Failed"),
            }

            let output: HostResult<()> = result.into_ffi_result();

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
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
            let _span =
                tracing::info_span!("host_function", function = "agent_cleanup_batch").entered();
            let input: CleanupAgentsInput = get_input(plugin, inputs[0])?;
            tracing::info!(
                count = input.issue_ids.len(),
                force = input.force,
                "Cleaning up batch agents"
            );

            let service_arc = user_data.get()?;
            let service = service_arc
                .lock()
                .map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(service.cleanup_agents(&input.issue_ids, input.force))?;

            // BatchCleanupResult is a success type, errors are inside it
            tracing::info!(
                success = true,
                cleaned = result.cleaned.len(),
                failed = result.failed.len(),
                "Completed"
            );

            let output = HostResult::Success(result);

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn cleanup_merged_agents_host_fn(service: Arc<AgentControlService>) -> Function {
    Function::new(
        "agent_cleanup_merged",
        [ValType::I64],
        [ValType::I64],
        UserData::new(service),
        |plugin: &mut CurrentPlugin,
         _inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<AgentControlService>>|
         -> Result<(), Error> {
            let _span =
                tracing::info_span!("host_function", function = "agent_cleanup_merged").entered();
            // No input needed, but we might receive an empty object
            tracing::info!("Cleaning up merged agents");

            let service_arc = user_data.get()?;
            let service = service_arc
                .lock()
                .map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(service.cleanup_merged_agents())?;

            match &result {
                Ok(res) => tracing::info!(
                    success = true,
                    cleaned = res.cleaned.len(),
                    failed = res.failed.len(),
                    "Completed"
                ),
                Err(e) => tracing::warn!(error = %e, "Failed"),
            }

            let output: HostResult<BatchCleanupResult> = result.into_ffi_result();

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
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
            let _span = tracing::info_span!("host_function", function = "agent_list").entered();
            tracing::info!("Listing agents");

            let service_arc = user_data.get()?;
            let service = service_arc
                .lock()
                .map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(service.list_agents())?;

            match &result {
                Ok(agents) => tracing::info!(success = true, count = agents.len(), "Completed"),
                Err(e) => tracing::warn!(error = %e, "Failed"),
            }

            let output: HostResult<Vec<AgentInfo>> = result.into_ffi_result();

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
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
        cleanup_merged_agents_host_fn(service.clone()),
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
        // Standard shell escaping: end quote, escaped quote, start quote
        // 'user'\''s issue' = 'user' + \' + 's issue'
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
        // Stop hooks now handle commit/PR creation - no explicit instruction needed
        assert!(!prompt.contains("When done, commit"));
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
    fn test_agent_type_emoji() {
        assert_eq!(AgentType::Claude.emoji(), "🤖");
        assert_eq!(AgentType::Gemini.emoji(), "💎");
    }

    #[test]
    fn test_agent_type_display_name() {
        assert_eq!(
            AgentType::Claude.display_name("473", "refactor-polish"),
            "🤖 473-refactor-polish"
        );
        assert_eq!(
            AgentType::Gemini.display_name("123", "fix-bug"),
            "💎 123-fix-bug"
        );
    }

    #[test]
    fn test_agent_type_display_name_truncates_long_slug() {
        let long_slug = "this-is-a-very-long-slug-that-should-be-truncated";
        let display = AgentType::Claude.display_name("123", long_slug);
        // Slug portion is truncated to 20 chars: "this-is-a-very-long-" (exactly 20)
        assert_eq!(display, "🤖 123-this-is-a-very-long-");
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

    #[test]
    fn test_parse_worktree_name_claude() {
        let parsed = super::parse_worktree_name("gh-123-fix-bug-claude").unwrap();
        assert_eq!(parsed.issue_id, "123");
        assert_eq!(parsed.slug, "fix-bug");
        assert_eq!(parsed.agent_type, Some(AgentType::Claude));
    }

    #[test]
    fn test_parse_worktree_name_gemini() {
        let parsed = super::parse_worktree_name("gh-456-add-feature-gemini").unwrap();
        assert_eq!(parsed.issue_id, "456");
        assert_eq!(parsed.slug, "add-feature");
        assert_eq!(parsed.agent_type, Some(AgentType::Gemini));
    }

    #[test]
    fn test_parse_worktree_name_slug_with_hyphens() {
        // Slug can contain hyphens
        let parsed = super::parse_worktree_name("gh-789-fix-the-big-bug-claude").unwrap();
        assert_eq!(parsed.issue_id, "789");
        assert_eq!(parsed.slug, "fix-the-big-bug");
        assert_eq!(parsed.agent_type, Some(AgentType::Claude));
    }

    #[test]
    fn test_parse_worktree_name_unknown_suffix() {
        // Unknown agent suffix returns None for agent_type
        let parsed = super::parse_worktree_name("gh-123-test-unknown").unwrap();
        assert_eq!(parsed.issue_id, "123");
        assert_eq!(parsed.slug, "test");
        assert_eq!(parsed.agent_type, None);
    }

    #[test]
    fn test_parse_worktree_name_invalid_format() {
        // Missing prefix
        assert!(super::parse_worktree_name("123-test-claude").is_none());

        // No hyphens after gh-
        assert!(super::parse_worktree_name("gh-nohyphens").is_none());

        // Only gh- prefix
        assert!(super::parse_worktree_name("gh-123").is_none());
    }
}
