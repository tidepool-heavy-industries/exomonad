//! High-level agent control service.
//!
//! Provides semantic operations for agent lifecycle management:
//! - SpawnAgent: Create agent directory, open Zellij tab
//! - CleanupAgent: Close tab, remove per-agent config
//! - ListAgents: Discover from Zellij tabs (source of truth for running agents)

use crate::common::TimeoutError;
use crate::domain::ItemState;
use crate::ffi::FFIBoundary;
use crate::{GithubOwner, GithubRepo, IssueNumber};
use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use tokio::fs;
use tokio::process::Command;
use tokio::time::{timeout, Duration};
use tracing::{debug, error, info, warn};

use super::github::{GitHubService, Repo};
use super::zellij_events;

const SPAWN_TIMEOUT: Duration = Duration::from_secs(60);
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

/// Static metadata for each agent type, replacing per-method match dispatch.
struct AgentMetadata {
    command: &'static str,
    prompt_flag: &'static str,
    suffix: &'static str,
    emoji: &'static str,
}

const CLAUDE_META: AgentMetadata = AgentMetadata {
    command: "claude",
    prompt_flag: "",
    suffix: "claude",
    emoji: "\u{1F916}", // 🤖
};

const GEMINI_META: AgentMetadata = AgentMetadata {
    command: "gemini",
    prompt_flag: "--prompt-interactive",
    suffix: "gemini",
    emoji: "\u{1F48E}", // 💎
};

impl AgentType {
    fn meta(&self) -> &'static AgentMetadata {
        match self {
            AgentType::Claude => &CLAUDE_META,
            AgentType::Gemini => &GEMINI_META,
        }
    }

    fn command(&self) -> &'static str {
        self.meta().command
    }
    fn prompt_flag(&self) -> &'static str {
        self.meta().prompt_flag
    }
    fn suffix(&self) -> &'static str {
        self.meta().suffix
    }
    fn emoji(&self) -> &'static str {
        self.meta().emoji
    }

    /// Generate a display name for Zellij tabs.
    ///
    /// Format: "{emoji} gh-{issue_id}-{short_slug}"
    /// The slug is truncated to 20 chars for readability.
    fn display_name(&self, issue_id: &str, slug: &str) -> String {
        format!("{} gh-{}-{}", self.emoji(), issue_id, slug)
    }
}

/// Options for spawning an agent.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnOptions {
    /// GitHub repository owner
    pub owner: GithubOwner,
    /// GitHub repository name
    pub repo: GithubRepo,
    /// Agent type (Claude or Gemini)
    #[serde(default)]
    pub agent_type: AgentType,
    /// Sub-repository path relative to project_dir (e.g., "egregore/").
    /// When set, the agent's project context targets this directory instead of project_dir.
    pub subrepo: Option<String>,
    /// Base branch to branch off of (default: "main").
    pub base_branch: Option<String>,
}

/// Options for spawning a named teammate (no GitHub issue required).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnGeminiTeammateOptions {
    /// Human-readable name (e.g., "mcp-hardener")
    pub name: String,
    /// Initial prompt/instructions
    pub prompt: String,
    /// Agent type (Claude or Gemini)
    #[serde(default)]
    pub agent_type: AgentType,
    /// Sub-repository path relative to project_dir
    pub subrepo: Option<String>,
    /// Base branch to branch off of (defaults to current branch).
    pub base_branch: Option<String>,
}

/// Options for spawning a worker agent in the current worktree (no branch/worktree).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnWorkerOptions {
    /// Human-readable name for the worker
    pub name: String,
    /// Implementation instructions
    pub prompt: String,
}

/// Options for spawning a subtree agent (isolated worktree).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnSubtreeOptions {
    /// Full task/prompt for the agent.
    pub task: String,
    /// Branch name suffix.
    pub branch_name: String,
    /// Parent Claude session ID for --resume --fork-session context inheritance.
    pub parent_session_id: String,
}

/// Result of spawning an agent.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SpawnResult {
    /// Path to the agent directory (.exomonad/agents/{agent_id}/)
    pub agent_dir: String,
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
    /// PR state.
    pub state: ItemState,
}

impl FFIBoundary for AgentPrInfo {}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum AgentStatus {
    Running,
    Stopped,
}

impl FFIBoundary for AgentStatus {}

/// Workspace topology for an agent — how it relates to the project directory.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Default)]
#[serde(rename_all = "snake_case")]
pub enum Topology {
    /// Unknown or legacy agent.
    #[default]
    Unspecified,
    /// Agent works in an isolated git worktree.
    WorktreePerAgent,
    /// Agent shares the project directory.
    SharedDir,
}

impl Topology {
    /// Convert from proto i32 representation.
    pub fn from_proto(value: i32) -> Self {
        match value {
            1 => Topology::WorktreePerAgent,
            2 => Topology::SharedDir,
            _ => Topology::Unspecified,
        }
    }

    /// Convert to proto i32 representation.
    pub fn to_proto(self) -> i32 {
        match self {
            Topology::Unspecified => 0,
            Topology::WorktreePerAgent => 1,
            Topology::SharedDir => 2,
        }
    }
}

/// Information about an active agent.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AgentInfo {
    /// Issue ID (e.g., "123")
    pub issue_id: String,
    /// Whether a Zellij tab exists for this agent
    pub has_tab: bool,
    /// Status of the agent
    pub status: AgentStatus,
    /// Workspace topology.
    #[serde(default)]
    pub topology: Topology,
    /// Path to agent directory (.exomonad/agents/{agent_id}/)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_dir: Option<String>,
    /// Slug from agent name (e.g., "fix-bug-in-parser")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub slug: Option<String>,
    /// Agent type ("claude" or "gemini")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_type: Option<String>,
    /// Associated PR if one exists
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
#[derive(Clone)]
pub struct AgentControlService {
    /// Project root directory
    project_dir: PathBuf,
    /// Base directory for worktrees (default: .exomonad/worktrees)
    worktree_base: PathBuf,
    /// GitHub service for fetching issues
    github: Option<GitHubService>,
    /// Zellij session name for event emission
    zellij_session: Option<String>,
    /// Server-generated UUID for event routing (EXOMONAD_SESSION_ID env var).
    event_session_id: Option<String>,
    /// MCP server port for per-agent endpoint URLs (set when running `exomonad serve`).
    mcp_server_port: Option<u16>,
}

impl AgentControlService {
    /// Create a new agent control service.
    pub fn new(project_dir: PathBuf, github: Option<GitHubService>) -> Self {
        let worktree_base = project_dir.join(".exomonad/worktrees");
        Self {
            project_dir,
            worktree_base,
            github,
            zellij_session: None,
            event_session_id: None,
            mcp_server_port: None,
        }
    }

    /// Set the worktree base directory.
    pub fn with_worktree_base(mut self, base: PathBuf) -> Self {
        self.worktree_base = base;
        self
    }

    /// Set the Zellij session name for event emission.
    pub fn with_zellij_session(mut self, session: String) -> Self {
        self.zellij_session = Some(session);
        self
    }

    /// Set the event session ID for worker coordination.
    pub fn with_event_session_id(mut self, id: String) -> Self {
        self.event_session_id = Some(id);
        self
    }

    /// Set the MCP server port for per-agent endpoint URL generation.
    pub fn with_mcp_server_port(mut self, port: u16) -> Self {
        self.mcp_server_port = Some(port);
        self
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
            project_dir: project_dir.clone(),
            worktree_base: project_dir.join(".exomonad/worktrees"),
            github,
            zellij_session: None,
            event_session_id: None,
            mcp_server_port: None,
        })
    }

    /// Resolve effective project dir for git operations.
    /// When subrepo is set, git operations target project_dir/subrepo instead.
    /// Validates that subrepo is relative and does not escape project_dir.
    fn effective_project_dir(&self, subrepo: Option<&str>) -> Result<PathBuf> {
        match subrepo {
            Some(sub) => {
                let sub_path = Path::new(sub);
                if sub_path.is_absolute() {
                    return Err(anyhow!("subrepo path must be relative: {}", sub));
                }
                for component in sub_path.components() {
                    if matches!(component, std::path::Component::ParentDir) {
                        return Err(anyhow!("subrepo path cannot contain '..': {}", sub));
                    }
                }
                let dir = self.project_dir.join(sub);
                info!(subrepo = %sub, effective_dir = %dir.display(), "Using subrepo for git operations");
                Ok(dir)
            }
            None => Ok(self.project_dir.clone()),
        }
    }

    // ========================================================================
    // Spawn Agent
    // ========================================================================

    /// Spawn an agent for a GitHub issue.
    ///
    /// This is the high-level semantic operation that:
    /// 1. Fetches issue from GitHub
    /// 2. Creates agent directory (.exomonad/agents/{agent_id}/)
    /// 3. Writes .mcp.json pointing to the Unix socket server
    /// 4. Opens Zellij tab with agent command (cwd = project_dir)
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

            // Resolve effective project dir.
            let effective_project_dir = self.effective_project_dir(options.subrepo.as_deref())?;

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

            // Generate slug and agent name
            let slug = slugify(&issue.title);
            let agent_suffix = options.agent_type.suffix();
            let internal_name = format!("gh-{}-{}-{}", issue_id, slug, agent_suffix);

            // Determine base branch
            let base = options.base_branch.as_deref().unwrap_or("main");
            let branch_name = if base == "main" {
                format!("gh-{}/{}-{}", issue_id, slug, agent_suffix)
            } else {
                format!("{}/{}-{}", base, slug, agent_suffix)
            };

            // Create worktree
            let worktree_path = self.worktree_base.join(&internal_name);

            // Clean up existing worktree if it exists (idempotency)
            if worktree_path.exists() {
                info!(path = %worktree_path.display(), "Removing existing worktree for idempotency");
                let _ = Command::new("git")
                    .args(["worktree", "remove", "--force", &worktree_path.to_string_lossy()])
                    .current_dir(&effective_project_dir)
                    .output()
                    .await;
            }

            info!(
                base_branch = %base,
                branch_name = %branch_name,
                worktree_path = %worktree_path.display(),
                "Creating git worktree"
            );

            // Create the worktree
            // git worktree add -b <new_branch> <path> <start_point>
            let output = Command::new("git")
                .args([
                    "worktree",
                    "add",
                    "-b",
                    &branch_name,
                    &worktree_path.to_string_lossy(),
                    base,
                ])
                .current_dir(&effective_project_dir)
                .output()
                .await
                .context("Failed to create git worktree")?;

            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                error!(stderr = %stderr, "git worktree add failed");
                return Err(anyhow!("git worktree add failed: {}", stderr));
            }
            info!(worktree_path = %worktree_path.display(), "Git worktree created successfully");

            // Use worktree path as agent_dir
            let agent_dir = worktree_path;

            // Write .mcp.json for the agent
            self.write_agent_mcp_config(&effective_project_dir, &agent_dir, options.agent_type)
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
                &issue.labels,
                &issue_url,
            );

            tracing::info!(
                issue_id,
                prompt_length = initial_prompt.len(),
                "Built initial prompt for agent"
            );

            // Zellij display name (emoji + short format)
            let display_name = options.agent_type.display_name(&issue_id, &slug);

            let mut env_vars = HashMap::new();
            env_vars.insert("EXOMONAD_AGENT_ID".to_string(), internal_name.clone());
            let session_id = self.event_session_id.as_deref().unwrap_or("default");
            env_vars.insert("EXOMONAD_SESSION_ID".to_string(), session_id.to_string());
            if let Some(port) = self.mcp_server_port {
                env_vars.insert("EXOMONAD_SERVER_PORT".to_string(), port.to_string());
            }

            // Open Zellij tab with cwd = worktree_path
            self.new_zellij_tab(
                &display_name,
                &agent_dir, // Use worktree path
                options.agent_type,
                Some(&initial_prompt),
                env_vars,
            )
            .await?;

            // Emit agent:started event
            if let Some(ref session) = self.zellij_session {
                let agent_id = crate::ui_protocol::AgentId::try_from(internal_name.clone())
                    .map_err(|e| anyhow!("Invalid agent_id: {}", e))?;
                let event = crate::ui_protocol::AgentEvent::AgentStarted {
                    agent_id,
                    timestamp: zellij_events::now_iso8601(),
                };
                if let Err(e) = zellij_events::emit_event(session, &event) {
                    warn!("Failed to emit agent:started event: {}", e);
                }
            }

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: agent_dir.to_string_lossy().to_string(),
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
    // Spawn Teammate (prompt-driven, no GitHub issue)
    // ========================================================================

    /// Spawn a named teammate with a direct prompt.
    ///
    /// Idempotent on teammate name: if already running, returns existing info.
    /// If config entry exists but Zellij tab is dead, cleans stale entry and respawns.
    /// No per-agent directories or MCP configs — agents share the repo's config.
    /// State lives in Teams config.json + Zellij tab only.
    #[tracing::instrument(skip(self, options), fields(name = %options.name))]
    pub async fn spawn_gemini_teammate(
        &self,
        options: &SpawnGeminiTeammateOptions,
    ) -> Result<SpawnResult> {
        info!(name = %options.name, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_gemini_teammate");

        let result = timeout(SPAWN_TIMEOUT, async {
            self.check_zellij_env()?;

            let effective_project_dir = self.effective_project_dir(options.subrepo.as_deref())?;

            // Sanitize name for internal use
            let slug = slugify(&options.name);
            let agent_suffix = options.agent_type.suffix();
            let internal_name = format!("{}-{}", slug, agent_suffix);
            let display_name = format!("{} {}", options.agent_type.emoji(), slug);

            // Idempotency check: if Zellij tab is alive, return existing info
            let tab_alive = self.is_zellij_tab_alive(&display_name).await;

            info!(
                name = %options.name,
                internal_name,
                tab_alive,
                "Idempotency check"
            );

            if tab_alive {
                info!(name = %options.name, "Teammate already running, returning existing");
                // TODO: Return actual worktree path if possible, but for now empty is fine as it's just info
                return Ok(SpawnResult {
                    agent_dir: String::new(),
                    tab_name: internal_name,
                    issue_title: options.name.clone(),
                    agent_type: options.agent_type.suffix().to_string(),
                });
            }

            // Determine base branch
            let base_branch = if let Some(ref b) = options.base_branch {
                b.clone()
            } else {
                // Default to current branch
                let current_branch_output = Command::new("git")
                    .args(["rev-parse", "--abbrev-ref", "HEAD"])
                    .current_dir(&effective_project_dir)
                    .output()
                    .await
                    .context("Failed to get current branch")?;
                String::from_utf8_lossy(&current_branch_output.stdout)
                    .trim()
                    .to_string()
            };

            // Use '.' separator to avoid directory/file conflicts in git refs
            // and avoid ambiguity with '-' word separators in slugs.
            let branch_name = format!("{}.{}", base_branch, slug);
            let worktree_path = self.worktree_base.join(&internal_name);

            // Clean up existing worktree if it exists
            if worktree_path.exists() {
                info!(path = %worktree_path.display(), "Removing existing worktree for idempotency");
                let _ = Command::new("git")
                    .args(["worktree", "remove", "--force", &worktree_path.to_string_lossy()])
                    .current_dir(&effective_project_dir)
                    .output()
                    .await;
            }

            info!(
                base_branch = %base_branch,
                branch_name = %branch_name,
                worktree_path = %worktree_path.display(),
                "Creating git worktree"
            );

            let output = Command::new("git")
                .args([
                    "worktree",
                    "add",
                    "-b",
                    &branch_name,
                    &worktree_path.to_string_lossy(),
                    &base_branch,
                ])
                .current_dir(&effective_project_dir)
                .output()
                .await
                .context("Failed to create git worktree")?;

            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                error!(stderr = %stderr, "git worktree add failed");
                return Err(anyhow!("git worktree add failed: {}", stderr));
            }

            let mut env_vars = HashMap::new();
            env_vars.insert("EXOMONAD_AGENT_ID".to_string(), internal_name.clone());
            let session_id = self.event_session_id.as_deref().unwrap_or("default");
            env_vars.insert("EXOMONAD_SESSION_ID".to_string(), session_id.to_string());
            if let Some(port) = self.mcp_server_port {
                env_vars.insert("EXOMONAD_SERVER_PORT".to_string(), port.to_string());
            }

            // Write per-agent MCP config into the worktree
            self.write_agent_mcp_config(&effective_project_dir, &worktree_path, options.agent_type)
                .await?;

            // For Gemini agents, point at worktree settings via env var
            if options.agent_type == AgentType::Gemini {
                let settings_path = worktree_path.join(".gemini").join("settings.json");
                env_vars.insert(
                    "GEMINI_CLI_SYSTEM_SETTINGS_PATH".to_string(),
                    settings_path.to_string_lossy().to_string(),
                );
            }

            // Open Zellij tab with cwd = worktree_path (Heavy/Isolated agents get Tabs)
            self.new_zellij_tab(
                &display_name,
                &worktree_path,
                options.agent_type,
                Some(&options.prompt),
                env_vars,
            )
            .await?;

            // Emit agent:started event
            if let Some(ref session) = self.zellij_session {
                let agent_id = crate::ui_protocol::AgentId::try_from(internal_name.clone())
                    .map_err(|e| anyhow!("Invalid agent_id: {}", e))?;
                let event = crate::ui_protocol::AgentEvent::AgentStarted {
                    agent_id,
                    timestamp: zellij_events::now_iso8601(),
                };
                if let Err(e) = zellij_events::emit_event(session, &event) {
                    warn!("Failed to emit agent:started event: {}", e);
                }
            }

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: String::new(),
                tab_name: internal_name,
                issue_title: options.name.clone(),
                agent_type: options.agent_type.suffix().to_string(),
            })
        })
        .await
        .map_err(|_| {
            let msg = format!("spawn_gemini_teammate timed out after {}s", SPAWN_TIMEOUT.as_secs());
            warn!(name = %options.name, error = %msg, "spawn_gemini_teammate timed out");
            anyhow::Error::new(TimeoutError { message: msg })
        })??;

        info!(name = %options.name, "spawn_gemini_teammate completed successfully");
        Ok(result)
    }

    /// Generate settings.json content for a Gemini worker.
    ///
    /// Constructs the JSON configuration including MCP server connection and lifecycle hooks.
    /// Note: Gemini hooks must be PascalCase (e.g. AfterAgent).
    pub(crate) fn generate_gemini_settings(mcp_url: &str) -> serde_json::Value {
        serde_json::json!({
            "mcpServers": {
                "exomonad": {
                    "httpUrl": mcp_url
                }
            },
            "hooks": {
                "BeforeTool": [
                    {
                        "matcher": "*",
                        "hooks": [
                            {
                                "type": "command",
                                "command": "exomonad hook before-tool --runtime gemini"
                            }
                        ]
                    }
                ],
                "AfterAgent": [
                    {
                        "matcher": "*",
                        "hooks": [
                            {
                                "type": "command",
                                "command": "exomonad hook worker-exit --runtime gemini"
                            }
                        ]
                    }
                ]
            }
        })
    }

    /// Spawn a Gemini worker agent (Phase 2/3).
    ///
    /// Creates a new git worktree and branch for isolation.
    #[tracing::instrument(skip(self, options), fields(name = %options.name))]
    pub async fn spawn_worker(&self, options: &SpawnWorkerOptions) -> Result<SpawnResult> {
        info!(name = %options.name, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_worker");

        let result = timeout(SPAWN_TIMEOUT, async {
            self.check_zellij_env()?;

            let mcp_port = self.mcp_server_port.ok_or_else(|| {
                anyhow!("MCP server port not set. Start server with `exomonad serve` before spawning agents.")
            })?;

            // Sanitize name for internal use
            let slug = slugify(&options.name);
            let internal_name = format!("{}-gemini", slug);
            let display_name = format!("{} {}", AgentType::Gemini.emoji(), slug);

            // Idempotency: if tab is already alive, return existing info
            let tab_alive = self.is_zellij_tab_alive(&display_name).await;
            if tab_alive {
                info!(name = %options.name, "Worker already running, returning existing");
                return Ok(SpawnResult {
                    agent_dir: String::new(),
                    tab_name: internal_name,
                    issue_title: options.name.clone(),
                    agent_type: "gemini".to_string(),
                });
            }

            // Get current branch
            let current_branch_output = Command::new("git")
                .args(["rev-parse", "--abbrev-ref", "HEAD"])
                .current_dir(&self.project_dir)
                .output()
                .await
                .context("Failed to get current branch")?;
            let current_branch = String::from_utf8_lossy(&current_branch_output.stdout)
                .trim()
                .to_string();

            let branch_name = format!("{}.{}", current_branch, slug);
            let worktree_path = self.worktree_base.join(&slug);

            // Clean up existing worktree
            if worktree_path.exists() {
                info!(path = %worktree_path.display(), "Removing existing worktree for idempotency");
                let _ = Command::new("git")
                    .args(["worktree", "remove", "--force", &worktree_path.to_string_lossy()])
                    .current_dir(&self.project_dir)
                    .output()
                    .await;
            }

            info!(
                base_branch = %current_branch,
                branch_name = %branch_name,
                worktree_path = %worktree_path.display(),
                "Creating git worktree for worker"
            );

            let output = Command::new("git")
                .args([
                    "worktree",
                    "add",
                    "-b",
                    &branch_name,
                    &worktree_path.to_string_lossy(),
                    &current_branch,
                ])
                .current_dir(&self.project_dir)
                .output()
                .await
                .context("Failed to create git worktree")?;

            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                error!(stderr = %stderr, "git worktree add failed");
                return Err(anyhow!("git worktree add failed: {}", stderr));
            }

            let mut env_vars = HashMap::new();
            env_vars.insert("EXOMONAD_AGENT_ID".to_string(), internal_name.clone());
            let session_id = self.event_session_id.as_deref().unwrap_or("default");
            env_vars.insert("EXOMONAD_SESSION_ID".to_string(), session_id.to_string());
            if let Some(port) = self.mcp_server_port {
                env_vars.insert("EXOMONAD_SERVER_PORT".to_string(), port.to_string());
            }

            // Write Gemini settings to worker config dir in project root
            let agent_config_dir = self.project_dir
                .join(".exomonad")
                .join("agents")
                .join(&internal_name);
            fs::create_dir_all(&agent_config_dir).await?;

            let settings_path = agent_config_dir.join("settings.json");
            let mcp_url = format!(
                "http://localhost:{}/agents/{}/mcp",
                mcp_port, internal_name
            );
            let settings = Self::generate_gemini_settings(&mcp_url);
            fs::write(&settings_path, serde_json::to_string_pretty(&settings)?).await?;
            info!(
                path = %settings_path.display(),
                url = %mcp_url,
                "Wrote worker Gemini settings to agent config dir"
            );

            env_vars.insert(
                "GEMINI_CLI_SYSTEM_SETTINGS_PATH".to_string(),
                settings_path.to_string_lossy().to_string(),
            );

            // Open Zellij TAB (not pane) with cwd = worktree_path
            self.new_zellij_tab(
                &display_name,
                &worktree_path,
                AgentType::Gemini,
                Some(&options.prompt),
                env_vars,
            )
            .await?;

            // Emit agent:started event
            if let Some(ref session) = self.zellij_session {
                let agent_id = crate::ui_protocol::AgentId::try_from(internal_name.clone())
                    .map_err(|e| anyhow!("Invalid agent_id: {}", e))?;
                let event = crate::ui_protocol::AgentEvent::AgentStarted {
                    agent_id,
                    timestamp: zellij_events::now_iso8601(),
                };
                if let Err(e) = zellij_events::emit_event(session, &event) {
                    warn!("Failed to emit agent:started event: {}", e);
                }
            }

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: worktree_path.to_string_lossy().to_string(),
                tab_name: internal_name,
                issue_title: options.name.clone(),
                agent_type: "gemini".to_string(),
            })
        })
        .await
        .map_err(|_| {
            let msg = format!("spawn_worker timed out after {}s", SPAWN_TIMEOUT.as_secs());
            warn!(name = %options.name, error = %msg, "spawn_worker timed out");
            anyhow::Error::new(TimeoutError { message: msg })
        })??;

        info!(name = %options.name, "spawn_worker completed successfully");
        Ok(result)
    }

    /// Spawn a subtree agent (Claude-only) in a new git worktree.
    #[tracing::instrument(skip(self, options), fields(branch_name = %options.branch_name))]
    pub async fn spawn_subtree(&self, options: &SpawnSubtreeOptions) -> Result<SpawnResult> {
        info!(branch_name = %options.branch_name, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_subtree");

        let result = timeout(SPAWN_TIMEOUT, async {
            self.check_zellij_env()?;

            // Depth check: session_id is the birth-branch.
            let session_id = self.event_session_id.as_deref().unwrap_or("root");
            let depth = if session_id == "root" {
                0
            } else {
                session_id.chars().filter(|&c| c == '.').count() + 1
            };

            if depth >= 2 {
                return Err(anyhow!("Subtree depth limit reached (max 2). Current session: {}, depth: {}", session_id, depth));
            }

            // Resolve effective project dir (subtree always spawns from current project root)
            let effective_project_dir = &self.project_dir;

            // Sanitize branch name for internal use
            let slug = slugify(&options.branch_name);
            let internal_name = format!("{}-claude", slug);
            let display_name = format!("{} {}", AgentType::Claude.emoji(), slug);

            // Idempotency check: if Zellij tab is alive, return existing info
            let tab_alive = self.is_zellij_tab_alive(&display_name).await;
            if tab_alive {
                info!(slug = %slug, "Subtree already running, returning existing");
                return Ok(SpawnResult {
                    agent_dir: String::new(),
                    tab_name: internal_name,
                    issue_title: options.branch_name.clone(),
                    agent_type: "claude".to_string(),
                });
            }

            // Get current branch for base
            let current_branch_output = Command::new("git")
                .args(["rev-parse", "--abbrev-ref", "HEAD"])
                .current_dir(effective_project_dir)
                .output()
                .await
                .context("Failed to get current branch")?;
            let current_branch = String::from_utf8_lossy(&current_branch_output.stdout)
                .trim()
                .to_string();

            // Branch: {current_branch}.{slug}
            let branch_name = format!("{}.{}", current_branch, slug);
            
            // Worktree location: {worktree_base}/{slug}
            let worktree_path = self.worktree_base.join(&slug);

            // Clean up existing worktree if it exists
            if worktree_path.exists() {
                info!(path = %worktree_path.display(), "Removing existing worktree for idempotency");
                let _ = Command::new("git")
                    .args(["worktree", "remove", "--force", &worktree_path.to_string_lossy()])
                    .current_dir(effective_project_dir)
                    .output()
                    .await;
            }

            info!(
                base_branch = %current_branch,
                branch_name = %branch_name,
                worktree_path = %worktree_path.display(),
                "Creating git worktree for subtree"
            );

            let output = Command::new("git")
                .args([
                    "worktree",
                    "add",
                    "-b",
                    &branch_name,
                    &worktree_path.to_string_lossy(),
                    &current_branch,
                ])
                .current_dir(effective_project_dir)
                .output()
                .await
                .context("Failed to create git worktree")?;

            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                error!(stderr = %stderr, "git worktree add failed");
                return Err(anyhow!("git worktree add failed: {}", stderr));
            }

            let mut env_vars = HashMap::new();
            env_vars.insert("EXOMONAD_AGENT_ID".to_string(), internal_name.clone());
            // Session ID = birth-branch (the new branch name)
            env_vars.insert("EXOMONAD_SESSION_ID".to_string(), branch_name.clone());
            if let Some(port) = self.mcp_server_port {
                env_vars.insert("EXOMONAD_SERVER_PORT".to_string(), port.to_string());
            }

            // Write .mcp.json in worktree root
            self.write_agent_mcp_config(effective_project_dir, &worktree_path, AgentType::Claude)
                .await?;

            // Build task prompt with worktree context warning
            let task_with_context = format!(
                "You are now in worktree {} on branch {}. All file paths from your inherited context are STALE — use relative paths only and re-read files before editing.\n\n{}",
                worktree_path.display(), branch_name, options.task
            );

            // Determine fork mode from parent_session_id
            let fork_id = if options.parent_session_id.is_empty() {
                None
            } else {
                Some(options.parent_session_id.as_str())
            };

            // Open Zellij tab with cwd = worktree_path
            self.new_zellij_tab_inner(
                &display_name,
                &worktree_path,
                AgentType::Claude,
                Some(&task_with_context),
                env_vars,
                fork_id,
            )
            .await?;

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: worktree_path.to_string_lossy().to_string(),
                tab_name: internal_name,
                issue_title: options.branch_name.clone(),
                agent_type: "claude".to_string(),
            })
        })
        .await
        .map_err(|_| {
            let msg = format!("spawn_subtree timed out after {}s", SPAWN_TIMEOUT.as_secs());
            warn!(branch_name = %options.branch_name, error = %msg, "spawn_subtree timed out");
            anyhow::Error::new(TimeoutError { message: msg })
        })??;

        info!(branch_name = %options.branch_name, "spawn_subtree completed successfully");
        Ok(result)
    }

    /// Spawn a Gemini leaf agent in a new git worktree.
    #[tracing::instrument(skip(self, options), fields(branch_name = %options.branch_name))]
    pub async fn spawn_leaf_subtree(&self, options: &SpawnSubtreeOptions) -> Result<SpawnResult> {
        info!(branch_name = %options.branch_name, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_leaf_subtree");

        let result = timeout(SPAWN_TIMEOUT, async {
            self.check_zellij_env()?;

            // No depth check for leaf nodes.

            let effective_project_dir = &self.project_dir;

            // Sanitize branch name
            let slug = slugify(&options.branch_name);
            let internal_name = format!("{}-gemini", slug); // gemini suffix
            let display_name = format!("{} {}", AgentType::Gemini.emoji(), slug);

            // Idempotency check
            let tab_alive = self.is_zellij_tab_alive(&display_name).await;
            if tab_alive {
                info!(slug = %slug, "Leaf subtree already running, returning existing");
                return Ok(SpawnResult {
                    agent_dir: String::new(),
                    tab_name: internal_name,
                    issue_title: options.branch_name.clone(),
                    agent_type: "gemini".to_string(),
                });
            }

            // Get current branch
            let current_branch_output = Command::new("git")
                .args(["rev-parse", "--abbrev-ref", "HEAD"])
                .current_dir(effective_project_dir)
                .output()
                .await
                .context("Failed to get current branch")?;
            let current_branch = String::from_utf8_lossy(&current_branch_output.stdout)
                .trim()
                .to_string();

            let branch_name = format!("{}.{}", current_branch, slug);
            let worktree_path = self.worktree_base.join(&slug);

            // Clean up existing worktree
            if worktree_path.exists() {
                info!(path = %worktree_path.display(), "Removing existing worktree for idempotency");
                let _ = Command::new("git")
                    .args(["worktree", "remove", "--force", &worktree_path.to_string_lossy()])
                    .current_dir(effective_project_dir)
                    .output()
                    .await;
            }

            info!(
                base_branch = %current_branch,
                branch_name = %branch_name,
                worktree_path = %worktree_path.display(),
                "Creating git worktree for leaf subtree"
            );

            let output = Command::new("git")
                .args([
                    "worktree",
                    "add",
                    "-b",
                    &branch_name,
                    &worktree_path.to_string_lossy(),
                    &current_branch,
                ])
                .current_dir(effective_project_dir)
                .output()
                .await
                .context("Failed to create git worktree")?;

            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                error!(stderr = %stderr, "git worktree add failed");
                return Err(anyhow!("git worktree add failed: {}", stderr));
            }

            let mut env_vars = HashMap::new();
            env_vars.insert("EXOMONAD_AGENT_ID".to_string(), internal_name.clone());
            // Session ID = birth-branch (the new branch name)
            env_vars.insert("EXOMONAD_SESSION_ID".to_string(), branch_name.clone());
            if let Some(port) = self.mcp_server_port {
                env_vars.insert("EXOMONAD_SERVER_PORT".to_string(), port.to_string());
            }

            // Write .gemini/settings.json in worktree root (using write_agent_mcp_config logic)
            self.write_agent_mcp_config(effective_project_dir, &worktree_path, AgentType::Gemini)
                .await?;

            // Set GEMINI_CLI_SYSTEM_SETTINGS_PATH
            let settings_path = worktree_path.join(".gemini").join("settings.json");
            env_vars.insert(
                "GEMINI_CLI_SYSTEM_SETTINGS_PATH".to_string(),
                settings_path.to_string_lossy().to_string(),
            );

            // Open Zellij TAB (not pane)
            self.new_zellij_tab(
                &display_name,
                &worktree_path,
                AgentType::Gemini,
                Some(&options.task),
                env_vars,
            )
            .await?;

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: worktree_path.to_string_lossy().to_string(),
                tab_name: internal_name,
                issue_title: options.branch_name.clone(),
                agent_type: "gemini".to_string(),
            })
        })
        .await
        .map_err(|_| {
            let msg = format!("spawn_leaf_subtree timed out after {}s", SPAWN_TIMEOUT.as_secs());
            warn!(branch_name = %options.branch_name, error = %msg, "spawn_leaf_subtree timed out");
            anyhow::Error::new(TimeoutError { message: msg })
        })??;

        info!(branch_name = %options.branch_name, "spawn_leaf_subtree completed successfully");
        Ok(result)
    }

    // ========================================================================
    // Cleanup Agent
    // ========================================================================

    /// Clean up an agent by identifier (internal_name or issue_id).
    ///
    /// Kills the Zellij tab, unregisters from Teams config.json,
    /// and removes per-agent config directory (`.exomonad/agents/{name}/`).
    #[tracing::instrument(skip(self))]
    pub async fn cleanup_agent(&self, identifier: &str, _force: bool) -> Result<()> {
        // Try to find agent in list (for metadata and tab matching).
        // Failure here is non-fatal to allow cleaning up worker panes (invisible to list_agents).
        let agents = self.list_agents().await.unwrap_or_default();
        let agent = agents.iter().find(|a| a.issue_id == identifier);

        info!(
            identifier,
            found = agent.is_some(),
            "Initiating cleanup_agent"
        );

        // Reconstruct names for paths and exact tab matching.
        // If agent not found, assume gemini worker (Phase 1 convention).
        let (suffix, display_name) = match agent {
            Some(a) => {
                let s = a.agent_type.as_deref().unwrap_or("gemini");
                let emoji = if s == "claude" {
                    AgentType::Claude.emoji()
                } else {
                    AgentType::Gemini.emoji()
                };
                (s, Some(format!("{} {}", emoji, identifier)))
            }
            None => ("gemini", None),
        };

        let internal_name = format!("{}-{}", identifier, suffix);

        // Close Zellij tab if found in list
        if let Some(target_tab) = display_name {
            let tabs = self.get_zellij_tabs().await.unwrap_or_default();
            for tab in &tabs {
                if tab == &target_tab {
                    if let Err(e) = self.close_zellij_tab(tab).await {
                        warn!(tab_name = %tab, error = %e, "Failed to close Zellij tab (may not exist)");
                    }
                    break;
                }
            }
        }

        // Remove temporary worker config directory (created by spawn_worker)
        let tmp_agent_dir = PathBuf::from(format!("/tmp/exomonad-agents/{}", internal_name));
        if tmp_agent_dir.exists() {
            if let Err(e) = fs::remove_dir_all(&tmp_agent_dir).await {
                warn!(
                    path = %tmp_agent_dir.display(),
                    error = %e,
                    "Failed to remove temp agent config dir (non-fatal)"
                );
            } else {
                info!(path = %tmp_agent_dir.display(), "Removed temp agent config dir");
            }
        }

        // Remove per-agent config directory (.exomonad/agents/{name}/)
        let agent_config_dir = self
            .project_dir
            .join(".exomonad")
            .join("agents")
            .join(&internal_name);
        if agent_config_dir.exists() {
            if let Err(e) = fs::remove_dir_all(&agent_config_dir).await {
                warn!(
                    path = %agent_config_dir.display(),
                    error = %e,
                    "Failed to remove per-agent config dir (non-fatal)"
                );
            } else {
                info!(path = %agent_config_dir.display(), "Removed per-agent config dir");
            }
        }

        // Remove git worktree if it exists
        let worktree_path = self.worktree_base.join(&internal_name);
        if worktree_path.exists() {
            info!(path = %worktree_path.display(), "Removing git worktree");
            let output = Command::new("git")
                .args([
                    "worktree",
                    "remove",
                    "--force",
                    &worktree_path.to_string_lossy(),
                ])
                .current_dir(&self.project_dir)
                .output()
                .await;
            match output {
                Ok(o) if o.status.success() => {
                    info!(path = %worktree_path.display(), "Git worktree removed");
                }
                Ok(o) => {
                    let stderr = String::from_utf8_lossy(&o.stderr);
                    warn!(
                        path = %worktree_path.display(),
                        stderr = %stderr,
                        "git worktree remove failed (non-fatal)"
                    );
                }
                Err(e) => {
                    warn!(
                        path = %worktree_path.display(),
                        error = %e,
                        "Failed to run git worktree remove (non-fatal)"
                    );
                }
            }
        }

        // Emit agent:stopped event
        if let Some(ref session) = self.zellij_session {
            if let Ok(agent_id) = crate::ui_protocol::AgentId::try_from(identifier.to_string()) {
                let event = crate::ui_protocol::AgentEvent::AgentStopped {
                    agent_id,
                    timestamp: zellij_events::now_iso8601(),
                };
                if let Err(e) = zellij_events::emit_event(session, &event) {
                    warn!("Failed to emit agent:stopped event: {}", e);
                }
            }
        }

        Ok(())
    }

    /// Clean up multiple agents.
    #[tracing::instrument(skip(self))]
    pub async fn cleanup_agents(
        &self,
        issue_ids: &[String],
        force: bool,
        subrepo: Option<&str>,
    ) -> BatchCleanupResult {
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

    /// Clean up agents whose work is complete.
    ///
    /// Without worktrees, there are no per-agent branches to check for merge status.
    /// This now simply cleans up stopped agents matching the given issue filter.
    #[tracing::instrument(skip(self))]
    pub async fn cleanup_merged_agents(
        &self,
        issues: &[String],
        subrepo: Option<&str>,
    ) -> Result<BatchCleanupResult> {
        let agents = self.list_agents().await?;
        let mut to_cleanup = Vec::new();

        let issue_filter: Option<HashSet<&str>> = if issues.is_empty() {
            None
        } else {
            Some(issues.iter().map(|s| s.as_str()).collect())
        };

        for agent in agents {
            if let Some(ref filter) = issue_filter {
                if !filter.contains(agent.issue_id.as_str()) {
                    continue;
                }
            }

            // Only clean up stopped agents (no running tab)
            if agent.status == AgentStatus::Stopped {
                info!(issue_id = %agent.issue_id, "Agent is stopped, marking for cleanup");
                to_cleanup.push(agent.issue_id);
            }
        }

        if to_cleanup.is_empty() {
            return Ok(BatchCleanupResult {
                cleaned: Vec::new(),
                failed: Vec::new(),
            });
        }

        Ok(self.cleanup_agents(&to_cleanup, false, subrepo).await)
    }

    // ========================================================================
    // List Agents
    // ========================================================================

    /// List all active agents by scanning the filesystem and verifying with Zellij.
    ///
    /// Discovery process:
    /// 1. Scan {worktree_base}/ for subtree agents (isolated worktrees)
    /// 2. Scan {project_dir}/.exomonad/agents/ for worker agents (shared worktree)
    /// 3. Verify liveness by checking Zellij tabs/panes
    #[tracing::instrument(skip(self))]
    pub async fn list_agents(&self) -> Result<Vec<AgentInfo>> {
        let mut agents = Vec::new();

        // Get all Zellij tabs for liveness check
        let tabs = self.get_zellij_tabs().await.unwrap_or_default();

        // 1. Scan worktree_base for subtree agents
        if self.worktree_base.exists() {
            let mut entries = fs::read_dir(&self.worktree_base).await?;
            while let Some(entry) = entries.next_entry().await? {
                if entry.file_type().await?.is_dir() {
                    let path = entry.path();
                    let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

                    // Check for .mcp.json (Claude) or .gemini/settings.json (Gemini)
                    let is_claude = path.join(".mcp.json").exists();
                    let is_gemini = path.join(".gemini/settings.json").exists();

                    if is_claude || is_gemini {
                        let agent_type = if is_claude { "claude" } else { "gemini" };
                        let emoji = if is_claude {
                            AgentType::Claude.emoji()
                        } else {
                            AgentType::Gemini.emoji()
                        };
                        let display_name = format!("{} {}", emoji, name);

                        let has_tab = tabs.iter().any(|t| t == &display_name);
                        let status = if has_tab {
                            AgentStatus::Running
                        } else {
                            AgentStatus::Stopped
                        };

                        agents.push(AgentInfo {
                            issue_id: name.to_string(),
                            has_tab,
                            status,
                            topology: Topology::WorktreePerAgent,
                            agent_dir: Some(path.to_string_lossy().to_string()),
                            slug: Some(name.to_string()),
                            agent_type: Some(agent_type.to_string()),
                            pr: None,
                        });

                        // 2. Scan subtree's .exomonad/agents for workers
                        let subtree_agents_dir = path.join(".exomonad/agents");
                        if subtree_agents_dir.exists() {
                            self.scan_workers(&subtree_agents_dir, &tabs, &mut agents)
                                .await?;
                        }
                    }
                }
            }
        }

        // 3. Scan root .exomonad/agents for workers
        let root_agents_dir = self.project_dir.join(".exomonad/agents");
        if root_agents_dir.exists() {
            self.scan_workers(&root_agents_dir, &tabs, &mut agents)
                .await?;
        }

        Ok(agents)
    }

    /// Helper to scan a directory for worker agents.
    async fn scan_workers(
        &self,
        dir: &Path,
        tabs: &[String],
        agents: &mut Vec<AgentInfo>,
    ) -> Result<()> {
        let mut entries = fs::read_dir(dir).await?;
        while let Some(entry) = entries.next_entry().await? {
            if entry.file_type().await?.is_dir() {
                let path = entry.path();
                let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

                // Workers are currently Gemini-only
                if name.ends_with("-gemini") {
                    let base_name = name.strip_suffix("-gemini").unwrap_or(name);
                    let display_name = format!("{} {}", AgentType::Gemini.emoji(), base_name);

                    // Liveness: for workers, they might be panes in a tab.
                    // Currently list_agents only sees tabs. Zellij doesn't easily query pane names across tabs.
                    // For now, if it's in a tab list, it's running.
                    let has_tab = tabs.iter().any(|t| t == &display_name);
                    let status = if has_tab {
                        AgentStatus::Running
                    } else {
                        AgentStatus::Stopped
                    };

                    agents.push(AgentInfo {
                        issue_id: name.to_string(),
                        has_tab,
                        status,
                        topology: Topology::SharedDir,
                        agent_dir: Some(path.to_string_lossy().to_string()),
                        slug: Some(base_name.to_string()),
                        agent_type: Some("gemini".to_string()),
                        pr: None,
                    });
                }
            }
        }
        Ok(())
    }

    // ========================================================================
    // Internal: Server Discovery
    // ========================================================================

    /// Read server.pid and extract the HTTP port.
    fn read_server_port(project_dir: &Path) -> Option<u16> {
        let pid_path = project_dir.join(".exomonad/server.pid");
        let content = std::fs::read_to_string(&pid_path).ok()?;
        let parsed: serde_json::Value = serde_json::from_str(&content).ok()?;

        let pid = parsed.get("pid")?.as_u64()? as u32;

        // Verify the process is still alive
        #[cfg(unix)]
        {
            use nix::sys::signal;
            use nix::unistd::Pid;
            if signal::kill(Pid::from_raw(pid as i32), None).is_err() {
                debug!(pid, "server.pid exists but process is dead, ignoring");
                return None;
            }
        }

        parsed
            .get("port")
            .and_then(|v| v.as_u64())
            .map(|port| port as u16)
    }

    // ========================================================================
    // Internal: Zellij
    // ========================================================================

    fn check_zellij_env(&self) -> Result<String> {
        std::env::var("ZELLIJ_SESSION_NAME")
            .context("Not running inside a Zellij session (ZELLIJ_SESSION_NAME not set)")
    }

    #[tracing::instrument(skip(self, prompt, env_vars))]
    async fn new_zellij_tab(
        &self,
        name: &str,
        cwd: &Path,
        agent_type: AgentType,
        prompt: Option<&str>,
        env_vars: HashMap<String, String>,
    ) -> Result<()> {
        self.new_zellij_tab_inner(name, cwd, agent_type, prompt, env_vars, None)
            .await
    }

    #[tracing::instrument(skip(self, prompt, env_vars, fork_session_id))]
    async fn new_zellij_tab_inner(
        &self,
        name: &str,
        cwd: &Path,
        agent_type: AgentType,
        prompt: Option<&str>,
        env_vars: HashMap<String, String>,
        fork_session_id: Option<&str>,
    ) -> Result<()> {
        info!(name, cwd = %cwd.display(), agent_type = ?agent_type, fork = fork_session_id.is_some(), "Creating Zellij tab");

        let cmd = agent_type.command();
        let agent_command = match (prompt, fork_session_id) {
            (Some(p), Some(session_id)) => {
                // Fork mode: claude --resume <id> --fork-session -p '<task>'
                let escaped_prompt = Self::escape_for_shell_command(p);
                let escaped_session = Self::escape_for_shell_command(session_id);
                debug!(
                    tab_name = name,
                    agent_type = ?agent_type,
                    prompt_length = p.len(),
                    "Spawning agent with session fork"
                );
                format!(
                    "{} --resume {} --fork-session -p {}",
                    cmd, escaped_session, escaped_prompt
                )
            }
            (Some(p), None) => {
                let escaped_prompt = Self::escape_for_shell_command(p);
                debug!(
                    tab_name = name,
                    agent_type = ?agent_type,
                    prompt_length = p.len(),
                    "Spawning agent with CLI prompt"
                );
                let flag = agent_type.prompt_flag();
                if flag.is_empty() {
                    format!("{} {}", cmd, escaped_prompt)
                } else {
                    format!("{} {} {}", cmd, flag, escaped_prompt)
                }
            }
            _ => cmd.to_string(),
        };

        // Prepend env vars to command (Zellij KDL doesn't support pane-level env blocks)
        let env_prefix = env_vars
            .iter()
            .map(|(k, v)| format!("{}={}", k, shell_escape::escape(v.into())))
            .collect::<Vec<_>>()
            .join(" ");
        let full_command = if env_prefix.is_empty() {
            agent_command
        } else {
            format!("{} {}", env_prefix, agent_command)
        };

        // Wrap in nix develop shell if flake.nix exists in cwd
        let full_command = if cwd.join("flake.nix").exists() {
            info!("Wrapping agent command in nix develop shell");
            // Inner command needs single-quote escaping for the sh -c wrapper
            let escaped = full_command.replace('\'', "'\\''");
            format!("nix develop -c sh -c '{}'", escaped)
        } else {
            full_command
        };

        // Escape the command for KDL string literal (escape backslashes, quotes, newlines)
        let kdl_escaped_command = Self::escape_for_kdl(&full_command);

        // Use login shell to ensure PATH is loaded (gemini, claude, etc.)
        let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/zsh".to_string());

        // Generate layout using zellij-gen library (includes zjstatus with Solarized Dark)
        let params = crate::layout::AgentTabParams {
            tab_name: name,
            pane_name: name,
            command: &kdl_escaped_command,
            cwd,
            shell: &shell,
            focus: true,
            close_on_exit: true,
        };

        let layout_content = crate::layout::generate_agent_layout(&params)
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

        // Explicitly rename pane to ensure it matches display_name exactly
        let _ = Command::new("zellij")
            .args(["action", "rename-pane", name])
            .output()
            .await;

        Ok(())
    }

    #[tracing::instrument(skip(self))]
    async fn get_zellij_tabs(&self) -> Result<Vec<String>> {
        debug!("Querying Zellij tab names");
        let output = Command::new("zellij")
            .args(["action", "query-tab-names"])
            .output()
            .await
            .context("Failed to execute zellij action query-tab-names")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // If zellij is not running or other error, return empty list instead of failing
            warn!(stderr = %stderr, "zellij query-tab-names failed, assuming no tabs");
            return Ok(Vec::new());
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        Ok(stdout.lines().map(|s| s.to_string()).collect())
    }

    /// Check if a Zellij tab with the given display name exists.
    async fn is_zellij_tab_alive(&self, display_name: &str) -> bool {
        self.get_zellij_tabs()
            .await
            .unwrap_or_default()
            .iter()
            .any(|tab| tab == display_name)
    }

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
    // Internal: Agent Config Files
    // ========================================================================

    /// Write MCP config for the agent directory.
    ///
    /// Claude agents get `.mcp.json`. Gemini agents get `.gemini/settings.json`.
    /// Requires a running HTTP server (`exomonad serve --port <port>`).
    async fn write_agent_mcp_config(
        &self,
        effective_dir: &Path,
        agent_dir: &Path,
        agent_type: AgentType,
    ) -> Result<()> {
        let port = Self::read_server_port(effective_dir).ok_or_else(|| {
            anyhow::anyhow!("No MCP server running. Start one with `exomonad serve --port <port>`.")
        })?;

        let agent_name = agent_dir
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown");

        let mcp_content = Self::generate_mcp_config(agent_name, port, agent_type);

        match agent_type {
            AgentType::Claude => {
                fs::write(agent_dir.join(".mcp.json"), mcp_content).await?;
                info!(agent_dir = %agent_dir.display(), "Wrote .mcp.json for Claude agent");
            }
            AgentType::Gemini => {
                let gemini_dir = agent_dir.join(".gemini");
                fs::create_dir_all(&gemini_dir).await?;
                fs::write(gemini_dir.join("settings.json"), mcp_content).await?;
                info!(agent_dir = %agent_dir.display(), "Wrote .gemini/settings.json for Gemini agent");
            }
        }
        Ok(())
    }

    /// Generate MCP configuration JSON for an agent.
    fn generate_mcp_config(name: &str, port: u16, agent_type: AgentType) -> String {
        match agent_type {
            AgentType::Claude => {
                format!(
                    r###"{{
  "mcpServers": {{
    "exomonad": {{
      "type": "http",
      "url": "http://localhost:{port}/agents/{name}/mcp"
    }}
  }}
}}"###,
                    port = port,
                    name = name,
                )
            }
            AgentType::Gemini => {
                format!(
                    r###"{{
  "mcpServers": {{
    "exomonad": {{
      "httpUrl": "http://localhost:{port}/agents/{name}/mcp"
    }}
  }},
  "hooks": {{
    "BeforeTool": [
      {{
        "matcher": "*",
        "hooks": [
          {{
            "type": "command",
            "command": "exomonad hook before-tool --runtime gemini"
          }}
        ]
      }}
    ],
    "AfterAgent": [
      {{
        "matcher": "*",
        "hooks": [
          {{
            "type": "command",
            "command": "exomonad hook after-agent --runtime gemini"
          }}
        ]
      }}
    ]
  }}
}}"###,
                    port = port,
                    name = name,
                )
            }
        }
    }

    // ========================================================================
    // Internal: Prompt Building
    // ========================================================================

    /// Build the initial prompt for a spawned agent.
    fn build_initial_prompt(
        issue_id: &str,
        title: &str,
        body: &str,
        labels: &[String],
        issue_url: &str,
    ) -> String {
        let labels_str = if labels.is_empty() {
            "None".to_string()
        } else {
            labels
                .iter()
                .map(|l| format!("`{}`", l))
                .collect::<Vec<_>>()
                .join(", ")
        };

        format!(
            r###"# Issue #{issue_id}: {title}

**Issue URL:** {issue_url}
**Labels:** {labels_str}

## Description

{body}"###,
            issue_id = issue_id,
            title = title,
            issue_url = issue_url,
            labels_str = labels_str,
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

#[cfg(test)]
mod tests {
    // Legacy helpers used only by tests for issue-driven agent dir name parsing.
    #[derive(Debug, PartialEq)]
    struct ParsedAgentDirName<'a> {
        issue_id: &'a str,
        slug: &'a str,
        agent_type: Option<super::AgentType>,
    }

    fn parse_agent_dir_name(name: &str) -> Option<ParsedAgentDirName<'_>> {
        let rest = name.strip_prefix("gh-")?;
        let (issue_id, rest) = rest.split_once('-')?;
        let (slug, agent_suffix) = rest.rsplit_once('-')?;
        let agent_type = match agent_suffix {
            "claude" => Some(super::AgentType::Claude),
            "gemini" => Some(super::AgentType::Gemini),
            _ => None,
        };
        Some(ParsedAgentDirName {
            issue_id,
            slug,
            agent_type,
        })
    }
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
            &["bug".to_string(), "priority".to_string()],
            "https://github.com/owner/repo/issues/123",
        );

        assert!(prompt.contains("# Issue #123: Fix the bug"));
        assert!(prompt.contains("Description"));
        assert!(prompt.contains("https://github.com/owner/repo/issues/123"));
        assert!(prompt.contains("**Labels:** `bug`, `priority`"));
    }

    #[test]
    fn test_build_initial_prompt_no_labels() {
        let prompt = AgentControlService::build_initial_prompt(
            "123",
            "Fix the bug",
            "Description",
            &[],
            "https://github.com/owner/repo/issues/123",
        );

        assert!(prompt.contains("**Labels:** None"));
    }

    #[test]
    fn test_agent_type_command() {
        assert_eq!(AgentType::Claude.command(), "claude");
        assert_eq!(AgentType::Gemini.command(), "gemini");
    }

    #[test]
    fn test_agent_type_prompt_flag() {
        assert_eq!(AgentType::Claude.prompt_flag(), "");
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
            "🤖 gh-473-refactor-polish"
        );
        assert_eq!(
            AgentType::Gemini.display_name("123", "fix-bug"),
            "💎 gh-123-fix-bug"
        );
    }

    #[test]
    fn test_agent_type_display_name_no_truncation() {
        let long_slug = "this-is-a-very-long-slug-that-should-be-truncated";
        let display = AgentType::Claude.display_name("123", long_slug);
        assert_eq!(
            display,
            "🤖 gh-123-this-is-a-very-long-slug-that-should-be-truncated"
        );
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
    fn test_parse_agent_dir_name_claude() {
        let parsed = parse_agent_dir_name("gh-123-fix-bug-claude").unwrap();
        assert_eq!(parsed.issue_id, "123");
        assert_eq!(parsed.slug, "fix-bug");
        assert_eq!(parsed.agent_type, Some(AgentType::Claude));
    }

    #[test]
    fn test_parse_agent_dir_name_gemini() {
        let parsed = parse_agent_dir_name("gh-456-add-feature-gemini").unwrap();
        assert_eq!(parsed.issue_id, "456");
        assert_eq!(parsed.slug, "add-feature");
        assert_eq!(parsed.agent_type, Some(AgentType::Gemini));
    }

    #[test]
    fn test_parse_agent_dir_name_slug_with_hyphens() {
        let parsed = parse_agent_dir_name("gh-789-fix-the-big-bug-claude").unwrap();
        assert_eq!(parsed.issue_id, "789");
        assert_eq!(parsed.slug, "fix-the-big-bug");
        assert_eq!(parsed.agent_type, Some(AgentType::Claude));
    }

    #[test]
    fn test_parse_agent_dir_name_unknown_suffix() {
        let parsed = parse_agent_dir_name("gh-123-test-unknown").unwrap();
        assert_eq!(parsed.issue_id, "123");
        assert_eq!(parsed.slug, "test");
        assert_eq!(parsed.agent_type, None);
    }

    #[test]
    fn test_parse_agent_dir_name_invalid_format() {
        assert!(parse_agent_dir_name("123-test-claude").is_none());
        assert!(parse_agent_dir_name("gh-nohyphens").is_none());
        assert!(parse_agent_dir_name("gh-123").is_none());
    }

    #[test]
    fn test_claude_mcp_config_format() {
        let config =
            AgentControlService::generate_mcp_config("test-claude", 7432, AgentType::Claude);
        let parsed: serde_json::Value = serde_json::from_str(&config).unwrap();
        assert_eq!(
            parsed["mcpServers"]["exomonad"]["url"],
            "http://localhost:7432/agents/test-claude/mcp"
        );
        assert!(parsed["mcpServers"]["exomonad"].get("httpUrl").is_none());
    }

    #[test]
    fn test_gemini_mcp_config_format() {
        let config =
            AgentControlService::generate_mcp_config("test-gemini", 7432, AgentType::Gemini);
        let parsed: serde_json::Value = serde_json::from_str(&config).unwrap();
        assert_eq!(
            parsed["mcpServers"]["exomonad"]["httpUrl"],
            "http://localhost:7432/agents/test-gemini/mcp"
        );
        assert!(parsed["mcpServers"]["exomonad"].get("url").is_none());

        // Check hooks
        let before_tool = &parsed["hooks"]["BeforeTool"];
        assert!(before_tool.is_array());
        let bt_hooks = &before_tool[0]["hooks"];
        assert_eq!(
            bt_hooks[0]["command"],
            "exomonad hook before-tool --runtime gemini"
        );

        let after_agent = &parsed["hooks"]["AfterAgent"];
        assert!(after_agent.is_array());
        let hooks_list = &after_agent[0]["hooks"];
        assert_eq!(
            hooks_list[0]["command"],
            "exomonad hook after-agent --runtime gemini"
        );
    }

    #[test]
    fn test_gemini_settings_schema_compliance() {
        let settings = AgentControlService::generate_gemini_settings("http://example.com/mcp");

        // Assertions based on manual schema validation

        // 1. Hooks must strictly use PascalCase, not kebab-case or camelCase.
        assert!(
            settings["hooks"].get("AfterAgent").is_some(),
            "hooks.AfterAgent is missing"
        );
        assert!(
            settings["hooks"].get("BeforeTool").is_some(),
            "hooks.BeforeTool is missing"
        );
        assert!(
            settings["hooks"].get("after-agent").is_none(),
            "Found invalid kebab-case 'after-agent'"
        );
        assert!(
            settings["hooks"].get("afterAgent").is_none(),
            "Found invalid camelCase 'afterAgent'"
        );

        // 2. The hook structure must match the array of matcher/hooks objects
        let after_agent = &settings["hooks"]["AfterAgent"];
        assert!(after_agent.is_array(), "hooks.AfterAgent must be an array");

        let first_rule = &after_agent[0];
        assert_eq!(
            first_rule["matcher"], "*",
            "AfterAgent matcher must be wildcard '*'"
        );

        let hooks_list = &first_rule["hooks"];
        assert!(
            hooks_list.is_array(),
            "hooks list inside rule must be an array"
        );

        let command_hook = &hooks_list[0];
        assert_eq!(
            command_hook["type"], "command",
            "Hook type must be 'command'"
        );
        assert_eq!(
            command_hook["command"], "exomonad hook worker-exit --runtime gemini",
            "Hook command mismatch"
        );

        // 3. Verify no args array is present (command string used)
        assert!(
            command_hook.get("args").is_none(),
            "args should not be present when using command string"
        );
    }
}
