//! High-level agent control service.
//!
//! Provides semantic operations for agent lifecycle management:
//! - SpawnAgent: Create worktree, write context, open Zellij tab
//! - CleanupAgent: Close tab, delete worktree
//! - ListAgents: List active agent worktrees
//!
//! These are high-level effects exposed to Haskell WASM, not granular operations.

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
use tracing::{debug, info, warn};

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
    prompt_flag: "--prompt",
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
        let short_slug: String = slug.chars().take(20).collect();
        format!("{} gh-{}-{}", self.emoji(), issue_id, short_slug)
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
    /// Optional Claude Code Team name to register the agent in.
    pub team_name: Option<String>,
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
    /// Optional Claude Code Team name to register the agent in.
    pub team_name: Option<String>,
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

/// Information about an active agent.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AgentInfo {
    /// Issue ID (e.g., "123")
    pub issue_id: String,
    /// Whether a Zellij tab exists for this agent
    pub has_tab: bool,
    /// Status of the agent
    pub status: AgentStatus,
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

/// Server connection info discovered from server.pid.
#[derive(Debug, Clone)]
enum ServerInfo {
    /// Unix socket path
    Socket(String),
    /// HTTP port
    Http(u16),
}

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
    /// Zellij session name for event emission
    zellij_session: Option<String>,
}

impl AgentControlService {
    /// Create a new agent control service.
    pub fn new(project_dir: PathBuf, github: Option<GitHubService>) -> Self {
        Self {
            project_dir,
            github,
            zellij_session: None,
        }
    }

    /// Set the Zellij session name for event emission.
    pub fn with_zellij_session(mut self, session: String) -> Self {
        self.zellij_session = Some(session);
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
            project_dir,
            github,
            zellij_session: None,
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

            // Create agent directory
            let agent_dir = effective_project_dir
                .join(".exomonad")
                .join("agents")
                .join(&internal_name);
            fs::create_dir_all(&agent_dir).await?;
            info!(agent_dir = %agent_dir.display(), "Created agent directory");

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

            // Discover and register in Claude Code Team if applicable
            let team_name = options.team_name.clone().or_else(|| std::env::var("CLAUDE_TEAM_NAME").ok());
            if let Some(ref team) = team_name {
                info!(team_name = %team, agent_name = %internal_name, "Registering agent in Claude Code Team");
                if let Err(e) = super::teams::TeamsService::register_agent(
                    team,
                    &internal_name,
                    &effective_project_dir,
                    &issue.body,
                ).await {
                    warn!(error = %e, "Failed to register agent in Claude Code Team (non-fatal)");
                }
            }

            let mut env_vars = HashMap::new();
            if let Some(ref team) = team_name {
                env_vars.insert("EXOMONAD_TEAM_NAME".to_string(), team.clone());
                env_vars.insert("EXOMONAD_AGENT_ID".to_string(), internal_name.clone());
                env_vars.insert("CLAUDE_TEAM_NAME".to_string(), team.clone());
            }

            // Open Zellij tab with cwd = project_dir (not a worktree)
            self.new_zellij_tab(
                &display_name,
                &effective_project_dir,
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
    /// Unlike `spawn_agent`, this does not require a GitHub issue or create a worktree.
    /// The agent runs from the effective project directory with its own agent dir for config.
    #[tracing::instrument(skip(self, options), fields(name = %options.name))]
    pub async fn spawn_gemini_teammate(&self, options: &SpawnGeminiTeammateOptions) -> Result<SpawnResult> {
        info!(name = %options.name, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_gemini_teammate");

        let result = timeout(SPAWN_TIMEOUT, async {
            self.check_zellij_env()?;

            let effective_project_dir = self.effective_project_dir(options.subrepo.as_deref())?;

            let agent_suffix = options.agent_type.suffix();
            let internal_name = format!("{}-{}", options.name, agent_suffix);

            // Create agent directory
            let agent_dir = effective_project_dir
                .join(".exomonad")
                .join("agents")
                .join(&internal_name);
            fs::create_dir_all(&agent_dir).await?;
            info!(agent_dir = %agent_dir.display(), "Created agent directory");

            // Write .mcp.json for the agent
            self.write_agent_mcp_config(&effective_project_dir, &agent_dir, options.agent_type)
                .await?;

            // Zellij display name
            let display_name = format!("{} {}", options.agent_type.emoji(), options.name);

            // Discover and register in Claude Code Team if applicable
            let team_name = options
                .team_name
                .clone()
                .or_else(|| std::env::var("CLAUDE_TEAM_NAME").ok());
            if let Some(ref team) = team_name {
                info!(team_name = %team, agent_name = %internal_name, "Registering teammate in Claude Code Team");
                if let Err(e) = super::teams::TeamsService::register_agent(
                    team,
                    &internal_name,
                    &effective_project_dir,
                    &options.prompt,
                )
                .await
                {
                    warn!(error = %e, "Failed to register teammate in Claude Code Team (non-fatal)");
                }
            }

            let mut env_vars = HashMap::new();
            if let Some(ref team) = team_name {
                env_vars.insert("EXOMONAD_TEAM_NAME".to_string(), team.clone());
                env_vars.insert("EXOMONAD_AGENT_ID".to_string(), internal_name.clone());
                env_vars.insert("CLAUDE_TEAM_NAME".to_string(), team.clone());
            }

            // Open Zellij tab with cwd = effective_project_dir
            self.new_zellij_tab(
                &display_name,
                &effective_project_dir,
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
                agent_dir: agent_dir.to_string_lossy().to_string(),
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

    // ========================================================================
    // Cleanup Agent
    // ========================================================================

    /// Clean up an agent (close Zellij tab, remove agent directory).
    #[tracing::instrument(skip(self))]
    pub async fn cleanup_agent(
        &self,
        issue_id: &str,
        _force: bool,
        subrepo: Option<&str>,
    ) -> Result<()> {
        let agents = self.list_agents(subrepo).await?;
        let prefix = format!("gh-{}-", issue_id);
        let mut found = false;

        for agent in agents {
            if let Some(ref agent_dir) = agent.agent_dir {
                let path = Path::new(agent_dir);
                if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                    if name.starts_with(&prefix) {
                        found = true;

                        // Close Zellij tab
                        if let Some(parsed) = parse_agent_dir_name(name) {
                            if let Some(agent_type) = parsed.agent_type {
                                let display_name = agent_type.display_name(issue_id, parsed.slug);
                                if let Err(e) = self.close_zellij_tab(&display_name).await {
                                    warn!(tab_name = %display_name, error = %e, "Failed to close Zellij tab (may not exist)");
                                }
                            }
                        }

                        // Unregister from Claude Code Teams
                        info!(agent_name = %name, "Unregistering agent from Claude Code Teams");
                        if let Err(e) =
                            super::teams::TeamsService::unregister_agent_from_all_teams(name).await
                        {
                            warn!(error = %e, "Failed to unregister agent from Claude Code Teams (non-fatal)");
                        }

                        // Remove agent directory
                        if path.exists() {
                            fs::remove_dir_all(path)
                                .await
                                .context("Failed to remove agent directory")?;
                            info!(agent_dir = %path.display(), "Removed agent directory");
                        }
                    }
                }
            }
        }

        if found {
            // Emit agent:stopped event
            if let Some(ref session) = self.zellij_session {
                let agent_id = crate::ui_protocol::AgentId::try_from(format!("gh-{}", issue_id))
                    .map_err(|e| anyhow!("Invalid agent_id: {}", e))?;
                let event = crate::ui_protocol::AgentEvent::AgentStopped {
                    agent_id,
                    timestamp: zellij_events::now_iso8601(),
                };
                if let Err(e) = zellij_events::emit_event(session, &event) {
                    warn!("Failed to emit agent:stopped event: {}", e);
                }
            }
            Ok(())
        } else {
            Err(anyhow!("No agent found for issue {}", issue_id))
        }
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
            match self.cleanup_agent(issue_id, force, subrepo).await {
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
        let agents = self.list_agents(subrepo).await?;
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

    /// List all active agents by scanning `.exomonad/agents/` directory.
    #[tracing::instrument(skip(self))]
    pub async fn list_agents(&self, subrepo: Option<&str>) -> Result<Vec<AgentInfo>> {
        let effective_project_dir = self.effective_project_dir(subrepo)?;

        // 1. Scan .exomonad/agents/ for agent directories
        let agents_dir = effective_project_dir.join(".exomonad").join("agents");
        let mut agent_dirs: HashMap<String, (PathBuf, String)> = HashMap::new(); // issue_id -> (path, dir_name)

        if agents_dir.exists() {
            let mut entries = fs::read_dir(&agents_dir).await?;
            while let Some(entry) = entries.next_entry().await? {
                if !entry.file_type().await?.is_dir() {
                    continue;
                }
                let name = entry.file_name().to_string_lossy().to_string();
                if name.starts_with("gh-") {
                    if let Some(parsed) = parse_agent_dir_name(&name) {
                        agent_dirs.insert(parsed.issue_id.to_string(), (entry.path(), name));
                    }
                }
            }
        }

        // 2. Get all Zellij tabs
        let zellij_tabs = self.get_zellij_tabs().await.unwrap_or_default();
        let mut tabs = HashSet::new();
        for tab_name in zellij_tabs {
            if let Some(issue_id) = self.extract_issue_id_from_tab_name(&tab_name) {
                tabs.insert(issue_id);
            }
        }

        // 3. Compute union of all issue IDs
        let mut all_issues: HashSet<String> = agent_dirs.keys().cloned().collect();
        all_issues.extend(tabs.iter().cloned());

        let mut agents = Vec::new();

        for issue_id in all_issues {
            let has_dir = agent_dirs.contains_key(&issue_id);
            let has_tab = tabs.contains(&issue_id);

            let status = if has_tab {
                AgentStatus::Running
            } else {
                AgentStatus::Stopped
            };

            // Skip entries with neither a dir nor a tab
            if !has_dir && !has_tab {
                continue;
            }

            let mut agent = AgentInfo {
                issue_id: issue_id.clone(),
                has_tab,
                status,
                agent_dir: None,
                slug: None,
                agent_type: None,
                pr: None,
            };

            if let Some((path, name)) = agent_dirs.get(&issue_id) {
                agent.agent_dir = Some(path.to_string_lossy().to_string());

                if let Some(parsed) = parse_agent_dir_name(name) {
                    agent.slug = Some(parsed.slug.to_string());
                    agent.agent_type = parsed.agent_type.map(|t| t.suffix().to_string());
                }
            }

            agents.push(agent);
        }

        Ok(agents)
    }

    // ========================================================================
    // Internal: Server Discovery
    // ========================================================================

    /// Read server.pid and extract the socket path if available.
    /// Falls back to reading the port for HTTP mode.
    fn read_server_info(project_dir: &Path) -> Option<ServerInfo> {
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

        // Check for socket path first (unix socket mode), then port (HTTP mode)
        if let Some(socket) = parsed.get("socket").and_then(|v| v.as_str()) {
            Some(ServerInfo::Socket(socket.to_string()))
        } else {
            parsed
                .get("port")
                .and_then(|v| v.as_u64())
                .map(|port| ServerInfo::Http(port as u16))
        }
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
        info!(name, cwd = %cwd.display(), agent_type = ?agent_type, "Creating Zellij tab");

        let cmd = agent_type.command();
        let agent_command = match prompt {
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
        let kdl_escaped_command = Self::escape_for_kdl(&agent_command);

        // Use login shell to ensure PATH is loaded (gemini, claude, etc.)
        let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/zsh".to_string());

        // Generate layout using zellij-gen library (includes zjstatus with Solarized Dark)
        let params = crate::layout::AgentTabParams {
            tab_name: name,
            pane_name: "Agent",
            command: &kdl_escaped_command,
            cwd,
            shell: &shell,
            focus: true,
            close_on_exit: true,
            env: env_vars,
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

    fn extract_issue_id_from_tab_name(&self, name: &str) -> Option<String> {
        // Tab names are formatted as: "{emoji} gh-{issue_id}-{short_slug}"
        // or potentially "{emoji} {issue_id}-{short_slug}" (old format)

        let parts: Vec<&str> = name.split_whitespace().collect();
        // Parts should be at least [emoji, "issue_id-slug"]
        if parts.len() < 2 {
            return None;
        }

        let identifier = parts[1];
        if let Some(rest) = identifier.strip_prefix("gh-") {
            rest.split('-').next().map(|s| s.to_string())
        } else {
            identifier.split('-').next().map(|s| s.to_string())
        }
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
    /// Prefers Unix socket if server.pid has a socket path, falls back to HTTP,
    /// then to stdio mode.
    async fn write_agent_mcp_config(
        &self,
        effective_dir: &Path,
        agent_dir: &Path,
        agent_type: AgentType,
    ) -> Result<()> {
        let server_info = Self::read_server_info(effective_dir);

        match agent_type {
            AgentType::Claude => {
                let sidecar_path = std::env::current_exe()
                    .ok()
                    .and_then(|p| p.to_str().map(String::from))
                    .unwrap_or_else(|| "exomonad".to_string());

                let mcp_content = match server_info {
                    Some(ServerInfo::Socket(socket_path)) => {
                        info!(socket = %socket_path, "Unix socket server detected, writing socket .mcp.json");
                        format!(
                            r###"{{
  "mcpServers": {{
    "exomonad": {{
      "url": "unix://{}/dev/mcp"
    }}
  }}
}}"###,
                            socket_path
                        )
                    }
                    Some(ServerInfo::Http(port)) => {
                        info!(port, "HTTP server detected, writing HTTP .mcp.json");
                        format!(
                            r###"{{
  "mcpServers": {{
    "exomonad": {{
      "type": "sse",
      "url": "http://localhost:{}/mcp"
    }}
  }}
}}"###,
                            port
                        )
                    }
                    None => {
                        info!("No server detected, writing stdio .mcp.json");
                        format!(
                            r###"{{
  "mcpServers": {{
    "exomonad": {{
      "command": "{}",
      "args": ["mcp-stdio"]
    }}
  }}
}}"###,
                            sidecar_path
                        )
                    }
                };
                fs::write(agent_dir.join(".mcp.json"), mcp_content).await?;
                info!(agent_dir = %agent_dir.display(), "Wrote .mcp.json for Claude agent");
            }
            AgentType::Gemini => {
                let gemini_content = match server_info {
                    Some(ServerInfo::Socket(socket_path)) => {
                        info!(socket = %socket_path, "Unix socket server detected, writing .gemini/settings.json");
                        format!(
                            r###"{{
  "mcpServers": {{
    "exomonad": {{
      "url": "unix://{}/dev/mcp"
    }}
  }}
}}"###,
                            socket_path
                        )
                    }
                    Some(ServerInfo::Http(port)) => {
                        info!(port, "HTTP server detected, writing .gemini/settings.json");
                        format!(
                            r###"{{
  "mcpServers": {{
    "exomonad": {{
      "url": "http://localhost:{}/mcp"
    }}
  }}
}}"###,
                            port
                        )
                    }
                    None => {
                        warn!("No server detected for Gemini agent — Gemini CLI does not support stdio MCP. Agent will have no MCP tools.");
                        return Ok(());
                    }
                };
                let gemini_dir = agent_dir.join(".gemini");
                fs::create_dir_all(&gemini_dir).await?;
                fs::write(gemini_dir.join("settings.json"), gemini_content).await?;
                info!(agent_dir = %agent_dir.display(), "Wrote .gemini/settings.json for Gemini agent");
            }
        }
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

/// Parsed components of an agent directory name.
#[derive(Debug, PartialEq)]
struct ParsedAgentDirName<'a> {
    issue_id: &'a str,
    slug: &'a str,
    agent_type: Option<AgentType>,
}

/// Parse an agent directory name into its components.
/// Format: gh-{issue_id}-{slug}-{agent_suffix}
/// Returns None if the name doesn't match the expected format.
fn parse_agent_dir_name(name: &str) -> Option<ParsedAgentDirName<'_>> {
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

    Some(ParsedAgentDirName {
        issue_id,
        slug,
        agent_type,
    })
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
            "🤖 gh-473-refactor-polish"
        );
        assert_eq!(
            AgentType::Gemini.display_name("123", "fix-bug"),
            "💎 gh-123-fix-bug"
        );
    }

    #[test]
    fn test_agent_type_display_name_truncates_long_slug() {
        let long_slug = "this-is-a-very-long-slug-that-should-be-truncated";
        let display = AgentType::Claude.display_name("123", long_slug);
        // Slug portion is truncated to 20 chars: "this-is-a-very-long-" (exactly 20)
        assert_eq!(display, "🤖 gh-123-this-is-a-very-long-");
    }

    #[test]
    fn test_extract_issue_id_from_tab_name() {
        let service = AgentControlService::new(PathBuf::from("/tmp"), None);

        assert_eq!(
            service.extract_issue_id_from_tab_name("🤖 gh-473-refactor-polish"),
            Some("473".to_string())
        );
        assert_eq!(
            service.extract_issue_id_from_tab_name("💎 gh-123-fix-bug"),
            Some("123".to_string())
        );
        // Old format
        assert_eq!(
            service.extract_issue_id_from_tab_name("🤖 473-refactor-polish"),
            Some("473".to_string())
        );
        // Invalid formats
        assert_eq!(service.extract_issue_id_from_tab_name("🤖"), None);
        assert_eq!(service.extract_issue_id_from_tab_name("just-text"), None);
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
        let parsed = super::parse_agent_dir_name("gh-123-fix-bug-claude").unwrap();
        assert_eq!(parsed.issue_id, "123");
        assert_eq!(parsed.slug, "fix-bug");
        assert_eq!(parsed.agent_type, Some(AgentType::Claude));
    }

    #[test]
    fn test_parse_agent_dir_name_gemini() {
        let parsed = super::parse_agent_dir_name("gh-456-add-feature-gemini").unwrap();
        assert_eq!(parsed.issue_id, "456");
        assert_eq!(parsed.slug, "add-feature");
        assert_eq!(parsed.agent_type, Some(AgentType::Gemini));
    }

    #[test]
    fn test_parse_agent_dir_name_slug_with_hyphens() {
        let parsed = super::parse_agent_dir_name("gh-789-fix-the-big-bug-claude").unwrap();
        assert_eq!(parsed.issue_id, "789");
        assert_eq!(parsed.slug, "fix-the-big-bug");
        assert_eq!(parsed.agent_type, Some(AgentType::Claude));
    }

    #[test]
    fn test_parse_agent_dir_name_unknown_suffix() {
        let parsed = super::parse_agent_dir_name("gh-123-test-unknown").unwrap();
        assert_eq!(parsed.issue_id, "123");
        assert_eq!(parsed.slug, "test");
        assert_eq!(parsed.agent_type, None);
    }

    #[test]
    fn test_parse_agent_dir_name_invalid_format() {
        assert!(super::parse_agent_dir_name("123-test-claude").is_none());
        assert!(super::parse_agent_dir_name("gh-nohyphens").is_none());
        assert!(super::parse_agent_dir_name("gh-123").is_none());
    }
}
