//! High-level agent control service.
//!
//! Provides semantic operations for agent lifecycle management:
//! - SpawnAgent: Create agent directory, open tmux window
//! - CleanupAgent: Close tab, remove per-agent config
//! - ListAgents: Discover from tmux windows (source of truth for running agents)

mod cleanup;
mod internal;
mod spawn;

pub(crate) use crate::common::TimeoutError;
pub(crate) use crate::domain::{
    AgentName, AgentPermissions, BirthBranch, ClaudeSessionUuid, ItemState, RoutingInfo, TeamName,
};
pub(crate) use crate::effects::EffectError;
pub(crate) use crate::ffi::FFIBoundary;
pub(crate) use crate::{GithubOwner, GithubRepo, IssueNumber};
pub(crate) use anyhow::{anyhow, Context, Result};
pub(crate) use serde::{Deserialize, Serialize};
pub(crate) use std::collections::{HashMap, HashSet};
pub(crate) use std::path::{Path, PathBuf};
pub(crate) use tokio::fs;
pub(crate) use tokio::process::Command;
pub(crate) use tokio::time::{timeout, Duration};
pub(crate) use tracing::{debug, info, warn, instrument};

pub(crate) use super::acp_registry::AcpRegistry;
pub(crate) use super::git_worktree::GitWorktreeService;
pub(crate) use super::github::{GitHubService, Repo};
pub(crate) use super::tmux_events;
pub(crate) use super::tmux_ipc;
pub(crate) use std::sync::Arc;

pub(crate) const SPAWN_TIMEOUT: Duration = Duration::from_secs(60);
pub(crate) const TMUX_TIMEOUT: Duration = Duration::from_secs(30);

/// Push the parent branch to the remote so child PRs can reference it as
/// their base. Non-fatal: warns on failure (supports local/airgapped setups
/// where no remote or non-GitHub remote is configured).
pub(crate) async fn ensure_branch_pushed(git_wt: &Arc<GitWorktreeService>, branch: &str, project_dir: &Path) {
    info!(branch = %branch, "Pushing parent branch to remote");
    let git_wt = git_wt.clone();
    let dir = project_dir.to_path_buf();
    let bookmark = crate::domain::BranchName::from(branch);
    match tokio::task::spawn_blocking(move || git_wt.push_bookmark(&dir, &bookmark)).await {
        Ok(Ok(())) => info!(branch = %branch, "Branch pushed successfully"),
        Ok(Err(e)) => {
            let anyhow_err = anyhow::Error::from(EffectError::from(e));
            warn!(
                branch = %branch,
                error = %anyhow_err,
                "Failed to push parent branch (non-fatal, PRs may not work)"
            )
        }
        Err(e) => warn!(branch = %branch, error = %e, "Push task panicked (non-fatal)"),
    }
}

// ============================================================================
// Types
// ============================================================================

/// Agent type for spawned agents.
///
/// Determines which CLI tool to use when spawning an agent in a tmux window.
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

    /// Custom binary agent (e.g., shoal-agent).
    Shoal,
}

/// Static metadata for each agent type, replacing per-method match dispatch.
pub(crate) struct AgentMetadata {
    pub(crate) command: &'static str,
    pub(crate) prompt_flag: &'static str,
    pub(crate) suffix: &'static str,
    pub(crate) emoji: &'static str,
}

pub(crate) const CLAUDE_META: AgentMetadata = AgentMetadata {
    command: "claude",
    prompt_flag: "",
    suffix: "claude",
    emoji: "\u{1F916}", // 🤖
};

pub(crate) const GEMINI_META: AgentMetadata = AgentMetadata {
    command: "gemini",
    prompt_flag: "--prompt-interactive",
    suffix: "gemini",
    emoji: "\u{1F48E}", // 💎
};

pub(crate) const SHOAL_META: AgentMetadata = AgentMetadata {
    command: "shoal-agent",
    prompt_flag: "",
    suffix: "shoal",
    emoji: "\u{1F30A}", // 🌊
};

impl AgentType {
    pub(crate) fn meta(&self) -> &'static AgentMetadata {
        match self {
            AgentType::Claude => &CLAUDE_META,
            AgentType::Gemini => &GEMINI_META,
            AgentType::Shoal => &SHOAL_META,
        }
    }

    pub(crate) fn command(&self) -> &'static str {
        self.meta().command
    }
    pub(crate) fn prompt_flag(&self) -> &'static str {
        self.meta().prompt_flag
    }
    /// Agent type suffix for naming (e.g., "claude", "gemini").
    pub fn suffix(&self) -> &'static str {
        self.meta().suffix
    }
    /// Emoji for display in tmux windows.
    pub fn emoji(&self) -> &'static str {
        self.meta().emoji
    }

    /// Generate a display name for tmux windows.
    ///
    /// Format: "{emoji} gh-{issue_id}-{short_slug}"
    /// The slug is truncated to 20 chars for readability.
    pub(crate) fn display_name(&self, issue_id: &str, slug: &str) -> String {
        format!("{} gh-{}-{}", self.emoji(), issue_id, slug)
    }

    /// tmux window display name for an agent with this type and slug.
    pub fn tab_display_name(&self, slug: &str) -> String {
        format!("{} {}", self.emoji(), slug)
    }

    /// Infer agent type from a worktree directory name (e.g., "feature-a-claude" → Claude).
    pub fn from_dir_name(dir_name: &str) -> Self {
        if dir_name.ends_with("-claude") {
            AgentType::Claude
        } else if dir_name.ends_with("-shoal") {
            AgentType::Shoal
        } else {
            AgentType::Gemini
        }
    }
}

/// Resolve the tmux window name of THIS agent from structural identity.
///
/// Root agent (no dots in birth_branch): "TL" tab (created by `exomonad init`).
/// Spawned subtree: "{emoji} {slug}" where slug = last segment of birth_branch.
/// Used for routing popup requests to the correct plugin instance.
/// Resolve the working directory for an agent from its birth branch.
///
/// Follows the dot-segment hierarchy: "main.feature-a" -> ".exo/worktrees/feature-a/".
pub fn resolve_working_dir(birth_branch: &str) -> PathBuf {
    if let Some((_, slug)) = birth_branch.rsplit_once('.') {
        PathBuf::from(format!(".exo/worktrees/{}/", slug))
    } else {
        PathBuf::from(".")
    }
}

/// Resolve the working directory for an agent from its tmux tab name.
///
/// Tab names are formatted as "{emoji} {slug}" or "TL".
pub fn resolve_worktree_from_tab(tab: &str) -> PathBuf {
    if tab == "TL" {
        PathBuf::from(".")
    } else {
        // Tab name is "{emoji} {slug}" (e.g. "💎 feature-a")
        if let Some((_, slug)) = tab.split_once(' ') {
            PathBuf::from(format!(".exo/worktrees/{}/", slug))
        } else {
            PathBuf::from(".")
        }
    }
}

pub fn resolve_own_tab_name(ctx: &crate::effects::EffectContext) -> String {
    let birth_branch_str = ctx.birth_branch.as_str();

    if birth_branch_str.contains('.') {
        let slug = birth_branch_str
            .rsplit_once('.')
            .map(|(_, s)| s)
            .unwrap_or(birth_branch_str);
        // Subtrees spawned by spawn_subtree are Claude; by spawn_leaf_subtree are Gemini.
        // The worktree dir name includes the suffix, but birth_branch doesn't.
        let agent_type = if ctx.agent_name.as_str().ends_with("-gemini") {
            AgentType::Gemini
        } else {
            AgentType::Claude
        };
        agent_type.tab_display_name(slug)
    } else {
        "TL".to_string()
    }
}

/// Resolve the tmux window name of the parent agent from structural identity.
///
/// Workers (Gemini): parent derived from birth_branch (inherited).
/// Subtree agents: parent is one dot-level up in branch hierarchy.
/// Root agents (no dots): parent is the TL tab.
/// Parent tabs are always Claude (TL role), so always use the Claude emoji.
pub fn resolve_parent_tab_name(ctx: &crate::effects::EffectContext) -> String {
    let birth_branch_str = ctx.birth_branch.as_str();

    if ctx.agent_name.is_gemini_worker() {
        // Worker: birth-branch is parent's birth-branch (inherited)
        if birth_branch_str.contains('.') {
            let slug = birth_branch_str
                .rsplit_once('.')
                .map(|(_, s)| s)
                .unwrap_or(birth_branch_str);
            AgentType::Claude.tab_display_name(slug)
        } else {
            "TL".to_string()
        }
    } else {
        // Subtree agent: parent is one level up
        if let Some(parent) = ctx.birth_branch.parent() {
            if parent.as_str().contains('.') {
                let slug = parent
                    .as_str()
                    .rsplit_once('.')
                    .map(|(_, s)| s)
                    .unwrap_or(parent.as_str());
                AgentType::Claude.tab_display_name(slug)
            } else {
                "TL".to_string()
            }
        } else {
            "TL".to_string()
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
    /// Agent type (Claude or Gemini)
    #[serde(default)]
    pub agent_type: AgentType,
    /// Sub-repository path relative to project_dir (e.g., "urchin/").
    /// When set, the agent's project context targets this directory instead of project_dir.
    pub subrepo: Option<String>,
    /// Base branch to branch off of (default: "main").
    pub base_branch: Option<BirthBranch>,
}

/// Options for spawning a named teammate (no GitHub issue required).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnGeminiTeammateOptions {
    /// Human-readable name (e.g., "mcp-hardener")
    pub name: AgentName,
    /// Initial prompt/instructions
    pub prompt: String,
    /// Agent type (Claude or Gemini)
    #[serde(default)]
    pub agent_type: AgentType,
    /// Sub-repository path relative to project_dir
    pub subrepo: Option<String>,
    /// Base branch to branch off of (defaults to current branch).
    pub base_branch: Option<BirthBranch>,
}

/// Claude-specific spawn flags for permission control.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ClaudeSpawnFlags {
    /// Permission mode (e.g., "plan", "default"). None = --dangerously-skip-permissions.
    pub permission_mode: Option<String>,
    /// Tool patterns to allow (e.g., "Read", "Grep").
    pub allowed_tools: Vec<String>,
    /// Tool patterns to disallow (e.g., "Bash").
    pub disallowed_tools: Vec<String>,
}

/// Options for spawning a worker agent in the current worktree (no branch/worktree).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnWorkerOptions {
    /// Human-readable name for the worker
    pub name: String,
    /// Implementation instructions
    pub prompt: String,
    /// Claude-specific permission flags (ignored for Gemini).
    #[serde(default)]
    pub claude_flags: ClaudeSpawnFlags,
}

/// Options for spawning a subtree agent (isolated worktree).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnSubtreeOptions {
    /// Full task/prompt for the agent.
    pub task: String,
    /// Branch name suffix.
    pub branch_name: String,
    /// Parent Claude session ID for --resume --fork-session context inheritance.
    pub parent_session_id: Option<ClaudeSessionUuid>,
    /// Optional role override.
    pub role: Option<String>,
    /// Agent type (claude or gemini). Required — no default.
    pub agent_type: AgentType,
    /// Claude-specific permission flags (ignored for Gemini).
    #[serde(default)]
    pub claude_flags: ClaudeSpawnFlags,
    /// Optional working directory. If Some, worktree creation is skipped.
    pub working_dir: Option<PathBuf>,
    /// Optional agent permissions.
    pub permissions: Option<AgentPermissions>,
    /// When true, creates a standalone git repo instead of a worktree.
    pub standalone_repo: bool,
    /// Directories from the parent project to be copied into the agent's worktree.
    pub allowed_dirs: Vec<String>,
}

/// Options for spawning a Gemini leaf subtree agent.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnLeafOptions {
    /// Full task/prompt for the agent.
    pub task: String,
    /// Branch name suffix.
    pub branch_name: String,
    /// Optional role override.
    pub role: Option<String>,
    /// Agent type (claude or gemini). Required — no default.
    pub agent_type: AgentType,
    /// Claude-specific permission flags (ignored for Gemini).
    #[serde(default)]
    pub claude_flags: ClaudeSpawnFlags,
    /// When true, creates a standalone git repo instead of a worktree.
    pub standalone_repo: bool,
    /// Directories from the parent project to be copied into the agent's worktree.
    pub allowed_dirs: Vec<String>,
}

/// Result of spawning an agent.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SpawnResult {
    /// Path to the agent directory (.exo/agents/{agent_id}/)
    pub agent_dir: PathBuf,
    /// tmux window name
    pub tab_name: String,
    /// Issue title
    pub issue_title: String,
    /// Agent type
    pub agent_type: AgentType,
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
    /// Whether a tmux window/pane exists for this agent.
    pub has_tab: bool,
    /// Workspace topology.
    #[serde(default)]
    pub topology: Topology,
    /// Path to agent directory (.exo/agents/{agent_id}/)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_dir: Option<PathBuf>,
    /// Slug from agent name (e.g., "fix-bug-in-parser")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub slug: Option<AgentName>,
    /// Agent type (Claude or Gemini)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_type: Option<AgentType>,
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
    pub(crate) project_dir: PathBuf,
    /// Base directory for worktrees (default: .exo/worktrees)
    pub(crate) worktree_base: PathBuf,
    /// GitHub service for fetching issues
    pub(crate) github: Option<GitHubService>,
    /// tmux session name for event emission
    pub(crate) tmux_session: Option<String>,
    /// Direct tmux IPC client.
    pub(crate) tmux_ipc: Option<super::tmux_ipc::TmuxIpc>,
    /// This agent's birth-branch (git identity). Root TL = "main".
    pub(crate) birth_branch: BirthBranch,
    /// Git worktree service
    pub(crate) git_wt: Arc<GitWorktreeService>,
    /// ACP connection registry for Gemini agents.
    pub(crate) acp_registry: Option<Arc<AcpRegistry>>,
    /// When true, spawned Gemini agents receive `--yolo` flag.
    pub(crate) yolo: bool,
}

impl AgentControlService {
    /// Create a new agent control service.
    pub fn new(
        project_dir: PathBuf,
        github: Option<GitHubService>,
        git_wt: Arc<GitWorktreeService>,
    ) -> Self {
        let worktree_base = project_dir.join(".exo/worktrees");
        Self {
            project_dir,
            worktree_base,
            github,
            tmux_session: None,
            tmux_ipc: None,
            birth_branch: BirthBranch::from("unset"),
            git_wt,
            acp_registry: None,
            yolo: false,
        }
    }

    /// Set the worktree base directory.
    pub fn with_worktree_base(mut self, base: PathBuf) -> Self {
        self.worktree_base = base;
        self
    }

    /// Set the ACP registry.
    pub fn with_acp_registry(mut self, registry: Arc<AcpRegistry>) -> Self {
        self.acp_registry = Some(registry);
        self
    }

    /// Set the tmux session name for event emission + direct IPC.
    pub fn with_tmux_session(mut self, session: String) -> Self {
        self.tmux_ipc = Some(super::tmux_ipc::TmuxIpc::new(&session));
        self.tmux_session = Some(session);
        self
    }

    /// Set the birth-branch (git identity) for this agent.
    pub fn with_birth_branch(mut self, branch: BirthBranch) -> Self {
        self.birth_branch = branch;
        self
    }

    /// Enable `--yolo` flag for spawned Gemini agents.
    pub fn with_yolo(mut self, yolo: bool) -> Self {
        self.yolo = yolo;
        self
    }

    /// Resolve the effective birth branch for spawn operations.
    ///
    /// Callers pass the birth branch from EffectContext. Falls back to `self.birth_branch`
    /// if no override is provided.
    pub(crate) fn effective_birth_branch(&self, override_bb: Option<&BirthBranch>) -> BirthBranch {
        override_bb
            .cloned()
            .unwrap_or_else(|| {
                debug_assert!(
                    self.birth_branch.as_str() != "unset",
                    "birth_branch was never initialized via with_birth_branch()"
                );
                self.birth_branch.clone()
            })
    }

    /// Common post-spawn bookkeeping.
    ///
    /// Creates the agent's config directory and writes the routing info.
    pub(crate) async fn finalize_spawn(
        &self,
        internal_name: &str,
        routing: RoutingInfo,
    ) -> Result<PathBuf> {
        let agent_config_dir = self.project_dir.join(".exo/agents").join(internal_name);
        fs::create_dir_all(&agent_config_dir).await?;
        routing.write_to_dir(&agent_config_dir).await?;

        Ok(agent_config_dir)
    }

    /// Initialize a standalone git repo at the given path.
    /// Creates the directory and runs `git init`, providing a .git boundary
    /// that prevents Claude's project discovery from traversing into the parent.
    pub(crate) async fn init_standalone_repo(&self, path: &Path) -> Result<()> {
        tokio::fs::create_dir_all(path).await?;
        let output = tokio::process::Command::new("git")
            .args(["init"])
            .current_dir(path)
            .output()
            .await?;
        if !output.status.success() {
            return Err(anyhow!(
                "git init failed at {}: {}",
                path.display(),
                String::from_utf8_lossy(&output.stderr)
            ));
        }
        tracing::info!("Initialized standalone repo at {}", path.display());
        Ok(())
    }

    /// Create from environment (loads secrets from ~/.exo/secrets).
    pub fn from_env() -> Result<Self> {
        let project_dir = std::env::current_dir().context("Failed to get current directory")?;

        // Try to load GitHub token from secrets
        let secrets = super::secrets::Secrets::load();
        let github = secrets
            .github_token()
            .and_then(|t| GitHubService::new(t).ok());

        let git_wt = Arc::new(GitWorktreeService::new(project_dir.clone()));

        Ok(Self {
            project_dir: project_dir.clone(),
            worktree_base: project_dir.join(".exo/worktrees"),
            github,
            tmux_session: None,
            tmux_ipc: None,
            birth_branch: BirthBranch::root()?,
            git_wt,
            acp_registry: None,
            yolo: false,
        })
    }

    /// Resolve effective project dir for git operations.
    /// When subrepo is set, git operations target project_dir/subrepo instead.
    /// Validates that subrepo is relative and does not escape project_dir.
    pub(crate) fn effective_project_dir(&self, subrepo: Option<&str>) -> Result<PathBuf> {
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

    /// Copy allowed directories into the agent's context.
    pub(crate) async fn copy_allowed_dirs(&self, target_dir: &Path, allowed_dirs: &[String]) -> Result<()> {
        if allowed_dirs.is_empty() {
            return Ok(());
        }

        let context_dir = target_dir.join(".exo/context");
        fs::create_dir_all(&context_dir).await?;

        for dir_str in allowed_dirs {
            let dir_path = Path::new(dir_str);

            // Validation: Reject absolute paths and path traversal
            if dir_path.is_absolute() {
                tracing::error!("allowed_dir '{}' rejected: must be relative", dir_str);
                continue;
            }
            if dir_str.contains("..") {
                tracing::error!("allowed_dir '{}' rejected: cannot contain '..'", dir_str);
                continue;
            }

            let source_dir = self.project_dir.join(dir_path);

            // Canonicalize and verify the resolved path is within project_dir
            match source_dir.canonicalize() {
                Ok(canonical_source) => {
                    let canonical_project = self.project_dir.canonicalize()?;
                    if !canonical_source.starts_with(&canonical_project) {
                        tracing::error!("allowed_dir '{}' rejected: outside project_dir", dir_str);
                        continue;
                    }
                    if !canonical_source.is_dir() {
                        tracing::error!("allowed_dir '{}' rejected: not a directory", dir_str);
                        continue;
                    }

                    tracing::info!("Copying allowed_dir '{}' to agent context", dir_str);

                    // Recursive copy
                    let target_subdir = context_dir.join(dir_path);
                    self.copy_dir_recursive(&canonical_source, &target_subdir)
                        .await?;
                }
                Err(e) => {
                    tracing::error!("allowed_dir '{}' rejected: {}", dir_str, e);
                    continue;
                }
            }
        }

        Ok(())
    }

    pub(crate) async fn copy_dir_recursive(&self, src: &Path, dst: &Path) -> Result<()> {
        let mut stack = vec![(src.to_path_buf(), dst.to_path_buf())];

        while let Some((curr_src, curr_dst)) = stack.pop() {
            fs::create_dir_all(&curr_dst).await?;
            let mut entries = fs::read_dir(&curr_src).await?;
            while let Some(entry) = entries.next_entry().await? {
                let ty = entry.file_type().await?;
                let entry_path = entry.path();
                let dest_path = curr_dst.join(entry.file_name());
                if ty.is_dir() {
                    stack.push((entry_path, dest_path));
                } else {
                    fs::copy(&entry_path, &dest_path).await?;
                }
            }
        }
        Ok(())
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
    pub(crate) struct ParsedAgentDirName<'a> {
        pub(crate) issue_id: &'a str,
        pub(crate) slug: &'a str,
        pub(crate) agent_type: Option<super::AgentType>,
    }

    pub(crate) fn parse_agent_dir_name(name: &str) -> Option<ParsedAgentDirName<'_>> {
        let rest = name.strip_prefix("gh-")?;
        let (issue_id, rest) = rest.split_once('-')?;
        let (slug, agent_suffix) = rest.rsplit_once('-')?;
        let agent_type = match agent_suffix {
            "claude" => Some(super::AgentType::Claude),
            "gemini" => Some(super::AgentType::Gemini),
            "shoal" => Some(super::AgentType::Shoal),
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
}
