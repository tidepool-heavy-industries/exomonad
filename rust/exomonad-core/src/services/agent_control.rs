//! High-level agent control service.
//!
//! Provides semantic operations for agent lifecycle management:
//! - SpawnAgent: Create agent directory, open tmux window
//! - CleanupAgent: Close tab, remove per-agent config
//! - ListAgents: Discover from tmux windows (source of truth for running agents)

use crate::common::TimeoutError;
use crate::domain::{
    AgentName, AgentPermissions, BirthBranch, ClaudeSessionUuid, ItemState, RoutingInfo,
};
use crate::effects::EffectError;
use crate::ffi::FFIBoundary;
use crate::{GithubOwner, GithubRepo, IssueNumber};
use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use tokio::fs;
use tokio::process::Command;
use tokio::time::{timeout, Duration};
use tracing::{debug, info, warn, instrument};

use super::acp_registry::AcpRegistry;
use super::git_worktree::GitWorktreeService;
use super::github::{GitHubService, Repo};
use super::tmux_events;
use std::sync::Arc;

const SPAWN_TIMEOUT: Duration = Duration::from_secs(60);
const TMUX_TIMEOUT: Duration = Duration::from_secs(30);

/// Push the parent branch to the remote so child PRs can reference it as
/// their base. Non-fatal: warns on failure (supports local/airgapped setups
/// where no remote or non-GitHub remote is configured).
async fn ensure_branch_pushed(git_wt: &Arc<GitWorktreeService>, branch: &str, project_dir: &Path) {
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

const SHOAL_META: AgentMetadata = AgentMetadata {
    command: "shoal-agent",
    prompt_flag: "",
    suffix: "shoal",
    emoji: "\u{1F30A}", // 🌊
};

impl AgentType {
    fn meta(&self) -> &'static AgentMetadata {
        match self {
            AgentType::Claude => &CLAUDE_META,
            AgentType::Gemini => &GEMINI_META,
            AgentType::Shoal => &SHOAL_META,
        }
    }

    fn command(&self) -> &'static str {
        self.meta().command
    }
    fn prompt_flag(&self) -> &'static str {
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
    fn display_name(&self, issue_id: &str, slug: &str) -> String {
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
/// Resolve the working directory for an agent from its EffectContext.
///
/// Root agents (birth_branch without dots) work in the project root.
/// Spawned agents (birth_branch with dots, e.g. "main.feature.scaffold")
/// work in `.exo/worktrees/{slug}/` where slug is the last dot-segment.
pub fn resolve_agent_working_dir(ctx: &crate::effects::EffectContext) -> PathBuf {
    resolve_working_dir(ctx.birth_branch.as_str())
}

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
    project_dir: PathBuf,
    /// Base directory for worktrees (default: .exo/worktrees)
    worktree_base: PathBuf,
    /// GitHub service for fetching issues
    github: Option<GitHubService>,
    /// tmux session name for event emission
    tmux_session: Option<String>,
    /// Direct tmux IPC client.
    tmux_ipc: Option<super::tmux_ipc::TmuxIpc>,
    /// This agent's birth-branch (git identity). Root TL = "main".
    birth_branch: BirthBranch,
    /// Git worktree service
    git_wt: Arc<GitWorktreeService>,
    /// ACP connection registry for Gemini agents.
    pub acp_registry: Option<Arc<AcpRegistry>>,
    /// When true, spawned Gemini agents receive `--yolo` flag.
    yolo: bool,
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
    fn effective_birth_branch(&self, override_bb: Option<&BirthBranch>) -> BirthBranch {
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
    async fn finalize_spawn(
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
    async fn init_standalone_repo(&self, path: &Path) -> Result<()> {
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

    /// Copy allowed directories into the agent's context.
    async fn copy_allowed_dirs(&self, target_dir: &Path, allowed_dirs: &[String]) -> Result<()> {
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

    async fn copy_dir_recursive(&self, src: &Path, dst: &Path) -> Result<()> {
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

    // ========================================================================
    // Spawn Agent
    // ========================================================================

    /// Spawn an agent for a GitHub issue.
    ///
    /// This is the high-level semantic operation that:
    /// 1. Fetches issue from GitHub
    /// 2. Creates agent directory (.exo/agents/{agent_id}/)
    /// 3. Writes .mcp.json pointing to the Unix socket server
    /// 4. Opens tmux window with agent command (cwd = project_dir)
    #[tracing::instrument(skip(self, options), fields(issue_id = %issue_number.as_u64()))]
    pub async fn spawn_agent(
        &self,
        issue_number: IssueNumber,
        options: &SpawnOptions,
        caller_bb: &BirthBranch,
    ) -> Result<SpawnResult> {
        let issue_id_log = issue_number.as_u64().to_string();
        info!(issue_id = %issue_id_log, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_agent");

        let result = timeout(SPAWN_TIMEOUT, async {
            // Validate we're in tmux
            self.resolve_tmux_session()?;

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

            // Determine base branch (use birth_branch for root detection)
            let default_base = self.birth_branch.as_parent_branch().to_string();
            let base = options
                .base_branch
                .as_ref()
                .map(|b| b.as_str().to_string())
                .unwrap_or(default_base);
            let branch_name = if self.birth_branch.depth() == 0 {
                format!("gh-{}/{}-{}", issue_id, slug, agent_suffix)
            } else {
                format!("{}/{}-{}", base, slug, agent_suffix)
            };

            // Create worktree
            let worktree_path = self.worktree_base.join(&internal_name);

            self.create_worktree_checked(&worktree_path, &branch_name, &base)
                .await?;

            // Use worktree path as agent_dir
            let agent_dir = worktree_path;

            // Write .mcp.json for the agent
            let role = match options.agent_type {
                AgentType::Claude => "tl",
                AgentType::Gemini => "dev",
                AgentType::Shoal => "shoal",
            };
            self.write_agent_mcp_config(
                &effective_project_dir,
                &agent_dir,
                options.agent_type,
                role,
            )
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

            // tmux display name (emoji + short format)
            let display_name = options.agent_type.display_name(&issue_id, &slug);

            let env_vars = self.common_spawn_env(
                &internal_name,
                self.effective_birth_branch(Some(caller_bb)).as_ref(),
                role,
            );

            // Open tmux window with cwd = worktree_path
            let window_id = self
                .new_tmux_window(
                    &display_name,
                    &agent_dir,
                    options.agent_type,
                    Some(&initial_prompt),
                    env_vars,
                )
                .await?;

            // Store window_id for message delivery and cleanup
            let routing = RoutingInfo::window(window_id.as_str());
            self.finalize_spawn(&internal_name, routing)
                .await?;

            self.emit_agent_started(&internal_name)?;

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: agent_dir.clone(),
                tab_name: internal_name,
                issue_title: issue.title,
                agent_type: options.agent_type,
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
        caller_bb: &BirthBranch,
    ) -> BatchSpawnResult {
        let mut result = BatchSpawnResult {
            spawned: Vec::new(),
            failed: Vec::new(),
        };

        for issue_id_str in issue_ids {
            // Parse issue ID
            match IssueNumber::try_from(issue_id_str.clone()) {
                Ok(issue_number) => {
                    match self.spawn_agent(issue_number, options, caller_bb).await {
                        Ok(spawn_result) => result.spawned.push(spawn_result),
                        Err(e) => {
                            warn!(issue_id = issue_id_str, error = %e, "Failed to spawn agent");
                            result.failed.push((issue_id_str.clone(), e.to_string()));
                        }
                    }
                }
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
    /// If config entry exists but tmux window is dead, cleans stale entry and respawns.
    /// No per-agent directories or MCP configs — agents share the repo's config.
    /// State lives in Teams config.json + tmux window only.
    #[tracing::instrument(skip(self, options), fields(name = %options.name.as_str()))]
    pub async fn spawn_gemini_teammate(
        &self,
        options: &SpawnGeminiTeammateOptions,
        caller_bb: &BirthBranch,
    ) -> Result<SpawnResult> {
        info!(name = %options.name, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_gemini_teammate");

        let result = timeout(SPAWN_TIMEOUT, async {
            self.resolve_tmux_session()?;

            let effective_project_dir = self.effective_project_dir(options.subrepo.as_deref())?;

            // Sanitize name for internal use
            let slug = slugify(options.name.as_str());
            let agent_suffix = options.agent_type.suffix();
            let internal_name = format!("{}-{}", slug, agent_suffix);
            let display_name = format!("{} {}", options.agent_type.emoji(), slug);

            // Idempotency check: if tmux window is alive, return existing info
            let tab_alive = self.is_tmux_window_alive(&display_name).await;

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
                    agent_dir: PathBuf::new(),
                    tab_name: internal_name,
                    issue_title: options.name.to_string(),
                    agent_type: options.agent_type,
                });
            }

            // Determine base branch
            let base_branch = if let Some(ref b) = options.base_branch {
                b.to_string()
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

            self.create_worktree_checked(&worktree_path, &branch_name, &base_branch)
                .await?;

            let mut env_vars = self.common_spawn_env(
                &internal_name,
                self.effective_birth_branch(Some(caller_bb)).as_ref(),
                "dev",
            );

            // Write per-agent MCP config into the worktree
            self.write_agent_mcp_config(
                &effective_project_dir,
                &worktree_path,
                options.agent_type,
                "dev",
            )
            .await?;

            // For Gemini agents, point at worktree settings via env var
            if options.agent_type == AgentType::Gemini {
                let settings_path = worktree_path.join(".gemini").join("settings.json");
                env_vars.insert(
                    "GEMINI_CLI_SYSTEM_SETTINGS_PATH".to_string(),
                    settings_path.to_string_lossy().to_string(),
                );
            }

            let window_id = self
                .new_tmux_window(
                    &display_name,
                    &worktree_path,
                    options.agent_type,
                    Some(&options.prompt),
                    env_vars,
                )
                .await?;

            // Store window_id for message delivery and cleanup
            let routing = RoutingInfo::window(window_id.as_str());
            self.finalize_spawn(&internal_name, routing)
                .await?;

            self.emit_agent_started(&internal_name)?;

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: PathBuf::new(),
                tab_name: internal_name,
                issue_title: options.name.to_string(),
                agent_type: options.agent_type,
            })
        })
        .await
        .map_err(|_| {
            let msg = format!(
                "spawn_gemini_teammate timed out after {}s",
                SPAWN_TIMEOUT.as_secs()
            );
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
    /// Generate settings.json for a Gemini worker using stdio MCP transport.
    pub(crate) fn generate_gemini_worker_settings(agent_name: &str) -> serde_json::Value {
        serde_json::json!({
            "mcpServers": {
                "exomonad": {
                    "type": "stdio",
                    "command": "exomonad",
                    "args": ["mcp-stdio", "--role", "worker", "--name", agent_name]
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
    #[instrument(skip_all, fields(name = %options.name, agent_type = "gemini"))]
    pub async fn spawn_worker(
        &self,
        options: &SpawnWorkerOptions,
        ctx: &crate::effects::EffectContext,
    ) -> Result<SpawnResult> {
        info!(name = %options.name, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_worker");

        let result = timeout(SPAWN_TIMEOUT, async {
            self.resolve_tmux_session()?;

            // Sanitize name for internal use
            let slug = slugify(&options.name);
            let internal_name = format!("{}-gemini", slug);
            let display_name = format!("{} {}", AgentType::Gemini.emoji(), slug);

            // Idempotency: check if agent config dir already exists (workers are panes, not tabs)
            let agent_config_dir = self.project_dir
                .join(".exo")
                .join("agents")
                .join(&internal_name);
            let settings_path = agent_config_dir.join("settings.json");
            if settings_path.exists() {
                info!(name = %options.name, path = %settings_path.display(), "Worker already spawned, returning existing");
                return Ok(SpawnResult {
                    agent_dir: PathBuf::new(),
                    tab_name: internal_name,
                    issue_title: options.name.clone(),
                    agent_type: AgentType::Gemini,
                });
            }

            let mut env_vars = self.common_spawn_env(&internal_name, self.effective_birth_branch(Some(&ctx.birth_branch)).as_ref(), "worker");

            // Write Gemini settings to worker config dir in project root
            fs::create_dir_all(&agent_config_dir).await?;

            // Write parent's birth_branch so the server can resolve it for notify_parent routing.
            // Workers don't have worktrees, so git-based resolution fails. This file is the fallback.
            let parent_bb = self.effective_birth_branch(Some(&ctx.birth_branch));
            fs::write(agent_config_dir.join(".birth_branch"), parent_bb.as_str()).await?;
            let settings = Self::generate_gemini_worker_settings(&internal_name);
            fs::write(&settings_path, serde_json::to_string_pretty(&settings)?).await?;
            info!(
                path = %settings_path.display(),
                name = %internal_name,
                "Wrote worker Gemini settings to agent config dir"
            );

            env_vars.insert(
                "GEMINI_CLI_SYSTEM_SETTINGS_PATH".to_string(),
                settings_path.to_string_lossy().to_string(),
            );

            // Resolve caller's context (tab and worktree) from its context.
            let caller_tab = resolve_own_tab_name(ctx);
            let caller_worktree = resolve_agent_working_dir(ctx);
            let absolute_worktree = self.project_dir.join(caller_worktree);

            // Write routing info so send_message can target this pane correctly.
            // Workers are panes in the parent's tab — pane_id is the stable identifier
            // Spawn pane in caller's tab, cwd = caller's worktree
            let pane_id = self.new_tmux_pane(
                &display_name,
                &absolute_worktree,
                AgentType::Gemini,
                Some(&options.prompt),
                env_vars,
                Some(&caller_tab),
                Some(&options.claude_flags),
            )
            .await?;

            // Store pane_id for message delivery and cleanup
            let routing = RoutingInfo::pane(pane_id.as_str(), &caller_tab);
            self.finalize_spawn(&internal_name, routing)
                .await?;

            // Register as synthetic team member for Claude Teams messaging
            let team_name = format!("exo-{}", self.effective_birth_branch(Some(&ctx.birth_branch)));
            if let Err(e) = crate::services::synthetic_members::register_synthetic_member(
                &team_name, &slug, "gemini-worker",
            ) {
                warn!(agent = %slug, team = %team_name, error = %e, "Failed to register synthetic team member (non-fatal)");
            }

            self.emit_agent_started(&internal_name)?;

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: PathBuf::new(),
                tab_name: internal_name,
                issue_title: options.name.clone(),
                agent_type: AgentType::Gemini,
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
    #[instrument(skip_all, fields(slug = %options.branch_name, agent_type = "claude"))]
    pub async fn spawn_subtree(
        &self,
        options: &SpawnSubtreeOptions,
        caller_bb: &BirthBranch,
    ) -> Result<SpawnResult> {
        info!(branch_name = %options.branch_name, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_subtree");

        let result = timeout(SPAWN_TIMEOUT, async {
            self.resolve_tmux_session()?;

            let effective_birth = self.effective_birth_branch(Some(caller_bb));

            // Depth check using typed birth-branch.
            let depth = effective_birth.depth();

            if depth >= 2 {
                return Err(anyhow!("Subtree depth limit reached (max 2). Current birth-branch: {}, depth: {}", effective_birth, depth));
            }

            let effective_project_dir = &self.project_dir;

            // Sanitize branch name for internal use
            let slug = slugify(&options.branch_name);
            let agent_type = options.agent_type;
            let agent_suffix = agent_type.suffix();
            let internal_name = format!("{}-{}", slug, agent_suffix);
            let display_name = format!("{} {}", agent_type.emoji(), slug);

            // Idempotency check: if tmux window is alive, return existing info
            let tab_alive = self.is_tmux_window_alive(&display_name).await;
            if tab_alive {
                info!(slug = %slug, "Subtree already running, returning existing");
                return Ok(SpawnResult {
                    agent_dir: self.worktree_base.join(&slug),
                    tab_name: internal_name,
                    issue_title: options.branch_name.clone(),
                    agent_type,
                });
            }

            // Parent branch derived from typed birth-branch.
            let current_branch = effective_birth.as_parent_branch();

            // Push parent branch so child PRs can reference it as base
            ensure_branch_pushed(&self.git_wt, current_branch, effective_project_dir).await;

            // Branch: {current_branch}.{slug}
            let child_birth = effective_birth.child(&slug);
            let branch_name = child_birth.to_string();

            // Path resolution: working_dir overrides the default worktree location.
            // standalone_repo: git init (fresh .git boundary) instead of git worktree add.
            // These are orthogonal: working_dir controls WHERE, standalone_repo controls HOW.
            let (worktree_path, is_custom_dir) = if let Some(ref custom_dir) = options.working_dir {
                (custom_dir.clone(), true)
            } else {
                (self.worktree_base.join(&slug), false)
            };

            if options.standalone_repo {
                self.init_standalone_repo(&worktree_path).await?;
                if !options.allowed_dirs.is_empty() {
                    self.copy_allowed_dirs(&worktree_path, &options.allowed_dirs).await?;
                }
            } else if !is_custom_dir {
                self.create_worktree_checked(&worktree_path, &branch_name, current_branch).await?;
            }

            self.create_socket_symlink(&worktree_path).await;

            let role = options.role.as_deref().unwrap_or("tl");
            let mut env_vars = self.common_spawn_env(&internal_name, &branch_name, role);
            // Enable Claude Code Agent Teams for native inter-agent messaging
            env_vars.insert(
                "CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS".to_string(),
                "1".to_string(),
            );
            self.write_agent_mcp_config(effective_project_dir, &worktree_path, agent_type, role)
                .await?;

            // Write .claude/settings.local.json with hooks (SessionStart registers UUID for --fork-session)
            let binary_path = crate::util::find_exomonad_binary();
            crate::hooks::HookConfig::write_persistent(&worktree_path, &binary_path, options.permissions.as_ref())
                .map_err(|e| anyhow!("Failed to write hook config in worktree: {}", e))?;
            info!(worktree = %worktree_path.display(), "Wrote hook configuration for spawned Claude agent");

            // Symlink Claude project dir so child can discover parent's sessions for --fork-session.
            // Claude Code encodes paths via [^a-zA-Z0-9] → '-' (lossy regex replacement).
            // Without this symlink, --resume --fork-session fails with "no conversation ID found".
            {
                let claude_projects_dir = dirs::home_dir()
                    .unwrap_or_default()
                    .join(".claude")
                    .join("projects");
                let encode_path = |p: &Path| -> String {
                    p.to_string_lossy()
                        .chars()
                        .map(|c| if c.is_ascii_alphanumeric() { c } else { '-' })
                        .collect()
                };
                let canonical_project_dir = self.project_dir.canonicalize().unwrap_or_else(|_| self.project_dir.clone());
                let parent_encoded = encode_path(&canonical_project_dir);
                let worktree_encoded = encode_path(&worktree_path);
                let parent_project = claude_projects_dir.join(&parent_encoded);
                let child_project = claude_projects_dir.join(&worktree_encoded);
                if parent_project.exists() && !child_project.exists() {
                    match std::os::unix::fs::symlink(&parent_project, &child_project) {
                        Ok(()) => info!(
                            parent = %parent_encoded,
                            child = %worktree_encoded,
                            "Symlinked Claude project dir for session inheritance"
                        ),
                        Err(e) => warn!(
                            parent = %parent_encoded,
                            child = %worktree_encoded,
                            error = %e,
                            "Failed to symlink Claude project dir (fork-session may not work)"
                        ),
                    }
                }
            }

            // Build task prompt with worktree context warning
            let mut task_with_context = format!(
                "You are now in worktree {} on branch {}. All file paths from your inherited context are STALE — use relative paths only and re-read files before editing.\n\n{}",
                worktree_path.display(), branch_name, options.task
            );

            if options.standalone_repo && !options.allowed_dirs.is_empty() {
                task_with_context.push_str("\n\nShared technical dependencies are available as read-only reference in `.exo/context/`. Do not modify files in this directory.");
            }

            // Determine fork mode from parent_session_id
            let fork_id = options.parent_session_id.as_ref().map(|id| id.as_str());

            // Open tmux window with cwd = worktree_path
            let agent_config_dir = self.project_dir.join(".exo").join("agents").join(&internal_name);
            let window_id = match self.new_tmux_window_inner(
                &display_name,
                &worktree_path,
                agent_type,
                Some(&task_with_context),
                env_vars,
                fork_id,
                Some(&options.claude_flags),
            )
            .await {
                Ok(wid) => wid,
                Err(e) => {
                    warn!(name = %slug, error = %e, "tmux window creation failed, rolling back");
                    let _ = fs::remove_dir_all(&agent_config_dir).await;
                    // Remove worktree if it was created
                    if worktree_path.exists() {
                        let git_wt = self.git_wt.clone();
                        let path = worktree_path.clone();
                        let _ = tokio::task::spawn_blocking(move || git_wt.remove_workspace(&path)).await;
                    }
                    return Err(e);
                }
            };

            // Store window_id for message delivery and cleanup
            let routing = RoutingInfo::window(window_id.as_str());
            self.finalize_spawn(&internal_name, routing)
                .await?;

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: worktree_path.clone(),
                tab_name: internal_name,
                issue_title: options.branch_name.clone(),
                agent_type,
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
    #[instrument(skip_all, fields(slug = %options.branch_name, agent_type = "gemini"))]
    pub async fn spawn_leaf_subtree(
        &self,
        options: &SpawnLeafOptions,
        caller_bb: &BirthBranch,
    ) -> Result<SpawnResult> {
        info!(branch_name = %options.branch_name, timeout_sec = SPAWN_TIMEOUT.as_secs(), "Starting spawn_leaf_subtree");

        let result = timeout(SPAWN_TIMEOUT, async {
            self.resolve_tmux_session()?;

            // No depth check for leaf nodes.

            let effective_birth = self.effective_birth_branch(Some(caller_bb));
            let effective_project_dir = &self.project_dir;

            // Parent branch derived from typed birth-branch.
            let current_branch = effective_birth.as_parent_branch().to_string();

            // Sanitize branch name
            let slug = slugify(&options.branch_name);
            let agent_type = options.agent_type;
            let agent_suffix = agent_type.suffix();
            let internal_name = format!("{}-{}", slug, agent_suffix);
            let display_name = format!("{} {}", agent_type.emoji(), slug);

            // Idempotency check
            let tab_alive = self.is_tmux_window_alive(&display_name).await;
            if tab_alive {
                info!(slug = %slug, "Leaf subtree already running, returning existing");
                return Ok(SpawnResult {
                    agent_dir: self.worktree_base.join(&slug),
                    tab_name: internal_name,
                    issue_title: options.branch_name.clone(),
                    agent_type,
                });
            }

            // Push parent branch so child PRs can reference it as base
            ensure_branch_pushed(&self.git_wt, &current_branch, effective_project_dir).await;

            let child_birth = effective_birth.child(&slug);
            let branch_name = child_birth.to_string();

            let worktree_path = self.worktree_base.join(&slug);

            if options.standalone_repo {
                self.init_standalone_repo(&worktree_path).await?;
                if !options.allowed_dirs.is_empty() {
                    self.copy_allowed_dirs(&worktree_path, &options.allowed_dirs).await?;
                }
            } else {
                self.create_worktree_checked(&worktree_path, &branch_name, &current_branch).await?;
            }

            self.create_socket_symlink(&worktree_path).await;

            let role = options.role.as_deref().unwrap_or("dev");
            let mut env_vars = self.common_spawn_env(&internal_name, &branch_name, role);
            self.write_agent_mcp_config(effective_project_dir, &worktree_path, agent_type, role)
                .await?;

            // Set GEMINI_CLI_SYSTEM_SETTINGS_PATH
            let settings_path = worktree_path.join(".gemini").join("settings.json");
            env_vars.insert(
                "GEMINI_CLI_SYSTEM_SETTINGS_PATH".to_string(),
                settings_path.to_string_lossy().to_string(),
            );

            let mut task = options.task.clone();
            if options.standalone_repo && !options.allowed_dirs.is_empty() {
                task.push_str("\n\nShared technical dependencies are available as read-only reference in `.exo/context/`. Do not modify files in this directory.");
            }

            // Open tmux window (not pane)
            // Task already includes leaf completion protocol — rendered by Haskell Prompt builder.
            let agent_config_dir = self.project_dir.join(".exo").join("agents").join(&internal_name);
            let window_id = match self.new_tmux_window(
                &display_name,
                &worktree_path,
                agent_type,
                Some(&task),
                env_vars,
            )
            .await {
                Ok(wid) => wid,
                Err(e) => {
                    warn!(name = %slug, error = %e, "tmux window creation failed, rolling back");
                    let _ = fs::remove_dir_all(&agent_config_dir).await;
                    // Remove worktree if it was created
                    if worktree_path.exists() {
                        let git_wt = self.git_wt.clone();
                        let path = worktree_path.clone();
                        let _ = tokio::task::spawn_blocking(move || git_wt.remove_workspace(&path)).await;
                    }
                    return Err(e);
                }
            };

            // Store window_id for message delivery and cleanup
            let routing = RoutingInfo::window(window_id.as_str());
            self.finalize_spawn(&internal_name, routing)
                .await?;

            // Register as synthetic team member for Claude Teams messaging
            let team_name = format!("exo-{}", effective_birth);
            if let Err(e) = crate::services::synthetic_members::register_synthetic_member(
                &team_name, &slug, "gemini-leaf",
            ) {
                warn!(agent = %slug, team = %team_name, error = %e, "Failed to register synthetic team member (non-fatal)");
            }

            Ok::<SpawnResult, anyhow::Error>(SpawnResult {
                agent_dir: worktree_path.clone(),
                tab_name: internal_name,
                issue_title: options.branch_name.clone(),
                agent_type,
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
    /// Kills the tmux window, unregisters from Teams config.json,
    /// and removes per-agent config directory (`.exo/agents/{name}/`).
    #[tracing::instrument(skip(self))]
    pub async fn cleanup_agent(&self, identifier: &str) -> Result<()> {
        // Remove synthetic team member registration (non-fatal if not registered)
        let team_name = format!("exo-{}", self.birth_branch);
        if let Err(e) =
            crate::services::synthetic_members::remove_synthetic_member(&team_name, identifier)
        {
            warn!(team = %team_name, member = %identifier, error = %e, "Failed to remove synthetic team member (non-fatal)");
        }

        // Try to find agent in list (for metadata and window matching).
        // Failure here is non-fatal to allow cleaning up worker panes (invisible to list_agents).
        let agents = self.list_agents().await.unwrap_or_default();
        let agent = agents.iter().find(|a| a.issue_id == identifier);

        info!(
            identifier,
            found = agent.is_some(),
            "Initiating cleanup_agent"
        );

        // Reconstruct names for paths and exact window matching.
        // If agent not found, assume gemini worker (Phase 1 convention).
        let (agent_type, display_name) = match agent {
            Some(a) => {
                let at = a.agent_type.unwrap_or(AgentType::Gemini);
                let emoji = at.emoji();
                (at, Some(format!("{} {}", emoji, identifier)))
            }
            None => (AgentType::Gemini, None),
        };

        let internal_name = format!("{}-{}", identifier, agent_type.suffix());

        // Remove per-agent config directory (.exo/agents/{name}/)
        let agent_config_dir = self
            .project_dir
            .join(".exo")
            .join("agents")
            .join(&internal_name);

        // Try direct cleanup via stored window_id (O(1), no listing needed)
        let mut window_closed = false;
        if let Ok(routing) = RoutingInfo::read_from_dir(&agent_config_dir).await {
            if let Some(wid_str) = routing.window_id {
                if let Ok(wid) = crate::services::tmux_ipc::WindowId::parse(&wid_str) {
                    let tmux = self.tmux()?;
                    match tokio::task::spawn_blocking(move || tmux.kill_window(&wid)).await {
                        Ok(Ok(())) => {
                            info!(identifier, "Closed tmux window via stored window_id");
                            window_closed = true;
                        }
                        Ok(Err(e)) => {
                            warn!(identifier, error = %e, "kill_window by stored ID failed, falling back to name match");
                        }
                        Err(e) => {
                            warn!(identifier, error = %e, "spawn_blocking join error");
                        }
                    }
                }
            }
        }

        // Close tmux window if found in list
        if !window_closed {
            if let Some(target_window) = display_name {
                let windows = self.get_tmux_windows().await.unwrap_or_default();
                for window in &windows {
                    if window == &target_window {
                        if let Err(e) = self.close_tmux_window(window).await {
                            warn!(window_name = %window, error = %e, "Failed to close tmux window (may not exist)");
                        }
                        break;
                    }
                }
            }
        }

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

        // Remove git worktree if it exists.
        // spawn_subtree/spawn_leaf_subtree use slug (identifier) as dir name,
        // spawn_agent/spawn_gemini_teammate use internal_name ({id}-{type}).
        // Try slug first, fall back to internal_name.
        let worktree_path = {
            let slug_path = self.worktree_base.join(identifier);
            if slug_path.exists() {
                slug_path
            } else {
                self.worktree_base.join(&internal_name)
            }
        };
        if worktree_path.exists() {
            let git_wt = self.git_wt.clone();
            let path = worktree_path.clone();
            let join_result =
                tokio::task::spawn_blocking(move || git_wt.remove_workspace(&path)).await;
            match join_result {
                Ok(Ok(())) => {
                    // Successfully removed workspace
                }
                Ok(Err(e)) => {
                    warn!(
                        path = %worktree_path.display(),
                        error = %e,
                        "Failed to remove git worktree (non-fatal)"
                    );
                }
                Err(join_err) => {
                    warn!(
                        path = %worktree_path.display(),
                        error = %join_err,
                        "Blocking task for git worktree removal panicked or was cancelled (non-fatal)"
                    );
                }
            }
        }

        // Emit agent:stopped event
        if let Some(ref session) = self.tmux_session {
            if let Ok(agent_id) = crate::ui_protocol::AgentId::try_from(identifier.to_string()) {
                let event = crate::ui_protocol::AgentEvent::AgentStopped {
                    agent_id,
                    timestamp: tmux_events::now_iso8601(),
                };
                if let Err(e) = tmux_events::emit_event(session, &event) {
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
        subrepo: Option<&str>,
    ) -> BatchCleanupResult {
        let mut result = BatchCleanupResult {
            cleaned: Vec::new(),
            failed: Vec::new(),
        };

        for issue_id in issue_ids {
            match self.cleanup_agent(issue_id).await {
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

            // Skip SharedDir (worker pane) agents — their liveness can't be
            // reliably detected via tab queries, so "Stopped" may be wrong.
            if agent.topology == Topology::SharedDir {
                continue;
            }

            // Only clean up stopped agents (no running tab)
            if !agent.has_tab {
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

        Ok(self.cleanup_agents(&to_cleanup, subrepo).await)
    }

    // ========================================================================
    // List Agents
    // ========================================================================

    /// List all active agents by scanning the filesystem and verifying with tmux.
    ///
    /// Discovery process:
    /// 1. Scan {worktree_base}/ for subtree agents (isolated worktrees)
    /// 2. Scan {project_dir}/.exo/agents/ for worker agents (shared worktree)
    /// 3. Verify liveness by checking tmux windows/panes
    #[tracing::instrument(skip(self))]
    pub async fn list_agents(&self) -> Result<Vec<AgentInfo>> {
        let mut agents = Vec::new();

        // Get all tmux windows for liveness check
        let windows = self.get_tmux_windows().await.unwrap_or_default();

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
                        let agent_type = if is_claude {
                            AgentType::Claude
                        } else {
                            AgentType::Gemini
                        };
                        let display_name = format!("{} {}", agent_type.emoji(), name);

                        let has_tab = windows.iter().any(|t| t == &display_name);

                        agents.push(AgentInfo {
                            issue_id: name.to_string(),
                            has_tab,
                            topology: Topology::WorktreePerAgent,
                            agent_dir: Some(path.clone()),
                            slug: Some(AgentName::from(name)),
                            agent_type: Some(agent_type),
                            pr: None,
                        });

                        // 2. Scan subtree's .exo/agents for workers
                        let subtree_agents_dir = path.join(".exo/agents");
                        if subtree_agents_dir.exists() {
                            self.scan_workers(&subtree_agents_dir, &windows, &mut agents)
                                .await?;
                        }
                    }
                }
            }
        }

        // 3. Scan root .exo/agents for workers
        let root_agents_dir = self.project_dir.join(".exo/agents");
        if root_agents_dir.exists() {
            self.scan_workers(&root_agents_dir, &windows, &mut agents)
                .await?;
        }

        Ok(agents)
    }

    /// Helper to scan a directory for worker agents.
    async fn scan_workers(
        &self,
        dir: &Path,
        windows: &[String],
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

                    // Liveness: for workers, they might be panes in a window.
                    // Currently list_agents only sees windows.
                    let has_tab = windows.iter().any(|t| t == &display_name);

                    agents.push(AgentInfo {
                        issue_id: name.to_string(),
                        has_tab,
                        topology: Topology::SharedDir,
                        agent_dir: Some(path.clone()),
                        slug: Some(AgentName::from(base_name)),
                        agent_type: Some(AgentType::Gemini),
                        pr: None,
                    });
                }
            }
        }
        Ok(())
    }

    // ========================================================================
    // Internal: tmux
    // ========================================================================

    fn resolve_tmux_session(&self) -> Result<String> {
        self.tmux_session
            .clone()
            .ok_or_else(|| anyhow!("No tmux session configured (call with_tmux_session)"))
    }

    /// Get the direct tmux IPC client, falling back to creating one from config or env.
    fn tmux(&self) -> Result<super::tmux_ipc::TmuxIpc> {
        if let Some(ref ipc) = self.tmux_ipc {
            return Ok(ipc.clone());
        }
        let session = self.resolve_tmux_session()?;
        Ok(super::tmux_ipc::TmuxIpc::new(&session))
    }

    /// Clean up an existing worktree (if present) and create a fresh one.
    ///
    /// Consolidates the idempotent cleanup + spawn_blocking + catch_unwind boilerplate
    /// shared across spawn_agent, spawn_subtree, spawn_leaf_subtree, and spawn_gemini_teammate.
    async fn create_worktree_checked(
        &self,
        worktree_path: &Path,
        branch_name: &str,
        base_branch: &str,
    ) -> Result<()> {
        if worktree_path.exists() {
            info!(path = %worktree_path.display(), "Removing existing workspace for idempotency");
            let git_wt = self.git_wt.clone();
            let path = worktree_path.to_path_buf();
            match tokio::task::spawn_blocking(move || git_wt.remove_workspace(&path)).await {
                Err(join_err) => {
                    warn!(error = %join_err, "Join error while removing existing workspace (non-fatal)");
                }
                Ok(Err(e)) => {
                    warn!(error = %e, "Failed to remove existing workspace (non-fatal)");
                }
                Ok(Ok(_)) => {}
            }
        }

        info!(
            base_branch = %base_branch,
            branch_name = %branch_name,
            worktree_path = %worktree_path.display(),
            "Creating git worktree"
        );

        let git_wt = self.git_wt.clone();
        let path = worktree_path.to_path_buf();
        let bookmark = crate::domain::BranchName::from(branch_name);
        let base = crate::domain::BranchName::from(base_branch);
        let result = tokio::task::spawn_blocking(move || {
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                git_wt.create_workspace(&path, &bookmark, &base)
            }))
        })
        .await
        .context("tokio task join error while creating git worktree")?;

        match result {
            Ok(Ok(())) => {}
            Ok(Err(e)) => {
                return Err(anyhow::Error::from(EffectError::from(e)))
                    .context("Failed to create git worktree")
            }
            Err(panic_val) => {
                let msg = panic_val
                    .downcast_ref::<String>()
                    .map(|s| s.as_str())
                    .or_else(|| panic_val.downcast_ref::<&str>().copied())
                    .unwrap_or("unknown panic");
                return Err(anyhow!("git worktree creation panicked: {}", msg));
            }
        }

        Ok(())
    }

    /// Build the common env vars shared by all spawn functions.
    fn common_spawn_env(
        &self,
        internal_name: &str,
        session_id: &str,
        role: &str,
    ) -> HashMap<String, String> {
        let mut env_vars = HashMap::new();
        env_vars.insert("EXOMONAD_AGENT_ID".to_string(), internal_name.to_string());
        env_vars.insert("EXOMONAD_SESSION_ID".to_string(), session_id.to_string());
        env_vars.insert("EXOMONAD_ROLE".to_string(), role.to_string());
        if let Some(ref session) = self.tmux_session {
            env_vars.insert("EXOMONAD_TMUX_SESSION".to_string(), session.clone());
        }

        // Propagate swarm run_id and parent agent identity for OTel resource attributes
        if let Ok(v) = std::env::var("EXOMONAD_SWARM_RUN_ID") {
            env_vars.insert("EXOMONAD_SWARM_RUN_ID".to_string(), v);
        }
        env_vars.insert(
            "EXOMONAD_PARENT_AGENT".to_string(),
            self.effective_birth_branch(None).to_string(),
        );

        // Propagate W3C traceparent for cross-agent trace correlation
        {
            use tracing_opentelemetry::OpenTelemetrySpanExt;
            let cx = tracing::Span::current().context();
            let mut injector = std::collections::HashMap::new();
            opentelemetry::global::get_text_map_propagator(|propagator| {
                propagator.inject_context(&cx, &mut injector);
            });
            if let Some(traceparent) = injector.get("traceparent") {
                env_vars.insert("TRACEPARENT".to_string(), traceparent.clone());
            }
        }

        env_vars
    }

    /// Emit an agent:started event if tmux_session is configured.
    fn emit_agent_started(&self, internal_name: &str) -> Result<()> {
        if let Some(ref session) = self.tmux_session {
            let agent_id = crate::ui_protocol::AgentId::try_from(internal_name.to_string())
                .map_err(|e| anyhow!("Invalid agent_id: {}", e))?;
            let event = crate::ui_protocol::AgentEvent::AgentStarted {
                agent_id,
                timestamp: tmux_events::now_iso8601(),
            };
            if let Err(e) = tmux_events::emit_event(session, &event) {
                warn!("Failed to emit agent:started event: {}", e);
            }
        }
        Ok(())
    }

    async fn new_tmux_window(
        &self,
        name: &str,
        cwd: &Path,
        agent_type: AgentType,
        prompt: Option<&str>,
        env_vars: HashMap<String, String>,
    ) -> Result<super::tmux_ipc::WindowId> {
        self.new_tmux_window_inner(name, cwd, agent_type, prompt, env_vars, None, None)
            .await
    }

    /// Build the full shell command string for an agent.
    ///
    /// Handles: agent CLI + prompt/flags → env var prefix → nix develop wrapping.
    /// Used by both `new_tmux_window_inner` and `new_tmux_pane`.
    fn build_agent_command(
        agent_type: AgentType,
        prompt: Option<&str>,
        fork_session_id: Option<&str>,
        env_vars: &HashMap<String, String>,
        cwd: &Path,
        claude_flags: Option<&ClaudeSpawnFlags>,
        yolo: bool,
    ) -> String {
        let cmd = agent_type.command();

        // Build permission flags for Claude agents
        let perms_flags = match agent_type {
            AgentType::Claude => {
                let mut flags = String::new();
                let mode = claude_flags.and_then(|f| f.permission_mode.as_deref());
                match mode {
                    Some(m) => {
                        flags.push_str(" --permission-mode ");
                        flags.push_str(m);
                    }
                    None => flags.push_str(" --dangerously-skip-permissions"),
                }
                if let Some(f) = claude_flags {
                    for tool in &f.allowed_tools {
                        flags.push_str(" --allowedTools ");
                        flags.push_str(&shell_escape::escape(tool.into()));
                    }
                    for tool in &f.disallowed_tools {
                        flags.push_str(" --disallowedTools ");
                        flags.push_str(&shell_escape::escape(tool.into()));
                    }
                }
                flags
            }
            AgentType::Gemini => {
                if yolo {
                    " --yolo".to_string()
                } else {
                    String::new()
                }
            }
            AgentType::Shoal => String::new(),
        };

        let agent_command = match (prompt, fork_session_id) {
            (Some(p), Some(session_id)) => {
                let escaped_prompt = Self::escape_for_shell_command(p);
                let escaped_session = Self::escape_for_shell_command(session_id);
                format!(
                    "{}{} --resume {} --fork-session {}",
                    cmd, perms_flags, escaped_session, escaped_prompt
                )
            }
            (Some(p), None) => {
                let escaped_prompt = Self::escape_for_shell_command(p);
                let flag = agent_type.prompt_flag();
                if flag.is_empty() {
                    format!("{}{} {}", cmd, perms_flags, escaped_prompt)
                } else {
                    format!("{}{} {} {}", cmd, perms_flags, flag, escaped_prompt)
                }
            }
            _ => format!("{}{}", cmd, perms_flags),
        };

        // Prepend env vars
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
        if cwd.join("flake.nix").exists() {
            info!("Wrapping agent command in nix develop shell");
            let escaped = full_command.replace('\'', "'\\''");
            format!("nix develop -c sh -c '{}'", escaped)
        } else {
            full_command
        }
    }

    async fn new_tmux_window_inner(
        &self,
        name: &str,
        cwd: &Path,
        agent_type: AgentType,
        prompt: Option<&str>,
        env_vars: HashMap<String, String>,
        fork_session_id: Option<&str>,
        claude_flags: Option<&ClaudeSpawnFlags>,
    ) -> Result<super::tmux_ipc::WindowId> {
        info!(name, cwd = %cwd.display(), agent_type = ?agent_type, fork = fork_session_id.is_some(), "Creating tmux window");

        let full_command = Self::build_agent_command(
            agent_type,
            prompt,
            fork_session_id,
            &env_vars,
            cwd,
            claude_flags,
            self.yolo,
        );
        let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/sh".to_string());
        let tmux = self.tmux()?;
        let window_name = name.to_string();
        let window_cwd = cwd.to_path_buf();

        let window_id = tokio::task::spawn_blocking(move || {
            tmux.new_window(&window_name, &window_cwd, &shell, &full_command)
        })
        .await
        .context("tokio task join error")?
        .context("Failed to create tmux window")?;

        Ok(window_id)
    }

    async fn get_tmux_windows(&self) -> Result<Vec<String>> {
        debug!("Querying tmux window names via direct IPC");
        let tmux = match self.tmux() {
            Ok(t) => t,
            Err(e) => {
                warn!("Failed to get tmux IPC client for list-windows: {}", e);
                return Ok(Vec::new());
            }
        };

        let result = timeout(
            TMUX_TIMEOUT,
            tokio::task::spawn_blocking(move || tmux.list_windows()),
        )
        .await
        .map_err(|_| {
            anyhow!(
                "tmux list-windows timed out after {}s",
                TMUX_TIMEOUT.as_secs()
            )
        })?
        .context("tokio task join error")?;

        match result {
            Ok(windows) => Ok(windows.into_iter().map(|w| w.window_name).collect()),
            Err(e) => {
                warn!("tmux list-windows IPC failed, assuming no windows: {}", e);
                Ok(Vec::new())
            }
        }
    }

    /// Check if a tmux window with the given display name exists.
    async fn is_tmux_window_alive(&self, display_name: &str) -> bool {
        self.get_tmux_windows()
            .await
            .unwrap_or_default()
            .iter()
            .any(|window| window == display_name)
    }

    async fn close_tmux_window(&self, name: &str) -> Result<()> {
        info!(name, "Closing tmux window");

        let tmux = self.tmux()?;
        let window_name = name.to_string();

        let window_id = tokio::task::spawn_blocking(move || {
            let windows = tmux.list_windows()?;
            windows
                .into_iter()
                .find(|w| w.window_name == window_name)
                .map(|w| w.window_id)
                .ok_or_else(|| anyhow!("Window not found: {}", window_name))
        })
        .await
        .context("tokio task join error")??;

        let tmux = self.tmux()?;
        timeout(
            TMUX_TIMEOUT,
            tokio::task::spawn_blocking(move || tmux.kill_window(&window_id)),
        )
        .await
        .map_err(|_| {
            anyhow::Error::new(TimeoutError {
                message: format!(
                    "tmux kill-window timed out after {}s",
                    TMUX_TIMEOUT.as_secs()
                ),
            })
        })?
        .context("tokio task join error")?
        .context("tmux kill-window failed")?;

        info!(name, "tmux kill-window successful");
        Ok(())
    }

    // ========================================================================
    // Internal: tmux Pane Creation
    // ========================================================================

    async fn new_tmux_pane(
        &self,
        name: &str,
        cwd: &Path,
        agent_type: AgentType,
        prompt: Option<&str>,
        env_vars: HashMap<String, String>,
        parent_window_name: Option<&str>,
        claude_flags: Option<&ClaudeSpawnFlags>,
    ) -> Result<super::tmux_ipc::PaneId> {
        info!(name, cwd = %cwd.display(), agent_type = ?agent_type, parent = ?parent_window_name, "Creating tmux pane");

        let full_command = Self::build_agent_command(
            agent_type,
            prompt,
            None,
            &env_vars,
            cwd,
            claude_flags,
            self.yolo,
        );
        let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/zsh".to_string());
        let tmux = self.tmux()?;

        // Find parent window ID by name
        let target_window = if let Some(wname) = parent_window_name {
            let wname = wname.to_string();
            let t = tmux.clone();
            let windows = tokio::task::spawn_blocking(move || t.list_windows())
                .await
                .context("tokio task join error")?
                .context("Failed to list tmux windows")?;
            windows
                .iter()
                .find(|w| w.window_name == wname)
                .map(|w| w.window_id.clone())
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "No tmux window found matching '{}' — cannot create pane",
                        wname
                    )
                })?
        } else {
            // Default to first window if no name provided
            let t = tmux.clone();
            let windows = tokio::task::spawn_blocking(move || t.list_windows())
                .await
                .context("tokio task join error")?
                .context("Failed to list tmux windows")?;
            windows
                .first()
                .map(|w| w.window_id.clone())
                .ok_or_else(|| {
                    anyhow!(
                        "No windows found in session {} — cannot create pane",
                        tmux.session_name()
                    )
                })?
        };

        let pane_cwd = cwd.to_path_buf();
        let t = tmux.clone();
        let pane_id = tokio::task::spawn_blocking(move || {
            let pane_id = t.split_window(&target_window, &pane_cwd, &shell, &full_command)?;
            // Rebalance panes into a grid after each split to prevent
            // exponential height decay (60 → 29 → 14 → 6 → 2 → 1 lines).
            if let Err(e) = t.select_layout(&target_window, "tiled") {
                tracing::warn!(error = %e, "Failed to apply tiled layout (non-fatal)");
            }
            Ok::<_, anyhow::Error>(pane_id)
        })
        .await
        .context("tokio task join error")?
        .context("Failed to create tmux pane")?;

        info!(name, pane_id = %pane_id, "Successfully created tmux pane");
        Ok(pane_id)
    }

    // ========================================================================
    // Internal: Agent Config Files
    // ========================================================================

    /// Write MCP config for the agent directory.
    ///
    /// Claude agents get `.mcp.json`. Gemini agents get `.gemini/settings.json`.
    /// Uses stdio transport via `exomonad mcp-stdio`.
    async fn write_agent_mcp_config(
        &self,
        _effective_dir: &Path,
        agent_dir: &Path,
        agent_type: AgentType,
        role: &str,
    ) -> Result<()> {
        let agent_name = agent_dir
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown");

        let mcp_content = Self::generate_mcp_config(agent_name, agent_type, role);

        match agent_type {
            AgentType::Claude => {
                fs::write(agent_dir.join(".mcp.json"), mcp_content).await?;
                info!(agent_dir = %agent_dir.display(), role = %role, "Wrote .mcp.json for Claude agent");
            }
            AgentType::Gemini => {
                let gemini_dir = agent_dir.join(".gemini");
                fs::create_dir_all(&gemini_dir).await?;
                fs::write(gemini_dir.join("settings.json"), mcp_content).await?;
                info!(agent_dir = %agent_dir.display(), role = %role, "Wrote .gemini/settings.json for Gemini agent");
            }
            AgentType::Shoal => {
                let exo_dir = agent_dir.join(".exo");
                fs::create_dir_all(&exo_dir).await?;
                fs::write(exo_dir.join("mcp.json"), mcp_content).await?;
                info!(agent_dir = %agent_dir.display(), role = %role, "Wrote .exo/mcp.json for Shoal agent");
            }
        }
        Ok(())
    }

    /// Symlink server socket into worktree so agents find it without walk-up.
    async fn create_socket_symlink(&self, worktree_path: &Path) {
        let source = self.project_dir.join(".exo/server.sock");
        let target_dir = worktree_path.join(".exo");
        let target = target_dir.join("server.sock");

        if let Err(e) = tokio::fs::create_dir_all(&target_dir).await {
            warn!(path = %target_dir.display(), error = %e, "Failed to create .exo/ in worktree");
            return;
        }

        // Ensure worktree .exo/ has a .gitignore so runtime artifacts don't cause
        // untracked file warnings (which force `git worktree remove --force`).
        let gitignore = target_dir.join(".gitignore");
        if !gitignore.exists() {
            if let Err(e) =
                tokio::fs::write(&gitignore, "# Runtime artifacts\nserver.sock\nserver.pid\n").await
            {
                tracing::warn!(path = %gitignore.display(), error = %e, "Failed to write .gitignore");
            }
        }

        if let Err(e) = tokio::fs::remove_file(&target).await {
            tracing::debug!(path = %target.display(), error = %e, "Could not remove old socket symlink");
        }

        match tokio::fs::symlink(&source, &target).await {
            Ok(()) => info!(
                source = %source.display(),
                target = %target.display(),
                "Symlinked server socket into worktree"
            ),
            Err(e) => warn!(
                source = %source.display(),
                target = %target.display(),
                error = %e,
                "Failed to symlink server socket"
            ),
        }
    }

    /// Generate MCP configuration JSON for an agent using stdio transport.
    fn generate_mcp_config(name: &str, agent_type: AgentType, role: &str) -> String {
        match agent_type {
            AgentType::Claude => serde_json::to_string_pretty(&serde_json::json!({
                "mcpServers": {
                    "exomonad": {
                        "type": "stdio",
                        "command": "exomonad",
                        "args": ["mcp-stdio", "--role", role, "--name", name]
                    }
                }
            }))
            .unwrap(),
            AgentType::Gemini => serde_json::to_string_pretty(&serde_json::json!({
                "mcpServers": {
                    "exomonad": {
                        "type": "stdio",
                        "command": "exomonad",
                        "args": ["mcp-stdio", "--role", role, "--name", name]
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
                                    "command": "exomonad hook after-agent --runtime gemini"
                                }
                            ]
                        }
                    ]
                }
            }))
            .unwrap(),
            AgentType::Shoal => serde_json::to_string_pretty(&serde_json::json!({
                "command": "exomonad",
                "args": ["mcp-stdio", "--role", role, "--name", name]
            }))
            .unwrap(),
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
            AgentControlService::generate_mcp_config("test-claude", AgentType::Claude, "tl");
        let parsed: serde_json::Value = serde_json::from_str(&config).unwrap();
        assert_eq!(parsed["mcpServers"]["exomonad"]["type"], "stdio");
        assert_eq!(parsed["mcpServers"]["exomonad"]["command"], "exomonad");
        let args = parsed["mcpServers"]["exomonad"]["args"].as_array().unwrap();
        assert_eq!(
            args,
            &["mcp-stdio", "--role", "tl", "--name", "test-claude"]
        );
    }

    #[test]
    fn test_gemini_mcp_config_format() {
        let config =
            AgentControlService::generate_mcp_config("test-gemini", AgentType::Gemini, "dev");
        let parsed: serde_json::Value = serde_json::from_str(&config).unwrap();
        assert_eq!(parsed["mcpServers"]["exomonad"]["command"], "exomonad");
        let args = parsed["mcpServers"]["exomonad"]["args"].as_array().unwrap();
        assert_eq!(
            args,
            &["mcp-stdio", "--role", "dev", "--name", "test-gemini"]
        );
        assert_eq!(parsed["mcpServers"]["exomonad"]["type"], "stdio");

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
    fn test_gemini_worker_settings_schema_compliance() {
        let settings = AgentControlService::generate_gemini_worker_settings("test-worker");

        // 1. MCP config uses stdio transport
        assert_eq!(settings["mcpServers"]["exomonad"]["type"], "stdio");
        assert_eq!(settings["mcpServers"]["exomonad"]["command"], "exomonad");
        let args = settings["mcpServers"]["exomonad"]["args"]
            .as_array()
            .unwrap();
        assert_eq!(
            args,
            &["mcp-stdio", "--role", "worker", "--name", "test-worker"]
        );

        // 2. Hooks must strictly use PascalCase
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

        // 3. The hook structure must match the array of matcher/hooks objects
        let after_agent = &settings["hooks"]["AfterAgent"];
        assert!(after_agent.is_array(), "hooks.AfterAgent must be an array");

        let first_rule = &after_agent[0];
        assert_eq!(first_rule["matcher"], "*");

        let hooks_list = &first_rule["hooks"];
        assert!(hooks_list.is_array());

        let command_hook = &hooks_list[0];
        assert_eq!(command_hook["type"], "command");
        assert_eq!(
            command_hook["command"], "exomonad hook worker-exit --runtime gemini",
            "Hook command mismatch"
        );
    }

    #[tokio::test]
    async fn test_copy_allowed_dirs_validation() {
        let temp_dir = tempfile::tempdir().unwrap();
        let project_dir = temp_dir.path().to_path_buf();

        // Setup source dirs
        let shared_context = project_dir.join("shared-context");
        fs::create_dir_all(&shared_context).await.unwrap();
        fs::write(shared_context.join("ref.txt"), "context data")
            .await
            .unwrap();

        let agent_wt = project_dir.join("agent-wt");
        fs::create_dir_all(&agent_wt).await.unwrap();

        let git_wt = Arc::new(crate::services::git_worktree::GitWorktreeService::new(
            project_dir.clone(),
        ));
        let service = AgentControlService::new(project_dir.clone(), None, git_wt);

        // Test valid copy
        service
            .copy_allowed_dirs(&agent_wt, &["shared-context".to_string()])
            .await
            .unwrap();
        assert!(agent_wt
            .join(".exo/context/shared-context/ref.txt")
            .exists());

        // Test invalid paths (should skip but not fail)
        service
            .copy_allowed_dirs(
                &agent_wt,
                &["/absolute".to_string(), "../outside".to_string()],
            )
            .await
            .unwrap();
        assert!(!agent_wt.join(".exo/context/absolute").exists());
        assert!(!agent_wt.join(".exo/context/outside").exists());
    }

    #[tokio::test]
    async fn test_create_socket_symlink() {
        let temp_dir = tempfile::tempdir().unwrap();
        let project_dir = temp_dir.path().to_path_buf();
        let exo_dir = project_dir.join(".exo");
        tokio::fs::create_dir_all(&exo_dir).await.unwrap();
        tokio::fs::write(exo_dir.join("server.sock"), "placeholder")
            .await
            .unwrap();

        let git_wt = Arc::new(crate::services::git_worktree::GitWorktreeService::new(
            project_dir.clone(),
        ));
        let service = AgentControlService::new(project_dir.clone(), None, git_wt);

        let worktree = temp_dir.path().join("child-wt");
        tokio::fs::create_dir_all(&worktree).await.unwrap();

        service.create_socket_symlink(&worktree).await;

        let link = worktree.join(".exo/server.sock");
        assert!(link.exists(), "Symlink should exist");
        let target = tokio::fs::read_link(&link).await.unwrap();
        assert_eq!(target, project_dir.join(".exo/server.sock"));
    }

    #[test]
    fn test_claude_project_path_encoding() {
        // Claude Code encodes paths via [^a-zA-Z0-9] → '-'
        // Verified against actual ~/.claude/projects/ directory names.
        let encode = |s: &str| -> String {
            s.chars()
                .map(|c| if c.is_ascii_alphanumeric() { c } else { '-' })
                .collect()
        };

        // Basic path
        assert_eq!(
            encode("/home/inanna/dev/exomonad"),
            "-home-inanna-dev-exomonad"
        );
        // Worktree path (dots and hyphens in segments)
        assert_eq!(
            encode("/home/inanna/dev/exomonad/.exo/worktrees/fork-session"),
            "-home-inanna-dev-exomonad--exo-worktrees-fork-session"
        );
        // Hidden dir (leading dot → double dash after parent separator)
        assert_eq!(
            encode("/home/inanna/.config/home-manager"),
            "-home-inanna--config-home-manager"
        );
        // Deep nested path with hyphens
        assert_eq!(
            encode("/home/inanna/dev/aegis-binder-diagnostic-framework"),
            "-home-inanna-dev-aegis-binder-diagnostic-framework"
        );
        // Path with spaces
        assert_eq!(
            encode("/home/user/My Projects/app"),
            "-home-user-My-Projects-app"
        );
    }
}
