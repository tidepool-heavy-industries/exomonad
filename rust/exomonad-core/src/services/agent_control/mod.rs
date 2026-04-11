//! High-level agent control service.
//!
//! Provides semantic operations for agent lifecycle management:
//! - SpawnAgent: Create agent directory, open tmux window
//! - CleanupAgent: Close tab, remove per-agent config
//! - ListAgents: Discover from tmux windows (source of truth for running agents)

mod cleanup;
mod internal;
mod spawn;

pub(crate) use internal::SpawnRollback;

pub(crate) use crate::common::TimeoutError;
pub(crate) use crate::domain::{
    AgentName, AgentPermissions, BirthBranch, BranchName, ClaudeSessionUuid, ItemState,
    RoutingInfo, Slug, TeamName,
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
pub(crate) use tracing::{debug, info, instrument, warn};

pub(crate) use super::agent_resolver::{AgentIdentityRecord, AgentResolver};
pub(crate) use super::git_worktree::GitWorktreeService;
pub(crate) use super::github::{GitHubClient, GitHubService, Repo};
pub(crate) use super::tmux_events;
pub(crate) use super::tmux_ipc;
pub(crate) use claude_teams_bridge::TeamRegistry;
pub(crate) use std::sync::Arc;

pub(crate) const SPAWN_TIMEOUT: Duration = Duration::from_secs(60);
pub(crate) const TMUX_TIMEOUT: Duration = Duration::from_secs(30);

/// Push the parent branch to the remote so child PRs can reference it as
/// their base. Non-fatal: warns on failure (supports local/airgapped setups
/// where no remote or non-GitHub remote is configured).
pub(crate) async fn ensure_branch_pushed(
    git_wt: &Arc<GitWorktreeService>,
    branch: &BranchName,
    project_dir: &Path,
) {
    info!(branch = %branch, "Pushing parent branch to remote");
    let git_wt = git_wt.clone();
    let dir = project_dir.to_path_buf();
    let bookmark = branch.clone();
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

/// Pairs a bare slug with its agent type, providing named accessors for all
/// derived naming forms. Construct at the boundary (from proto fields, dir names,
/// MCP args), then thread through code as a typed value.
///
/// # Naming concepts
/// - **slug**: Bare human-readable identifier (`"feature-a"`)
/// - **internal_name**: Suffixed directory/identity name as `AgentName` (`"feature-a-claude"`)
/// - **display_name**: tmux window name (`"🤖 feature-a-claude"`)
///
/// `internal_name()` returns `AgentName` (a validated newtype), not `String`.
/// This makes it impossible to accidentally pass a bare slug where an internal
/// name is expected — the type system catches it.
#[derive(Debug, Clone)]
pub struct AgentIdentity {
    slug: String,
    agent_type: AgentType,
}

impl AgentIdentity {
    /// Construct from a bare slug and agent type.
    pub fn new(slug: String, agent_type: AgentType) -> Self {
        Self { slug, agent_type }
    }

    /// Parse from an internal name (e.g., `"feature-a-claude"` → slug=`"feature-a"`, type=Claude).
    /// Falls back to Gemini if no known suffix is found.
    pub fn from_internal_name(name: &str) -> Self {
        let agent_type = AgentType::from_dir_name(name);
        let suffix = format!("-{}", agent_type.suffix());
        let slug = name.strip_suffix(&suffix).unwrap_or(name).to_string();
        Self { slug, agent_type }
    }

    /// Bare slug without type suffix.
    pub fn slug(&self) -> &str {
        &self.slug
    }

    /// Agent type.
    pub fn agent_type(&self) -> AgentType {
        self.agent_type
    }

    /// Suffixed directory/identity name as a validated `AgentName`.
    ///
    /// Used for: worktree dirs, agent config dirs, synthetic member names,
    /// MCP --name flag, EXOMONAD_AGENT_ID env var.
    pub fn internal_name(&self) -> AgentName {
        // Safe: slug is non-empty (validated at construction) and suffix is non-empty,
        // so the formatted string is always non-empty.
        AgentName::from(format!("{}-{}", self.slug, self.agent_type.suffix()).as_str())
    }

    /// tmux window display name (e.g., `"🤖 feature-a-claude"`).
    ///
    /// Includes the type suffix so `resolve_worktree_from_tab` (which extracts the
    /// segment after the emoji) yields the internal_name, matching the worktree directory.
    pub fn display_name(&self) -> String {
        format!("{} {}", self.agent_type.emoji(), self.internal_name())
    }
}

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

    /// Plain long-running process (no MCP, no agent identity, no worktree).
    /// Used for companion processes like mock servers, log tailers, etc.
    Process,
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

pub(crate) const PROCESS_META: AgentMetadata = AgentMetadata {
    command: "",
    prompt_flag: "",
    suffix: "process",
    emoji: "\u{2699}\u{FE0F}", // ⚙️
};

impl AgentType {
    pub(crate) fn meta(&self) -> &'static AgentMetadata {
        match self {
            AgentType::Claude => &CLAUDE_META,
            AgentType::Gemini => &GEMINI_META,
            AgentType::Shoal => &SHOAL_META,
            AgentType::Process => &PROCESS_META,
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
        } else if dir_name.ends_with("-process") {
            AgentType::Process
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
/// Follows the dot-segment hierarchy: last segment is the agent name (suffixed).
/// Example: `"main.feature-a-claude"` → `".exo/worktrees/feature-a-claude/"`.
pub fn resolve_working_dir(birth_branch: &str) -> PathBuf {
    if let Some((_, slug)) = birth_branch.rsplit_once('.') {
        PathBuf::from(format!(".exo/worktrees/{}/", slug))
    } else {
        PathBuf::from(".")
    }
}

/// Resolve the working directory for an agent from its tmux tab name.
///
/// Tab names are formatted as `"{emoji} {agent_name}"` or `"TL"`.
/// Example: `"💎 feature-a-gemini"` → `".exo/worktrees/feature-a-gemini/"`.
pub fn resolve_worktree_from_tab(tab: &str) -> PathBuf {
    if tab == "TL" {
        PathBuf::from(".")
    } else {
        // Tab name is "{emoji} {agent_name}" (e.g. "💎 feature-a-gemini")
        if let Some((_, agent_name)) = tab.split_once(' ') {
            PathBuf::from(format!(".exo/worktrees/{}/", agent_name))
        } else {
            PathBuf::from(".")
        }
    }
}

pub fn resolve_own_tab_name(ctx: &crate::effects::EffectContext) -> String {
    let birth_branch_str = ctx.birth_branch.as_str();

    if birth_branch_str.contains('.') {
        // Last dot-segment is the agent_name (suffixed), matching the tmux tab format.
        let agent_name = birth_branch_str
            .rsplit_once('.')
            .map(|(_, s)| s)
            .unwrap_or(birth_branch_str);
        let agent_type = AgentType::from_dir_name(agent_name);
        agent_type.tab_display_name(agent_name)
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
    pub subrepo: Option<PathBuf>,
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
    pub subrepo: Option<PathBuf>,
    /// Base branch to branch off of (defaults to current branch).
    pub base_branch: Option<BirthBranch>,
}

/// Claude-specific spawn flags for permission control.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ClaudeSpawnFlags {
    /// Permission mode. None = --dangerously-skip-permissions.
    pub permission_mode: Option<crate::domain::PermissionMode>,
    /// Tool patterns to allow (e.g., "Read", "Grep").
    pub allowed_tools: Vec<String>,
    /// Tool patterns to disallow (e.g., "Bash").
    pub disallowed_tools: Vec<String>,
}

/// Options for spawning a worker agent in the current worktree (no branch/worktree).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnWorkerOptions {
    /// Human-readable name for the worker
    pub name: AgentName,
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
    pub role: Option<crate::domain::Role>,
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
    pub role: Option<crate::domain::Role>,
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
    /// Agent's internal name (suffixed, e.g., "feature-a-claude").
    /// Typed as `AgentName` to prevent confusion with bare slugs.
    pub agent_name: AgentName,
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
    /// Internal name for the agent (e.g., "feature-a-claude").
    pub internal_name: AgentName,
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
///
/// Generic over `C` (capability context) which provides shared registries
/// via Has* trait bounds. The same `C` is typically `Services`.
#[derive(Clone)]
pub struct AgentControlService<C> {
    /// Capability context providing shared registries.
    pub(crate) ctx: Arc<C>,
    /// Base directory for worktrees (default: .exo/worktrees)
    pub(crate) worktree_base: PathBuf,
    /// tmux session name for event emission
    pub(crate) tmux_session: Option<String>,
    /// This agent's birth-branch (git identity). Root TL = "main".
    pub(crate) birth_branch: BirthBranch,
    /// When true, spawned Gemini agents receive `--yolo` flag.
    pub(crate) yolo: bool,
    /// WASM name for role context resolution (default: "devswarm").
    pub(crate) wasm_name: String,
    /// Pre-serialized extra MCP servers to include in spawned agent configs.
    pub(crate) extra_mcp_servers: HashMap<String, serde_json::Value>,
}

impl<
        C: super::HasGitHubClient
            + super::HasAcpRegistry
            + super::HasTeamRegistry
            + super::HasAgentResolver
            + super::HasProjectDir
            + super::HasGitWorktreeService
            + super::HasTmuxIpc
            + 'static,
    > AgentControlService<C>
{
    /// Create a new agent control service.
    pub fn new(ctx: Arc<C>) -> Self {
        let worktree_base = ctx.project_dir().join(".exo/worktrees");
        Self {
            ctx,
            worktree_base,
            tmux_session: None,
            birth_branch: BirthBranch::from("unset"),
            yolo: false,
            wasm_name: "devswarm".to_string(),
            extra_mcp_servers: HashMap::new(),
        }
    }

    /// Set the worktree base directory.
    pub fn with_worktree_base(mut self, base: PathBuf) -> Self {
        self.worktree_base = base;
        self
    }

    /// Set the WASM name for role context resolution.
    pub fn with_wasm_name(mut self, wasm_name: String) -> Self {
        self.wasm_name = wasm_name;
        self
    }

    /// Set the tmux session name for event emission.
    pub fn with_tmux_session(mut self, session: String) -> Self {
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

    /// Set extra MCP servers to include in spawned agent configs.
    pub fn with_extra_mcp_servers(mut self, servers: HashMap<String, serde_json::Value>) -> Self {
        self.extra_mcp_servers = servers;
        self
    }

    /// Project root directory (from capability context).
    pub(crate) fn project_dir(&self) -> &std::path::Path {
        self.ctx.project_dir()
    }

    /// GitHub client (from capability context).
    pub(crate) fn github(&self) -> Option<&Arc<GitHubClient>> {
        self.ctx.github_client()
    }

    /// Git worktree service Arc (from capability context).
    pub(crate) fn git_wt(&self) -> &Arc<GitWorktreeService> {
        self.ctx.git_worktree_service()
    }

    /// Team registry (from capability context).
    pub(crate) fn team_registry(&self) -> &TeamRegistry {
        self.ctx.team_registry()
    }

    /// Agent resolver (from capability context).
    pub(crate) fn agent_resolver(&self) -> &AgentResolver {
        self.ctx.agent_resolver()
    }

    /// Resolve the effective birth branch for spawn operations.
    ///
    /// Callers pass the birth branch from EffectContext. Falls back to `self.birth_branch`
    /// if no override is provided.
    pub(crate) fn effective_birth_branch(&self, override_bb: Option<&BirthBranch>) -> BirthBranch {
        override_bb.cloned().unwrap_or_else(|| {
            debug_assert!(
                self.birth_branch.as_str() != "unset",
                "birth_branch was never initialized via with_birth_branch()"
            );
            self.birth_branch.clone()
        })
    }

    /// Common post-spawn bookkeeping.
    ///
    /// Creates the agent's config directory, writes routing info, and registers
    /// identity with the resolver if available.
    pub(crate) async fn finalize_spawn(
        &self,
        agent_name: &AgentName,
        routing: RoutingInfo,
        identity: Option<AgentIdentityRecord>,
    ) -> Result<PathBuf> {
        let agent_config_dir = self
            .project_dir()
            .join(".exo/agents")
            .join(agent_name.as_str());
        fs::create_dir_all(&agent_config_dir).await?;
        routing.write_to_dir(&agent_config_dir).await?;

        if let Some(record) = identity {
            if let Err(e) = self.agent_resolver().register(record).await {
                warn!(agent = %agent_name, error = %e, "Failed to register agent identity (non-fatal)");
            }
        }

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

    /// Resolve effective project dir for git operations.
    /// When subrepo is set, git operations target project_dir/subrepo instead.
    /// Validates that subrepo is relative and does not escape project_dir.
    pub(crate) fn effective_project_dir(&self, subrepo: Option<&Path>) -> Result<PathBuf> {
        match subrepo {
            Some(sub_path) => {
                if sub_path.is_absolute() {
                    return Err(anyhow!(
                        "subrepo path must be relative: {}",
                        sub_path.display()
                    ));
                }
                for component in sub_path.components() {
                    if matches!(component, std::path::Component::ParentDir) {
                        return Err(anyhow!(
                            "subrepo path cannot contain '..': {}",
                            sub_path.display()
                        ));
                    }
                }
                let dir = self.project_dir().join(sub_path);
                info!(subrepo = %sub_path.display(), effective_dir = %dir.display(), "Using subrepo for git operations");
                Ok(dir)
            }
            None => Ok(self.project_dir().to_path_buf()),
        }
    }

    /// Copy allowed directories into the agent's context.
    pub(crate) async fn copy_allowed_dirs(
        &self,
        target_dir: &Path,
        allowed_dirs: &[String],
    ) -> Result<()> {
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

            let source_dir = self.project_dir().join(dir_path);

            // Canonicalize and verify the resolved path is within project_dir
            match source_dir.canonicalize() {
                Ok(canonical_source) => {
                    let canonical_project = self.project_dir().canonicalize()?;
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

/// Resolve role context file with two-tier fallback: project-local > global.
///
/// Checks `.exo/roles/{wasm_name}/context/{role}.md` in the project directory first,
/// then falls back to `~/.exo/roles/{wasm_name}/context/{role}.md`.
pub fn resolve_role_context_path(
    project_dir: &Path,
    wasm_name: &str,
    role: &str,
) -> Option<PathBuf> {
    let local = project_dir.join(format!(".exo/roles/{}/context/{}.md", wasm_name, role));
    if local.exists() {
        return Some(local);
    }
    if let Ok(home) = std::env::var("HOME") {
        let global =
            PathBuf::from(home).join(format!(".exo/roles/{}/context/{}.md", wasm_name, role));
        if global.exists() {
            return Some(global);
        }
    }
    None
}

/// Create a URL-safe slug from a title.
pub fn slugify(title: &str) -> String {
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
            "process" => Some(super::AgentType::Process),
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

    // =========================================================================
    // resolve_working_dir tests
    // =========================================================================

    #[test]
    fn test_resolve_working_dir_root_branch() {
        // Root agents (no dots) resolve to project root
        assert_eq!(resolve_working_dir("main"), PathBuf::from("."));
        assert_eq!(resolve_working_dir("develop"), PathBuf::from("."));
        assert_eq!(resolve_working_dir("my-feature"), PathBuf::from("."));
    }

    #[test]
    fn test_resolve_working_dir_one_level() {
        // Single dot: last segment is the agent name (suffixed)
        assert_eq!(
            resolve_working_dir("main.feature-a-claude"),
            PathBuf::from(".exo/worktrees/feature-a-claude/")
        );
        assert_eq!(
            resolve_working_dir("main.remove-option-mcp-gemini"),
            PathBuf::from(".exo/worktrees/remove-option-mcp-gemini/")
        );
    }

    #[test]
    fn test_resolve_working_dir_nested() {
        // Multiple dots: agent name is always the LAST segment
        assert_eq!(
            resolve_working_dir("main.tui-port-2-claude.pdv-snapshot-enums-gemini"),
            PathBuf::from(".exo/worktrees/pdv-snapshot-enums-gemini/")
        );
        assert_eq!(
            resolve_working_dir("main.auth-claude.oauth-provider-gemini"),
            PathBuf::from(".exo/worktrees/oauth-provider-gemini/")
        );
        assert_eq!(
            resolve_working_dir("main.a.b.c.d"),
            PathBuf::from(".exo/worktrees/d/")
        );
    }

    #[test]
    fn test_resolve_working_dir_agent_name_uniqueness() {
        // Two different birth branches with the same agent name resolve to the same dir.
        // This is correct: same agent name = same directory (by design, collision).
        let dir_a = resolve_working_dir("main.tl-a-claude.my-feature-gemini");
        let dir_b = resolve_working_dir("main.tl-b-claude.my-feature-gemini");
        assert_eq!(
            dir_a, dir_b,
            "Same agent name = same worktree dir (by design)"
        );
    }

    // =========================================================================
    // resolve_worktree_from_tab tests
    // =========================================================================

    #[test]
    fn test_resolve_worktree_from_tab_tl() {
        assert_eq!(resolve_worktree_from_tab("TL"), PathBuf::from("."));
    }

    #[test]
    fn test_resolve_worktree_from_tab_emoji_agent_name() {
        assert_eq!(
            resolve_worktree_from_tab("💎 feature-a-gemini"),
            PathBuf::from(".exo/worktrees/feature-a-gemini/")
        );
        assert_eq!(
            resolve_worktree_from_tab("🤖 auth-service-claude"),
            PathBuf::from(".exo/worktrees/auth-service-claude/")
        );
    }

    #[test]
    fn test_resolve_worktree_from_tab_no_space() {
        // No space separator: falls back to project root
        assert_eq!(resolve_worktree_from_tab("unknown"), PathBuf::from("."));
    }
}
