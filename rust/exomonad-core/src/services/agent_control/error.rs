use thiserror::Error;

#[derive(Debug, Error)]
pub enum SpawnError {
    #[error("branch already exists: {0}")]
    BranchExists(String),
    #[error("git worktree is locked: {0}")]
    WorktreeLocked(String),
    #[error("git lock file conflict (another git op in progress)")]
    LockConflict,
    #[error("push rejected (non-fast-forward, remote diverged)")]
    PushRejected,
    #[error("not running inside a tmux session")]
    TmuxNotInSession,
    #[error("spawn timed out after {seconds}s")]
    Timeout { seconds: u64 },
    #[error("depth limit reached (max {max}, current {current})")]
    DepthLimit { max: u32, current: u32 },
    #[error("GitHub service not available (GITHUB_TOKEN not set)")]
    GitHubUnavailable,
    #[error("failed to write hook config in worktree: {reason}")]
    HookConfigFailed { reason: String },
    #[error("$HOME not set — cannot resolve Claude project dir for --fork-session")]
    HomeDirNotSet,
    #[error("git init failed at {path}: {stderr}")]
    GitInitFailed { path: String, stderr: String },
    #[error("invalid subrepo path: {reason}")]
    InvalidSubrepoPath { reason: String },
    #[error("path traversal detected in subrepo path: {path}")]
    PathTraversal { path: String },
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

impl From<std::io::Error> for SpawnError {
    fn from(err: std::io::Error) -> Self {
        Self::Other(anyhow::Error::from(err))
    }
}

impl From<serde_json::Error> for SpawnError {
    fn from(err: serde_json::Error) -> Self {
        Self::Other(anyhow::Error::from(err))
    }
}

impl SpawnError {
    /// Stable code for FFI cross-language matching.
    pub fn code(&self) -> &'static str {
        match self {
            Self::BranchExists(_) => "worktree.branch_exists",
            Self::WorktreeLocked(_) => "worktree.locked",
            Self::LockConflict => "worktree.lock_conflict",
            Self::PushRejected => "worktree.push_rejected",
            Self::TmuxNotInSession => "tmux.not_in_session",
            Self::Timeout { .. } => "spawn.timeout",
            Self::DepthLimit { .. } => "spawn.depth_limit",
            Self::GitHubUnavailable => "github_unavailable",
            Self::HookConfigFailed { .. } => "hook_config_failed",
            Self::HomeDirNotSet => "home_dir_not_set",
            Self::GitInitFailed { .. } => "git_init_failed",
            Self::InvalidSubrepoPath { .. } => "invalid_subrepo_path",
            Self::PathTraversal { .. } => "path_traversal",
            Self::Other(_) => "spawn.other",
        }
    }
}

pub type SpawnResult2<T> = std::result::Result<T, SpawnError>;
