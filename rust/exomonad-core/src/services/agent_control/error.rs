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
            Self::Other(_) => "spawn.other",
        }
    }
}

pub type SpawnResult2<T> = std::result::Result<T, SpawnError>;
