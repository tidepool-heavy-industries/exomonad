//! `Git` capability — local git operations. Signatures firm up in Wave 1 (adapt
//! exomonad-core `GitService`).

use crate::types::Branch;
use async_trait::async_trait;
use std::path::Path;
use thiserror::Error;

/// Git failures, source-preserving (`#[from]` into [`CapError`](crate::CapError)).
#[derive(Debug, Error)]
pub enum GitError {
    #[error("git {op} failed: {detail}")]
    Failed { op: &'static str, detail: String },
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

#[async_trait]
pub trait Git {
    async fn current_branch(&self) -> Result<Branch, GitError>;
    /// The current `HEAD` commit sha (full 40-char). Used to sha-tag review verdicts so the
    /// sidecar only escalates an approval that still matches the committed state.
    async fn head_sha(&self) -> Result<String, GitError>;
    async fn is_clean(&self) -> Result<bool, GitError>;
    async fn fetch(&self) -> Result<(), GitError>;
    /// Merge `branch` into the current branch (the local fold; no remote). Non-interactive.
    async fn merge(&self, branch: &Branch) -> Result<(), GitError>;
    async fn worktree_add(&self, branch: &Branch, at: &Path) -> Result<(), GitError>;
    async fn worktree_remove(&self, at: &Path) -> Result<(), GitError>;
}
