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
    async fn is_clean(&self) -> Result<bool, GitError>;
    async fn worktree_add(&self, branch: &Branch, at: &Path) -> Result<(), GitError>;
    async fn worktree_remove(&self, at: &Path) -> Result<(), GitError>;
}
