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
    /// The merge-base (common ancestor) sha of `HEAD` and `refish` — i.e. this branch's fork
    /// point off `refish`. `Ok(None)` when `refish` doesn't resolve or shares no history (so a
    /// caller can fall back to another base). Used to give a reviewer a real `git diff` base
    /// instead of a branch *name* that may not be a live ref.
    async fn merge_base(&self, refish: &str) -> Result<Option<String>, GitError>;
    /// The closest fork point of HEAD: the most-recent merge-base of HEAD against any other
    /// local branch, excluding HEAD itself (so the branch's own descendant branches — e.g.
    /// reviewer children forked from HEAD — don't collapse the diff to empty). `None` if no
    /// other branch shares history with HEAD. Name-agnostic: needs no parent-branch name.
    async fn fork_point(&self) -> Result<Option<String>, GitError>;
    async fn is_clean(&self) -> Result<bool, GitError>;
    async fn fetch(&self) -> Result<(), GitError>;
    /// Merge `branch` into the current branch (the local fold; no remote). Non-interactive.
    async fn merge(&self, branch: &Branch) -> Result<(), GitError>;
    async fn worktree_add(&self, branch: &Branch, at: &Path) -> Result<(), GitError>;
    async fn worktree_remove(&self, at: &Path) -> Result<(), GitError>;
}
