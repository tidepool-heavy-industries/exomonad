//! `Git` capability — local git operations.

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

/// One commit and the files it touched — what [`Git::commits_between`] yields per commit.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CommitFiles {
    /// Full 40-char sha.
    pub sha: String,
    /// Paths changed by this commit, repo-relative.
    pub files: Vec<String>,
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
    /// The raw `git status --porcelain` lines — one per entry, empty when the tree is clean.
    /// [`is_clean`](Git::is_clean) answers *whether*; this answers *what*, so a gate that refuses
    /// a dirty tree can NAME the offending files instead of making the agent go re-run git itself.
    async fn status_porcelain(&self) -> Result<Vec<String>, GitError>;
    /// The commits in `base..head` (reachable from `head`, not from `base`), **newest first**, each
    /// with the files it changed (`git log --format=%H --name-only base..head`). `Err` when `base`
    /// does not resolve — unlike the fail-open ancestry predicates, a caller enumerating a branch's
    /// commits needs to know it got an empty answer because there are none, not because the base
    /// was garbage.
    async fn commits_between(&self, base: &str, head: &str) -> Result<Vec<CommitFiles>, GitError>;
    /// True if HEAD has commits that `base` does not. `Ok(false)` when the base ref does not
    /// resolve or any git error occurs (fail-open — never block a stop on a bad base).
    async fn is_ahead_of(&self, base: &str) -> Result<bool, GitError>;
    /// True if `base` has commits that HEAD does not — i.e. `base` advanced past this branch's
    /// fork point, so the branch should be rebased onto it before it's submitted for merge.
    /// `Ok(false)` when the base ref does not resolve or any git error occurs (fail-open — a
    /// parent branch that isn't a live ref, e.g. the root's `root`, must never block a submit).
    async fn is_behind(&self, base: &str) -> Result<bool, GitError>;
    async fn fetch(&self) -> Result<(), GitError>;
    /// Merge `branch` into the current branch (the local fold; no remote). Non-interactive.
    async fn merge(&self, branch: &Branch) -> Result<(), GitError>;
    async fn worktree_add(&self, branch: &Branch, at: &Path) -> Result<(), GitError>;
    /// Remove the worktree at `at` — **force/reclaim semantics**: uncommitted state in the
    /// worktree *directory* (dirty files, untracked artifacts) is discarded, but the branch
    /// ref is untouched, so committed work survives. Both callers (birth rollback, post-merge
    /// reclaim) want exactly that.
    async fn worktree_remove(&self, at: &Path) -> Result<(), GitError>;
}
