//! `impl Git for Runtime` — local git operations.
//!
//! **Leaf R1.** Adapt exomonad-core `GitService` (`services/git.rs`). Use
//! `tokio::process::Command` (or `spawn_blocking` around the existing sync executor) —
//! NEVER block the tokio executor inside an `async fn`.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{Branch, Git, GitError};
use std::path::Path;

#[async_trait]
impl Git for Runtime {
    async fn current_branch(&self) -> Result<Branch, GitError> {
        todo!("R1: git rev-parse --abbrev-ref HEAD in self.working_dir; parse to Branch")
    }

    async fn is_clean(&self) -> Result<bool, GitError> {
        todo!("R1: git status --porcelain; empty output => clean")
    }

    async fn worktree_add(&self, _branch: &Branch, _at: &Path) -> Result<(), GitError> {
        todo!("R1: git worktree add -b <branch> <at>")
    }

    async fn worktree_remove(&self, _at: &Path) -> Result<(), GitError> {
        todo!("R1: git worktree remove <at>")
    }
}
