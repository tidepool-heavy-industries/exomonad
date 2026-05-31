//! `impl Git for Runtime` — local git operations.
//!
//! **Leaf R1.** Adapt exomonad-core `GitService` (`services/git.rs`). Use
//! `tokio::process::Command` (or `spawn_blocking` around the existing sync executor) —
//! NEVER block the tokio executor inside an `async fn`.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{Branch, Git, GitError};
use std::path::Path;
use tokio::process::Command;

#[async_trait]
impl Git for Runtime {
    async fn current_branch(&self) -> Result<Branch, GitError> {
        let output = self.git(&["rev-parse", "--abbrev-ref", "HEAD"]).await?;
        let s = String::from_utf8_lossy(&output.stdout).trim().to_string();
        Branch::new(s).map_err(|e| GitError::Failed {
            op: "current_branch",
            detail: e.to_string(),
        })
    }

    async fn is_clean(&self) -> Result<bool, GitError> {
        let output = self.git(&["status", "--porcelain"]).await?;
        Ok(String::from_utf8_lossy(&output.stdout).trim().is_empty())
    }

    async fn worktree_add(&self, branch: &Branch, at: &Path) -> Result<(), GitError> {
        self.git(&["worktree", "add", "-b", branch.as_str(), &at.to_string_lossy()])
            .await?;
        Ok(())
    }

    async fn worktree_remove(&self, at: &Path) -> Result<(), GitError> {
        self.git(&["worktree", "remove", &at.to_string_lossy()])
            .await?;
        Ok(())
    }
}

impl Runtime {
    async fn git(&self, args: &[&str]) -> Result<std::process::Output, GitError> {
        let output = Command::new("git")
            .current_dir(self.working_dir())
            .args(args)
            .output()
            .await?;

        if !output.status.success() {
            return Err(GitError::Failed {
                op: "git",
                detail: String::from_utf8_lossy(&output.stderr).trim().to_string(),
            });
        }

        Ok(output)
    }
}
