//! Git worktree management via plain git CLI.
//!
//! Replaces `JjWorkspaceService` (jj-lib). All operations shell out to git.

use crate::domain::BranchName;
use anyhow::{Context, Result};
use std::path::{Path, PathBuf};
use tracing::{error, info, warn};

/// Service for git worktree operations via git CLI.
pub struct GitWorktreeService {
    project_dir: PathBuf,
}

impl GitWorktreeService {
    pub fn new(project_dir: PathBuf) -> Self {
        Self { project_dir }
    }

    /// Create a new git worktree with a new branch based on a given base.
    ///
    /// Equivalent to: `git worktree add -b {branch} {path} {base}`
    pub fn create_workspace(
        &self,
        path: &Path,
        branch: &BranchName,
        base: &BranchName,
    ) -> Result<()> {
        info!(path = %path.display(), branch = %branch, base = %base, "Creating git worktree");

        let output = std::process::Command::new("git")
            .args([
                "worktree",
                "add",
                "-b",
                branch.as_str(),
                &path.to_string_lossy(),
                base.as_str(),
            ])
            .current_dir(&self.project_dir)
            .output()
            .context("Failed to run git worktree add")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            error!(stderr = %stderr, "git worktree add failed");
            return Err(anyhow::anyhow!("git worktree add failed: {}", stderr));
        }

        info!(path = %path.display(), branch = %branch, "Worktree created successfully");
        Ok(())
    }

    /// Remove a git worktree.
    ///
    /// Equivalent to: `git worktree remove --force {path}`
    pub fn remove_workspace(&self, path: &Path) -> Result<()> {
        info!(path = %path.display(), "Removing git worktree");

        let output = std::process::Command::new("git")
            .args(["worktree", "remove", "--force", &path.to_string_lossy()])
            .current_dir(&self.project_dir)
            .output()
            .context("Failed to run git worktree remove")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // If the worktree dir doesn't exist, git worktree remove fails — clean up manually
            if path.exists() {
                warn!(stderr = %stderr, "git worktree remove failed, removing directory manually");
                std::fs::remove_dir_all(path).with_context(|| {
                    format!("Failed to remove worktree dir: {}", path.display())
                })?;
            } else {
                warn!(stderr = %stderr, "git worktree remove failed (directory already gone)");
            }
            // Also prune stale worktree entries
            let _ = std::process::Command::new("git")
                .args(["worktree", "prune"])
                .current_dir(&self.project_dir)
                .output();
        }

        info!(path = %path.display(), "Worktree removed");
        Ok(())
    }

    /// Push a branch to the remote.
    ///
    /// Equivalent to: `git push origin {branch}` (run in workspace_path)
    pub fn push_bookmark(&self, workspace_path: &Path, branch: &BranchName) -> Result<()> {
        info!(branch = %branch, path = %workspace_path.display(), "Pushing branch");

        let output = std::process::Command::new("git")
            .args(["push", "origin", branch.as_str()])
            .current_dir(workspace_path)
            .output()
            .context("Failed to run git push")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            error!(stderr = %stderr, "git push failed");
            return Err(anyhow::anyhow!("git push failed: {}", stderr));
        }

        info!(branch = %branch, "Branch pushed successfully");
        Ok(())
    }

    /// Fetch from remote.
    ///
    /// Equivalent to: `git fetch` (run in workspace_path)
    pub fn fetch(&self, workspace_path: &Path) -> Result<()> {
        info!(path = %workspace_path.display(), "git fetch");

        let output = std::process::Command::new("git")
            .args(["fetch"])
            .current_dir(workspace_path)
            .output()
            .context("Failed to run git fetch")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            error!(stderr = %stderr, "git fetch failed");
            return Err(anyhow::anyhow!("git fetch failed: {}", stderr));
        }

        info!("git fetch succeeded");
        Ok(())
    }

    /// Get the current branch name in a workspace.
    ///
    /// Equivalent to: `git rev-parse --abbrev-ref HEAD`
    pub fn get_workspace_bookmark(&self, workspace_path: &Path) -> Result<Option<String>> {
        let output = std::process::Command::new("git")
            .args(["rev-parse", "--abbrev-ref", "HEAD"])
            .current_dir(workspace_path)
            .output()
            .context("Failed to run git rev-parse")?;

        if output.status.success() {
            let branch = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if branch != "HEAD" && !branch.is_empty() {
                return Ok(Some(branch));
            }
        }
        Ok(None)
    }

    /// Delete a local branch.
    ///
    /// Equivalent to: `git branch -D {name}` (from project_dir)
    pub fn delete_bookmark(&self, name: &BranchName) -> Result<()> {
        info!(branch = %name, "Deleting local branch");

        let output = std::process::Command::new("git")
            .args(["branch", "-D", name.as_str()])
            .current_dir(&self.project_dir)
            .output()
            .context("Failed to run git branch -D")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            error!(stderr = %stderr, "git branch -D failed");
            return Err(anyhow::anyhow!("git branch -D failed: {}", stderr));
        }

        info!(branch = %name, "Branch deleted");
        Ok(())
    }

    /// Create a local branch, optionally at a specific revision.
    ///
    /// Equivalent to: `git branch {name} [revision]`
    pub fn create_bookmark(
        &self,
        workspace_path: &Path,
        name: &BranchName,
        revision: Option<&crate::domain::Revision>,
    ) -> Result<()> {
        info!(branch = %name, revision = ?revision, path = %workspace_path.display(), "Creating local branch");

        let mut args = vec!["branch", name.as_str()];
        let rev_str;
        if let Some(rev) = revision {
            rev_str = rev.as_str().to_string();
            args.push(&rev_str);
        }

        let output = std::process::Command::new("git")
            .args(&args)
            .current_dir(workspace_path)
            .output()
            .context("Failed to run git branch")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            error!(stderr = %stderr, "git branch failed");
            return Err(anyhow::anyhow!("git branch failed: {}", stderr));
        }

        info!(branch = %name, "Branch created");
        Ok(())
    }
}
