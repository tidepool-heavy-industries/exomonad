//! Git worktree management service.
//!
//! All operations shell out to git.

use crate::domain::BranchName;
use crate::effects::EffectError;
use std::path::{Path, PathBuf};
use thiserror::Error;
use tracing::{error, info, warn};

/// Custom error type for git worktree operations.
#[derive(Debug, Error)]
pub enum WorktreeError {
    #[error("Branch already exists: {branch}")]
    BranchExists { branch: String },
    #[error("Path already exists: {path}")]
    PathExists { path: String },
    #[error("Base branch not found: {branch}")]
    BaseBranchNotFound { branch: String },
    #[error("Git lock file conflict: {message}")]
    LockFileConflict { message: String },
    #[error("Push rejected (non-fast-forward?): {message}")]
    PushRejected { message: String },
    #[error("Git error: {message}")]
    GitError { message: String },
}

impl From<WorktreeError> for EffectError {
    fn from(err: WorktreeError) -> Self {
        match err {
            WorktreeError::BranchExists { branch } => EffectError::custom(
                "worktree.branch_exists",
                format!("Branch already exists: {}", branch),
            ),
            WorktreeError::PathExists { path } => EffectError::custom(
                "worktree.path_exists",
                format!("Path already exists: {}", path),
            ),
            WorktreeError::BaseBranchNotFound { branch } => EffectError::custom(
                "worktree.base_branch_not_found",
                format!("Base branch not found: {}", branch),
            ),
            WorktreeError::LockFileConflict { message } => {
                EffectError::custom("worktree.lock_conflict", message)
            }
            WorktreeError::PushRejected { message } => {
                EffectError::custom("worktree.push_rejected", message)
            }
            WorktreeError::GitError { message } => {
                EffectError::custom("worktree.git_error", message)
            }
        }
    }
}

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
    ) -> Result<(), WorktreeError> {
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
            .map_err(|e| WorktreeError::GitError {
                message: format!("Failed to run git worktree add: {}", e),
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            error!(stderr = %stderr, "git worktree add failed");
            return Err(self.parse_git_stderr(&stderr));
        }

        info!(path = %path.display(), branch = %branch, "Worktree created successfully");
        Ok(())
    }

    /// Remove a git worktree.
    ///
    /// Equivalent to: `git worktree remove --force {path}`
    pub fn remove_workspace(&self, path: &Path) -> Result<(), WorktreeError> {
        info!(path = %path.display(), "Removing git worktree");

        let output = std::process::Command::new("git")
            .args(["worktree", "remove", "--force", &path.to_string_lossy()])
            .current_dir(&self.project_dir)
            .output()
            .map_err(|e| WorktreeError::GitError {
                message: format!("Failed to run git worktree remove: {}", e),
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // If the worktree dir doesn't exist, git worktree remove fails — clean up manually
            if path.exists() {
                warn!(stderr = %stderr, "git worktree remove failed, removing directory manually");
                std::fs::remove_dir_all(path).map_err(|e| WorktreeError::GitError {
                    message: format!("Failed to remove worktree dir {}: {}", path.display(), e),
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
    pub fn push_bookmark(
        &self,
        workspace_path: &Path,
        branch: &BranchName,
    ) -> Result<(), WorktreeError> {
        info!(branch = %branch, path = %workspace_path.display(), "Pushing branch");

        let output = std::process::Command::new("git")
            .args(["push", "origin", branch.as_str()])
            .current_dir(workspace_path)
            .output()
            .map_err(|e| WorktreeError::GitError {
                message: format!("Failed to run git push: {}", e),
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            error!(stderr = %stderr, "git push failed");
            return Err(self.parse_git_stderr(&stderr));
        }

        info!(branch = %branch, "Branch pushed successfully");
        Ok(())
    }

    /// Fetch from remote.
    ///
    /// Equivalent to: `git fetch` (run in workspace_path)
    pub fn fetch(&self, workspace_path: &Path) -> Result<(), WorktreeError> {
        info!(path = %workspace_path.display(), "git fetch");

        let output = std::process::Command::new("git")
            .args(["fetch"])
            .current_dir(workspace_path)
            .output()
            .map_err(|e| WorktreeError::GitError {
                message: format!("Failed to run git fetch: {}", e),
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            error!(stderr = %stderr, "git fetch failed");
            return Err(self.parse_git_stderr(&stderr));
        }

        info!("git fetch succeeded");
        Ok(())
    }

    /// Get the current branch name in a workspace.
    ///
    /// Equivalent to: `git rev-parse --abbrev-ref HEAD`
    pub fn get_workspace_bookmark(
        &self,
        workspace_path: &Path,
    ) -> Result<Option<String>, WorktreeError> {
        let output = std::process::Command::new("git")
            .args(["branch", "--show-current"])
            .current_dir(workspace_path)
            .output()
            .map_err(|e| WorktreeError::GitError {
                message: format!("Failed to run git rev-parse: {}", e),
            })?;

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
    pub fn delete_bookmark(&self, name: &BranchName) -> Result<(), WorktreeError> {
        info!(branch = %name, "Deleting local branch");

        let output = std::process::Command::new("git")
            .args(["branch", "-D", name.as_str()])
            .current_dir(&self.project_dir)
            .output()
            .map_err(|e| WorktreeError::GitError {
                message: format!("Failed to run git branch -D: {}", e),
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            error!(stderr = %stderr, "git branch -D failed");
            return Err(self.parse_git_stderr(&stderr));
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
    ) -> Result<(), WorktreeError> {
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
            .map_err(|e| WorktreeError::GitError {
                message: format!("Failed to run git branch: {}", e),
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            error!(stderr = %stderr, "git branch failed");
            return Err(self.parse_git_stderr(&stderr));
        }

        info!(branch = %name, "Branch created");
        Ok(())
    }

    /// Parse git stderr into a WorktreeError.
    fn parse_git_stderr(&self, stderr: &str) -> WorktreeError {
        if stderr.contains("already exists") {
            if stderr.contains("branch named") {
                let branch = stderr.split('\'').nth(1).unwrap_or("unknown").to_string();
                WorktreeError::BranchExists { branch }
            } else {
                let path = stderr
                    .trim_start_matches("fatal: ")
                    .trim_end_matches(" already exists")
                    .to_string();
                WorktreeError::PathExists { path }
            }
        } else if stderr.contains("not a valid object")
            || stderr.contains("not a commit")
            || stderr.contains("invalid reference")
        {
            let branch = stderr.split('\'').nth(1).unwrap_or("unknown").to_string();
            WorktreeError::BaseBranchNotFound { branch }
        } else if stderr.contains(".lock") {
            WorktreeError::LockFileConflict {
                message: stderr.trim().to_string(),
            }
        } else if stderr.contains("non-fast-forward") || stderr.contains("rejected") {
            WorktreeError::PushRejected {
                message: stderr.trim().to_string(),
            }
        } else {
            WorktreeError::GitError {
                message: stderr.trim().to_string(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::Command;
    use tempfile::TempDir;

    fn init_test_repo() -> (TempDir, GitWorktreeService) {
        let temp = TempDir::new().expect("failed to create temp dir");
        let repo_dir = temp.path();

        let run = |args: &[&str]| {
            let status = Command::new("git")
                .args(args)
                .current_dir(repo_dir)
                .status()
                .expect("failed to run git command");
            assert!(status.success(), "git command failed: {:?}", args);
        };

        run(&["init"]);
        run(&["config", "user.email", "test@example.com"]);
        run(&["config", "user.name", "Test User"]);
        run(&["commit", "--allow-empty", "-m", "Initial commit"]);

        let service = GitWorktreeService::new(repo_dir.to_path_buf());
        (temp, service)
    }

    fn get_default_branch(repo_dir: &std::path::Path) -> String {
        let output = Command::new("git")
            .args(["branch", "--show-current"])
            .current_dir(repo_dir)
            .output()
            .expect("failed to get default branch");
        String::from_utf8_lossy(&output.stdout).trim().to_string()
    }

    #[test]
    fn test_create_workspace_happy_path() {
        let (temp, service) = init_test_repo();
        let default_branch = get_default_branch(temp.path());
        let worktree_path = temp.path().join("worktree-1");
        let branch = BranchName::from("test-branch");
        let base = BranchName::from(default_branch.as_str());

        service
            .create_workspace(&worktree_path, &branch, &base)
            .unwrap();

        assert!(worktree_path.exists());
        assert!(worktree_path.join(".git").exists());
    }

    #[test]
    fn test_remove_workspace_happy_path() {
        let (temp, service) = init_test_repo();
        let default_branch = get_default_branch(temp.path());
        let worktree_path = temp.path().join("worktree-1");
        let branch = BranchName::from("test-branch");
        let base = BranchName::from(default_branch.as_str());

        service
            .create_workspace(&worktree_path, &branch, &base)
            .unwrap();
        assert!(worktree_path.exists());

        service.remove_workspace(&worktree_path).unwrap();
        assert!(!worktree_path.exists());
    }

    #[test]
    fn test_create_bookmark_delete_bookmark_roundtrip() {
        let (temp, service) = init_test_repo();
        let branch = BranchName::from("test-branch");

        service.create_bookmark(temp.path(), &branch, None).unwrap();

        let output = Command::new("git")
            .args(["branch", "--list", "test-branch"])
            .current_dir(temp.path())
            .output()
            .unwrap();
        assert!(String::from_utf8_lossy(&output.stdout).contains("test-branch"));

        service.delete_bookmark(&branch).unwrap();

        let output = Command::new("git")
            .args(["branch", "--list", "test-branch"])
            .current_dir(temp.path())
            .output()
            .unwrap();
        assert!(!String::from_utf8_lossy(&output.stdout).contains("test-branch"));
    }

    #[test]
    fn test_get_workspace_bookmark() {
        let (temp, service) = init_test_repo();
        let default_branch = get_default_branch(temp.path());
        let worktree_path = temp.path().join("worktree-1");
        let branch = BranchName::from("test-branch");
        let base = BranchName::from(default_branch.as_str());

        service
            .create_workspace(&worktree_path, &branch, &base)
            .unwrap();

        let current = service.get_workspace_bookmark(&worktree_path).unwrap();
        assert_eq!(current, Some("test-branch".to_string()));
    }

    #[test]
    fn test_create_workspace_duplicate_branch() {
        let (temp, service) = init_test_repo();
        let default_branch = get_default_branch(temp.path());
        let branch = BranchName::from("test-branch");
        let base = BranchName::from(default_branch.as_str());

        service
            .create_workspace(&temp.path().join("wt1"), &branch, &base)
            .unwrap();
        let result = service.create_workspace(&temp.path().join("wt2"), &branch, &base);

        assert!(
            matches!(result, Err(WorktreeError::BranchExists { .. })),
            "Expected BranchExists, got: {:?}",
            result
        );
    }

    #[test]
    fn test_create_workspace_non_existent_base() {
        let (temp, service) = init_test_repo();
        let worktree_path = temp.path().join("worktree-1");
        let branch = BranchName::from("test-branch");
        let base = BranchName::from("nonexistent-base-xyz");

        let result = service.create_workspace(&worktree_path, &branch, &base);

        assert!(
            matches!(result, Err(WorktreeError::BaseBranchNotFound { .. })),
            "Expected BaseBranchNotFound, got: {:?}",
            result
        );
    }

    #[test]
    fn test_remove_workspace_non_existent_path() {
        let (temp, service) = init_test_repo();
        let path = temp.path().join("nonexistent-worktree-xyz");

        // Should succeed (idempotent)
        service.remove_workspace(&path).unwrap();
    }

    #[test]
    fn test_push_bookmark_without_remote() {
        let (temp, service) = init_test_repo();
        let branch = BranchName::from("test-branch");
        service.create_bookmark(temp.path(), &branch, None).unwrap();

        let result = service.push_bookmark(temp.path(), &branch);

        assert!(result.is_err());
    }

    /// End-to-end: simulates the file_pr resolution chain.
    ///
    /// Given a dot-separated birth_branch (e.g. "main.remove-option-mcp"):
    /// 1. resolve_working_dir → ".exo/worktrees/remove-option-mcp/"
    /// 2. Create a worktree there on that branch
    /// 3. get_workspace_bookmark → must return "main.remove-option-mcp"
    ///
    /// This is the exact chain file_pr uses. If step 3 returns a different
    /// branch, file_pr will find/update the wrong PR.
    #[test]
    fn test_file_pr_resolution_chain_dot_branch() {
        let (temp, service) = init_test_repo();
        let default_branch = get_default_branch(temp.path());

        // Simulate exomonad's dot-separated branch naming (suffixed agent names)
        let birth_branch = format!("{}.remove-option-mcp-gemini", default_branch);
        let branch = BranchName::from(birth_branch.as_str());
        let base = BranchName::from(default_branch.as_str());

        // Step 1: resolve_working_dir (same logic as EffectContext construction)
        let relative_dir = crate::services::agent_control::resolve_working_dir(&birth_branch);
        assert_eq!(
            relative_dir,
            std::path::PathBuf::from(".exo/worktrees/remove-option-mcp-gemini/")
        );

        // Step 2: create worktree at the resolved path (relative to project root)
        let worktree_path = temp.path().join(&relative_dir);
        std::fs::create_dir_all(worktree_path.parent().unwrap()).unwrap();
        service
            .create_workspace(&worktree_path, &branch, &base)
            .unwrap();

        // Step 3: get_workspace_bookmark must return the dot-separated branch
        let resolved = service.get_workspace_bookmark(&worktree_path).unwrap();
        assert_eq!(
            resolved,
            Some(birth_branch.clone()),
            "get_workspace_bookmark must return the exact birth_branch"
        );
    }

    /// Same chain but with a deeply nested branch (3 levels).
    #[test]
    fn test_file_pr_resolution_chain_deep_nesting() {
        let (temp, service) = init_test_repo();
        let default_branch = get_default_branch(temp.path());

        let birth_branch = format!(
            "{}.tui-port-2-claude.pdv-snapshot-enums-gemini",
            default_branch
        );
        let branch = BranchName::from(birth_branch.as_str());
        let base = BranchName::from(default_branch.as_str());

        let relative_dir = crate::services::agent_control::resolve_working_dir(&birth_branch);
        assert_eq!(
            relative_dir,
            std::path::PathBuf::from(".exo/worktrees/pdv-snapshot-enums-gemini/")
        );

        let worktree_path = temp.path().join(&relative_dir);
        std::fs::create_dir_all(worktree_path.parent().unwrap()).unwrap();
        service
            .create_workspace(&worktree_path, &branch, &base)
            .unwrap();

        let resolved = service.get_workspace_bookmark(&worktree_path).unwrap();
        assert_eq!(resolved, Some(birth_branch));
    }

    /// Branch verification: after create_workspace, get_workspace_bookmark returns exact branch name.
    #[test]
    fn test_create_workspace_branch_verification() {
        let (temp, service) = init_test_repo();
        let default_branch = get_default_branch(temp.path());
        let worktree_path = temp.path().join("wt-verify");
        let branch = BranchName::from("test-verify-branch");
        let base = BranchName::from(default_branch.as_str());

        service
            .create_workspace(&worktree_path, &branch, &base)
            .unwrap();

        let actual = service.get_workspace_bookmark(&worktree_path).unwrap();
        assert_eq!(actual, Some("test-verify-branch".to_string()));
    }

    /// Branch verification with dotted branch name (ExoMonad convention).
    #[test]
    fn test_create_workspace_branch_verification_dotted() {
        let (temp, service) = init_test_repo();
        let default_branch = get_default_branch(temp.path());
        let worktree_path = temp.path().join("wt-dotted");
        let branch_name = format!("{}.feat-a-gemini", default_branch);
        let branch = BranchName::from(branch_name.as_str());
        let base = BranchName::from(default_branch.as_str());

        service
            .create_workspace(&worktree_path, &branch, &base)
            .unwrap();

        let actual = service.get_workspace_bookmark(&worktree_path).unwrap();
        assert_eq!(actual, Some(branch_name));
    }

    /// Branch verification with deeply dotted branch name.
    #[test]
    fn test_create_workspace_branch_verification_deep() {
        let (temp, service) = init_test_repo();
        let default_branch = get_default_branch(temp.path());
        let worktree_path = temp.path().join("wt-deep");
        let branch_name = format!("{}.tl.sub.leaf-gemini", default_branch);
        let branch = BranchName::from(branch_name.as_str());
        let base = BranchName::from(default_branch.as_str());

        service
            .create_workspace(&worktree_path, &branch, &base)
            .unwrap();

        let actual = service.get_workspace_bookmark(&worktree_path).unwrap();
        assert_eq!(actual, Some(branch_name));
    }

    /// Resolution chain with agent-suffixed branch.
    #[test]
    fn test_file_pr_resolution_chain_agent_suffix() {
        let (temp, service) = init_test_repo();
        let default_branch = get_default_branch(temp.path());

        let birth_branch = format!("{}.fix-auth-gemini", default_branch);
        let branch = BranchName::from(birth_branch.as_str());
        let base = BranchName::from(default_branch.as_str());

        let relative_dir = crate::services::agent_control::resolve_working_dir(&birth_branch);
        assert_eq!(
            relative_dir,
            std::path::PathBuf::from(".exo/worktrees/fix-auth-gemini/")
        );

        let worktree_path = temp.path().join(&relative_dir);
        std::fs::create_dir_all(worktree_path.parent().unwrap()).unwrap();
        service
            .create_workspace(&worktree_path, &branch, &base)
            .unwrap();

        let resolved = service.get_workspace_bookmark(&worktree_path).unwrap();
        assert_eq!(resolved, Some(birth_branch));
    }

    /// Resolution chain with claude-suffixed branch.
    #[test]
    fn test_file_pr_resolution_chain_claude_suffix() {
        let (temp, service) = init_test_repo();
        let default_branch = get_default_branch(temp.path());

        let birth_branch = format!("{}.tl-auth-claude", default_branch);
        let branch = BranchName::from(birth_branch.as_str());
        let base = BranchName::from(default_branch.as_str());

        let relative_dir = crate::services::agent_control::resolve_working_dir(&birth_branch);
        let worktree_path = temp.path().join(&relative_dir);
        std::fs::create_dir_all(worktree_path.parent().unwrap()).unwrap();
        service
            .create_workspace(&worktree_path, &branch, &base)
            .unwrap();

        let resolved = service.get_workspace_bookmark(&worktree_path).unwrap();
        assert_eq!(resolved, Some(birth_branch));
    }

    /// Resolution chain with 4 levels deep.
    #[test]
    fn test_file_pr_resolution_chain_deep_4_levels() {
        let (temp, service) = init_test_repo();
        let default_branch = get_default_branch(temp.path());

        let birth_branch = format!("{}.tl.sub.leaf.worker-gemini", default_branch);
        let branch = BranchName::from(birth_branch.as_str());
        let base = BranchName::from(default_branch.as_str());

        let relative_dir = crate::services::agent_control::resolve_working_dir(&birth_branch);
        assert_eq!(
            relative_dir,
            std::path::PathBuf::from(".exo/worktrees/worker-gemini/")
        );

        let worktree_path = temp.path().join(&relative_dir);
        std::fs::create_dir_all(worktree_path.parent().unwrap()).unwrap();
        service
            .create_workspace(&worktree_path, &branch, &base)
            .unwrap();

        let resolved = service.get_workspace_bookmark(&worktree_path).unwrap();
        assert_eq!(resolved, Some(birth_branch));
    }

    /// Sibling collision: same slug from different parents → same worktree dir (known limitation).
    #[test]
    fn test_resolve_working_dir_sibling_collision() {
        let dir_a =
            crate::services::agent_control::resolve_working_dir("main.tl-a.my-feature-gemini");
        let dir_b =
            crate::services::agent_control::resolve_working_dir("main.tl-b.my-feature-gemini");
        assert_eq!(dir_a, dir_b, "Same slug = same dir (known limitation)");
    }

    /// resolve_working_dir for agent-suffixed branches.
    #[test]
    fn test_resolve_working_dir_agent_suffixed() {
        assert_eq!(
            crate::services::agent_control::resolve_working_dir("main.fix-auth-gemini"),
            std::path::PathBuf::from(".exo/worktrees/fix-auth-gemini/")
        );
    }

    /// resolve_working_dir for root branches.
    #[test]
    fn test_resolve_working_dir_root() {
        assert_eq!(
            crate::services::agent_control::resolve_working_dir("main"),
            std::path::PathBuf::from(".")
        );
    }

    /// Verify that two sibling agents with different birth branches resolve
    /// to different worktrees and get_workspace_bookmark returns the correct
    /// branch for each.
    #[test]
    fn test_file_pr_resolution_chain_sibling_isolation() {
        let (temp, service) = init_test_repo();
        let default_branch = get_default_branch(temp.path());
        let base = BranchName::from(default_branch.as_str());

        let branch_a = format!("{}.feature-a-claude", default_branch);
        let branch_b = format!("{}.feature-b-claude", default_branch);

        let dir_a = temp
            .path()
            .join(crate::services::agent_control::resolve_working_dir(
                &branch_a,
            ));
        let dir_b = temp
            .path()
            .join(crate::services::agent_control::resolve_working_dir(
                &branch_b,
            ));

        std::fs::create_dir_all(dir_a.parent().unwrap()).unwrap();
        service
            .create_workspace(&dir_a, &BranchName::from(branch_a.as_str()), &base)
            .unwrap();
        service
            .create_workspace(&dir_b, &BranchName::from(branch_b.as_str()), &base)
            .unwrap();

        let resolved_a = service.get_workspace_bookmark(&dir_a).unwrap();
        let resolved_b = service.get_workspace_bookmark(&dir_b).unwrap();

        assert_eq!(resolved_a, Some(branch_a));
        assert_eq!(resolved_b, Some(branch_b));
        assert_ne!(resolved_a, resolved_b);
    }
}
