//! Git worktree management for session isolation.
//!
//! Each session gets its own git worktree, providing:
//! - Branch isolation (changes don't affect main working tree)
//! - Parallel execution (multiple sessions can run concurrently)
//! - Clean separation (session can be forked/resumed independently)
//!
//! ## Directory Layout
//!
//! ```text
//! .mantle/
//!   worktrees/
//!     implement-user-auth-3f2a9c/    # Worktree for session
//!       .git                         # Worktree git link
//!       src/                         # Working files
//!       ...
//! ```

use std::path::{Path, PathBuf};
use std::process::Command;

/// Error types for worktree operations.
#[derive(Debug, thiserror::Error)]
pub enum WorktreeError {
    #[error("Failed to create worktrees directory: {0}")]
    CreateDir(#[source] std::io::Error),

    #[error("Failed to execute git command: {0}")]
    GitExec(#[source] std::io::Error),

    #[error("Git command failed: {0}")]
    GitFailed(String),

    #[error("Failed to remove worktree directory: {0}")]
    RemoveDir(#[source] std::io::Error),

    #[error("Worktree already exists: {0}")]
    AlreadyExists(PathBuf),

    #[error("Worktree not found: {0}")]
    NotFound(PathBuf),

    #[error("Invalid branch name: {0}")]
    InvalidBranch(String),
}

pub type Result<T> = std::result::Result<T, WorktreeError>;

/// Manages git worktrees for mantle sessions.
pub struct WorktreeManager {
    /// Repository root (where .git is)
    repo_root: PathBuf,
    /// Directory for worktrees (.mantle/worktrees)
    worktrees_dir: PathBuf,
}

impl WorktreeManager {
    /// Create a new worktree manager.
    ///
    /// # Arguments
    /// * `repo_root` - Path to the git repository root
    /// * `worktrees_dir` - Path to the worktrees directory (.mantle/worktrees)
    pub fn new(repo_root: &Path, worktrees_dir: &Path) -> Result<Self> {
        // Ensure worktrees directory exists
        if !worktrees_dir.exists() {
            std::fs::create_dir_all(worktrees_dir).map_err(WorktreeError::CreateDir)?;
        }

        Ok(Self {
            repo_root: repo_root.to_path_buf(),
            worktrees_dir: worktrees_dir.to_path_buf(),
        })
    }

    /// Create a new worktree for a session.
    ///
    /// Creates a new branch and worktree in one operation.
    ///
    /// # Arguments
    /// * `branch` - Branch name (e.g., "implement/user-auth-3f2a9c")
    /// * `base_branch` - Optional base branch (defaults to HEAD)
    ///
    /// # Returns
    /// Path to the created worktree
    pub fn create(&self, branch: &str, base_branch: Option<&str>) -> Result<PathBuf> {
        self.validate_branch_name(branch)?;

        // Convert branch name to directory name (replace / with -)
        let dir_name = branch.replace('/', "-");
        let worktree_path = self.worktrees_dir.join(&dir_name);

        if worktree_path.exists() {
            return Err(WorktreeError::AlreadyExists(worktree_path));
        }

        // Build git worktree add command
        // git worktree add -b <branch> <path> [<base>]
        let mut cmd = Command::new("git");
        cmd.arg("worktree")
            .arg("add")
            .arg("-b")
            .arg(branch)
            .arg(&worktree_path)
            .current_dir(&self.repo_root);

        if let Some(base) = base_branch {
            cmd.arg(base);
        }

        let output = cmd.output().map_err(WorktreeError::GitExec)?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(WorktreeError::GitFailed(stderr.to_string()));
        }

        Ok(worktree_path)
    }

    /// Create a worktree by checking out an existing branch.
    ///
    /// Use this when the branch already exists (e.g., resuming a session).
    ///
    /// # Arguments
    /// * `branch` - Existing branch name
    ///
    /// # Returns
    /// Path to the created worktree
    pub fn checkout(&self, branch: &str) -> Result<PathBuf> {
        self.validate_branch_name(branch)?;

        let dir_name = branch.replace('/', "-");
        let worktree_path = self.worktrees_dir.join(&dir_name);

        if worktree_path.exists() {
            return Err(WorktreeError::AlreadyExists(worktree_path));
        }

        // git worktree add <path> <branch>
        let output = Command::new("git")
            .arg("worktree")
            .arg("add")
            .arg(&worktree_path)
            .arg(branch)
            .current_dir(&self.repo_root)
            .output()
            .map_err(WorktreeError::GitExec)?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(WorktreeError::GitFailed(stderr.to_string()));
        }

        Ok(worktree_path)
    }

    /// Remove a worktree.
    ///
    /// This removes the worktree from git and deletes the directory.
    ///
    /// # Arguments
    /// * `worktree_path` - Path to the worktree to remove
    /// * `force` - Force removal even if there are uncommitted changes
    pub fn remove(&self, worktree_path: &Path, force: bool) -> Result<()> {
        if !worktree_path.exists() {
            return Err(WorktreeError::NotFound(worktree_path.to_path_buf()));
        }

        // git worktree remove <path> [--force]
        let mut cmd = Command::new("git");
        cmd.arg("worktree")
            .arg("remove")
            .arg(worktree_path)
            .current_dir(&self.repo_root);

        if force {
            cmd.arg("--force");
        }

        let output = cmd.output().map_err(WorktreeError::GitExec)?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(WorktreeError::GitFailed(stderr.to_string()));
        }

        Ok(())
    }

    /// Delete a branch.
    ///
    /// Use this after removing the worktree if the branch is no longer needed.
    ///
    /// # Arguments
    /// * `branch` - Branch name to delete
    /// * `force` - Force delete even if not fully merged
    pub fn delete_branch(&self, branch: &str, force: bool) -> Result<()> {
        let flag = if force { "-D" } else { "-d" };

        let output = Command::new("git")
            .arg("branch")
            .arg(flag)
            .arg(branch)
            .current_dir(&self.repo_root)
            .output()
            .map_err(WorktreeError::GitExec)?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(WorktreeError::GitFailed(stderr.to_string()));
        }

        Ok(())
    }

    /// List all worktrees managed by mantle.
    ///
    /// Returns paths to all worktrees in the .mantle/worktrees directory.
    pub fn list(&self) -> Result<Vec<PathBuf>> {
        if !self.worktrees_dir.exists() {
            return Ok(Vec::new());
        }

        let mut worktrees = Vec::new();

        for entry in std::fs::read_dir(&self.worktrees_dir).map_err(WorktreeError::CreateDir)? {
            let entry = entry.map_err(WorktreeError::CreateDir)?;
            let path = entry.path();
            if path.is_dir() {
                worktrees.push(path);
            }
        }

        Ok(worktrees)
    }

    /// Check if a branch exists.
    pub fn branch_exists(&self, branch: &str) -> Result<bool> {
        let output = Command::new("git")
            .arg("show-ref")
            .arg("--verify")
            .arg("--quiet")
            .arg(format!("refs/heads/{}", branch))
            .current_dir(&self.repo_root)
            .output()
            .map_err(WorktreeError::GitExec)?;

        Ok(output.status.success())
    }

    /// Get the current HEAD commit hash.
    pub fn head_commit(&self) -> Result<String> {
        let output = Command::new("git")
            .arg("rev-parse")
            .arg("HEAD")
            .current_dir(&self.repo_root)
            .output()
            .map_err(WorktreeError::GitExec)?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(WorktreeError::GitFailed(stderr.to_string()));
        }

        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    }

    /// Prune stale worktree entries.
    ///
    /// Removes worktree entries for which the working directory no longer exists.
    pub fn prune(&self) -> Result<()> {
        let output = Command::new("git")
            .arg("worktree")
            .arg("prune")
            .current_dir(&self.repo_root)
            .output()
            .map_err(WorktreeError::GitExec)?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(WorktreeError::GitFailed(stderr.to_string()));
        }

        Ok(())
    }

    /// Validate a branch name.
    fn validate_branch_name(&self, branch: &str) -> Result<()> {
        // Basic validation - git has complex rules, but we catch obvious issues
        if branch.is_empty() {
            return Err(WorktreeError::InvalidBranch(
                "Branch name cannot be empty".to_string(),
            ));
        }

        if branch.starts_with('-') {
            return Err(WorktreeError::InvalidBranch(
                "Branch name cannot start with '-'".to_string(),
            ));
        }

        if branch.contains("..") {
            return Err(WorktreeError::InvalidBranch(
                "Branch name cannot contain '..'".to_string(),
            ));
        }

        if branch.ends_with('/') || branch.starts_with('/') {
            return Err(WorktreeError::InvalidBranch(
                "Branch name cannot start or end with '/'".to_string(),
            ));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn setup_git_repo() -> TempDir {
        let temp_dir = TempDir::new().unwrap();

        // Initialize git repo
        Command::new("git")
            .args(["init"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        // Configure git user for commits
        Command::new("git")
            .args(["config", "user.email", "test@test.com"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        Command::new("git")
            .args(["config", "user.name", "Test"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        // Create initial commit
        std::fs::write(temp_dir.path().join("README.md"), "# Test").unwrap();
        Command::new("git")
            .args(["add", "."])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();
        Command::new("git")
            .args(["commit", "-m", "Initial commit"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        temp_dir
    }

    #[test]
    fn test_create_worktree() {
        let temp_dir = setup_git_repo();
        let worktrees_dir = temp_dir.path().join(".mantle/worktrees");

        let manager = WorktreeManager::new(temp_dir.path(), &worktrees_dir).unwrap();
        let path = manager.create("test-branch-abc123", None).unwrap();

        assert!(path.exists());
        assert!(path.join(".git").exists());
        assert!(manager.branch_exists("test-branch-abc123").unwrap());
    }

    #[test]
    fn test_create_worktree_with_slash() {
        let temp_dir = setup_git_repo();
        let worktrees_dir = temp_dir.path().join(".mantle/worktrees");

        let manager = WorktreeManager::new(temp_dir.path(), &worktrees_dir).unwrap();
        let path = manager.create("implement/user-auth-abc123", None).unwrap();

        // Directory name should have slashes replaced with dashes
        assert!(path.ends_with("implement-user-auth-abc123"));
        assert!(path.exists());
        assert!(manager.branch_exists("implement/user-auth-abc123").unwrap());
    }

    #[test]
    fn test_remove_worktree() {
        let temp_dir = setup_git_repo();
        let worktrees_dir = temp_dir.path().join(".mantle/worktrees");

        let manager = WorktreeManager::new(temp_dir.path(), &worktrees_dir).unwrap();
        let path = manager.create("test-remove-abc123", None).unwrap();

        assert!(path.exists());

        manager.remove(&path, false).unwrap();
        assert!(!path.exists());
    }

    #[test]
    fn test_list_worktrees() {
        let temp_dir = setup_git_repo();
        let worktrees_dir = temp_dir.path().join(".mantle/worktrees");

        let manager = WorktreeManager::new(temp_dir.path(), &worktrees_dir).unwrap();

        manager.create("test-list-1", None).unwrap();
        manager.create("test-list-2", None).unwrap();

        let worktrees = manager.list().unwrap();
        assert_eq!(worktrees.len(), 2);
    }

    #[test]
    fn test_duplicate_worktree_fails() {
        let temp_dir = setup_git_repo();
        let worktrees_dir = temp_dir.path().join(".mantle/worktrees");

        let manager = WorktreeManager::new(temp_dir.path(), &worktrees_dir).unwrap();
        manager.create("test-dup-abc123", None).unwrap();

        let result = manager.create("test-dup-abc123", None);
        assert!(matches!(result, Err(WorktreeError::AlreadyExists(_))));
    }

    #[test]
    fn test_invalid_branch_names() {
        let temp_dir = setup_git_repo();
        let worktrees_dir = temp_dir.path().join(".mantle/worktrees");

        let manager = WorktreeManager::new(temp_dir.path(), &worktrees_dir).unwrap();

        assert!(manager.create("", None).is_err());
        assert!(manager.create("-starts-with-dash", None).is_err());
        assert!(manager.create("has..double-dots", None).is_err());
        assert!(manager.create("/starts-with-slash", None).is_err());
        assert!(manager.create("ends-with-slash/", None).is_err());
    }

    #[test]
    fn test_head_commit() {
        let temp_dir = setup_git_repo();
        let worktrees_dir = temp_dir.path().join(".mantle/worktrees");

        let manager = WorktreeManager::new(temp_dir.path(), &worktrees_dir).unwrap();
        let commit = manager.head_commit().unwrap();

        // Should be a 40-char hex string
        assert_eq!(commit.len(), 40);
        assert!(commit.chars().all(|c| c.is_ascii_hexdigit()));
    }
}
