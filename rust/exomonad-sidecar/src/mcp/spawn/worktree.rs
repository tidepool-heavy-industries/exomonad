//! Git worktree operations for agent spawning.

use anyhow::{anyhow, Context, Result};
use std::path::{Path, PathBuf};
use tokio::process::Command;
use tracing::{debug, info, warn};

use super::types::AgentInfo;

/// Manages git worktrees for spawned agents.
pub struct WorktreeManager {
    /// Project root directory (where .git lives)
    project_dir: PathBuf,

    /// Base directory for worktrees
    worktree_dir: PathBuf,
}

impl WorktreeManager {
    /// Create a new worktree manager.
    pub fn new(project_dir: PathBuf, worktree_dir: PathBuf) -> Self {
        Self {
            project_dir,
            worktree_dir,
        }
    }

    /// Get the worktree path for an issue.
    pub fn worktree_path(&self, issue_id: &str, slug: &str) -> PathBuf {
        self.worktree_dir.join(format!("gh-{}-{}", issue_id, slug))
    }

    /// Get the branch name for an issue.
    pub fn branch_name(issue_id: &str, slug: &str) -> String {
        format!("gh-{}/{}", issue_id, slug)
    }

    /// Fetch origin/main to ensure we have latest.
    pub async fn fetch_origin(&self) -> Result<()> {
        info!("Fetching origin/main");

        let output = Command::new("git")
            .args(["fetch", "origin", "main"])
            .current_dir(&self.project_dir)
            .output()
            .await
            .context("Failed to execute git fetch")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            warn!(stderr = %stderr, "git fetch warning (continuing anyway)");
        }

        Ok(())
    }

    /// Create a new worktree for an issue.
    ///
    /// Returns the worktree path. If the worktree already exists, returns the existing path.
    pub async fn create_worktree(&self, issue_id: &str, slug: &str) -> Result<PathBuf> {
        let worktree_path = self.worktree_path(issue_id, slug);
        let branch_name = Self::branch_name(issue_id, slug);

        // Check if already exists
        if worktree_path.exists() {
            info!(
                path = %worktree_path.display(),
                "Worktree already exists, reusing"
            );
            return Ok(worktree_path);
        }

        // Ensure parent directory exists
        if let Some(parent) = worktree_path.parent() {
            tokio::fs::create_dir_all(parent)
                .await
                .context("Failed to create worktree parent directory")?;
        }

        info!(
            path = %worktree_path.display(),
            branch = %branch_name,
            "Creating worktree"
        );

        // Create worktree with new branch from origin/main
        let output = Command::new("git")
            .args([
                "worktree",
                "add",
                "-b",
                &branch_name,
                worktree_path.to_str().unwrap(),
                "origin/main",
            ])
            .current_dir(&self.project_dir)
            .output()
            .await
            .context("Failed to execute git worktree add")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);

            // Check if branch already exists
            if stderr.contains("already exists") {
                debug!("Branch already exists, creating worktree without -b");

                // Try creating worktree using existing branch
                let output = Command::new("git")
                    .args([
                        "worktree",
                        "add",
                        worktree_path.to_str().unwrap(),
                        &branch_name,
                    ])
                    .current_dir(&self.project_dir)
                    .output()
                    .await
                    .context("Failed to execute git worktree add (existing branch)")?;

                if !output.status.success() {
                    let stderr = String::from_utf8_lossy(&output.stderr);
                    return Err(anyhow!("git worktree add failed: {}", stderr));
                }
            } else {
                return Err(anyhow!("git worktree add failed: {}", stderr));
            }
        }

        Ok(worktree_path)
    }

    /// Delete a worktree.
    pub async fn delete_worktree(&self, issue_id: &str, slug: &str, force: bool) -> Result<()> {
        let worktree_path = self.worktree_path(issue_id, slug);

        if !worktree_path.exists() {
            debug!(path = %worktree_path.display(), "Worktree doesn't exist, nothing to delete");
            return Ok(());
        }

        info!(path = %worktree_path.display(), force, "Deleting worktree");

        let mut args = vec!["worktree", "remove"];
        if force {
            args.push("--force");
        }
        args.push(worktree_path.to_str().unwrap());

        let output = Command::new("git")
            .args(&args)
            .current_dir(&self.project_dir)
            .output()
            .await
            .context("Failed to execute git worktree remove")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!("git worktree remove failed: {}", stderr));
        }

        Ok(())
    }

    /// List all agent worktrees (matching gh-* pattern).
    pub async fn list_worktrees(&self) -> Result<Vec<AgentInfo>> {
        let output = Command::new("git")
            .args(["worktree", "list", "--porcelain"])
            .current_dir(&self.project_dir)
            .output()
            .await
            .context("Failed to execute git worktree list")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!("git worktree list failed: {}", stderr));
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        let mut agents = Vec::new();

        // Parse porcelain output
        // Format:
        // worktree /path/to/worktree
        // HEAD <sha>
        // branch refs/heads/<branch>
        // <blank line>

        let mut current_path: Option<PathBuf> = None;
        let mut current_branch: Option<String> = None;

        for line in stdout.lines() {
            if let Some(path) = line.strip_prefix("worktree ") {
                current_path = Some(PathBuf::from(path));
            } else if let Some(branch) = line.strip_prefix("branch refs/heads/") {
                current_branch = Some(branch.to_string());
            } else if line.is_empty() {
                // End of entry
                if let (Some(path), Some(branch)) = (current_path.take(), current_branch.take()) {
                    // Check if this is an agent worktree (gh-* pattern)
                    if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                        if name.starts_with("gh-") {
                            // Extract issue ID from name (gh-123-slug -> 123)
                            let issue_id = name
                                .strip_prefix("gh-")
                                .and_then(|s| s.split('-').next())
                                .unwrap_or("")
                                .to_string();

                            // Check for uncommitted changes
                            let has_changes = self.has_uncommitted_changes(&path).await;

                            agents.push(AgentInfo {
                                issue_id,
                                worktree_path: path.to_string_lossy().to_string(),
                                branch_name: branch,
                                has_changes,
                            });
                        }
                    }
                }
            }
        }

        Ok(agents)
    }

    /// Check if a worktree has uncommitted changes.
    async fn has_uncommitted_changes(&self, worktree_path: &Path) -> bool {
        let output = Command::new("git")
            .args(["status", "--porcelain"])
            .current_dir(worktree_path)
            .output()
            .await;

        match output {
            Ok(o) => !o.stdout.is_empty(),
            Err(_) => false,
        }
    }
}

/// Create a URL-safe slug from a title.
pub fn slugify(title: &str) -> String {
    title
        .to_lowercase()
        .chars()
        .map(|c| {
            if c.is_alphanumeric() {
                c
            } else if c.is_whitespace() || c == '-' || c == '_' {
                '-'
            } else {
                '-'
            }
        })
        .collect::<String>()
        .split('-')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join("-")
        .chars()
        .take(50) // Limit length
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slugify() {
        assert_eq!(slugify("Fix the Bug"), "fix-the-bug");
        assert_eq!(slugify("Add new feature!"), "add-new-feature");
        assert_eq!(slugify("  Multiple   Spaces  "), "multiple-spaces");
        assert_eq!(slugify("CamelCase"), "camelcase");
        assert_eq!(slugify("with-dashes-already"), "with-dashes-already");
        assert_eq!(slugify("special@#$chars"), "special-chars");
    }

    #[test]
    fn test_branch_name() {
        assert_eq!(
            WorktreeManager::branch_name("123", "fix-bug"),
            "gh-123/fix-bug"
        );
    }
}
