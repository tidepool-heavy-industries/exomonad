use crate::services::docker::CommandExecutor;
use crate::services::repo;
use crate::{GithubOwner, GithubRepo};
use anyhow::{Context, Result};
use duct::cmd;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Get current branch name from local git repository.
///
/// This is a standalone helper function that calls git directly without
/// requiring Docker or the GitService. Used by file_pr and copilot_review
/// services to extract agent IDs from branch names.
pub fn get_current_branch() -> Result<String> {
    let branch = cmd!("git", "branch", "--show-current")
        .read()
        .context("Failed to execute git branch --show-current")?;

    let branch = branch.trim();
    if branch.is_empty() {
        anyhow::bail!("Not on a branch (detached HEAD?)");
    }

    Ok(branch.to_string())
}

/// Extract agent ID from a branch name following gh-{number}/{slug} convention.
///
/// Returns the agent ID in the format "gh-{number}" if the branch follows the
/// convention, or None if it doesn't match the expected pattern.
///
/// # Examples
///
/// ```ignore
/// # use crate::services::git::extract_agent_id;
/// assert_eq!(extract_agent_id("gh-123/feat-add-sidebar"), Some("gh-123".to_string()));
/// assert_eq!(extract_agent_id("main"), None);
/// assert_eq!(extract_agent_id("gh-456"), Some("gh-456".to_string()));
/// ```
pub fn extract_agent_id(branch: &str) -> Option<String> {
    branch
        .strip_prefix("gh-")
        .and_then(|s| s.split('/').next())
        .filter(|id| !id.is_empty())
        .map(|id| format!("gh-{}", id))
}

/// A git commit with metadata.
///
/// Returned by [`GitService::get_recent_commits()`].
#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub struct Commit {
    /// Full commit hash (SHA-1).
    pub hash: String,

    /// First line of the commit message.
    pub message: String,

    /// Author name and email (e.g., "John Doe <john@example.com>").
    pub author: String,

    /// Commit date as Unix timestamp (seconds since epoch).
    pub date: String,
}

/// Information about a git worktree.
///
/// Returned by [`GitService::get_worktree()`].
#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub struct WorktreeInfo {
    /// Absolute path to the worktree directory.
    pub path: String,

    /// Current branch name (or "HEAD" if detached).
    pub branch: String,
}

/// Git repository information.
///
/// Returned by [`GitService::get_repo_info()`].
#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub struct RepoInfo {
    /// Current branch name.
    pub branch: String,

    /// Repository owner (parsed from remote URL, if available).
    ///
    /// For GitHub repos, this is the user/org name (e.g., "anthropics").
    pub owner: Option<GithubOwner>,

    /// Repository name (parsed from remote URL, if available).
    ///
    /// For GitHub repos, this is the repo name (e.g., "exomonad").
    pub name: Option<GithubRepo>,
}

/// Git operations service.
///
/// Executes git commands via the configured executor.
///
/// # Examples
///
/// ```ignore
/// use crate::services::git::GitService;
/// use crate::services::local::LocalExecutor;
/// use std::sync::Arc;
///
/// # async fn example() -> anyhow::Result<()> {
/// let executor = Arc::new(LocalExecutor::new());
/// let git = GitService::new(executor);
///
/// let branch = git.get_branch(".").await?;
/// println!("On branch: {}", branch);
/// # Ok(())
/// # }
/// ```
#[derive(Clone)]
pub struct GitService {
    executor: Arc<dyn CommandExecutor>,
}

impl GitService {
    pub fn new(executor: Arc<dyn CommandExecutor>) -> Self {
        Self { executor }
    }

    #[tracing::instrument(skip(self), fields(cmd = ?args))]
    async fn exec_git(&self, dir: &str, args: &[&str]) -> Result<String> {
        let mut cmd = vec!["git"];
        cmd.extend_from_slice(args);
        self.executor.exec(dir, &cmd).await
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_branch(&self, dir: &str) -> Result<String> {
        let output = self
            .exec_git(dir, &["rev-parse", "--abbrev-ref", "HEAD"])
            .await?;
        Ok(output.trim().to_string())
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_worktree(&self, dir: &str) -> Result<WorktreeInfo> {
        let path = self
            .exec_git(dir, &["rev-parse", "--show-toplevel"])
            .await?;
        let branch = self.get_branch(dir).await?;
        Ok(WorktreeInfo {
            path: path.trim().to_string(),
            branch,
        })
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_dirty_files(&self, dir: &str) -> Result<Vec<String>> {
        let output = self.exec_git(dir, &["status", "--porcelain"]).await?;
        Ok(output.lines().map(|l| l.to_string()).collect())
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_recent_commits(&self, dir: &str, n: u32) -> Result<Vec<Commit>> {
        let format = "%H|%an|%ad|%s";
        let output = self
            .exec_git(
                dir,
                &[
                    "log",
                    &format!("-n{}", n),
                    &format!("--format={}", format),
                    "--date=unix",
                ],
            )
            .await?;

        let mut commits = Vec::new();
        for line in output.lines() {
            let parts: Vec<&str> = line.split('|').collect();
            if parts.len() >= 4 {
                commits.push(Commit {
                    hash: parts[0].to_string(),
                    message: parts[3..].join("|"),
                    author: parts[1].to_string(),
                    date: parts[2].to_string(),
                });
            }
        }
        Ok(commits)
    }

    #[tracing::instrument(skip(self))]
    pub async fn has_unpushed_commits(&self, dir: &str) -> Result<u32> {
        let output = self
            .exec_git(dir, &["rev-list", "--count", "@{upstream}..HEAD"])
            .await;

        match output {
            Ok(count_str) => {
                let count = count_str.trim().parse::<u32>().unwrap_or(0);
                tracing::debug!(count, "Unpushed commits count");
                Ok(count)
            }
            Err(e) => {
                tracing::warn!(
                    error = %e,
                    "Failed to check unpushed commits. Assuming no upstream."
                );
                Ok(0)
            }
        }
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_remote_url(&self, dir: &str) -> Result<String> {
        let output = self.exec_git(dir, &["remote", "get-url", "origin"]).await?;
        Ok(output.trim().to_string())
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_repo_info(&self, dir: &str) -> Result<RepoInfo> {
        let branch = self.get_branch(dir).await?;
        let remote_url = self.get_remote_url(dir).await.ok();

        let (owner, name) = remote_url
            .as_ref()
            .and_then(|url| repo::parse_github_url(url))
            .unzip();

        Ok(RepoInfo {
            branch,
            owner: owner.as_ref().map(|s| GithubOwner::from(s.as_str())),
            name: name.as_ref().map(|s| GithubRepo::from(s.as_str())),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::future::Future;
    use std::pin::Pin;
    use std::sync::Mutex;

    struct MockExecutor {
        responses: Mutex<Vec<Result<String>>>,
    }

    impl MockExecutor {
        fn new(responses: Vec<Result<String>>) -> Self {
            Self {
                responses: Mutex::new(responses),
            }
        }
    }

    impl CommandExecutor for MockExecutor {
        fn exec<'a>(
            &'a self,
            _dir: &'a str,
            _cmd: &'a [&'a str],
        ) -> Pin<Box<dyn Future<Output = Result<String>> + Send + 'a>> {
            let response = self.responses.lock().unwrap().remove(0);
            Box::pin(async move { response })
        }
    }

    #[tokio::test]
    async fn test_get_branch() {
        let mock = Arc::new(MockExecutor::new(vec![Ok("main\n".to_string())]));
        let git = GitService::new(mock);
        let branch = git.get_branch("/app").await.unwrap();
        assert_eq!(branch, "main");
    }

    #[tokio::test]
    async fn test_get_worktree() {
        let mock = Arc::new(MockExecutor::new(vec![
            Ok("/app\n".to_string()),        // rev-parse --show-toplevel
            Ok("feature/123\n".to_string()), // rev-parse --abbrev-ref HEAD
        ]));
        let git = GitService::new(mock);
        let wt = git.get_worktree("/app/src").await.unwrap();
        assert_eq!(wt.path, "/app");
        assert_eq!(wt.branch, "feature/123");
    }

    #[tokio::test]
    async fn test_get_commits() {
        let log_output =
            "hash1|Author One|2023-01-01|Message 1\nhash2|Author Two|2023-01-02|Message 2\n";
        let mock = Arc::new(MockExecutor::new(vec![Ok(log_output.to_string())]));
        let git = GitService::new(mock);
        let commits = git.get_recent_commits("/app", 2).await.unwrap();
        assert_eq!(commits.len(), 2);
        assert_eq!(commits[0].hash, "hash1");
        assert_eq!(commits[1].message, "Message 2");
    }

    #[test]
    fn test_extract_agent_id_valid_branches() {
        // Standard format: gh-{number}/{slug}
        assert_eq!(
            extract_agent_id("gh-123/feat-add-sidebar"),
            Some("gh-123".to_string())
        );
        assert_eq!(
            extract_agent_id("gh-456/fix-bug-with-events"),
            Some("gh-456".to_string())
        );

        // Branch without slug (just gh-{number})
        assert_eq!(extract_agent_id("gh-789"), Some("gh-789".to_string()));

        // Multi-part slug
        assert_eq!(
            extract_agent_id("gh-111/feat/nested/path"),
            Some("gh-111".to_string())
        );
    }

    #[test]
    fn test_extract_agent_id_invalid_branches() {
        // Regular branches without gh- prefix
        assert_eq!(extract_agent_id("main"), None);
        assert_eq!(extract_agent_id("develop"), None);
        assert_eq!(extract_agent_id("feature/something"), None);

        // Malformed gh- branches
        assert_eq!(extract_agent_id("gh-"), None);
        assert_eq!(extract_agent_id("gh-/no-number"), None);

        // Empty string
        assert_eq!(extract_agent_id(""), None);
    }
}
