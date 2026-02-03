use crate::common::{FFIBoundary, HostResult, IntoFFIResult};
use crate::services::docker::DockerExecutor;
use anyhow::{Context, Result};
use duct::cmd;
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use extism_convert::Json;
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
/// ```
/// # use exomonad_runtime::services::git::extract_agent_id;
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

    /// Commit date in ISO 8601 format.
    pub date: String,
}

impl FFIBoundary for Commit {}

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

impl FFIBoundary for WorktreeInfo {}

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
    pub owner: Option<String>,

    /// Repository name (parsed from remote URL, if available).
    ///
    /// For GitHub repos, this is the repo name (e.g., "exomonad").
    pub name: Option<String>,
}

impl FFIBoundary for RepoInfo {}

/// Git operations service.
///
/// Executes git commands via the configured executor (local subprocess or Docker).
/// All operations are asynchronous.
///
/// # Examples
///
/// ```no_run
/// use exomonad_runtime::services::git::GitService;
/// use exomonad_runtime::services::local::LocalExecutor;
/// use std::sync::Arc;
///
/// # async fn example() -> anyhow::Result<()> {
/// let executor = Arc::new(LocalExecutor::new());
/// let git = GitService::new(executor);
///
/// // Get current branch
/// let branch = git.get_branch("", ".").await?;
/// println!("On branch: {}", branch);
/// # Ok(())
/// # }
/// ```
#[derive(Clone)]
pub struct GitService {
    docker: Arc<dyn DockerExecutor>,
}

impl GitService {
    /// Create a new GitService with the given executor.
    ///
    /// # Arguments
    ///
    /// * `docker` - Executor for running git commands (LocalExecutor or DockerService)
    pub fn new(docker: Arc<dyn DockerExecutor>) -> Self {
        Self { docker }
    }

    #[tracing::instrument(skip(self), fields(cmd = ?args))]
    async fn exec_git(&self, container: &str, dir: &str, args: &[&str]) -> Result<String> {
        let mut cmd = vec!["git"];
        cmd.extend_from_slice(args);
        self.docker.exec(container, dir, &cmd).await
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_branch(&self, container: &str, dir: &str) -> Result<String> {
        let output = self
            .exec_git(container, dir, &["rev-parse", "--abbrev-ref", "HEAD"])
            .await?;
        Ok(output.trim().to_string())
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_worktree(&self, container: &str, dir: &str) -> Result<WorktreeInfo> {
        let path = self
            .exec_git(container, dir, &["rev-parse", "--show-toplevel"])
            .await?;
        let branch = self.get_branch(container, dir).await?;
        Ok(WorktreeInfo {
            path: path.trim().to_string(),
            branch,
        })
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_dirty_files(&self, container: &str, dir: &str) -> Result<Vec<String>> {
        let output = self
            .exec_git(container, dir, &["status", "--porcelain"])
            .await?;
        Ok(output.lines().map(|l| l.to_string()).collect())
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_recent_commits(
        &self,
        container: &str,
        dir: &str,
        n: u32,
    ) -> Result<Vec<Commit>> {
        let format = "%H|%an|%ad|%s";
        let output = self
            .exec_git(
                container,
                dir,
                &["log", &format!("-n{}", n), &format!("--format={}", format)],
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
    pub async fn has_unpushed_commits(&self, container: &str, dir: &str) -> Result<bool> {
        let output = self
            .exec_git(
                container,
                dir,
                &["rev-list", "--count", "@{upstream}..HEAD"],
            )
            .await;

        match output {
            Ok(count_str) => {
                let count = count_str.trim().parse::<u32>().unwrap_or(0);
                tracing::debug!(count, "Unpushed commits count");
                Ok(count > 0)
            }
            Err(e) => {
                tracing::warn!(
                    error = %e,
                    "Failed to check unpushed commits. Assuming no upstream."
                );
                Ok(false)
            }
        }
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_remote_url(&self, container: &str, dir: &str) -> Result<String> {
        let output = self
            .exec_git(container, dir, &["remote", "get-url", "origin"])
            .await?;

        let url = output.trim().to_string();
        Ok(url)
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_repo_info(&self, container: &str, dir: &str) -> Result<RepoInfo> {
        let branch = self.get_branch(container, dir).await?;
        let remote_url = self.get_remote_url(container, dir).await.ok();

        let (owner, name) = remote_url
            .as_ref()
            .and_then(|url| parse_github_url(url))
            .unzip();

        Ok(RepoInfo {
            branch,
            owner,
            name,
        })
    }
}

/// Parse a GitHub URL (HTTPS or SSH) into (owner, repo) tuple.
pub fn parse_github_url(url: &str) -> Option<(String, String)> {
    let cleaned = url
        .replace("git@github.com:", "https://github.com/")
        .replace(".git", "");

    let parts: Vec<&str> = cleaned.split('/').collect();

    match parts.as_slice() {
        [.., owner, repo] if !owner.is_empty() && !repo.is_empty() => {
            Some((owner.to_string(), repo.to_string()))
        }
        _ => None,
    }
}

// Host Function Types

#[derive(Deserialize, Serialize)]
struct GitHostInput {
    #[serde(rename = "workingDir")]
    working_dir: String,
    #[serde(rename = "containerId")]
    container_id: String,
}

impl FFIBoundary for GitHostInput {}

#[derive(Deserialize, Serialize)]
struct GitLogInput {
    #[serde(rename = "workingDir")]
    working_dir: String,
    #[serde(rename = "containerId")]
    container_id: String,
    limit: u32,
}

impl FFIBoundary for GitLogInput {}

fn block_on<F: std::future::Future>(future: F) -> Result<F::Output, Error> {
    match tokio::runtime::Handle::try_current() {
        Ok(handle) => Ok(handle.block_on(future)),
        Err(_) => Err(Error::msg(
            "No Tokio runtime available for async Git operation",
        )),
    }
}

// Host Function Factories

pub fn git_get_branch_host_fn(git_service: Arc<GitService>) -> Function {
    Function::new(
        "git_get_branch",
        [ValType::I64],
        [ValType::I64],
        UserData::new(git_service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<GitService>>|
         -> Result<(), Error> {
            let _span = tracing::info_span!("host_function", function = "git_get_branch").entered();

            if inputs.is_empty() {
                return Err(Error::msg("git_get_branch: expected input argument"));
            }
            let Json(input): Json<GitHostInput> = plugin.memory_get_val(&inputs[0])?;
            tracing::info!(dir = %input.working_dir, container = %input.container_id, "Getting git branch");

            let git_arc = user_data.get()?;
            let git = git_arc.lock().map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(git.get_branch(&input.container_id, &input.working_dir))?;

            match &result {
                Ok(branch) => tracing::info!(success = true, branch = %branch, "Completed"),
                Err(e) => tracing::warn!(error = %e, "Failed"),
            }

            let output: HostResult<String> = result.into_ffi_result();

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn git_get_worktree_host_fn(git_service: Arc<GitService>) -> Function {
    Function::new(
        "git_get_worktree",
        [ValType::I64],
        [ValType::I64],
        UserData::new(git_service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<GitService>>|
         -> Result<(), Error> {
            let _span = tracing::info_span!("host_function", function = "git_get_worktree").entered();

            if inputs.is_empty() {
                return Err(Error::msg("git_get_worktree: expected input argument"));
            }
            let Json(input): Json<GitHostInput> = plugin.memory_get_val(&inputs[0])?;
            tracing::info!(dir = %input.working_dir, container = %input.container_id, "Getting git worktree");

            let git_arc = user_data.get()?;
            let git = git_arc.lock().map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(git.get_worktree(&input.container_id, &input.working_dir))?;

            match &result {
                Ok(wt) => tracing::info!(success = true, path = %wt.path, branch = %wt.branch, "Completed"),
                Err(e) => tracing::warn!(error = %e, "Failed"),
            }

            let output: HostResult<WorktreeInfo> = result.into_ffi_result();

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn git_get_dirty_files_host_fn(git_service: Arc<GitService>) -> Function {
    Function::new(
        "git_get_dirty_files",
        [ValType::I64],
        [ValType::I64],
        UserData::new(git_service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<GitService>>|
         -> Result<(), Error> {
            let _span = tracing::info_span!("host_function", function = "git_get_dirty_files").entered();

            if inputs.is_empty() {
                return Err(Error::msg("git_get_dirty_files: expected input argument"));
            }
            let Json(input): Json<GitHostInput> = plugin.memory_get_val(&inputs[0])?;
            tracing::info!(dir = %input.working_dir, container = %input.container_id, "Getting dirty files");

            let git_arc = user_data.get()?;
            let git = git_arc.lock().map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(git.get_dirty_files(&input.container_id, &input.working_dir))?;

            match &result {
                Ok(files) => tracing::info!(success = true, count = files.len(), "Completed"),
                Err(e) => tracing::warn!(error = %e, "Failed"),
            }

            let output: HostResult<Vec<String>> = result.into_ffi_result();

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn git_get_recent_commits_host_fn(git_service: Arc<GitService>) -> Function {
    Function::new(
        "git_get_recent_commits",
        [ValType::I64],
        [ValType::I64],
        UserData::new(git_service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<GitService>>|
         -> Result<(), Error> {
            let _span = tracing::info_span!("host_function", function = "git_get_recent_commits").entered();

            if inputs.is_empty() {
                return Err(Error::msg(
                    "git_get_recent_commits: expected input argument",
                ));
            }
            let Json(input): Json<GitLogInput> = plugin.memory_get_val(&inputs[0])?;
            tracing::info!(dir = %input.working_dir, container = %input.container_id, limit = input.limit, "Getting recent commits");

            let git_arc = user_data.get()?;
            let git = git_arc.lock().map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(git.get_recent_commits(
                &input.container_id,
                &input.working_dir,
                input.limit,
            ))?;

            match &result {
                Ok(commits) => tracing::info!(success = true, count = commits.len(), "Completed"),
                Err(e) => tracing::warn!(error = %e, "Failed"),
            }

            let output: HostResult<Vec<Commit>> = result.into_ffi_result();

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn git_has_unpushed_commits_host_fn(git_service: Arc<GitService>) -> Function {
    Function::new(
        "git_has_unpushed_commits",
        [ValType::I64],
        [ValType::I64],
        UserData::new(git_service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<GitService>>|
         -> Result<(), Error> {
            let _span = tracing::info_span!("host_function", function = "git_has_unpushed_commits").entered();

            if inputs.is_empty() {
                return Err(Error::msg(
                    "git_has_unpushed_commits: expected input argument",
                ));
            }
            let Json(input): Json<GitHostInput> = plugin.memory_get_val(&inputs[0])?;
            tracing::info!(dir = %input.working_dir, container = %input.container_id, "Checking unpushed commits");

            let git_arc = user_data.get()?;
            let git = git_arc.lock().map_err(|_| Error::msg("Poisoned lock"))?;

            let result =
                block_on(git.has_unpushed_commits(&input.container_id, &input.working_dir))?;

            match &result {
                Ok(has_unpushed) => tracing::info!(success = true, has_unpushed, "Completed"),
                Err(e) => tracing::warn!(error = %e, "Failed"),
            }

            let output: HostResult<bool> = result.into_ffi_result();

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn git_get_remote_url_host_fn(git_service: Arc<GitService>) -> Function {
    Function::new(
        "git_get_remote_url",
        [ValType::I64],
        [ValType::I64],
        UserData::new(git_service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<GitService>>|
         -> Result<(), Error> {
            let _span = tracing::info_span!("host_function", function = "git_get_remote_url").entered();

            if inputs.is_empty() {
                return Err(Error::msg("git_get_remote_url: expected input argument"));
            }
            let Json(input): Json<GitHostInput> = plugin.memory_get_val(&inputs[0])?;
            tracing::info!(dir = %input.working_dir, container = %input.container_id, "Getting git remote URL");

            let git_arc = user_data.get()?;
            let git = git_arc.lock().map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(git.get_remote_url(&input.container_id, &input.working_dir))?;

            match &result {
                Ok(url) => tracing::info!(success = true, url = %url, "Completed"),
                Err(e) => tracing::warn!(error = %e, "Failed"),
            }

            let output: HostResult<String> = result.into_ffi_result();

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn git_get_repo_info_host_fn(git_service: Arc<GitService>) -> Function {
    Function::new(
        "git_get_repo_info",
        [ValType::I64],
        [ValType::I64],
        UserData::new(git_service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<GitService>>|
         -> Result<(), Error> {
            let _span = tracing::info_span!("host_function", function = "git_get_repo_info").entered();

            if inputs.is_empty() {
                return Err(Error::msg("git_get_repo_info: expected input argument"));
            }
            let Json(input): Json<GitHostInput> = plugin.memory_get_val(&inputs[0])?;
            tracing::info!(dir = %input.working_dir, container = %input.container_id, "Getting git repo info");

            let git_arc = user_data.get()?;
            let git = git_arc.lock().map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(git.get_repo_info(&input.container_id, &input.working_dir))?;

            match &result {
                Ok(info) => tracing::info!(success = true, branch = %info.branch, owner = ?info.owner, name = ?info.name, "Completed"),
                Err(e) => tracing::warn!(error = %e, "Failed"),
            }

            let output: HostResult<RepoInfo> = result.into_ffi_result();

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
            Ok(())
        },
    )
    .with_namespace("env")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::future::Future;
    use std::pin::Pin;
    use std::sync::Mutex;

    struct MockDocker {
        responses: Mutex<Vec<Result<String>>>,
    }

    impl MockDocker {
        fn new(responses: Vec<Result<String>>) -> Self {
            Self {
                responses: Mutex::new(responses),
            }
        }
    }

    impl DockerExecutor for MockDocker {
        fn exec<'a>(
            &'a self,
            _container: &'a str,
            _dir: &'a str,
            _cmd: &'a [&'a str],
        ) -> Pin<Box<dyn Future<Output = Result<String>> + Send + 'a>> {
            let response = self.responses.lock().unwrap().remove(0);
            Box::pin(async move { response })
        }
    }

    #[tokio::test]
    async fn test_get_branch() {
        let mock = Arc::new(MockDocker::new(vec![Ok("main\n".to_string())]));
        let git = GitService::new(mock);
        let branch = git.get_branch("c1", "/app").await.unwrap();
        assert_eq!(branch, "main");
    }

    #[tokio::test]
    async fn test_get_worktree() {
        let mock = Arc::new(MockDocker::new(vec![
            Ok("/app\n".to_string()),        // rev-parse --show-toplevel
            Ok("feature/123\n".to_string()), // rev-parse --abbrev-ref HEAD
        ]));
        let git = GitService::new(mock);
        let wt = git.get_worktree("c1", "/app/src").await.unwrap();
        assert_eq!(wt.path, "/app");
        assert_eq!(wt.branch, "feature/123");
    }

    #[tokio::test]
    async fn test_get_commits() {
        let log_output =
            "hash1|Author One|2023-01-01|Message 1\nhash2|Author Two|2023-01-02|Message 2\n";
        let mock = Arc::new(MockDocker::new(vec![Ok(log_output.to_string())]));
        let git = GitService::new(mock);
        let commits = git.get_recent_commits("c1", "/app", 2).await.unwrap();
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
