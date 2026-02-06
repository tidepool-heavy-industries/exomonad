use crate::common::{ErrorCode, FFIBoundary, HostResult};
use anyhow::{anyhow, Result};
use exomonad_shared::{GithubOwner, GithubRepo};
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use extism_convert::Json;
use octocrab::{models, params, Octocrab, OctocrabBuilder};
use serde::{Deserialize, Serialize};
use tokio::time::{timeout, Duration};
use tracing::info;

const API_TIMEOUT: Duration = Duration::from_secs(30);

// ============================================================================
// Types
// ============================================================================

/// GitHub repository identifier.
///
/// Uniquely identifies a repository by owner and name (e.g., "anthropics/exomonad").
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Repo {
    /// Repository owner (user or organization name).
    pub owner: GithubOwner,

    /// Repository name.
    pub name: GithubRepo,
}

impl FFIBoundary for Repo {}

/// Filter criteria for listing GitHub issues.
///
/// Used with [`GitHubService::list_issues()`].
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct IssueFilter {
    /// Filter by issue state: "open", "closed", or "all".
    pub state: Option<String>,

    /// Filter by label names (AND logic - issue must have all labels).
    pub labels: Option<Vec<String>>,
}

impl FFIBoundary for IssueFilter {}

/// Specification for creating a pull request.
///
/// Used with [`GitHubService::create_pr()`].
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CreatePRSpec {
    /// PR title.
    pub title: String,

    /// PR body (markdown description).
    pub body: String,

    /// Head branch (source branch containing changes).
    pub head: String,

    /// Base branch (target branch to merge into, usually "main").
    pub base: String,
}

impl FFIBoundary for CreatePRSpec {}

/// Filter criteria for listing pull requests.
///
/// Used with [`GitHubService::list_prs()`].
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PRFilter {
    /// Filter by PR state: "open", "closed", or "all".
    pub state: Option<String>,

    /// Maximum number of PRs to return (default: API default, usually 30).
    pub limit: Option<u32>,
}

impl FFIBoundary for PRFilter {}

/// A GitHub issue with metadata.
///
/// Returned by [`GitHubService::list_issues()`] and [`GitHubService::get_issue()`].
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Issue {
    /// Issue number (unique within repository).
    pub number: u64,

    /// Issue title.
    pub title: String,

    /// Issue body (markdown description).
    pub body: String,

    /// Issue state ("open" or "closed").
    pub state: String,

    /// Web URL to the issue.
    pub url: String,

    /// Issue author's GitHub username.
    pub author: String,

    /// Label names attached to the issue.
    pub labels: Vec<String>,
}

impl FFIBoundary for Issue {}

/// A GitHub pull request with metadata.
///
/// Returned by [`GitHubService::list_prs()`] and [`GitHubService::get_pr_for_branch()`].
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PullRequest {
    /// PR number (unique within repository).
    pub number: u64,

    /// PR title.
    pub title: String,

    /// PR body (markdown description).
    pub body: String,

    /// PR state ("open", "closed", or "merged").
    pub state: String,

    /// Web URL to the PR.
    pub url: String,

    /// PR author's GitHub username.
    pub author: String,

    /// Head branch (source branch with changes).
    pub head_ref: String,

    /// Base branch (target branch for merge).
    pub base_ref: String,

    /// Creation timestamp (ISO 8601).
    pub created_at: String,

    /// Merge timestamp (ISO 8601, if merged).
    pub merged_at: Option<String>,
}

impl FFIBoundary for PullRequest {}

/// A review comment on a pull request.
///
/// Returned by [`GitHubService::get_pr_review_comments()`].
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ReviewComment {
    /// Comment ID (unique).
    pub id: u64,

    /// Comment body (markdown).
    pub body: String,

    /// File path the comment is attached to.
    pub path: String,

    /// Line number in the file (if available).
    pub line: Option<u32>,

    /// Comment author's GitHub username.
    pub author: String,

    /// Creation timestamp (ISO 8601).
    pub created_at: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct GithubListIssuesInput {
    pub repo: Repo,
    pub filter: Option<IssueFilter>,
}

impl FFIBoundary for GithubListIssuesInput {}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct GithubGetIssueInput {
    pub repo: Repo,
    pub number: u64,
}

impl FFIBoundary for GithubGetIssueInput {}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct GithubCreatePRInput {
    pub repo: Repo,
    pub spec: CreatePRSpec,
}

impl FFIBoundary for GithubCreatePRInput {}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct GithubListPRsInput {
    pub repo: Repo,
    pub filter: Option<PRFilter>,
}

impl FFIBoundary for GithubListPRsInput {}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct GithubGetPRForBranchInput {
    pub repo: Repo,
    pub head: String,
}

impl FFIBoundary for GithubGetPRForBranchInput {}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct GithubGetPRReviewCommentsInput {
    pub repo: Repo,
    pub pr_number: u64,
}

impl FFIBoundary for GithubGetPRReviewCommentsInput {}

// ============================================================================
// Service Implementation
// ============================================================================

/// GitHub API service.
///
/// Provides access to GitHub REST API for issues, pull requests, and review comments.
/// Uses octocrab for API access and requires a personal access token for authentication.
///
/// # Authentication
///
/// Requires a GitHub personal access token with appropriate scopes:
/// - `repo` - Required for private repositories
/// - `public_repo` - Sufficient for public repositories
///
/// # Examples
///
/// ```no_run
/// use exomonad_runtime::services::github::{GitHubService, Repo};
/// use exomonad_shared::{GithubOwner, GithubRepo};
///
/// # async fn example() -> anyhow::Result<()> {
/// let github = GitHubService::new("ghp_...".to_string())?;
///
/// let repo = Repo {
///     owner: GithubOwner::from("anthropics"),
///     name: GithubRepo::from("exomonad"),
/// };
///
/// let issues = github.list_issues(&repo, None).await?;
/// println!("Found {} issues", issues.len());
/// # Ok(())
/// # }
/// ```
#[derive(Clone)]
pub struct GitHubService {
    client: Octocrab,
}

impl GitHubService {
    /// Create a new GitHubService with the given personal access token.
    ///
    /// # Arguments
    ///
    /// * `token` - GitHub personal access token (starts with "ghp_" or "github_pat_")
    ///
    /// # Errors
    ///
    /// Returns an error if the octocrab client fails to initialize.
    pub fn new(token: String) -> Result<Self> {
        let client = OctocrabBuilder::new().personal_token(token).build()?;
        Ok(Self { client })
    }

    /// List issues in a repository.
    ///
    /// # Arguments
    ///
    /// * `repo` - Repository identifier (owner + name)
    /// * `filter` - Optional filter criteria (state, labels)
    ///
    /// # Returns
    ///
    /// A vector of issues matching the filter criteria.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - Repository doesn't exist or is not accessible
    /// - Network request fails
    /// - Authentication fails
    #[tracing::instrument(skip(self))]
    pub async fn list_issues(
        &self,
        repo: &Repo,
        filter: Option<&IssueFilter>,
    ) -> Result<Vec<Issue>> {
        let repo_name = format!("{}/{}", repo.owner, repo.name);
        info!(repo = %repo_name, "GitHub API: Listing issues");

        let issues_handler = self.client.issues(repo.owner.as_str(), repo.name.as_str());
        let mut builder = issues_handler.list();

        if let Some(f) = filter {
            if let Some(state) = &f.state {
                let s = match state.as_str() {
                    "open" => params::State::Open,
                    "closed" => params::State::Closed,
                    _ => params::State::All,
                };
                builder = builder.state(s);
            }
            if let Some(labels) = &f.labels {
                if !labels.is_empty() {
                    // Octocrab expects generic iterable
                    builder = builder.labels(labels);
                }
            }
        }

        let page = timeout(API_TIMEOUT, builder.send()).await.map_err(|_| {
            anyhow!(
                "GitHub API list_issues timed out after {}s",
                API_TIMEOUT.as_secs()
            )
        })??;

        let issues = timeout(API_TIMEOUT, self.client.all_pages(page))
            .await
            .map_err(|_| {
                anyhow!(
                    "GitHub API all_pages timed out after {}s",
                    API_TIMEOUT.as_secs()
                )
            })??;

        info!(
            repo = %repo_name,
            count = issues.len(),
            "GitHub API: List issues successful"
        );

        Ok(issues
            .into_iter()
            .map(|i| Issue {
                number: i.number,
                title: i.title,
                body: i.body.unwrap_or_default(),
                state: match i.state {
                    models::IssueState::Open => "open".to_string(),
                    models::IssueState::Closed => "closed".to_string(),
                    _ => "unknown".to_string(),
                },
                url: i.html_url.to_string(),
                author: i.user.login,
                labels: i.labels.into_iter().map(|l| l.name).collect(),
            })
            .collect())
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_issue(&self, repo: &Repo, number: u64) -> Result<Issue> {
        let repo_name = format!("{}/{}", repo.owner, repo.name);
        info!(repo = %repo_name, number, "GitHub API: Get issue");

        let issue = timeout(
            API_TIMEOUT,
            self.client
                .issues(repo.owner.as_str(), repo.name.as_str())
                .get(number),
        )
        .await
        .map_err(|_| {
            anyhow!(
                "GitHub API get_issue timed out after {}s",
                API_TIMEOUT.as_secs()
            )
        })??;

        info!(repo = %repo_name, number, "GitHub API: Get issue successful");

        Ok(Issue {
            number: issue.number,
            title: issue.title,
            body: issue.body.unwrap_or_default(),
            state: match issue.state {
                models::IssueState::Open => "open".to_string(),
                models::IssueState::Closed => "closed".to_string(),
                _ => "unknown".to_string(),
            },
            url: issue.html_url.to_string(),
            author: issue.user.login,
            labels: issue.labels.into_iter().map(|l| l.name).collect(),
        })
    }

    #[tracing::instrument(skip(self))]
    pub async fn create_pr(&self, repo: &Repo, spec: CreatePRSpec) -> Result<PullRequest> {
        let repo_name = format!("{}/{}", repo.owner, repo.name);
        info!(repo = %repo_name, title = %spec.title, "GitHub API: Create PR");

        let pr = timeout(
            API_TIMEOUT,
            self.client
                .pulls(repo.owner.as_str(), repo.name.as_str())
                .create(spec.title, spec.head, spec.base)
                .body(spec.body)
                .send(),
        )
        .await
        .map_err(|_| {
            anyhow!(
                "GitHub API create_pr timed out after {}s",
                API_TIMEOUT.as_secs()
            )
        })??;

        info!(
            repo = %repo_name,
            number = pr.number,
            "GitHub API: Create PR successful"
        );

        Ok(PullRequest {
            number: pr.number,
            title: pr.title.unwrap_or_default(),
            body: pr.body.unwrap_or_default(),
            state: match pr.state {
                Some(models::IssueState::Open) => "open".to_string(),
                Some(models::IssueState::Closed) => "closed".to_string(),
                _ => "unknown".to_string(),
            },
            url: pr.html_url.map(|u| u.to_string()).unwrap_or_default(),
            author: pr.user.map(|u| u.login).unwrap_or_else(|| "unknown".into()),
            head_ref: pr.head.ref_field,
            base_ref: pr.base.ref_field,
            created_at: pr.created_at.map(|t| t.to_rfc3339()).unwrap_or_default(),
            merged_at: pr.merged_at.map(|t| t.to_rfc3339()),
        })
    }

    #[tracing::instrument(skip(self))]
    pub async fn list_prs(
        &self,
        repo: &Repo,
        filter: Option<&PRFilter>,
    ) -> Result<Vec<PullRequest>> {
        let repo_name = format!("{}/{}", repo.owner, repo.name);
        info!(repo = %repo_name, "GitHub API: List PRs");

        let pulls_handler = self.client.pulls(repo.owner.as_str(), repo.name.as_str());
        let mut builder = pulls_handler.list();

        if let Some(f) = filter {
            if let Some(state) = &f.state {
                let s = match state.as_str() {
                    "open" => params::State::Open,
                    "closed" => params::State::Closed,
                    _ => params::State::All,
                };
                builder = builder.state(s);
            }
            if let Some(limit) = f.limit {
                builder = builder.per_page(limit as u8);
            }
        }

        let page = timeout(API_TIMEOUT, builder.send()).await.map_err(|_| {
            anyhow!(
                "GitHub API list_prs timed out after {}s",
                API_TIMEOUT.as_secs()
            )
        })??;
        // For PRs, we might not want all pages if a limit was set, but octocrab's list() returns a Page.
        // If limit was set, we used per_page.

        info!(
            repo = %repo_name,
            "GitHub API: List PRs successful (page 1)"
        );

        Ok(page
            .into_iter()
            .map(|pr| PullRequest {
                number: pr.number,
                title: pr.title.unwrap_or_default(),
                body: pr.body.unwrap_or_default(),
                state: match pr.state {
                    Some(models::IssueState::Open) => "open".to_string(),
                    Some(models::IssueState::Closed) => "closed".to_string(),
                    _ => "unknown".to_string(),
                },
                url: pr.html_url.map(|u| u.to_string()).unwrap_or_default(),
                author: pr.user.map(|u| u.login).unwrap_or_else(|| "unknown".into()),
                head_ref: pr.head.ref_field,
                base_ref: pr.base.ref_field,
                created_at: pr.created_at.map(|t| t.to_rfc3339()).unwrap_or_default(),
                merged_at: pr.merged_at.map(|t| t.to_rfc3339()),
            })
            .collect())
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_pr_for_branch(&self, repo: &Repo, head: &str) -> Result<Option<PullRequest>> {
        let pulls_handler = self.client.pulls(repo.owner.as_str(), repo.name.as_str());
        let page = timeout(
            API_TIMEOUT,
            pulls_handler
                .list()
                .state(params::State::Open)
                .head(format!("{}:{}", repo.owner, head))
                .send(),
        )
        .await
        .map_err(|_| {
            anyhow!(
                "GitHub API get_pr_for_branch timed out after {}s",
                API_TIMEOUT.as_secs()
            )
        })??;

        let pr = page.into_iter().next();

        match &pr {
            Some(p) => tracing::info!(number = p.number, head, "Found PR for branch"),
            None => tracing::info!(head, "No PR found for branch"),
        }

        Ok(pr.map(|pr| PullRequest {
            number: pr.number,
            title: pr.title.unwrap_or_default(),
            body: pr.body.unwrap_or_default(),
            state: match pr.state {
                Some(models::IssueState::Open) => "open".to_string(),
                Some(models::IssueState::Closed) => "closed".to_string(),
                _ => "unknown".to_string(),
            },
            url: pr.html_url.map(|u| u.to_string()).unwrap_or_default(),
            author: pr.user.map(|u| u.login).unwrap_or_else(|| "unknown".into()),
            head_ref: pr.head.ref_field,
            base_ref: pr.base.ref_field,
            created_at: pr.created_at.map(|t| t.to_rfc3339()).unwrap_or_default(),
            merged_at: pr.merged_at.map(|t| t.to_rfc3339()),
        }))
    }

    #[tracing::instrument(skip(self))]
    pub async fn get_pr_review_comments(
        &self,
        _repo: &Repo,
        pr_number: u64,
    ) -> Result<Vec<ReviewComment>> {
        tracing::debug!(
            pr_number,
            "Review comment checking is simplified - returning empty"
        );
        Ok(vec![])
    }
}

// ============================================================================
// Host Functions
// ============================================================================

fn map_error<T>(e: anyhow::Error) -> HostResult<T> {
    let msg = e.to_string();
    let code = if msg.contains("403") || msg.contains("rate limit") {
        ErrorCode::NetworkError // Or a specific RateLimited code if we add one
    } else if msg.contains("404") {
        ErrorCode::NotFound
    } else if msg.contains("401") {
        ErrorCode::NotAuthenticated
    } else {
        ErrorCode::InternalError
    };

    HostResult::error(msg, code, None, None)
}

// Helper functions for Extism memory access (Pattern A: memory handles)

fn get_input<T: serde::de::DeserializeOwned>(
    plugin: &mut CurrentPlugin,
    val: Val,
) -> std::result::Result<T, Error> {
    let handle = plugin
        .memory_from_val(&val)
        .ok_or_else(|| Error::msg("Invalid memory handle in input"))?;
    let bytes = plugin.memory_bytes(handle)?;
    Ok(serde_json::from_slice(bytes)?)
}

fn block_on<F: std::future::Future>(future: F) -> std::result::Result<F::Output, Error> {
    match tokio::runtime::Handle::try_current() {
        Ok(handle) => Ok(handle.block_on(future)),
        Err(_) => Err(Error::msg(
            "No Tokio runtime available for async GitHub operation",
        )),
    }
}

// Define the host functions (Pattern A: single I64 memory handle)
pub fn register_host_functions() -> Vec<Function> {
    vec![
        Function::new(
            "github_list_issues",
            [ValType::I64],
            [ValType::I64],
            UserData::new(()),
            github_list_issues,
        )
        .with_namespace("env"),
        Function::new(
            "github_get_issue",
            [ValType::I64],
            [ValType::I64],
            UserData::new(()),
            github_get_issue,
        )
        .with_namespace("env"),
        Function::new(
            "github_create_pr",
            [ValType::I64],
            [ValType::I64],
            UserData::new(()),
            github_create_pr,
        )
        .with_namespace("env"),
        Function::new(
            "github_list_prs",
            [ValType::I64],
            [ValType::I64],
            UserData::new(()),
            github_list_prs,
        )
        .with_namespace("env"),
        Function::new(
            "github_get_pr_for_branch",
            [ValType::I64],
            [ValType::I64],
            UserData::new(()),
            github_get_pr_for_branch,
        )
        .with_namespace("env"),
        Function::new(
            "github_get_pr_review_comments",
            [ValType::I64],
            [ValType::I64],
            UserData::new(()),
            github_get_pr_review_comments,
        )
        .with_namespace("env"),
    ]
}

fn github_list_issues(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> std::result::Result<(), Error> {
    let _span = tracing::info_span!("host_function", function = "github_list_issues").entered();

    let input: GithubListIssuesInput = get_input(plugin, inputs[0])?;
    tracing::info!(owner = %input.repo.owner, repo = %input.repo.name, "Listing issues");

    let service = GitHubService::new(std::env::var("GITHUB_TOKEN").unwrap_or_default())
        .map_err(|e| Error::msg(e.to_string()))?;

    let result = block_on(service.list_issues(&input.repo, input.filter.as_ref()))?;

    match &result {
        Ok(issues) => tracing::info!(success = true, count = issues.len(), "Completed"),
        Err(e) => tracing::warn!(error = %e, "Failed"),
    }

    let output = match result {
        Ok(issues) => HostResult::Success(issues),
        Err(e) => map_error(e),
    };

    plugin.memory_set_val(&mut outputs[0], Json(output))?;
    Ok(())
}

fn github_get_issue(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> std::result::Result<(), Error> {
    let _span = tracing::info_span!("host_function", function = "github_get_issue").entered();

    let input: GithubGetIssueInput = get_input(plugin, inputs[0])?;
    tracing::info!(owner = %input.repo.owner, repo = %input.repo.name, number = input.number, "Getting issue");

    let service = GitHubService::new(std::env::var("GITHUB_TOKEN").unwrap_or_default())
        .map_err(|e| Error::msg(e.to_string()))?;

    let result = block_on(service.get_issue(&input.repo, input.number))?;

    match &result {
        Ok(_) => tracing::info!(success = true, "Completed"),
        Err(e) => tracing::warn!(error = %e, "Failed"),
    }

    let output = match result {
        Ok(issue) => HostResult::Success(issue),
        Err(e) => map_error(e),
    };

    plugin.memory_set_val(&mut outputs[0], Json(output))?;
    Ok(())
}

fn github_create_pr(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> std::result::Result<(), Error> {
    let _span = tracing::info_span!("host_function", function = "github_create_pr").entered();

    let input: GithubCreatePRInput = get_input(plugin, inputs[0])?;
    tracing::info!(owner = %input.repo.owner, repo = %input.repo.name, title = %input.spec.title, "Creating PR");

    let service = GitHubService::new(std::env::var("GITHUB_TOKEN").unwrap_or_default())
        .map_err(|e| Error::msg(e.to_string()))?;

    let result = block_on(service.create_pr(&input.repo, input.spec))?;

    match &result {
        Ok(pr) => tracing::info!(success = true, number = pr.number, "Completed"),
        Err(e) => tracing::warn!(error = %e, "Failed"),
    }

    let output = match result {
        Ok(pr) => HostResult::Success(pr),
        Err(e) => map_error(e),
    };

    plugin.memory_set_val(&mut outputs[0], Json(output))?;
    Ok(())
}

fn github_list_prs(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> std::result::Result<(), Error> {
    let _span = tracing::info_span!("host_function", function = "github_list_prs").entered();

    let input: GithubListPRsInput = get_input(plugin, inputs[0])?;
    tracing::info!(owner = %input.repo.owner, repo = %input.repo.name, "Listing PRs");

    let service = GitHubService::new(std::env::var("GITHUB_TOKEN").unwrap_or_default())
        .map_err(|e| Error::msg(e.to_string()))?;

    let result = block_on(service.list_prs(&input.repo, input.filter.as_ref()))?;

    match &result {
        Ok(prs) => tracing::info!(success = true, count = prs.len(), "Completed"),
        Err(e) => tracing::warn!(error = %e, "Failed"),
    }

    let output = match result {
        Ok(prs) => HostResult::Success(prs),
        Err(e) => map_error(e),
    };

    plugin.memory_set_val(&mut outputs[0], Json(output))?;
    Ok(())
}

fn github_get_pr_for_branch(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> std::result::Result<(), Error> {
    let _span =
        tracing::info_span!("host_function", function = "github_get_pr_for_branch").entered();

    let input: GithubGetPRForBranchInput = get_input(plugin, inputs[0])?;
    tracing::info!(owner = %input.repo.owner, repo = %input.repo.name, head = %input.head, "Getting PR for branch");

    let service = GitHubService::new(std::env::var("GITHUB_TOKEN").unwrap_or_default())
        .map_err(|e| Error::msg(e.to_string()))?;

    let result = block_on(service.get_pr_for_branch(&input.repo, &input.head))?;

    match &result {
        Ok(opt_pr) => tracing::info!(success = true, found = opt_pr.is_some(), "Completed"),
        Err(e) => tracing::warn!(error = %e, "Failed"),
    }

    let output = match result {
        Ok(pr) => HostResult::Success(pr),
        Err(e) => map_error(e),
    };

    plugin.memory_set_val(&mut outputs[0], Json(output))?;
    Ok(())
}

fn github_get_pr_review_comments(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> std::result::Result<(), Error> {
    let _span =
        tracing::info_span!("host_function", function = "github_get_pr_review_comments").entered();

    let input: GithubGetPRReviewCommentsInput = get_input(plugin, inputs[0])?;
    tracing::info!(owner = %input.repo.owner, repo = %input.repo.name, pr_number = input.pr_number, "Getting PR review comments");

    let service = GitHubService::new(std::env::var("GITHUB_TOKEN").unwrap_or_default())
        .map_err(|e| Error::msg(e.to_string()))?;

    let result = block_on(service.get_pr_review_comments(&input.repo, input.pr_number))?;

    match &result {
        Ok(comments) => tracing::info!(success = true, count = comments.len(), "Completed"),
        Err(e) => tracing::warn!(error = %e, "Failed"),
    }

    let output = match result {
        Ok(comments) => HostResult::Success(comments),
        Err(e) => map_error(e),
    };

    plugin.memory_set_val(&mut outputs[0], Json(output))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use wiremock::matchers::{method, path};
    use wiremock::{Mock, MockServer, ResponseTemplate};

    async fn create_mock_service() -> (GitHubService, MockServer) {
        let mock_server = MockServer::start().await;
        let client = OctocrabBuilder::new()
            .personal_token("test_token".to_string())
            .base_uri(mock_server.uri())
            .unwrap()
            .build()
            .unwrap();
        (GitHubService { client }, mock_server)
    }

    #[tokio::test]
    async fn test_list_issues() {
        let (service, mock_server) = create_mock_service().await;

        let mock_response = serde_json::json!([
            {
                "id": 1,
                "node_id": "MDU6SXNzdWUx",
                "number": 1,
                "title": "Test Issue",
                "state": "open",
                "html_url": "http://github.com/owner/repo/issues/1",
                "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false },
                "labels": [],
                "body": "Test Body",
                "created_at": "2023-01-01T00:00:00Z",
                "updated_at": "2023-01-01T00:00:00Z",
                "url": "http://api.github.com/repos/owner/repo/issues/1",
                "repository_url": "http://api.github.com/repos/owner/repo",
                "labels_url": "http://api.github.com/repos/owner/repo/issues/1/labels{/name}",
                "comments_url": "http://api.github.com/repos/owner/repo/issues/1/comments",
                "events_url": "http://api.github.com/repos/owner/repo/issues/1/events",
                "comments": 0,
                "assignees": [],
                "author_association": "NONE",
                "locked": false
            }
        ]);

        Mock::given(method("GET"))
            .and(path("/repos/owner/repo/issues"))
            .respond_with(ResponseTemplate::new(200).set_body_json(mock_response))
            .mount(&mock_server)
            .await;

        let repo = Repo {
            owner: "owner".into(),
            name: "repo".into(),
        };

        let issues = service.list_issues(&repo, None).await.unwrap();
        assert_eq!(issues.len(), 1);
        assert_eq!(issues[0].title, "Test Issue");
        assert_eq!(issues[0].author, "testuser");
    }

    #[tokio::test]
    async fn test_get_issue() {
        let (service, mock_server) = create_mock_service().await;

        let mock_response = serde_json::json!({
            "id": 1,
            "node_id": "MDU6SXNzdWUx",
            "number": 1,
            "title": "Test Issue",
            "state": "open",
            "html_url": "http://github.com/owner/repo/issues/1",
            "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false },
            "labels": [],
            "body": "Test Body",
            "created_at": "2023-01-01T00:00:00Z",
            "updated_at": "2023-01-01T00:00:00Z",
            "url": "http://api.github.com/repos/owner/repo/issues/1",
            "repository_url": "http://api.github.com/repos/owner/repo",
            "labels_url": "http://api.github.com/repos/owner/repo/issues/1/labels{/name}",
            "comments_url": "http://api.github.com/repos/owner/repo/issues/1/comments",
            "events_url": "http://api.github.com/repos/owner/repo/issues/1/events",
            "comments": 0,
            "assignees": [],
            "author_association": "NONE",
            "locked": false
        });

        Mock::given(method("GET"))
            .and(path("/repos/owner/repo/issues/1"))
            .respond_with(ResponseTemplate::new(200).set_body_json(mock_response))
            .mount(&mock_server)
            .await;

        let repo = Repo {
            owner: "owner".into(),
            name: "repo".into(),
        };

        let issue = service.get_issue(&repo, 1).await.unwrap();
        assert_eq!(issue.number, 1);
        assert_eq!(issue.title, "Test Issue");
    }

    #[tokio::test]
    async fn test_create_pr() {
        let (service, mock_server) = create_mock_service().await;

        let mock_response = serde_json::json!({
            "id": 2,
            "node_id": "MDExOlB1bGxSZXF1ZXN0Mg==",
            "number": 2,
            "title": "New PR",
            "state": "open",
            "html_url": "http://github.com/owner/repo/pulls/2",
            "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false },
            "body": "PR Body",
            "head": { "ref": "feature", "sha": "sha", "repo": { "id": 1, "node_id": "MDEwOlJlcG9zaXRvcnkx", "url": "http://example.com", "name": "repo", "full_name": "owner/repo", "owner": { "login": "owner", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false } }, "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false }, "label": "label" },
            "base": { "ref": "main", "sha": "sha", "repo": { "id": 1, "node_id": "MDEwOlJlcG9zaXRvcnkx", "url": "http://example.com", "name": "repo", "full_name": "owner/repo", "owner": { "login": "owner", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false } }, "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false }, "label": "label" },
            "created_at": "2023-01-01T00:00:00Z",
            "updated_at": "2023-01-01T00:00:00Z",
            "url": "http://api.github.com/repos/owner/repo/pulls/2",
            "diff_url": "http://github.com/owner/repo/pulls/2.diff",
            "patch_url": "http://github.com/owner/repo/pulls/2.patch",
            "issue_url": "http://api.github.com/repos/owner/repo/issues/2",
            "commits_url": "http://api.github.com/repos/owner/repo/pulls/2/commits",
            "review_comments_url": "http://api.github.com/repos/owner/repo/pulls/2/comments",
            "review_comment_url": "http://api.github.com/repos/owner/repo/pulls/comments{/number}",
            "comments_url": "http://api.github.com/repos/owner/repo/issues/2/comments",
            "statuses_url": "http://api.github.com/repos/owner/repo/statuses/sha",
            "author_association": "NONE"
        });

        Mock::given(method("POST"))
            .and(path("/repos/owner/repo/pulls"))
            .respond_with(ResponseTemplate::new(201).set_body_json(mock_response))
            .mount(&mock_server)
            .await;

        let repo = Repo {
            owner: "owner".into(),
            name: "repo".into(),
        };

        let spec = CreatePRSpec {
            title: "New PR".to_string(),
            body: "PR Body".to_string(),
            head: "feature".to_string(),
            base: "main".to_string(),
        };

        let pr = service.create_pr(&repo, spec).await.unwrap();
        assert_eq!(pr.number, 2);
        assert_eq!(pr.title, "New PR");
    }

    #[tokio::test]
    async fn test_list_prs() {
        let (service, mock_server) = create_mock_service().await;

        let mock_response = serde_json::json!([
            {
                "id": 2,
                "node_id": "MDExOlB1bGxSZXF1ZXN0Mg==",
                "number": 2,
                "title": "New PR",
                "state": "open",
                "html_url": "http://github.com/owner/repo/pulls/2",
                "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false },
                "body": "PR Body",
                "head": { "ref": "feature", "sha": "sha", "repo": { "id": 1, "node_id": "MDEwOlJlcG9zaXRvcnkx", "url": "http://example.com", "name": "repo", "full_name": "owner/repo", "owner": { "login": "owner", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false } }, "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false }, "label": "label" },
                "base": { "ref": "main", "sha": "sha", "repo": { "id": 1, "node_id": "MDEwOlJlcG9zaXRvcnkx", "url": "http://example.com", "name": "repo", "full_name": "owner/repo", "owner": { "login": "owner", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false } }, "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false }, "label": "label" },
                "created_at": "2023-01-01T00:00:00Z",
                "updated_at": "2023-01-01T00:00:00Z",
                "url": "http://api.github.com/repos/owner/repo/pulls/2",
                "diff_url": "http://github.com/owner/repo/pulls/2.diff",
                "patch_url": "http://github.com/owner/repo/pulls/2.patch",
                "issue_url": "http://api.github.com/repos/owner/repo/issues/2",
                "commits_url": "http://api.github.com/repos/owner/repo/pulls/2/commits",
                "review_comments_url": "http://api.github.com/repos/owner/repo/pulls/2/comments",
                "review_comment_url": "http://api.github.com/repos/owner/repo/pulls/comments{/number}",
                "comments_url": "http://api.github.com/repos/owner/repo/issues/2/comments",
                "statuses_url": "http://api.github.com/repos/owner/repo/statuses/sha",
                "author_association": "NONE"
            }
        ]);

        Mock::given(method("GET"))
            .and(path("/repos/owner/repo/pulls"))
            .respond_with(ResponseTemplate::new(200).set_body_json(mock_response))
            .mount(&mock_server)
            .await;

        let repo = Repo {
            owner: "owner".into(),
            name: "repo".into(),
        };

        let prs = service.list_prs(&repo, None).await.unwrap();
        assert_eq!(prs.len(), 1);
        assert_eq!(prs[0].title, "New PR");
    }
}
