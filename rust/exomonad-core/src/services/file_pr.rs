// File PR service - creates/updates GitHub PRs using octocrab
//
// Uses octocrab (GitHub API) for idempotent PR management.
// Returns immediately with PR URL and number.

use super::external::{ExternalGitHubService as GitHubService, ExternalService};
use crate::protocol::{ServiceRequest, ServiceResponse};
use crate::services::git::{self, parse_github_url};
use anyhow::{Context, Result};
use duct::cmd;
use serde::{Deserialize, Serialize};
use tracing::{debug, info, warn};

use super::zellij_events;

// ============================================================================
// Types
// ============================================================================

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct FilePRInput {
    pub title: String,
    pub body: String,
    pub base_branch: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct FilePROutput {
    pub pr_url: String,
    pub pr_number: u64,
    pub head_branch: String,
    pub base_branch: String,
    pub created: bool, // true if newly created, false if already existed
}

// ============================================================================
// Git Helpers (synchronous)
// ============================================================================

/// Detect the base branch for a PR.
/// Priority: explicit > convention (strip last / segment) > "main"
fn detect_base_branch(head: &str, explicit: Option<&str>) -> String {
    if let Some(base) = explicit {
        if !base.is_empty() {
            return base.to_string();
        }
    }
    // Convention: branch "parent/child" targets "parent"
    if let Some(pos) = head.rfind('/') {
        head[..pos].to_string()
    } else {
        "main".to_string()
    }
}

/// Get the git remote URL for origin.
fn get_remote_url() -> Result<String> {
    let output = cmd!("git", "remote", "get-url", "origin")
        .read()
        .context("Failed to get git remote URL")?;
    Ok(output.trim().to_string())
}

/// Get owner and repo from the git remote.
fn get_owner_repo() -> Result<(String, String)> {
    let remote_url = get_remote_url()?;
    parse_github_url(&remote_url)
        .ok_or_else(|| anyhow::anyhow!("Failed to parse GitHub URL from remote: {}", remote_url))
}

// ============================================================================
// Async Implementation (octocrab)
// ============================================================================

/// Check if a PR already exists for the given head branch.
async fn check_existing_pr_async(
    github: &GitHubService,
    owner: &str,
    repo: &str,
    head_branch: &str,
) -> Result<Option<(String, u64, String, String)>> {
    info!(
        "[FilePR] Checking for existing PR for branch: {}",
        head_branch
    );

    // GitHub API expects head in format "owner:branch" for cross-fork PRs,
    // or just "branch" for same-repo PRs. We use "owner:branch" for consistency.
    let head_filter = format!("{}:{}", owner, head_branch);

    let req = ServiceRequest::GitHubListPullRequests {
        owner: owner.into(),
        repo: repo.into(),
        state: Some("open".to_string()),
        limit: Some(10),
        head: Some(head_filter),
    };

    let response = github
        .call(req)
        .await
        .map_err(|e| anyhow::anyhow!("GitHub API error checking for existing PR: {}", e))?;

    match response {
        ServiceResponse::GitHubPullRequests { pull_requests } => {
            if let Some(pr) = pull_requests.first() {
                debug!("[FilePR] Found existing PR: {} (#{}) ", pr.url, pr.number);
                Ok(Some((
                    pr.url.clone(),
                    pr.number as u64,
                    pr.head_ref_name.clone(),
                    pr.base_ref_name.clone(),
                )))
            } else {
                debug!("[FilePR] No existing PR found");
                Ok(None)
            }
        }
        _ => Err(anyhow::anyhow!("Unexpected response type from GitHub API")),
    }
}

/// Create a new PR using octocrab.
async fn create_pr_async(
    github: &GitHubService,
    owner: &str,
    repo: &str,
    head_branch: &str,
    input: &FilePRInput,
) -> Result<FilePROutput> {
    info!("[FilePR] Creating new PR: {}", input.title);
    info!("[FilePR] Head branch: {}", head_branch);

    let base_branch = detect_base_branch(head_branch, input.base_branch.as_deref());
    info!("[FilePR] Base branch: {}", base_branch);

    let req = ServiceRequest::GitHubCreatePR {
        owner: owner.into(),
        repo: repo.into(),
        title: input.title.clone(),
        body: input.body.clone(),
        head: head_branch.to_string(),
        base: base_branch.clone(),
    };

    let response = github
        .call(req)
        .await
        .map_err(|e| anyhow::anyhow!("GitHub API error creating PR: {}", e))?;

    match response {
        ServiceResponse::GitHubPR {
            number,
            url,
            head_ref_name,
            base_ref_name,
            ..
        } => {
            let pr_number = number as u64;
            info!("[FilePR] Created PR: {} (#{}) ", url, pr_number);

            // Emit pr:filed event (only if in Zellij session)
            if let Ok(session) = std::env::var("ZELLIJ_SESSION_NAME") {
                if let Some(agent_id_str) = git::extract_agent_id(head_branch) {
                    match crate::ui_protocol::AgentId::try_from(agent_id_str) {
                        Ok(agent_id) => {
                            let event = crate::ui_protocol::AgentEvent::PrFiled {
                                agent_id,
                                pr_number,
                                timestamp: zellij_events::now_iso8601(),
                            };
                            if let Err(e) = zellij_events::emit_event(&session, &event) {
                                warn!("Failed to emit pr:filed event: {}", e);
                            }
                        }
                        Err(e) => {
                            warn!(
                                "Invalid agent_id in branch '{}', skipping event: {}",
                                head_branch, e
                            );
                        }
                    }
                }
            }

            Ok(FilePROutput {
                pr_url: url,
                pr_number,
                head_branch: head_ref_name,
                base_branch: base_ref_name,
                created: true,
            })
        }
        _ => Err(anyhow::anyhow!("Unexpected response type from GitHub API")),
    }
}

/// Main file_pr async implementation.
pub async fn file_pr_async(input: &FilePRInput) -> Result<FilePROutput> {
    info!("[FilePR] Starting file_pr operation (octocrab)");

    // Get owner/repo from git remote
    let (owner, repo) = get_owner_repo()?;
    info!("[FilePR] Repository: {}/{}", owner, repo);

    // Get current branch
    let head_branch = git::get_current_branch()?;
    info!("[FilePR] Current branch: {}", head_branch);

    // Create GitHub service from environment
    let github = GitHubService::from_env().context("GITHUB_TOKEN not set or invalid")?;

    // First check if PR already exists for current branch
    if let Some((url, number, head, base)) =
        check_existing_pr_async(&github, &owner, &repo, &head_branch).await?
    {
        info!("[FilePR] PR already exists: {} (#{}) ", url, number);
        return Ok(FilePROutput {
            pr_url: url,
            pr_number: number,
            head_branch: head,
            base_branch: base,
            created: false,
        });
    }

    // Create new PR
    create_pr_async(&github, &owner, &repo, &head_branch, input).await
}

// ============================================================================
// Sync wrapper
// ============================================================================

/// Main file_pr implementation (sync wrapper around async).
pub fn file_pr(input: &FilePRInput) -> Result<FilePROutput> {
    // Try to use existing tokio runtime, or create a new one
    match tokio::runtime::Handle::try_current() {
        Ok(handle) => handle.block_on(file_pr_async(input)),
        Err(_) => {
            // Create a new runtime for this call
            let rt = tokio::runtime::Runtime::new().context("Failed to create tokio runtime")?;
            rt.block_on(file_pr_async(input))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_github_url() {
        // SSH URL
        let result = parse_github_url("git@github.com:owner/repo.git");
        assert_eq!(result, Some(("owner".to_string(), "repo".to_string())));

        // HTTPS URL
        let result = parse_github_url("https://github.com/owner/repo.git");
        assert_eq!(result, Some(("owner".to_string(), "repo".to_string())));

        // HTTPS URL without .git
        let result = parse_github_url("https://github.com/owner/repo");
        assert_eq!(result, Some(("owner".to_string(), "repo".to_string())));
    }

    #[test]
    fn test_detect_base_branch_explicit() {
        assert_eq!(
            detect_base_branch("feature/my-work", Some("develop")),
            "develop"
        );
    }

    #[test]
    fn test_detect_base_branch_convention() {
        assert_eq!(
            detect_base_branch("main/subtask/leaf", None),
            "main/subtask"
        );
        assert_eq!(detect_base_branch("feature/my-work", None), "feature");
    }

    #[test]
    fn test_detect_base_branch_no_slash() {
        assert_eq!(detect_base_branch("my-branch", None), "main");
    }

    #[test]
    fn test_detect_base_branch_empty_explicit() {
        assert_eq!(detect_base_branch("feature/work", Some("")), "feature");
    }
}
