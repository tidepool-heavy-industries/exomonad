// File PR service - creates/updates GitHub PRs using octocrab
//
// Uses octocrab (GitHub API) for idempotent PR management.
// Returns immediately with PR URL and number.

use crate::common::{ErrorCode, FFIBoundary, HostResult};
use crate::services::git::{self, parse_github_url};
use anyhow::{Context, Result};
use duct::cmd;
use exomonad_services::{ExternalService, GitHubService};
use exomonad_shared::protocol::{ServiceRequest, ServiceResponse};
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use extism_convert::Json;
use serde::{Deserialize, Serialize};
use tracing::{debug, info, warn};

use super::zellij_events;

// ============================================================================
// Types
// ============================================================================

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct FilePRInput {
    pub title: String,
    pub body: String,
}

impl FFIBoundary for FilePRInput {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FilePROutput {
    pub pr_url: String,
    pub pr_number: u64,
    pub head_branch: String,
    pub base_branch: String,
    pub created: bool, // true if newly created, false if already existed
}

impl FFIBoundary for FilePROutput {}

// ============================================================================
// Host Output Wrapper
// ============================================================================

fn map_error(e: anyhow::Error) -> HostResult<FilePROutput> {
    let msg = e.to_string();
    let code = if msg.contains("not on a branch")
        || msg.contains("not a git repository")
        || msg.contains("no remote")
        || msg.contains("No remote")
    {
        ErrorCode::GitError
    } else if msg.contains("GITHUB_TOKEN")
        || msg.contains("not logged")
        || msg.contains("unauthorized")
        || msg.contains("401")
    {
        ErrorCode::NotAuthenticated
    } else if msg.contains("already exists") || msg.contains("422") {
        ErrorCode::AlreadyExists
    } else {
        ErrorCode::InternalError
    };

    HostResult::error(msg, code, None, None)
}

// ============================================================================
// Git Helpers (synchronous)
// ============================================================================

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

    // Default base branch to main
    let base_branch = "main".to_string();

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

            // Emit pr:filed event
            if let Some(agent_id_str) = git::extract_agent_id(head_branch) {
                match exomonad_ui_protocol::AgentId::try_from(agent_id_str) {
                    Ok(agent_id) => {
                        let event = exomonad_ui_protocol::AgentEvent::PrFiled {
                            agent_id,
                            pr_number,
                            timestamp: zellij_events::now_iso8601(),
                        };
                        if let Err(e) = zellij_events::emit_event(&event) {
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
async fn file_pr_async(input: &FilePRInput) -> Result<FilePROutput> {
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

// ============================================================================
// Host Functions
// ============================================================================

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

pub fn register_host_functions() -> Vec<Function> {
    vec![Function::new(
        "file_pr",
        [ValType::I64],
        [ValType::I64],
        UserData::new(()),
        file_pr_host_fn,
    )
    .with_namespace("env")]
}

fn file_pr_host_fn(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> std::result::Result<(), Error> {
    let _span = tracing::info_span!("host_function", function = "file_pr").entered();
    let input: FilePRInput = get_input(plugin, inputs[0])?;
    tracing::info!(title = %input.title, "Filing PR");

    let result = file_pr(&input);

    match &result {
        Ok(pr) => {
            tracing::info!(success = true, pr_url = %pr.pr_url, created = pr.created, "Completed")
        }
        Err(e) => tracing::warn!(error = %e, "Failed"),
    }

    let output: HostResult<FilePROutput> = match result {
        Ok(val) => HostResult::Success(val),
        Err(e) => map_error(e),
    };

    plugin.memory_set_val(&mut outputs[0], Json(output))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_host_error_codes() {
        let err: HostResult<FilePROutput> = map_error(anyhow::anyhow!("not on a branch"));
        match err {
            HostResult::Error(e) => assert_eq!(e.code, ErrorCode::GitError),
            _ => panic!("Expected error"),
        }

        let err: HostResult<FilePROutput> = map_error(anyhow::anyhow!("GITHUB_TOKEN not set"));
        match err {
            HostResult::Error(e) => assert_eq!(e.code, ErrorCode::NotAuthenticated),
            _ => panic!("Expected error"),
        }
    }

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
}
