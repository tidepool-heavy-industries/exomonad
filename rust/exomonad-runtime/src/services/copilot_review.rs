// Copilot Review service - polls for GitHub Copilot review comments
//
// Uses `gh api` to check for Copilot review comments on a PR.
// Blocks until comments are found or timeout is reached.

use crate::common::{FFIBoundary, HostResult, IntoFFIResult};
use crate::services::git;
use anyhow::{Context, Result};
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use serde::{Deserialize, Serialize};
use std::process::Command;
use std::thread;
use std::time::{Duration, Instant};
use tracing::{debug, info, warn};

use super::zellij_events;

// ============================================================================
// Types
// ============================================================================

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct WaitForCopilotReviewInput {
    pub pr_number: u64,
    #[serde(default = "default_timeout")]
    pub timeout_secs: u64,
    #[serde(default = "default_poll_interval")]
    pub poll_interval_secs: u64,
}

impl FFIBoundary for WaitForCopilotReviewInput {}

fn default_timeout() -> u64 {
    300 // 5 minutes
}

fn default_poll_interval() -> u64 {
    30
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CopilotReviewOutput {
    pub status: String,
    pub comments: Vec<CopilotComment>,
}

impl FFIBoundary for CopilotReviewOutput {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CopilotComment {
    pub path: String,
    pub line: Option<u64>,
    pub body: String,
    pub diff_hunk: Option<String>,
}

impl FFIBoundary for CopilotComment {}

// ============================================================================
// Service
// ============================================================================

/// Get repo owner/name from git remote
fn get_repo_info() -> Result<(String, String)> {
    let output = Command::new("gh")
        .args(["repo", "view", "--json", "owner,name"])
        .output()
        .context("Failed to execute gh repo view")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        anyhow::bail!("Failed to get repo info: {}", stderr.trim());
    }

    #[derive(Deserialize)]
    struct RepoInfo {
        owner: RepoOwner,
        name: String,
    }

    #[derive(Deserialize)]
    struct RepoOwner {
        login: String,
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let info: RepoInfo = serde_json::from_str(&stdout).context("Failed to parse repo info JSON")?;

    Ok((info.owner.login, info.name))
}

/// Fetch PR review comments using gh api, filtering for current commit and unresolved threads
fn fetch_pr_comments(owner: &str, repo: &str, pr_number: u64) -> Result<Vec<CopilotComment>> {
    // 1. Get current PR head SHA and unresolved threads via GraphQL
    // This allows us to filter by both commit SHA and resolution status in one call
    let query = r#"
        query($owner: String!, $repo: String!, $pr: Int!) {
          repository(owner: $owner, name: $repo) {
            pullRequest(number: $pr) {
              headRefOid
              reviewThreads(first: 100) {
                nodes {
                  isResolved
                  comments(first: 100) {
                    nodes {
                      commit { oid }
                      path
                      line
                      body
                      diffHunk
                      author { login }
                    }
                  }
                }
              }
            }
          }
        }
    "#;

    let output = Command::new("gh")
        .args([
            "api",
            "graphql",
            "-f",
            &format!("query={}", query),
            "-f",
            &format!("owner={}", owner),
            "-f",
            &format!("repo={}", repo),
            "-F",
            &format!("pr={}", pr_number),
        ])
        .output()
        .context("Failed to execute gh api graphql for PR comments")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        anyhow::bail!("Failed to fetch PR comments via GraphQL: {}", stderr.trim());
    }

    let stdout = String::from_utf8_lossy(&output.stdout);

    #[derive(Deserialize)]
    struct GraphQLResponse {
        data: GraphQLData,
    }
    #[derive(Deserialize)]
    struct GraphQLData {
        repository: GraphQLRepo,
    }
    #[derive(Deserialize)]
    struct GraphQLRepo {
        #[serde(rename = "pullRequest")]
        pull_request: GraphQLPR,
    }
    #[derive(Deserialize)]
    struct GraphQLPR {
        #[serde(rename = "headRefOid")]
        head_ref_oid: String,
        #[serde(rename = "reviewThreads")]
        review_threads: GraphQLThreads,
    }
    #[derive(Deserialize)]
    struct GraphQLThreads {
        nodes: Vec<GraphQLThread>,
    }
    #[derive(Deserialize)]
    struct GraphQLThread {
        #[serde(rename = "isResolved")]
        is_resolved: bool,
        comments: GraphQLComments,
    }
    #[derive(Deserialize)]
    struct GraphQLComments {
        nodes: Vec<GraphQLComment>,
    }
    #[derive(Deserialize)]
    struct GraphQLComment {
        commit: GraphQLCommit,
        path: String,
        line: Option<u64>,
        body: String,
        #[serde(rename = "diffHunk")]
        diff_hunk: String,
        author: GraphQLAuthor,
    }
    #[derive(Deserialize)]
    struct GraphQLCommit {
        oid: String,
    }
    #[derive(Deserialize)]
    struct GraphQLAuthor {
        login: String,
    }

    let response: GraphQLResponse =
        serde_json::from_str(&stdout).context("Failed to parse GraphQL response")?;

    let head_sha = response.data.repository.pull_request.head_ref_oid;
    let mut copilot_comments = Vec::new();

    info!("[CopilotReview] PR head SHA: {}", head_sha);

    for thread in response.data.repository.pull_request.review_threads.nodes {
        info!("[CopilotReview] Thread isResolved: {}", thread.is_resolved);
        
        if thread.is_resolved {
            continue;
        }

        for comment in thread.comments.nodes {
            let author = &comment.author.login;
            let commit_oid = &comment.commit.oid;
            let is_copilot = is_copilot_comment(author, None);
            let is_current = commit_oid == &head_sha;

            info!(
                "[CopilotReview] Comment by {}: is_copilot={}, is_current={} (commit={})",
                author, is_copilot, is_current, commit_oid
            );

            // Filter for Copilot comments on the latest commit
            if is_copilot && is_current {
                info!("[CopilotReview] Adding comment to list");
                copilot_comments.push(CopilotComment {
                    path: comment.path,
                    line: comment.line,
                    body: comment.body,
                    diff_hunk: Some(comment.diff_hunk),
                });
            }
        }
    }

    info!(
        "[CopilotReview] Found {} unresolved current Copilot comments",
        copilot_comments.len()
    );

    Ok(copilot_comments)
}

/// Check if a comment is from Copilot
fn is_copilot_comment(login: &str, user_type: Option<&str>) -> bool {
    let login_lower = login.to_lowercase();

    // GitHub Copilot code review appears as "copilot" user
    if login_lower.contains("copilot") {
        return true;
    }

    // Also check for Bot type with copilot-related names
    if user_type == Some("Bot") && login_lower.contains("copilot") {
        return true;
    }

    // GitHub's AI code review (if using different branding)
    if login_lower == "github-advanced-security[bot]" {
        return true;
    }

    false
}

/// Also check PR reviews (not just inline comments)
fn fetch_pr_reviews(owner: &str, repo: &str, pr_number: u64) -> Result<bool> {
    let endpoint = format!("/repos/{}/{}/pulls/{}/reviews", owner, repo, pr_number);

    debug!("[CopilotReview] Fetching reviews from: {}", endpoint);

    let output = Command::new("gh")
        .args(["api", &endpoint])
        .output()
        .context("Failed to execute gh api for PR reviews")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        anyhow::bail!("Failed to fetch PR reviews: {}", stderr.trim());
    }

    let stdout = String::from_utf8_lossy(&output.stdout);

    #[derive(Deserialize)]
    struct PRReview {
        user: ReviewUser,
        #[allow(dead_code)]
        state: String,
    }

    #[derive(Deserialize)]
    struct ReviewUser {
        login: String,
        #[serde(rename = "type")]
        user_type: Option<String>,
    }

    let reviews: Vec<PRReview> =
        serde_json::from_str(&stdout).context("Failed to parse PR reviews JSON")?;

    // Check if any Copilot review exists (any state - APPROVED, CHANGES_REQUESTED, COMMENTED)
    let has_copilot_review = reviews
        .iter()
        .any(|r| is_copilot_comment(&r.user.login, r.user.user_type.as_deref()));

    if has_copilot_review {
        debug!("[CopilotReview] Found Copilot review");
    }

    Ok(has_copilot_review)
}

/// Main wait_for_copilot_review implementation
pub fn wait_for_copilot_review(input: &WaitForCopilotReviewInput) -> Result<CopilotReviewOutput> {
    info!(
        "[CopilotReview] Waiting for Copilot review on PR #{} (timeout: {}s, poll: {}s)",
        input.pr_number, input.timeout_secs, input.poll_interval_secs
    );

    let (owner, repo) = get_repo_info()?;
    info!("[CopilotReview] Repository: {}/{}", owner, repo);

    let deadline = Instant::now() + Duration::from_secs(input.timeout_secs);
    let poll_interval = Duration::from_secs(input.poll_interval_secs);

    loop {
        // Check for inline comments
        let comments = fetch_pr_comments(&owner, &repo, input.pr_number)?;

        if !comments.is_empty() {
            info!("[CopilotReview] Found {} active Copilot comments", comments.len());

            // Emit copilot:reviewed event
            if let Ok(branch) = git::get_current_branch() {
                if let Some(agent_id_str) = git::extract_agent_id(&branch) {
                    match exomonad_ui_protocol::AgentId::try_from(agent_id_str) {
                        Ok(agent_id) => {
                            let event = exomonad_ui_protocol::AgentEvent::CopilotReviewed {
                                agent_id,
                                comment_count: comments.len() as u32,
                                timestamp: zellij_events::now_iso8601(),
                            };
                            if let Err(e) = zellij_events::emit_event(&event) {
                                warn!("Failed to emit copilot:reviewed event: {}", e);
                            }
                        }
                        Err(e) => {
                            warn!(
                                "Invalid agent_id in branch '{}', skipping event: {}",
                                branch, e
                            );
                        }
                    }
                }
            }

            return Ok(CopilotReviewOutput {
                status: "reviewed".to_string(),
                comments,
            });
        }

        // Also check for review (without inline comments)
        if fetch_pr_reviews(&owner, &repo, input.pr_number)? {
            info!("[CopilotReview] Found Copilot review (no inline comments)");

            // Emit copilot:reviewed event with 0 comments
            if let Ok(branch) = git::get_current_branch() {
                if let Some(agent_id_str) = git::extract_agent_id(&branch) {
                    match exomonad_ui_protocol::AgentId::try_from(agent_id_str) {
                        Ok(agent_id) => {
                            let event = exomonad_ui_protocol::AgentEvent::CopilotReviewed {
                                agent_id,
                                comment_count: 0,
                                timestamp: zellij_events::now_iso8601(),
                            };
                            if let Err(e) = zellij_events::emit_event(&event) {
                                warn!("Failed to emit copilot:reviewed event: {}", e);
                            }
                        }
                        Err(e) => {
                            warn!(
                                "Invalid agent_id in branch '{}', skipping event: {}",
                                branch, e
                            );
                        }
                    }
                }
            }

            return Ok(CopilotReviewOutput {
                status: "reviewed".to_string(),
                comments: vec![],
            });
        }

        // Check timeout
        if Instant::now() > deadline {
            warn!("[CopilotReview] Timeout reached waiting for Copilot review");
            return Ok(CopilotReviewOutput {
                status: "timeout".to_string(),
                comments: vec![],
            });
        }

        debug!(
            "[CopilotReview] No Copilot review yet, sleeping for {}s",
            input.poll_interval_secs
        );
        thread::sleep(poll_interval);
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

fn set_output<T: Serialize>(
    plugin: &mut CurrentPlugin,
    data: &T,
) -> std::result::Result<Val, Error> {
    let json = serde_json::to_vec(data)?;
    let handle = plugin.memory_new(json)?;
    Ok(plugin.memory_to_val(handle))
}

pub fn register_host_functions() -> Vec<Function> {
    vec![Function::new(
        "wait_for_copilot_review",
        [ValType::I64],
        [ValType::I64],
        UserData::new(()),
        wait_for_copilot_review_host_fn,
    )
    .with_namespace("env")]
}

fn wait_for_copilot_review_host_fn(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> std::result::Result<(), Error> {
    let _span =
        tracing::info_span!("host_function", function = "wait_for_copilot_review").entered();
    let input: WaitForCopilotReviewInput = get_input(plugin, inputs[0])?;
    tracing::info!(
        pr_number = input.pr_number,
        timeout = input.timeout_secs,
        "Waiting for Copilot review"
    );

    let result = wait_for_copilot_review(&input);

    match &result {
        Ok(out) => {
            tracing::info!(success = true, status = %out.status, comment_count = out.comments.len(), "Completed")
        }
        Err(e) => tracing::warn!(error = %e, "Failed"),
    }

    let output: HostResult<CopilotReviewOutput> = result.into_ffi_result();

    outputs[0] = set_output(plugin, &output)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_copilot_comment() {
        assert!(is_copilot_comment("copilot", None));
        assert!(is_copilot_comment("copilot[bot]", Some("Bot")));
        assert!(is_copilot_comment("Copilot", None));
        assert!(is_copilot_comment(
            "github-advanced-security[bot]",
            Some("Bot")
        ));
        assert!(!is_copilot_comment("octocat", None));
        assert!(!is_copilot_comment("github-actions[bot]", Some("Bot")));
    }

    #[test]
    fn test_default_values() {
        assert_eq!(default_timeout(), 300);
        assert_eq!(default_poll_interval(), 30);
    }
}
