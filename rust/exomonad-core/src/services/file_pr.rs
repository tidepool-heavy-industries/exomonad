// File PR service - creates/updates GitHub PRs using `gh` CLI
//
// Shells out to `gh` for PR operations. `gh` handles auth, fork awareness,
// and provides clear error messages. Octocrab stays for other GitHub operations.

use crate::services::git;
use anyhow::{Context, Result};
use duct::cmd;
use serde::{Deserialize, Serialize};
use tracing::{info, warn};

use super::zellij_events;

// ============================================================================
// Types
// ============================================================================

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct FilePRInput {
    pub title: String,
    pub body: String,
    pub base_branch: Option<String>,
    pub working_dir: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct FilePROutput {
    pub pr_url: String,
    pub pr_number: u64,
    pub head_branch: String,
    pub base_branch: String,
    pub created: bool,
}

/// Parsed from `gh pr list/create/view --json` output.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct GhPr {
    number: u64,
    url: String,
    head_ref_name: String,
    base_ref_name: String,
}

/// Structured errors for file_pr operations.
#[derive(Debug, thiserror::Error)]
enum FilePrError {
    #[error("git push failed: {0}")]
    PushFailed(String),

    #[error("gh pr create failed: {0}")]
    CreateFailed(String),

    #[error("gh pr edit failed: {0}")]
    UpdateFailed(String),

    #[error("gh pr list failed: {0}")]
    ListFailed(String),
}

// ============================================================================
// Git Helpers
// ============================================================================

/// Detect the base branch for a PR.
/// Priority: explicit > convention (strip last / segment) > convention (strip last . segment) > "main"
fn detect_base_branch(head: &str, explicit: Option<&str>) -> String {
    if let Some(base) = explicit {
        if !base.is_empty() {
            return base.to_string();
        }
    }
    // Convention 1: branch "parent/child" targets "parent" (Standard Git)
    if let Some(pos) = head.rfind('/') {
        return head[..pos].to_string();
    }
    // Convention 2: branch "parent.child" targets "parent" (ExoMonad Subtrees)
    if let Some(pos) = head.rfind('.') {
        return head[..pos].to_string();
    }
    "main".to_string()
}

// ============================================================================
// `gh` CLI operations
// ============================================================================

fn push_branch(dir: &str) -> Result<(), FilePrError> {
    let output = cmd!("git", "push", "-u", "origin", "HEAD")
        .dir(dir)
        .stderr_to_stdout()
        .read()
        .map_err(|e| FilePrError::PushFailed(e.to_string()))?;
    info!("[FilePR] Push: {}", output);
    Ok(())
}

fn find_existing_pr(head_branch: &str, dir: &str) -> Result<Option<GhPr>, FilePrError> {
    let json = cmd!(
        "gh",
        "pr",
        "list",
        "--head",
        head_branch,
        "--state",
        "open",
        "--json",
        "number,url,headRefName,baseRefName",
        "--limit",
        "1"
    )
    .dir(dir)
    .read()
    .map_err(|e| FilePrError::ListFailed(e.to_string()))?;

    let prs: Vec<GhPr> = serde_json::from_str(&json)
        .map_err(|e| FilePrError::ListFailed(format!("JSON parse: {e}")))?;
    Ok(prs.into_iter().next())
}

fn update_pr(number: u64, title: &str, body: &str, dir: &str) -> Result<(), FilePrError> {
    cmd!(
        "gh",
        "pr",
        "edit",
        number.to_string(),
        "--title",
        title,
        "--body",
        body
    )
    .dir(dir)
    .read()
    .map_err(|e| FilePrError::UpdateFailed(e.to_string()))?;
    Ok(())
}

fn create_pr(
    title: &str,
    body: &str,
    base: &str,
    head: &str,
    dir: &str,
) -> Result<GhPr, FilePrError> {
    // gh pr create outputs the PR URL to stdout on success (no --json support)
    let url = cmd!("gh", "pr", "create", "--title", title, "--body", body, "--base", base)
        .dir(dir)
        .read()
        .map_err(|e| FilePrError::CreateFailed(e.to_string()))?;
    info!("[FilePR] Created PR: {}", url.trim());

    // Fetch structured PR data via `gh pr view` using the branch name (stable, no URL parsing)
    let view_json = cmd!(
        "gh",
        "pr",
        "view",
        head,
        "--json",
        "number,url,headRefName,baseRefName"
    )
    .dir(dir)
    .read()
    .map_err(|e| FilePrError::CreateFailed(format!("gh pr view after create: {e}")))?;

    serde_json::from_str(&view_json)
        .map_err(|e| FilePrError::CreateFailed(format!("JSON parse: {e}")))
}

// ============================================================================
// Main implementation
// ============================================================================

/// File a PR using `gh` CLI. Pushes the branch, creates or updates the PR.
pub async fn file_pr_async(input: &FilePRInput) -> Result<FilePROutput> {
    let dir = input.working_dir.as_deref().unwrap_or(".");

    // Get branch from the agent's working directory, not server CWD
    let head = cmd!("git", "branch", "--show-current")
        .dir(dir)
        .read()
        .context("Failed to get current branch")?;
    let head = head.trim().to_string();
    if head.is_empty() {
        anyhow::bail!("Not on a branch (detached HEAD?) in {}", dir);
    }

    let base = detect_base_branch(&head, input.base_branch.as_deref());

    info!("[FilePR] head={} base={} dir={}", head, base, dir);

    // Push first
    push_branch(dir)?;

    // Check for existing PR
    if let Some(pr) = find_existing_pr(&head, dir)? {
        info!("[FilePR] Updating existing PR #{}", pr.number);
        update_pr(pr.number, &input.title, &input.body, dir)?;
        info!("[FilePR] Updated PR #{}: {}", pr.number, pr.url);
        return Ok(FilePROutput {
            pr_url: pr.url,
            pr_number: pr.number,
            head_branch: pr.head_ref_name,
            base_branch: pr.base_ref_name,
            created: false,
        });
    }

    // Create new PR
    info!("[FilePR] Creating PR: {}", input.title);
    let pr = create_pr(&input.title, &input.body, &base, &head, dir)?;

    // Emit pr:filed event (only if in Zellij session)
    if let Ok(session) = std::env::var("ZELLIJ_SESSION_NAME") {
        if let Some(agent_id_str) = git::extract_agent_id(&head) {
            match crate::ui_protocol::AgentId::try_from(agent_id_str) {
                Ok(agent_id) => {
                    let event = crate::ui_protocol::AgentEvent::PrFiled {
                        agent_id,
                        pr_number: pr.number,
                        timestamp: zellij_events::now_iso8601(),
                    };
                    if let Err(e) = zellij_events::emit_event(&session, &event) {
                        warn!("Failed to emit pr:filed event: {}", e);
                    }
                }
                Err(e) => {
                    warn!(
                        "Invalid agent_id in branch '{}', skipping event: {}",
                        head, e
                    );
                }
            }
        }
    }

    Ok(FilePROutput {
        pr_url: pr.url,
        pr_number: pr.number,
        head_branch: pr.head_ref_name,
        base_branch: pr.base_ref_name,
        created: true,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_base_branch_explicit() {
        assert_eq!(
            detect_base_branch("feature/my-work", Some("develop")),
            "develop"
        );
    }

    #[test]
    fn test_detect_base_branch_convention() {
        // Slash convention
        assert_eq!(
            detect_base_branch("main/subtask/leaf", None),
            "main/subtask"
        );
        assert_eq!(detect_base_branch("feature/my-work", None), "feature");

        // Dot convention (ExoMonad subtrees)
        assert_eq!(detect_base_branch("main.my-feature", None), "main");
        assert_eq!(
            detect_base_branch("parent.child.grandchild", None),
            "parent.child"
        );
    }

    #[test]
    fn test_branch_naming_roundtrip_root() {
        let branch = format!("{}.{}", "main", "auth-service");
        assert_eq!(detect_base_branch(&branch, None), "main");
    }

    #[test]
    fn test_branch_naming_roundtrip_nested() {
        let branch = format!("{}.{}", "main.auth-service", "middleware");
        assert_eq!(detect_base_branch(&branch, None), "main.auth-service");
    }

    #[test]
    fn test_branch_naming_roundtrip_deep() {
        assert_eq!(
            detect_base_branch("main.feature.sub.leaf", None),
            "main.feature.sub"
        );
    }

    #[test]
    fn test_detect_base_branch_no_slash() {
        assert_eq!(detect_base_branch("my-branch", None), "main");
    }

    #[test]
    fn test_detect_base_branch_empty_explicit() {
        assert_eq!(detect_base_branch("feature/work", Some("")), "feature");
    }

    #[test]
    fn test_detect_base_branch_depth_4() {
        assert_eq!(
            detect_base_branch("main.core-eval.optimize.inline.inline-coalg", None),
            "main.core-eval.optimize.inline"
        );
        assert_eq!(detect_base_branch("main.a.b.c.d.e", None), "main.a.b.c.d");
    }
}
