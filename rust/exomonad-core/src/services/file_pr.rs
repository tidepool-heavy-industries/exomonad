// File PR service - creates/updates GitHub PRs using GitHub API via octocrab
//
// Replaces `gh` CLI subprocess calls with octocrab typed API.

use crate::domain::{BranchName, PRNumber};
use crate::services::git;
use crate::services::git_worktree::GitWorktreeService;
use crate::services::github::{build_octocrab, map_octo_err};
use crate::services::repo;
use anyhow::{Context, Result};
use octocrab::params;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tracing::{info, warn};

use super::tmux_events;

// ============================================================================
// Types
// ============================================================================

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct FilePRInput {
    pub title: String,
    pub body: String,
    pub base_branch: Option<BranchName>,
    pub working_dir: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct FilePROutput {
    pub pr_url: String,
    pub pr_number: PRNumber,
    pub head_branch: BranchName,
    pub base_branch: BranchName,
    pub created: bool,
}

/// Parsed from GitHub API response.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct GhPr {
    number: u64,
    url: String,
    head_ref_name: BranchName,
    base_ref_name: BranchName,
}

/// Structured errors for file_pr operations.
#[derive(Debug, thiserror::Error)]
enum FilePrError {
    #[error("push failed: {0}")]
    Push(String),

    #[error("GitHub PR create failed: {0}")]
    Create(String),

    #[error("GitHub PR edit failed: {0}")]
    Update(String),

    #[error("GitHub PR list failed: {0}")]
    List(String),
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
// Octocrab operations
// ============================================================================

async fn find_existing_pr(
    octo: &octocrab::Octocrab,
    owner: &str,
    repo: &str,
    head_branch: &str,
) -> Result<Option<GhPr>, FilePrError> {
    let page = octo
        .pulls(owner, repo)
        .list()
        .head(head_branch)
        .state(params::State::Open)
        .per_page(1)
        .send()
        .await
        .map_err(|e| FilePrError::List(map_octo_err(e)))?;

    let pr = page.into_iter().next().map(|p| GhPr {
        number: p.number,
        url: p.html_url.map(|u| u.to_string()).unwrap_or_default(),
        head_ref_name: BranchName::from(p.head.ref_field.as_str()),
        base_ref_name: BranchName::from(p.base.ref_field.as_str()),
    });
    Ok(pr)
}

async fn update_pr(
    octo: &octocrab::Octocrab,
    owner: &str,
    repo: &str,
    number: PRNumber,
    title: &str,
    body: &str,
) -> Result<(), FilePrError> {
    octo.pulls(owner, repo)
        .update(number.as_u64())
        .title(title)
        .body(body)
        .send()
        .await
        .map_err(|e| FilePrError::Update(map_octo_err(e)))?;
    Ok(())
}

async fn create_pr(
    octo: &octocrab::Octocrab,
    owner: &str,
    repo: &str,
    title: &str,
    body: &str,
    base: &str,
    head: &str,
) -> Result<GhPr, FilePrError> {
    let pr = octo
        .pulls(owner, repo)
        .create(title, head, base)
        .body(body)
        .send()
        .await
        .map_err(|e| FilePrError::Create(map_octo_err(e)))?;

    Ok(GhPr {
        number: pr.number,
        url: pr.html_url.map(|u| u.to_string()).unwrap_or_default(),
        head_ref_name: BranchName::from(pr.head.ref_field.as_str()),
        base_ref_name: BranchName::from(pr.base.ref_field.as_str()),
    })
}

// ============================================================================
// Main implementation
// ============================================================================

/// File a PR using GitHub API. Pushes the branch, creates or updates the PR.
pub async fn file_pr_async(
    input: &FilePRInput,
    git_wt: Arc<GitWorktreeService>,
) -> Result<FilePROutput> {
    let dir = input.working_dir.as_deref().unwrap_or(".");
    let octo = build_octocrab()?;
    let repo_info = repo::get_repo_info(dir).await?;

    // Get branch from the agent's working directory, not server CWD
    let dir_path = std::path::PathBuf::from(dir);
    let git_wt_clone = git_wt.clone();
    let head = tokio::task::spawn_blocking(move || git_wt_clone.get_workspace_bookmark(&dir_path))
        .await
        .context("spawn_blocking failed")?
        .context("Failed to get workspace bookmark")?
        .ok_or_else(|| anyhow::anyhow!("No bookmark found for workspace at {}", dir))?;

    let base = BranchName::from(
        detect_base_branch(&head, input.base_branch.as_ref().map(|b| b.as_str())).as_str(),
    );

    info!("[FilePR] head={} base={} dir={}", head, base, dir);

    // Push first
    {
        let dir_path = std::path::PathBuf::from(dir);
        let bookmark = BranchName::from(head.as_str());
        let git_wt_clone = git_wt.clone();
        tokio::task::spawn_blocking(move || git_wt_clone.push_bookmark(&dir_path, &bookmark))
            .await
            .context("spawn_blocking failed")?
            .map_err(|e| FilePrError::Push(e.to_string()))?;
        info!("[FilePR] Pushed bookmark: {}", head);
    }

    // Check for existing PR
    let existing = find_existing_pr(&octo, &repo_info.owner, &repo_info.repo, &head).await?;
    if let Some(pr) = existing {
        let pr_number = PRNumber::new(pr.number);
        info!("[FilePR] Updating existing PR #{}", pr_number);
        update_pr(
            &octo,
            &repo_info.owner,
            &repo_info.repo,
            pr_number,
            &input.title,
            &input.body,
        )
        .await?;
        info!("[FilePR] Updated PR #{}: {}", pr_number, pr.url);
        return Ok(FilePROutput {
            pr_url: pr.url,
            pr_number,
            head_branch: pr.head_ref_name,
            base_branch: pr.base_ref_name,
            created: false,
        });
    }

    // Create new PR
    info!("[FilePR] Creating PR: {}", input.title);
    let pr = create_pr(
        &octo,
        &repo_info.owner,
        &repo_info.repo,
        &input.title,
        &input.body,
        base.as_str(),
        &head,
    )
    .await?;

    // Emit pr:filed event (only if in tmux session)
    if let Ok(session) = std::env::var("EXOMONAD_TMUX_SESSION") {
        if let Some(agent_id_str) = git::extract_agent_id(&head) {
            match crate::ui_protocol::AgentId::try_from(agent_id_str) {
                Ok(agent_id) => {
                    let event = crate::ui_protocol::AgentEvent::PrFiled {
                        agent_id,
                        pr_number: PRNumber::new(pr.number),
                        timestamp: tmux_events::now_iso8601(),
                    };
                    if let Err(e) = tmux_events::emit_event(&session, &event) {
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
        pr_number: PRNumber::new(pr.number),
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

    #[tokio::test]
    async fn test_file_pr_async_no_git_repo() -> Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let git_wt = Arc::new(GitWorktreeService::new(temp_dir.path().to_path_buf()));
        let input = FilePRInput {
            title: "Test PR".to_string(),
            body: "Test Body".to_string(),
            base_branch: None,
            working_dir: Some(temp_dir.path().to_string_lossy().to_string()),
        };

        let result = file_pr_async(&input, git_wt).await;
        assert!(result.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_file_pr_async_head_detection() -> Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let dir = temp_dir.path();

        // 1. Init git repo
        use std::process::Command;
        Command::new("git").args(["init", "-b", "main"]).current_dir(dir).status()?;
        Command::new("git").args(["config", "user.email", "test@example.com"]).current_dir(dir).status()?;
        Command::new("git").args(["config", "user.name", "Test"]).current_dir(dir).status()?;
        std::fs::write(dir.join("README.md"), "test")?;
        Command::new("git").args(["add", "README.md"]).current_dir(dir).status()?;
        Command::new("git").args(["commit", "-m", "init"]).current_dir(dir).status()?;

        // 2. Create a feature branch
        Command::new("git").args(["checkout", "-b", "feature-branch"]).current_dir(dir).status()?;
        let git_wt = Arc::new(GitWorktreeService::new(dir.to_path_buf()));

        let input = FilePRInput {
            title: "Test PR".to_string(),
            body: "Test Body".to_string(),
            base_branch: None,
            working_dir: Some(dir.to_string_lossy().to_string()),
        };

        // Since we didn't setup an origin remote, push will fail.
        let result = file_pr_async(&input, git_wt).await;

        if let Err(ref e) = result {
            let err_msg = e.to_string();
            assert!(
                err_msg.contains("push failed"),
                "Expected push failed error, got: {}",
                err_msg
            );
        } else {
            panic!("Expected file_pr_async to fail on push, but it succeeded?!");
        }

        Ok(())
    }
}
