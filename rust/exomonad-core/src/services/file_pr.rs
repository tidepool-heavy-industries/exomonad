// File PR service - creates/updates GitHub PRs using GitHub API via octocrab
//
// Replaces `gh` CLI subprocess calls with octocrab typed API.

use crate::domain::{BirthBranch, BranchName, PRNumber};
use crate::services::git;
use crate::services::git_worktree::GitWorktreeService;
use crate::services::github::{build_octocrab, map_octo_err, GitHubClient};
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
#[derive(Debug)]
struct GhPr {
    number: u64,
    url: String,
    head_ref_name: BranchName,
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

/// Resolve the base branch for a PR using the `BirthBranch` domain type.
///
/// Priority: explicit override > `BirthBranch::parent()` (dot hierarchy) > "main".
fn resolve_base_branch(head: &BranchName, explicit: Option<&BranchName>) -> BranchName {
    if let Some(base) = explicit {
        return base.clone();
    }
    BirthBranch::from(head.as_str())
        .parent()
        .map(|p| BranchName::from(p.as_str()))
        .unwrap_or_else(|| BranchName::from("main"))
}

// ============================================================================
// Octocrab operations
// ============================================================================

async fn find_existing_pr(
    octo: &octocrab::Octocrab,
    owner: &str,
    repo: &str,
    head_branch: &BranchName,
) -> Result<Option<GhPr>, FilePrError> {
    // GitHub API requires `owner:ref-name` format for the `head` parameter.
    // Without the prefix, GitHub returns unfiltered results.
    let qualified_head = format!("{}:{}", owner, head_branch);
    let page = octo
        .pulls(owner, repo)
        .list()
        .head(&qualified_head)
        .state(params::State::Open)
        .per_page(1)
        .send()
        .await
        .map_err(|e| FilePrError::List(map_octo_err(e)))?;

    let pr = page.into_iter().next().map(|p| GhPr {
        number: p.number,
        url: p.html_url.map(|u| u.to_string()).unwrap_or_default(),
        head_ref_name: BranchName::from(p.head.ref_field.as_str()),
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
    base: &BranchName,
) -> Result<(), FilePrError> {
    octo.pulls(owner, repo)
        .update(number.as_u64())
        .title(title)
        .body(body)
        .base(base.as_str())
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
    base: &BranchName,
    head: &BranchName,
) -> Result<GhPr, FilePrError> {
    let pr = octo
        .pulls(owner, repo)
        .create(title, head.as_str(), base.as_str())
        .body(body)
        .send()
        .await
        .map_err(|e| FilePrError::Create(map_octo_err(e)))?;

    Ok(GhPr {
        number: pr.number,
        url: pr.html_url.map(|u| u.to_string()).unwrap_or_default(),
        head_ref_name: BranchName::from(pr.head.ref_field.as_str()),
    })
}

// ============================================================================
// Main implementation
// ============================================================================

/// File a PR using GitHub API. Pushes the branch, creates or updates the PR.
///
/// If `github` is provided, uses its managed client. Otherwise falls back to `build_octocrab()`.
pub async fn file_pr_async(
    input: &FilePRInput,
    git_wt: Arc<GitWorktreeService>,
    github: Option<&GitHubClient>,
) -> Result<FilePROutput> {
    let dir = input.working_dir.as_deref().unwrap_or(".");
    let octo = match github {
        Some(client) => client.get().await?,
        None => build_octocrab()?,
    };
    let repo_info = repo::get_repo_info(dir).await?;

    // Get branch from the agent's working directory, not server CWD
    let dir_path = std::path::PathBuf::from(dir);
    let git_wt_clone = git_wt.clone();
    let head_str =
        tokio::task::spawn_blocking(move || git_wt_clone.get_workspace_bookmark(&dir_path))
            .await
            .context("spawn_blocking failed")?
            .context("Failed to get workspace bookmark")?
            .ok_or_else(|| anyhow::anyhow!("No bookmark found for workspace at {}", dir))?;
    let head = BranchName::from(head_str.as_str());

    let base = resolve_base_branch(&head, input.base_branch.as_ref());

    info!("[FilePR] head={} base={} dir={}", head, base, dir);

    // Push first
    {
        let dir_path = std::path::PathBuf::from(dir);
        let bookmark = head.clone();
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
            &base,
        )
        .await?;
        info!("[FilePR] Updated PR #{}: {}", pr_number, pr.url);
        return Ok(FilePROutput {
            pr_url: pr.url,
            pr_number,
            head_branch: pr.head_ref_name,
            base_branch: base,
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
        &base,
        &head,
    )
    .await?;

    // Emit pr:filed event (only if in tmux session)
    if let Ok(session) = std::env::var("EXOMONAD_TMUX_SESSION") {
        if let Some(agent_id_str) = git::extract_agent_id(head.as_str()) {
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
        base_branch: base,
        created: true,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // resolve_base_branch tests
    // =========================================================================

    #[test]
    fn test_resolve_base_branch_explicit_override() {
        let head = BranchName::from("main.feat");
        let explicit = BranchName::from("develop");
        assert_eq!(resolve_base_branch(&head, Some(&explicit)), BranchName::from("develop"));
    }

    #[test]
    fn test_resolve_base_branch_root_no_dots() {
        let head = BranchName::from("my-branch");
        assert_eq!(resolve_base_branch(&head, None), BranchName::from("main"));
    }

    #[test]
    fn test_resolve_base_branch_single_dot() {
        let head = BranchName::from("main.my-feature");
        assert_eq!(resolve_base_branch(&head, None), BranchName::from("main"));
    }

    #[test]
    fn test_resolve_base_branch_double_dot() {
        let head = BranchName::from("main.auth-service.middleware");
        assert_eq!(
            resolve_base_branch(&head, None),
            BranchName::from("main.auth-service")
        );
    }

    #[test]
    fn test_resolve_base_branch_deep_nesting() {
        let head = BranchName::from("main.a.b.c.d.e");
        assert_eq!(
            resolve_base_branch(&head, None),
            BranchName::from("main.a.b.c.d")
        );
    }

    #[test]
    fn test_resolve_base_branch_agent_suffixed() {
        let head = BranchName::from("main.fix-auth-gemini");
        assert_eq!(resolve_base_branch(&head, None), BranchName::from("main"));
    }

    #[test]
    fn test_resolve_base_branch_agent_suffixed_nested() {
        let head = BranchName::from("main.tl-auth-claude.fix-oauth-gemini");
        assert_eq!(
            resolve_base_branch(&head, None),
            BranchName::from("main.tl-auth-claude")
        );
    }

    #[test]
    fn test_resolve_base_branch_no_slash_convention() {
        // Slash convention is dead — no dots means fallback to "main"
        let head = BranchName::from("feature/my-work");
        assert_eq!(resolve_base_branch(&head, None), BranchName::from("main"));
    }

    #[test]
    fn test_resolve_base_branch_none_explicit() {
        let head = BranchName::from("main.feat");
        assert_eq!(resolve_base_branch(&head, None), BranchName::from("main"));
    }

    #[test]
    fn test_resolve_base_branch_consistency_with_birth_branch() {
        // Verify resolve_base_branch matches BirthBranch::parent() for all cases
        let cases = [
            "main",
            "main.feat",
            "main.auth.middleware",
            "main.a.b.c.d.e",
            "main.fix-auth-gemini",
            "main.tl-auth-claude.fix-oauth-gemini",
        ];
        for case in &cases {
            let head = BranchName::from(*case);
            let expected = BirthBranch::from(*case)
                .parent()
                .map(|p| BranchName::from(p.as_str()))
                .unwrap_or_else(|| BranchName::from("main"));
            assert_eq!(
                resolve_base_branch(&head, None),
                expected,
                "Mismatch for branch '{}'",
                case
            );
        }
    }

    #[test]
    fn test_resolve_base_branch_depth_4() {
        assert_eq!(
            resolve_base_branch(
                &BranchName::from("main.core-eval.optimize.inline.inline-coalg"),
                None
            ),
            BranchName::from("main.core-eval.optimize.inline")
        );
    }

    // =========================================================================
    // file_pr_async integration tests
    // =========================================================================

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

        let result = file_pr_async(&input, git_wt, None).await;
        assert!(result.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_file_pr_async_head_detection() -> Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let dir = temp_dir.path();

        use std::process::Command;
        assert!(Command::new("git")
            .args(["init", "-b", "main"])
            .current_dir(dir)
            .status()?
            .success());
        assert!(Command::new("git")
            .args(["config", "user.email", "test@example.com"])
            .current_dir(dir)
            .status()?
            .success());
        assert!(Command::new("git")
            .args(["config", "user.name", "Test"])
            .current_dir(dir)
            .status()?
            .success());
        std::fs::write(dir.join("README.md"), "test")?;
        assert!(Command::new("git")
            .args(["add", "README.md"])
            .current_dir(dir)
            .status()?
            .success());
        assert!(Command::new("git")
            .args(["commit", "-m", "init"])
            .current_dir(dir)
            .status()?
            .success());

        assert!(Command::new("git")
            .args(["checkout", "-b", "feature-branch"])
            .current_dir(dir)
            .status()?
            .success());
        let git_wt = Arc::new(GitWorktreeService::new(dir.to_path_buf()));

        let input = FilePRInput {
            title: "Test PR".to_string(),
            body: "Test Body".to_string(),
            base_branch: None,
            working_dir: Some(dir.to_string_lossy().to_string()),
        };

        let result = file_pr_async(&input, git_wt, None).await;

        if let Err(ref e) = result {
            let err_msg = e.to_string();
            assert!(
                err_msg.contains("push failed") || err_msg.contains("GitHub token"),
                "Expected push or token error, got: {}",
                err_msg
            );
        } else {
            panic!("Expected file_pr_async to fail, but it succeeded?!");
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_file_pr_async_base_branch_auto_detected() -> Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let dir = temp_dir.path();

        use std::process::Command;
        assert!(Command::new("git")
            .args(["init", "-b", "main"])
            .current_dir(dir)
            .status()?
            .success());
        assert!(Command::new("git")
            .args(["config", "user.email", "test@example.com"])
            .current_dir(dir)
            .status()?
            .success());
        assert!(Command::new("git")
            .args(["config", "user.name", "Test"])
            .current_dir(dir)
            .status()?
            .success());
        std::fs::write(dir.join("README.md"), "test")?;
        assert!(Command::new("git")
            .args(["add", "README.md"])
            .current_dir(dir)
            .status()?
            .success());
        assert!(Command::new("git")
            .args(["commit", "-m", "init"])
            .current_dir(dir)
            .status()?
            .success());

        // Create a dot-separated branch (ExoMonad convention)
        assert!(Command::new("git")
            .args(["checkout", "-b", "main.feat-a-gemini"])
            .current_dir(dir)
            .status()?
            .success());

        let git_wt = Arc::new(GitWorktreeService::new(dir.to_path_buf()));
        let bookmark = git_wt.get_workspace_bookmark(dir)?;
        assert_eq!(bookmark, Some("main.feat-a-gemini".to_string()));

        // Verify base detection via resolve_base_branch
        let head = BranchName::from("main.feat-a-gemini");
        let base = resolve_base_branch(&head, None);
        assert_eq!(base, BranchName::from("main"));

        Ok(())
    }
}
