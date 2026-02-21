use crate::domain::PRNumber;
use anyhow::{Context, Result};
use std::sync::Arc;
use tokio::process::Command;
use tokio::time::Duration;
use tracing::{error, info, warn};

use crate::services::jj_workspace::JjWorkspaceService;

const MERGE_TIMEOUT: Duration = Duration::from_secs(120);

pub struct MergePROutput {
    pub success: bool,
    pub message: String,
    pub jj_fetched: bool,
}

pub async fn merge_pr_async(
    pr_number: PRNumber,
    strategy: &str,
    working_dir: &str,
    jj: Arc<JjWorkspaceService>,
) -> Result<MergePROutput> {
    let dir = if working_dir.is_empty() {
        "."
    } else {
        working_dir
    };
    let strat = if strategy.is_empty() {
        "squash"
    } else {
        strategy
    };

    info!(pr_number = pr_number.as_u64(), strategy = strat, working_dir = dir, "Merging PR");

    // Get branch name before merge to identify potential worktree
    let branch_output = tokio::time::timeout(
        Duration::from_secs(10),
        Command::new("gh")
            .args([
                "pr",
                "view",
                &pr_number.to_string(),
                "--json",
                "headRefName",
                "-q",
                ".headRefName",
            ])
            .current_dir(dir)
            .output(),
    )
    .await;

    let branch_name = match branch_output {
        Ok(Ok(output)) if output.status.success() => {
            String::from_utf8_lossy(&output.stdout).trim().to_string()
        }
        _ => String::new(),
    };

    if !branch_name.is_empty() {
        info!(branch_name = %branch_name, "PR branch name identified");
    }

    // Step 1: gh pr merge
    let strategy_flag = format!("--{}", strat);
    let output = tokio::time::timeout(
        MERGE_TIMEOUT,
        Command::new("gh")
            .args(["pr", "merge", &pr_number.to_string(), &strategy_flag])
            .current_dir(dir)
            .output(),
    )
    .await
    .map_err(|_| anyhow::anyhow!("gh pr merge timed out after {}s", MERGE_TIMEOUT.as_secs()))?
    .context("Failed to run gh pr merge")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        error!(stderr = %stderr, "gh pr merge failed");
        return Ok(MergePROutput {
            success: false,
            message: format!("gh pr merge failed: {}", stderr),
            jj_fetched: false,
        });
    }

    let merge_msg = String::from_utf8_lossy(&output.stdout).trim().to_string();
    info!(pr_number = pr_number.as_u64(), "PR merged successfully");

    // Cleanup child worktree if it exists
    if let Some((_, slug)) = branch_name.rsplit_once('.') {
        let worktree_path = std::path::PathBuf::from(dir)
            .join(".exo")
            .join("worktrees")
            .join(slug);

        if worktree_path.exists() {
            info!(
                path = %worktree_path.display(),
                slug = %slug,
                "Cleaning up child worktree after successful merge"
            );
            let jj_clone = jj.clone();
            let path_clone = worktree_path.clone();
            let cleanup_result =
                tokio::task::spawn_blocking(move || jj_clone.remove_workspace(&path_clone)).await;

            match cleanup_result {
                Ok(Ok(())) => info!(
                    path = %worktree_path.display(),
                    "Worktree cleaned up successfully"
                ),
                Ok(Err(e)) => warn!(
                    error = %e,
                    path = %worktree_path.display(),
                    "Failed to clean up worktree (non-fatal)"
                ),
                Err(e) => warn!(
                    error = %e,
                    path = %worktree_path.display(),
                    "Worktree cleanup task failed (non-fatal)"
                ),
            }
        }
    }

    // Step 2: jj git fetch (best-effort, triggers auto-rebase) via jj-lib
    let dir_path = std::path::PathBuf::from(dir);
    let jj_clone = jj.clone();
    let jj_result = tokio::task::spawn_blocking(move || {
        jj_clone.fetch(&dir_path)
    }).await;

    let jj_fetched = match jj_result {
        Ok(Ok(())) => {
            info!("jj git fetch succeeded via jj-lib (auto-rebase triggered)");
            true
        }
        Ok(Err(e)) => {
            info!(error = %e, "jj git fetch failed via jj-lib");
            false
        }
        Err(e) => {
            info!(error = %e, "jj fetch spawn_blocking failed");
            false
        }
    };

    Ok(MergePROutput {
        success: true,
        message: if merge_msg.is_empty() {
            format!("PR #{} merged via {}", pr_number, strat)
        } else {
            merge_msg
        },
        jj_fetched,
    })
}
