use crate::domain::PRNumber;
use anyhow::{Context, Result};
use std::sync::Arc;
use tokio::process::Command;
use tokio::time::Duration;
use tracing::{error, info};

use crate::services::git_worktree::GitWorktreeService;

const MERGE_TIMEOUT: Duration = Duration::from_secs(120);

pub struct MergePROutput {
    pub success: bool,
    pub message: String,
    pub git_fetched: bool,
    pub branch_name: String,
}

pub async fn merge_pr_async(
    pr_number: PRNumber,
    strategy: &str,
    working_dir: &str,
    git_wt: Arc<GitWorktreeService>,
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

    info!(
        pr_number = pr_number.as_u64(),
        strategy = strat,
        working_dir = dir,
        "Merging PR"
    );

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
            git_fetched: false,
            branch_name,
        });
    }

    let merge_msg = String::from_utf8_lossy(&output.stdout).trim().to_string();
    info!(pr_number = pr_number.as_u64(), "PR merged successfully");

    // Step 2: git fetch (best-effort, pulls merged changes)
    let dir_path = std::path::PathBuf::from(dir);
    let git_wt_clone = git_wt.clone();
    let git_result = tokio::task::spawn_blocking(move || git_wt_clone.fetch(&dir_path)).await;

    let git_fetched = match git_result {
        Ok(Ok(())) => {
            info!("git fetch succeeded");
            true
        }
        Ok(Err(e)) => {
            info!(error = %e, "git fetch failed");
            false
        }
        Err(e) => {
            info!(error = %e, "git fetch spawn_blocking failed");
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
        git_fetched,
        branch_name,
    })
}
