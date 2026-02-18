use anyhow::{Context, Result};
use std::sync::Arc;
use tokio::process::Command;
use tracing::{error, info};

use crate::services::jj_workspace::JjWorkspaceService;

pub struct MergePROutput {
    pub success: bool,
    pub message: String,
    pub jj_fetched: bool,
}

pub async fn merge_pr_async(
    pr_number: i64,
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

    info!(pr_number, strategy = strat, working_dir = dir, "Merging PR");

    // Step 1: gh pr merge
    let strategy_flag = format!("--{}", strat);
    let output = Command::new("gh")
        .args(["pr", "merge", &pr_number.to_string(), &strategy_flag])
        .current_dir(dir)
        .output()
        .await
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
    info!(pr_number, "PR merged successfully");

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
