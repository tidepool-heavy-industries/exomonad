use anyhow::{Context, Result};
use tokio::process::Command;
use tracing::{error, info};

pub struct MergePROutput {
    pub success: bool,
    pub message: String,
    pub jj_fetched: bool,
}

pub async fn merge_pr_async(
    pr_number: i64,
    strategy: &str,
    working_dir: &str,
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

    // Step 2: jj git fetch (best-effort, triggers auto-rebase)
    let jj_output = Command::new("jj")
        .args(["git", "fetch"])
        .current_dir(dir)
        .output()
        .await;

    let jj_fetched = match jj_output {
        Ok(o) if o.status.success() => {
            info!("jj git fetch succeeded (auto-rebase triggered)");
            true
        }
        Ok(o) => {
            let stderr = String::from_utf8_lossy(&o.stderr);
            info!(stderr = %stderr, "jj git fetch failed (jj may not be initialized)");
            false
        }
        Err(e) => {
            info!(error = %e, "jj not available, skipping fetch");
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
