use crate::domain::PRNumber;
use crate::services::git_worktree::GitWorktreeService;
use crate::services::github::{build_octocrab, map_octo_err};
use crate::services::repo;
use anyhow::Result;
use octocrab::params::pulls::MergeMethod;
use std::sync::Arc;
use tokio::time::Duration;
use tracing::{error, info};

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

    let octo = build_octocrab()?;
    let repo_info = repo::get_repo_info(dir).await?;

    // Get branch name before merge to identify potential worktree
    let pr = match tokio::time::timeout(
        Duration::from_secs(10),
        octo.pulls(&repo_info.owner, &repo_info.repo)
            .get(pr_number.as_u64()),
    )
    .await
    {
        Ok(Ok(pr)) => Some(pr),
        _ => None,
    };

    let branch_name = pr
        .as_ref()
        .map(|p| p.head.ref_field.clone())
        .unwrap_or_default();

    if !branch_name.is_empty() {
        info!(branch_name = %branch_name, "PR branch name identified");
    }

    // Step 1: merge PR
    let merge_method = match strat {
        "merge" => MergeMethod::Merge,
        "squash" => MergeMethod::Squash,
        "rebase" => MergeMethod::Rebase,
        _ => MergeMethod::Squash,
    };

    let result = tokio::time::timeout(
        MERGE_TIMEOUT,
        octo.pulls(&repo_info.owner, &repo_info.repo)
            .merge(pr_number.as_u64())
            .method(merge_method)
            .send(),
    )
    .await;

    let merge_res = match result {
        Ok(Ok(res)) => Ok(res),
        Ok(Err(e)) => Err(anyhow::anyhow!("GitHub merge failed: {}", map_octo_err(e))),
        Err(_) => Err(anyhow::anyhow!(
            "GitHub merge timed out after {}s",
            MERGE_TIMEOUT.as_secs()
        )),
    };

    if let Err(e) = merge_res {
        error!(error = %e, "GitHub merge failed");
        return Ok(MergePROutput {
            success: false,
            message: e.to_string(),
            git_fetched: false,
            branch_name,
        });
    }

    let merge_res = merge_res.unwrap();
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
        message: if let Some(msg) = merge_res.message {
            if msg.is_empty() {
                format!("PR #{} merged via {}", pr_number, strat)
            } else {
                msg
            }
        } else {
            format!("PR #{} merged via {}", pr_number, strat)
        },
        git_fetched,
        branch_name,
    })
}
