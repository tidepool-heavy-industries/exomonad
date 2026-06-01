//! `impl GitHub for Runtime` — GitHub PR operations via octocrab.
//!
//! **Leaf R1.** Adapt exomonad-core `GitHubService` (`services/github.rs`,
//! `services/external/github.rs`) + `build_octocrab()`. Async-native (octocrab is async),
//! so no `spawn_blocking` needed — just `.await` the client calls.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{Branch, GitHub, GitHubError};
use octocrab::models::pulls::ReviewState;
use octocrab::{params, Octocrab, OctocrabBuilder};
use tokio::process::Command;

#[async_trait]
impl GitHub for Runtime {
    async fn file_pr(&self, title: &str, body: &str, base: &Branch) -> Result<u64, GitHubError> {
        if let Some(n) = self.pr_for_branch(self.branch()).await? {
            return Ok(n);
        }

        let (owner, repo) = self.repo().await?;
        let octo = self.octocrab().await?;
        let pr = octo
            .pulls(owner, repo)
            .create(title, self.branch().as_str(), base.as_str())
            .body(body)
            .send()
            .await
            .map_err(|e| GitHubError::Failed {
                op: "file_pr",
                detail: e.to_string(),
            })?;

        Ok(pr.number)
    }

    async fn pr_for_branch(&self, branch: &Branch) -> Result<Option<u64>, GitHubError> {
        let (owner, repo) = self.repo().await?;
        let octo = self.octocrab().await?;
        let page = octo
            .pulls(&owner, &repo)
            .list()
            .state(params::State::Open)
            .head(format!("{}:{}", owner, branch.as_str()))
            .send()
            .await
            .map_err(|e| GitHubError::Failed {
                op: "pr_for_branch",
                detail: e.to_string(),
            })?;

        Ok(page.into_iter().next().map(|pr| pr.number))
    }

    async fn merge_pr(&self, pr: u64) -> Result<(), GitHubError> {
        let (owner, repo) = self.repo().await?;
        let octo = self.octocrab().await?;
        octo.pulls(owner, repo)
            .merge(pr)
            .send()
            .await
            .map_err(|e| GitHubError::Failed {
                op: "merge_pr",
                detail: e.to_string(),
            })?;
        Ok(())
    }

    async fn has_unaddressed_changes(&self, pr: u64) -> Result<bool, GitHubError> {
        let (owner, repo) = self.repo().await?;
        let octo = self.octocrab().await?;
        let reviews = octo
            .pulls(owner, repo)
            .list_reviews(pr)
            .send()
            .await
            .map_err(|e| GitHubError::Failed {
                op: "has_unaddressed_changes",
                detail: e.to_string(),
            })?;

        if let Some(last_review) = reviews.into_iter().last() {
            Ok(matches!(
                last_review.state,
                Some(ReviewState::ChangesRequested)
            ))
        } else {
            Ok(false)
        }
    }
}

impl Runtime {
    async fn octocrab(&self) -> Result<Octocrab, GitHubError> {
        let token = std::env::var("GITHUB_TOKEN").map_err(|_| GitHubError::Failed {
            op: "auth",
            detail: "GITHUB_TOKEN not set".to_string(),
        })?;
        OctocrabBuilder::new()
            .personal_token(token)
            .build()
            .map_err(|e| GitHubError::Failed {
                op: "auth",
                detail: e.to_string(),
            })
    }

    async fn repo(&self) -> Result<(String, String), GitHubError> {
        let output = Command::new("git")
            .current_dir(self.working_dir())
            .args(["remote", "get-url", "origin"])
            .output()
            .await?;

        if !output.status.success() {
            return Err(GitHubError::Failed {
                op: "repo",
                detail: String::from_utf8_lossy(&output.stderr).trim().to_string(),
            });
        }

        let url = String::from_utf8_lossy(&output.stdout).trim().to_string();
        parse_github_url(&url).ok_or_else(|| GitHubError::Failed {
            op: "repo",
            detail: format!("Failed to parse GitHub URL: {}", url),
        })
    }
}

fn parse_github_url(url: &str) -> Option<(String, String)> {
    let url = url.trim().strip_suffix(".git").unwrap_or(url.trim());
    if let Some(path) = url.strip_prefix("git@github.com:") {
        let parts: Vec<&str> = path.split('/').collect();
        if parts.len() == 2 {
            return Some((parts[0].to_string(), parts[1].to_string()));
        }
    } else if let Some(path) = url.strip_prefix("https://github.com/") {
        let parts: Vec<&str> = path.split('/').collect();
        if parts.len() == 2 {
            return Some((parts[0].to_string(), parts[1].to_string()));
        }
    }
    None
}
