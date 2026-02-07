//! Git effect handler for the `git.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::git`.

use crate::services::git::GitService;
use async_trait::async_trait;
use exomonad_core::effects::{
    dispatch_git_effect, EffectError, EffectHandler, EffectResult, GitEffects,
};
use exomonad_proto::effects::git::*;
use std::sync::Arc;

/// Git effect handler.
///
/// Handles all effects in the `git.*` namespace by delegating to
/// the generated `dispatch_git_effect` function.
pub struct GitHandler {
    service: Arc<GitService>,
}

impl GitHandler {
    pub fn new(service: Arc<GitService>) -> Self {
        Self { service }
    }
}

#[async_trait]
impl EffectHandler for GitHandler {
    fn namespace(&self) -> &str {
        "git"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_git_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl GitEffects for GitHandler {
    async fn get_branch(&self, req: GetBranchRequest) -> EffectResult<GetBranchResponse> {
        let working_dir = if req.working_dir.is_empty() {
            ".".to_string()
        } else {
            req.working_dir
        };

        let branch = self
            .service
            .get_branch(&working_dir)
            .await
            .map_err(|e| EffectError::custom("git_error", e.to_string()))?;

        Ok(GetBranchResponse {
            branch,
            detached: false,
        })
    }

    async fn get_status(&self, req: GetStatusRequest) -> EffectResult<GetStatusResponse> {
        let working_dir = if req.working_dir.is_empty() {
            ".".to_string()
        } else {
            req.working_dir
        };

        let dirty_files = self
            .service
            .get_dirty_files(&working_dir)
            .await
            .map_err(|e| EffectError::custom("git_error", e.to_string()))?;

        // Parse git status --porcelain output
        let mut staged = Vec::new();
        let mut unstaged = Vec::new();
        let mut untracked = Vec::new();

        for line in &dirty_files {
            if line.len() < 3 {
                continue;
            }
            let status = &line[..2];
            let file = line[3..].to_string();

            match status.chars().collect::<Vec<_>>().as_slice() {
                ['?', '?'] => untracked.push(file),
                [s, _] if *s != ' ' => staged.push(file.clone()),
                [_, u] if *u != ' ' => unstaged.push(file),
                _ => {}
            }
        }

        Ok(GetStatusResponse {
            dirty_files: unstaged,
            staged_files: staged,
            untracked_files: untracked,
        })
    }

    async fn get_commits(&self, req: GetCommitsRequest) -> EffectResult<GetCommitsResponse> {
        let working_dir = if req.working_dir.is_empty() {
            ".".to_string()
        } else {
            req.working_dir
        };

        let limit = if req.limit <= 0 { 10 } else { req.limit as u32 };

        let raw_commits = self
            .service
            .get_recent_commits(&working_dir, limit)
            .await
            .map_err(|e| EffectError::custom("git_error", e.to_string()))?;

        let commits: Vec<Commit> = raw_commits
            .into_iter()
            .map(|c| Commit {
                sha: c.hash.clone(),
                short_sha: c.hash.chars().take(7).collect(),
                message: c.message,
                author: c.author.clone(),
                author_email: extract_email(&c.author).unwrap_or_default(),
                timestamp: parse_git_date(&c.date).unwrap_or(0),
            })
            .collect();

        Ok(GetCommitsResponse { commits })
    }

    async fn has_unpushed_commits(
        &self,
        req: HasUnpushedCommitsRequest,
    ) -> EffectResult<HasUnpushedCommitsResponse> {
        let working_dir = if req.working_dir.is_empty() {
            ".".to_string()
        } else {
            req.working_dir
        };

        let count = self
            .service
            .has_unpushed_commits(&working_dir)
            .await
            .map_err(|e| EffectError::custom("git_error", e.to_string()))?;

        Ok(HasUnpushedCommitsResponse {
            has_unpushed: count > 0,
            count: count as i32,
        })
    }

    async fn get_remote_url(&self, req: GetRemoteUrlRequest) -> EffectResult<GetRemoteUrlResponse> {
        let working_dir = if req.working_dir.is_empty() {
            ".".to_string()
        } else {
            req.working_dir
        };

        let url = self
            .service
            .get_remote_url(&working_dir)
            .await
            .map_err(|e| EffectError::custom("git_error", e.to_string()))?;

        Ok(GetRemoteUrlResponse { url })
    }

    async fn get_repo_info(&self, req: GetRepoInfoRequest) -> EffectResult<GetRepoInfoResponse> {
        let working_dir = if req.working_dir.is_empty() {
            ".".to_string()
        } else {
            req.working_dir
        };

        let info = self
            .service
            .get_repo_info(&working_dir)
            .await
            .map_err(|e| EffectError::custom("git_error", e.to_string()))?;

        Ok(GetRepoInfoResponse {
            branch: info.branch,
            owner: info.owner.unwrap_or_default(),
            name: info.name.unwrap_or_default(),
        })
    }

    async fn get_worktree(&self, req: GetWorktreeRequest) -> EffectResult<GetWorktreeResponse> {
        let working_dir = if req.working_dir.is_empty() {
            ".".to_string()
        } else {
            req.working_dir
        };

        let info = self
            .service
            .get_worktree(&working_dir)
            .await
            .map_err(|e| EffectError::custom("git_error", e.to_string()))?;

        Ok(GetWorktreeResponse {
            path: info.path,
            branch: info.branch,
            head_commit: String::new(),
            is_linked: false,
        })
    }
}

/// Extract email from git author string like "Name <email@example.com>"
fn extract_email(author: &str) -> Option<String> {
    let start = author.find('<')?;
    let end = author.find('>')?;
    if end > start {
        Some(author[start + 1..end].to_string())
    } else {
        None
    }
}

/// Parse git date string to Unix timestamp
fn parse_git_date(date: &str) -> Option<i64> {
    date.trim().parse::<i64>().ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_email() {
        assert_eq!(
            extract_email("John Doe <john@example.com>"),
            Some("john@example.com".to_string())
        );
        assert_eq!(extract_email("No Email"), None);
    }
}
