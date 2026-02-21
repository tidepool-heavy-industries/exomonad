use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::path::Path;
use tokio::process::Command;

/// Shared repository information.
#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub struct RepoInfo {
    /// Repository owner (e.g., "anthropics").
    pub owner: String,
    /// Repository name (e.g., "exomonad").
    pub repo: String,
}

/// Get repository owner and name from git remote.
///
/// This function calls `git remote get-url origin` and parses the owner and repo
/// from the resulting URL (supporting both HTTPS and SSH formats).
pub async fn get_repo_info<P: AsRef<Path>>(working_dir: P) -> Result<RepoInfo> {
    let output = Command::new("git")
        .arg("-C")
        .arg(working_dir.as_ref())
        .args(["remote", "get-url", "origin"])
        .output()
        .await
        .context("Failed to execute git remote get-url origin")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        anyhow::bail!("Failed to get remote URL: {}", stderr.trim());
    }

    let url = String::from_utf8_lossy(&output.stdout).trim().to_string();

    let (owner, repo) = parse_github_url(&url)
        .ok_or_else(|| anyhow::anyhow!("Failed to parse GitHub URL: {}", url))?;

    Ok(RepoInfo { owner, repo })
}

/// Parse a GitHub URL (HTTPS or SSH) into (owner, repo) tuple.
pub fn parse_github_url(url: &str) -> Option<(String, String)> {
    // Normalize SSH-style GitHub remotes to HTTPS-style.
    let normalized = url.replace("git@github.com:", "https://github.com/");

    // Only strip a trailing `.git` suffix; do not remove interior ".git" substrings
    // which may legitimately appear in owner or repo names.
    let cleaned = normalized.strip_suffix(".git").unwrap_or(&normalized);

    let parts: Vec<&str> = cleaned.split('/').collect();

    match parts.as_slice() {
        [.., owner, repo] if !owner.is_empty() && !repo.is_empty() => {
            Some((owner.to_string(), repo.to_string()))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_github_url_https() {
        let (owner, repo) = parse_github_url("https://github.com/anthropics/exomonad").unwrap();
        assert_eq!(owner, "anthropics");
        assert_eq!(repo, "exomonad");
    }

    #[test]
    fn test_parse_github_url_ssh() {
        let (owner, repo) = parse_github_url("git@github.com:anthropics/exomonad.git").unwrap();
        assert_eq!(owner, "anthropics");
        assert_eq!(repo, "exomonad");
    }

    #[test]
    fn test_parse_github_url_with_git_suffix() {
        let (owner, repo) = parse_github_url("https://github.com/anthropics/exomonad.git").unwrap();
        assert_eq!(owner, "anthropics");
        assert_eq!(repo, "exomonad");
    }

    #[test]
    fn test_parse_github_url_invalid() {
        assert!(parse_github_url("not-a-url").is_none());
        assert!(parse_github_url("").is_none());
    }
}
