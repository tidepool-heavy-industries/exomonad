//! `GitHub` capability. PR identifiers are `u64` in this scaffold — Wave 1 newtypes the
//! PR number and firms the review/CI shapes (adapt exomonad-core `GitHubService`).

use crate::types::Branch;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum GitHubError {
    #[error("github {op} failed: {detail}")]
    Failed { op: &'static str, detail: String },
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ReviewState {
    Approved,
    ChangesRequested,
    Commented,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CiStatus {
    Passing,
    Failing,
    Pending,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum MergeStrategy {
    #[default]
    Squash,
    Merge,
    Rebase,
}

impl MergeStrategy {
    pub fn parse(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "squash" => Self::Squash,
            "merge" => Self::Merge,
            "rebase" => Self::Rebase,
            _ => Self::Squash,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Squash => "squash",
            Self::Merge => "merge",
            Self::Rebase => "rebase",
        }
    }
}

#[async_trait]
pub trait GitHub {
    /// Create or update the PR for the current branch; returns the PR number.
    async fn file_pr(&self, title: &str, body: &str, base: &Branch) -> Result<u64, GitHubError>;
    /// The open PR number for a branch, if any.
    async fn pr_for_branch(&self, branch: &Branch) -> Result<Option<u64>, GitHubError>;
    async fn merge_pr(&self, pr: u64, strategy: MergeStrategy) -> Result<(), GitHubError>;
    /// Does the open PR have unaddressed `ChangesRequested`? (the live stop-gate).
    async fn has_unaddressed_changes(&self, pr: u64) -> Result<bool, GitHubError>;
    /// The latest review decision on the PR. None if no reviews have arrived.
    async fn review_state(&self, pr: u64) -> Result<Option<ReviewState>, GitHubError>;
    /// The rolled-up CI conclusion for the PR's HEAD.
    async fn ci_status(&self, pr: u64) -> Result<CiStatus, GitHubError>;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn review_state_serde_round_trip() {
        let states = vec![
            ReviewState::Approved,
            ReviewState::ChangesRequested,
            ReviewState::Commented,
        ];
        for s in states {
            let j = serde_json::to_string(&s).unwrap();
            let back: ReviewState = serde_json::from_str(&j).unwrap();
            assert_eq!(s, back);
        }
    }

    #[test]
    fn ci_status_serde_round_trip() {
        let statuses = vec![CiStatus::Passing, CiStatus::Failing, CiStatus::Pending];
        for s in statuses {
            let j = serde_json::to_string(&s).unwrap();
            let back: CiStatus = serde_json::from_str(&j).unwrap();
            assert_eq!(s, back);
        }
    }

    #[test]
    fn review_state_json_representation() {
        assert_eq!(
            serde_json::to_value(ReviewState::Approved).unwrap(),
            serde_json::json!("approved")
        );
        assert_eq!(
            serde_json::to_value(ReviewState::ChangesRequested).unwrap(),
            serde_json::json!("changes_requested")
        );
    }
}
