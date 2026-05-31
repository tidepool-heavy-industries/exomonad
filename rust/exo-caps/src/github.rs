//! `GitHub` capability. PR identifiers are `u64` in this scaffold — Wave 1 newtypes the
//! PR number and firms the review/CI shapes (adapt exomonad-core `GitHubService`).

use crate::error::CapResult;
use crate::types::Branch;
use async_trait::async_trait;

#[async_trait]
pub trait GitHub {
    /// Create or update the PR for the current branch; returns the PR number.
    async fn file_pr(&self, title: &str, body: &str, base: &Branch) -> CapResult<u64>;
    /// The open PR number for a branch, if any.
    async fn pr_for_branch(&self, branch: &Branch) -> CapResult<Option<u64>>;
    async fn merge_pr(&self, pr: u64) -> CapResult<()>;
    /// Does the open PR have unaddressed `ChangesRequested`? (the live stop-gate).
    async fn has_unaddressed_changes(&self, pr: u64) -> CapResult<bool>;
}
