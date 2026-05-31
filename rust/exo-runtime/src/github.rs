//! `impl GitHub for Runtime` — GitHub PR operations via octocrab.
//!
//! **Leaf R1.** Adapt exomonad-core `GitHubService` (`services/github.rs`,
//! `services/external/github.rs`) + `build_octocrab()`. Async-native (octocrab is async),
//! so no `spawn_blocking` needed — just `.await` the client calls.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{Branch, GitHub, GitHubError};

#[async_trait]
impl GitHub for Runtime {
    async fn file_pr(&self, _title: &str, _body: &str, _base: &Branch) -> Result<u64, GitHubError> {
        todo!("R1: create-or-update PR for self.branch against base; return PR number")
    }

    async fn pr_for_branch(&self, _branch: &Branch) -> Result<Option<u64>, GitHubError> {
        todo!("R1: list open PRs head=<branch>; return first PR number or None")
    }

    async fn merge_pr(&self, _pr: u64) -> Result<(), GitHubError> {
        todo!("R1: merge the PR (octocrab pulls().merge)")
    }

    async fn has_unaddressed_changes(&self, _pr: u64) -> Result<bool, GitHubError> {
        todo!("R1: inspect reviews; true if latest is ChangesRequested with no newer push")
    }
}
