//! `merge_pr` tool — merges a child agent's PR with readiness guards.
//!
//! This tool ports the logic from `MergePR.hs` to the policy layer. It uses the
//! `Git` and `GitHub` capabilities to verify readiness (checking for unaddressed
//! changes and self-merge attempts) before performing the merge.

use exo_caps::{CapResult, Git, GitHub, MergeStrategy};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::tool::{ok_json, parse, schema_json, Tool, ToolOutput};

/// Arguments for the `merge_pr` tool.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct MergePrArgs {
    /// The PR number to merge.
    pub pr: u64,
    /// The merge strategy to use (squash, merge, or rebase). Defaults to squash.
    pub strategy: Option<String>,
    /// If true, skip the readiness guard (unaddressed changes check).
    #[serde(default)]
    pub force: bool,
}

/// The `merge_pr` tool implementation.
pub struct MergePr;

impl MergePr {
    /// The core logic for merging a PR.
    ///
    /// 1. **Self-merge guard**: Prevents an agent from merging its own PR.
    /// 2. **Readiness guard**: Checks if there are unaddressed `ChangesRequested` reviews.
    /// 3. **Merge**: Executes the merge via the GitHub capability.
    /// 4. **Fetch**: Syncs local state after a successful merge (best-effort).
    pub async fn run<C: Git + GitHub>(ctx: &C, args: MergePrArgs) -> CapResult<ToolOutput> {
        // 1. Self-merge guard (always enforced)
        let current_branch = ctx.current_branch().await?;
        if let Some(own_pr) = ctx.pr_for_branch(&current_branch).await? {
            if own_pr == args.pr {
                return Ok(ToolOutput::text(format!(
                    "Cannot merge your own PR #{}. Your parent agent will merge this PR after reviewing. Call notify_parent instead.",
                    args.pr
                )));
            }
        }

        // 2. Readiness guard: check for unaddressed changes
        if !args.force && ctx.has_unaddressed_changes(args.pr).await? {
            return Ok(ToolOutput::text(format!(
                "Copilot requested changes on PR #{}. Wait for the agent to push fixes or use force=true (if supported).",
                args.pr
            )));
        }

        // 3. Merge
        let strategy = args
            .strategy
            .as_deref()
            .map(MergeStrategy::parse)
            .unwrap_or_default();
        ctx.merge_pr(args.pr, strategy).await?;

        // 4. Fetch (best-effort): pulls merged changes
        if let Err(e) = ctx.fetch().await {
            tracing::warn!(error = %e, "post-merge git fetch failed (ignoring)");
        }

        // NOTE: Agent teardown (worktree reclaim + pane kill) is parent-side at
        // convergence by design (Spawner::reclaim_worktree / kill_pane) —
        // merge_pr does not shut down the merged agent.

        Ok(ToolOutput::with_data(
            format!(
                "merged PR #{} using {} strategy",
                args.pr,
                strategy.as_str()
            ),
            json!({ "pr": args.pr, "strategy": strategy.as_str() }),
        ))
    }
}

#[async_trait::async_trait]
impl<R: Git + GitHub + Send + Sync> Tool<R> for MergePr {
    fn name(&self) -> &str {
        "merge_pr"
    }

    fn schema(&self) -> serde_json::Value {
        schema_json(schemars::schema_for!(MergePrArgs))
    }

    async fn call(&self, ctx: &R, args: serde_json::Value) -> CapResult<serde_json::Value> {
        let args: MergePrArgs = parse(args)?;
        let out = Self::run(ctx, args).await?;
        ok_json(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};
    use exo_caps::Branch;

    #[tokio::test]
    async fn test_merge_pr_happy_path() {
        let mock = MockRuntime::default();
        let args = MergePrArgs {
            pr: 123,
            strategy: None,
            force: false,
        };

        let out = MergePr::run(&mock, args).await.unwrap();

        assert_eq!(out.text, "merged PR #123 using squash strategy");
        assert_eq!(out.data, Some(json!({ "pr": 123, "strategy": "squash" })));

        // Verify merge was called
        let calls = mock.calls_made();
        assert!(calls.contains(&Call::MergePr {
            pr: 123,
            strategy: MergeStrategy::Squash
        }));
        assert!(calls.contains(&Call::Fetch));
    }

    #[tokio::test]
    async fn test_merge_pr_with_explicit_strategy() {
        let mock = MockRuntime::default();
        let args = MergePrArgs {
            pr: 123,
            strategy: Some("rebase".into()),
            force: false,
        };

        let out = MergePr::run(&mock, args).await.unwrap();

        assert_eq!(out.text, "merged PR #123 using rebase strategy");
        assert_eq!(out.data, Some(json!({ "pr": 123, "strategy": "rebase" })));

        let calls = mock.calls_made();
        assert!(calls.contains(&Call::MergePr {
            pr: 123,
            strategy: MergeStrategy::Rebase
        }));
    }

    #[tokio::test]
    async fn test_merge_pr_fetch_failure_ignored() {
        let mock = MockRuntime::failing("fetch");
        let args = MergePrArgs {
            pr: 123,
            strategy: None,
            force: false,
        };

        let out = MergePr::run(&mock, args).await.unwrap();

        assert_eq!(out.text, "merged PR #123 using squash strategy");
        let calls = mock.calls_made();
        assert!(calls.contains(&Call::MergePr {
            pr: 123,
            strategy: MergeStrategy::Squash
        }));
        // fetch should NOT be in calls if it failed (in MockRuntime implementation)
        // Wait, MockRuntime record happens AFTER failure check.
        // Let's check MockRuntime fetch impl.
    }

    #[tokio::test]
    async fn test_merge_pr_self_merge_blocked() {
        let mock = MockRuntime {
            current_branch: Branch::new("feat.topic".into()).unwrap(),
            pr_for_branch: Some(123), // Current branch has PR 123
            ..Default::default()
        };

        let args = MergePrArgs {
            pr: 123,
            strategy: None,
            force: false,
        }; // Trying to merge own PR 123
        let out = MergePr::run(&mock, args).await.unwrap();

        assert!(out.text.contains("Cannot merge your own PR #123"));
        assert_eq!(out.data, None);

        // Verify merge was NOT called
        let calls = mock.calls_made();
        for call in calls {
            if let Call::MergePr { .. } = call {
                panic!("merge_pr should not have been called");
            }
        }
    }

    #[tokio::test]
    async fn test_merge_pr_unaddressed_changes_blocked() {
        let mock = MockRuntime {
            has_unaddressed_changes: true,
            ..Default::default()
        };

        let args = MergePrArgs {
            pr: 123,
            strategy: None,
            force: false,
        };
        let out = MergePr::run(&mock, args).await.unwrap();

        assert!(out.text.contains("Copilot requested changes on PR #123"));
        assert_eq!(out.data, None);

        // Verify merge was NOT called
        let calls = mock.calls_made();
        for call in calls {
            if let Call::MergePr { .. } = call {
                panic!("merge_pr should not have been called");
            }
        }
    }

    #[tokio::test]
    async fn test_merge_pr_force_skips_readiness_guard() {
        let mock = MockRuntime {
            has_unaddressed_changes: true, // Blocked by default
            ..Default::default()
        };

        let args = MergePrArgs {
            pr: 123,
            strategy: None,
            force: true, // Skip the guard
        };
        let out = MergePr::run(&mock, args).await.unwrap();

        assert_eq!(out.text, "merged PR #123 using squash strategy");
        assert_eq!(out.data, Some(json!({ "pr": 123, "strategy": "squash" })));

        // Verify merge WAS called
        let calls = mock.calls_made();
        assert!(calls.contains(&Call::MergePr {
            pr: 123,
            strategy: MergeStrategy::Squash
        }));
    }

    #[tokio::test]
    async fn test_merge_pr_force_still_respects_self_merge_guard() {
        let mock = MockRuntime {
            current_branch: Branch::new("feat.topic".into()).unwrap(),
            pr_for_branch: Some(123),
            has_unaddressed_changes: true,
            ..Default::default()
        };

        let args = MergePrArgs {
            pr: 123,
            strategy: None,
            force: true, // Try to force self-merge
        };
        let out = MergePr::run(&mock, args).await.unwrap();

        assert!(out.text.contains("Cannot merge your own PR #123"));
        assert_eq!(out.data, None);

        // Verify merge was NOT called
        let calls = mock.calls_made();
        for call in calls {
            if let Call::MergePr { .. } = call {
                panic!("merge_pr should not have been called");
            }
        }
    }

    #[tokio::test]
    async fn test_merge_pr_error_path() {
        let mock = MockRuntime::failing("merge_pr");
        let args = MergePrArgs {
            pr: 123,
            strategy: None,
            force: false,
        };

        let res = MergePr::run(&mock, args).await;
        assert!(res.is_err());
    }
}
