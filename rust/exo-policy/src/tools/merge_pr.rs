//! `merge_pr` tool — merges a child agent's PR with readiness guards.
//!
//! This tool ports the logic from `MergePR.hs` to the policy layer. It uses the
//! `Git` and `GitHub` capabilities to verify readiness (checking for unaddressed
//! changes and self-merge attempts) before performing the merge.

use exo_caps::{CapResult, Git, GitHub};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::tool::{ok_json, parse, schema_json, Tool, ToolOutput};

/// Arguments for the `merge_pr` tool.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct MergePrArgs {
    /// The PR number to merge.
    pub pr: u64,
}

/// The `merge_pr` tool implementation.
pub struct MergePr;

impl MergePr {
    /// The core logic for merging a PR.
    ///
    /// 1. **Self-merge guard**: Prevents an agent from merging its own PR.
    /// 2. **Readiness guard**: Checks if there are unaddressed `ChangesRequested` reviews.
    /// 3. **Merge**: Executes the merge via the GitHub capability.
    pub async fn run<C: Git + GitHub>(ctx: &C, args: MergePrArgs) -> CapResult<ToolOutput> {
        // 1. Self-merge guard
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
        if ctx.has_unaddressed_changes(args.pr).await? {
            return Ok(ToolOutput::text(format!(
                "Copilot requested changes on PR #{}. Wait for the agent to push fixes or use force=true (if supported).",
                args.pr
            )));
        }

        // 3. Merge
        // TODO(cap): GitHub::merge_pr does not support merge strategies (squash/merge/rebase) yet.
        ctx.merge_pr(args.pr).await?;

        // TODO(cap): Git capability does not support 'pull' or 'fetch' to sync local state after merge.
        // TODO(cap): No capability for agent shutdown/cleanup yet.

        Ok(ToolOutput::with_data(
            format!("merged PR #{}", args.pr),
            json!({ "pr": args.pr }),
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
        let args = MergePrArgs { pr: 123 };

        let out = MergePr::run(&mock, args).await.unwrap();

        assert_eq!(out.text, "merged PR #123");
        assert_eq!(out.data, Some(json!({ "pr": 123 })));

        // Verify merge was called
        let calls = mock.calls_made();
        assert!(calls.contains(&Call::MergePr { pr: 123 }));
    }

    #[tokio::test]
    async fn test_merge_pr_self_merge_blocked() {
        let branch = Branch::new("feat.topic".into()).unwrap();
        let mock = MockRuntime {
            current_branch: branch.clone(),
            pr_for_branch: Some(123), // Current branch has PR 123
            ..Default::default()
        };

        let args = MergePrArgs { pr: 123 }; // Trying to merge own PR 123
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

        let args = MergePrArgs { pr: 123 };
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
    async fn test_merge_pr_error_path() {
        let mock = MockRuntime::failing("merge_pr");
        let args = MergePrArgs { pr: 123 };

        let res = MergePr::run(&mock, args).await;
        assert!(res.is_err());
    }
}
