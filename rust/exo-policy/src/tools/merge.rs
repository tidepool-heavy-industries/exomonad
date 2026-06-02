//! `merge` tool — fold a child agent's branch into this node's branch (local `git merge`).
//!
//! v2/node-mode convergence is on-disk: children are git worktrees in the *same* repo, so a TL
//! folds a finished child by merging its branch locally — no PR, no remote, no GitHub. Review,
//! when added, runs *before* this (gating the merge); this tool is just the fold. A merge
//! conflict surfaces as a tool error for the TL to resolve.

use exo_caps::{Branch, CapResult, Git};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::tool::{ok_json, parse, schema_json, Tool, ToolOutput};

/// Arguments for the `merge` tool.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct MergeArgs {
    /// The child's branch to fold into this node's branch (e.g. `main.root.feature`).
    pub branch: String,
}

/// The `merge` tool: local fold of a child branch.
pub struct Merge;

impl Merge {
    pub async fn run<C: Git>(ctx: &C, args: MergeArgs) -> CapResult<ToolOutput> {
        let branch = Branch::new(args.branch.clone())?;
        ctx.merge(&branch).await?;
        Ok(ToolOutput::with_data(
            format!("merged branch {}", branch.as_str()),
            json!({ "branch": branch.as_str() }),
        ))
    }
}

#[async_trait::async_trait]
impl<R: Git + Send + Sync> Tool<R> for Merge {
    fn name(&self) -> &str {
        "merge"
    }

    fn schema(&self) -> serde_json::Value {
        schema_json(schemars::schema_for!(MergeArgs))
    }

    async fn call(&self, ctx: &R, args: serde_json::Value) -> CapResult<serde_json::Value> {
        let args: MergeArgs = parse(args)?;
        let out = Self::run(ctx, args).await?;
        ok_json(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};

    #[tokio::test]
    async fn test_merge_local_fold() {
        let mock = MockRuntime::default();
        let out = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.feature".into(),
            },
        )
        .await
        .unwrap();

        assert_eq!(out.text, "merged branch main.root.feature");
        assert_eq!(out.data, Some(json!({ "branch": "main.root.feature" })));
        let calls = mock.calls_made();
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::Merge { branch } if branch.as_str() == "main.root.feature")));
    }

    #[tokio::test]
    async fn test_merge_error_path() {
        let mock = MockRuntime::failing("merge");
        let res = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.feature".into(),
            },
        )
        .await;
        assert!(res.is_err());
    }
}
