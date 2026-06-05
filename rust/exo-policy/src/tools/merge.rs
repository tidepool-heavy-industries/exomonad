//! `merge` tool — fold a child agent's branch into this node's branch (local `git merge`).
//!
//! v2/node-mode convergence is on-disk: children are git worktrees in the *same* repo, so a TL
//! folds a finished child by merging its branch locally — no PR, no remote, no GitHub. Review,
//! when added, runs *before* this (gating the merge); this tool is just the fold. A merge
//! conflict surfaces as a tool error for the TL to resolve.

use exo_caps::{AgentName, Branch, CapResult, Git, Spawner};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::tool::{ok_json, parse, schema_json, Tool, ToolOutput};

/// Arguments for the `merge` tool.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct MergeArgs {
    /// The child's branch to fold into this node's branch (e.g. `main.root.feature`).
    pub branch: String,
    /// The child's agent name (e.g. `feature`). Derived from branch if omitted.
    pub child: Option<String>,
}

/// The `merge` tool: local fold of a child branch.
pub struct Merge;

impl Merge {
    pub async fn run<C: Git + Spawner>(ctx: &C, args: MergeArgs) -> CapResult<ToolOutput> {
        let branch = Branch::new(args.branch.clone())?;
        ctx.merge(&branch).await?;

        let mut teardown = String::new();
        let child_candidate = args
            .child
            .or_else(|| branch.as_str().rsplit('.').next().map(|s| s.to_string()));

        if let Some(name_str) = child_candidate {
            if let Ok(child) = AgentName::new(name_str) {
                let killed = ctx.kill_pane(&child).await;
                let reclaimed = ctx.reclaim_worktree(&child).await;

                let k_msg = match killed {
                    Ok(_) => "ok".to_string(),
                    Err(e) => e.to_string(),
                };
                let r_msg = match reclaimed {
                    Ok(_) => "ok".to_string(),
                    Err(e) => e.to_string(),
                };

                teardown = if k_msg == "ok" && r_msg == "ok" {
                    format!(" (reclaimed {})", child.as_str())
                } else {
                    format!(" (teardown best-effort: kill={} reclaim={})", k_msg, r_msg)
                };
            }
        }

        Ok(ToolOutput::with_data(
            format!("merged branch {}{}", branch.as_str(), teardown),
            json!({ "branch": branch.as_str() }),
        ))
    }
}

#[async_trait::async_trait]
impl<R: Git + Spawner + Send + Sync> Tool<R> for Merge {
    fn name(&self) -> &str {
        "merge"
    }

    fn description(&self) -> &str {
        "Fold a child's branch into yours with a local `git merge` (the child names it in its \
         `submit_branch` [READY] message). Children are worktrees of the same repo, so this needs \
         no remote, no PR. A merge conflict surfaces as an error for you to resolve."
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
                child: None,
            },
        )
        .await
        .unwrap();

        assert_eq!(
            out.text,
            "merged branch main.root.feature (reclaimed feature)"
        );
        assert_eq!(out.data, Some(json!({ "branch": "main.root.feature" })));
        let calls = mock.calls_made();
        assert!(calls.iter().any(
            |c| matches!(c, Call::Merge { branch } if branch.as_str() == "main.root.feature")
        ));
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::KillPane { child } if child.as_str() == "feature")));
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::ReclaimWorktree { child } if child.as_str() == "feature")));
    }

    #[tokio::test]
    async fn test_merge_explicit_child() {
        let mock = MockRuntime::default();
        // Agent name v1.2 becomes branch v1-2. Explicitly passing the real name
        // should override the branch-based heuristic.
        let out = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.v1-2".into(),
                child: Some("v1.2".into()),
            },
        )
        .await
        .unwrap();

        assert_eq!(out.text, "merged branch main.root.v1-2 (reclaimed v1.2)");
        let calls = mock.calls_made();
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::KillPane { child } if child.as_str() == "v1.2")));
    }

    #[tokio::test]
    async fn test_merge_teardown_failure_formatting() {
        let mock = MockRuntime::failing("kill_pane");
        let out = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.feature".into(),
                child: None,
            },
        )
        .await
        .unwrap();

        // The exact error message depends on SpawnError's Display impl in MockRuntime
        assert!(out.text.contains("teardown best-effort: kill="));
        assert!(out.text.contains("reclaim=ok"));
    }

    #[tokio::test]
    async fn test_merge_error_path() {
        let mock = MockRuntime::failing("merge");
        let res = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.feature".into(),
                child: None,
            },
        )
        .await;
        assert!(res.is_err());
    }
}
