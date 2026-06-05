//! `tree` tool — show the caller's place in the swarm: its subtree (folded recursively from the
//! `.exo/children.jsonl` ledgers) + its parent, with a per-node pane-liveness proxy.
//!
//! A thin shim over the [`Topology`] cap: the runtime owns identity + IO and does the ledger
//! walk + tmux probe; this tool just surfaces the structured view. Served by Root/Tl (the roles
//! that have children).

use exo_caps::{CapResult, Topology};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::tool::{ok_json, parse, schema_json, Tool, ToolOutput};

/// Arguments for the `tree` tool — none; it reports the caller's own position.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct TreeArgs {}

/// The `tree` tool.
pub struct Tree;

impl Tree {
    pub async fn run<C: Topology>(ctx: &C, _args: TreeArgs) -> CapResult<ToolOutput> {
        let view = ctx.topology().await?;
        let (total, alive) = count(&view.node);
        let summary = format!(
            "{} — parent: {}; {} node(s) in subtree, {} alive",
            view.node.name,
            view.parent.as_deref().unwrap_or("none (root)"),
            total,
            alive,
        );
        let data = serde_json::to_value(&view).map_err(|e| exo_caps::CapError::Json {
            context: "tree topology view".into(),
            source: e,
        })?;
        Ok(ToolOutput::with_data(summary, data))
    }
}

/// Count `(total, alive)` over a subtree, inclusive of the node itself.
fn count(node: &exo_caps::TreeNode) -> (usize, usize) {
    node.children
        .iter()
        .fold((1, usize::from(node.pane_alive)), |(t, a), c| {
            let (ct, ca) = count(c);
            (t + ct, a + ca)
        })
}

#[async_trait::async_trait]
impl<R: Topology + Send + Sync> Tool<R> for Tree {
    fn name(&self) -> &str {
        "tree"
    }

    fn description(&self) -> &str {
        "Show your place in the swarm tree: your subtree (every descendant, folded from the \
         on-disk ledgers) plus your parent, with a per-node liveness flag (`pane_alive` = the \
         node's tmux pane still exists). Read-only; takes no arguments."
    }

    fn schema(&self) -> serde_json::Value {
        schema_json(schemars::schema_for!(TreeArgs))
    }

    async fn call(&self, ctx: &R, args: serde_json::Value) -> CapResult<serde_json::Value> {
        let args: TreeArgs = parse(args)?;
        let out = Self::run(ctx, args).await?;
        ok_json(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::MockRuntime;

    #[tokio::test]
    async fn test_tree_returns_view() {
        let mock = MockRuntime::default();
        let out = Tree::run(&mock, TreeArgs {}).await.unwrap();

        // summary mentions self + parent
        assert!(out.text.contains("mock"));
        assert!(out.text.contains("mock-parent"));

        // structured data round-trips back to the view (self + one child)
        let data = out.data.expect("tree returns structured data");
        let view: exo_caps::TopologyView = serde_json::from_value(data).unwrap();
        assert_eq!(view.node.name, "mock");
        assert_eq!(view.parent.as_deref(), Some("mock-parent"));
        assert_eq!(view.node.children.len(), 1);
        assert_eq!(view.node.children[0].name, "child-a");
    }
}
