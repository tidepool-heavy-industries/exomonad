//! `tree` tool — show the caller's place in the swarm: its subtree (folded recursively from the
//! `.exo/children.jsonl` ledgers) + its parent, with a per-node pane-liveness proxy.
//!
//! A thin shim over the [`Topology`] cap: the runtime owns identity + IO and does the ledger
//! walk + tmux probe; this tool just surfaces the structured view. Served by Root/Tl (the roles
//! that have children).

use exo_caps::{
    paths::status_path, CapError, CapResult, Fs, NodeStatus, PaneId, Topology, TreeNode,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use exo_framework::{ok_json, parse, schema_json, Tool, ToolOutput};

/// Arguments for the `tree` tool — none; it reports the caller's own position.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct TreeArgs {}

/// The `tree` tool.
pub struct Tree;

impl Tree {
    pub async fn run<C: Topology + Fs>(ctx: &C, _args: TreeArgs) -> CapResult<ToolOutput> {
        let view = ctx.topology().await?;
        let (total, alive) = count(&view.node);

        let home = std::env::var("HOME")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("."));
        let run_id = std::env::var("EXOMONAD_SWARM_RUN_ID").unwrap_or_default();

        let mut status_map = HashMap::new();
        Self::collect_status(ctx, &view.node, &home, &run_id, &mut status_map).await;

        let mut text = String::new();
        use std::fmt::Write;
        if let Some(parent) = &view.parent {
            writeln!(&mut text, "parent: {parent}").unwrap();
        } else {
            writeln!(&mut text, "parent: none (root)").unwrap();
        }
        Self::format_node(&view.node, 0, &status_map, None, &mut text);

        let mut data = serde_json::to_value(&view).map_err(|e| CapError::Json {
            context: "tree topology view".into(),
            source: e,
        })?;

        // Enrich data with status
        if let Some(node_val) = data.get_mut("node") {
            Self::enrich_json(node_val, &status_map, None);
        }

        // Add summary line at the bottom
        writeln!(&mut text, "\n{} node(s) in subtree, {} alive", total, alive).unwrap();

        Ok(ToolOutput::with_data(text, data))
    }

    async fn collect_status<C: Fs>(
        ctx: &C,
        node: &TreeNode,
        home: &Path,
        run_id: &str,
        map: &mut HashMap<String, NodeStatus>,
    ) {
        if let Ok(pane) = PaneId::new(node.pane.clone()) {
            let path = status_path(home, run_id, &pane);
            if let Ok(bytes) = ctx.read(&path).await {
                if let Ok(status) = serde_json::from_slice::<NodeStatus>(&bytes) {
                    map.insert(node.name.clone(), status);
                }
            }
        }
        for child in &node.children {
            // Manual recursion for async
            Box::pin(Self::collect_status(ctx, child, home, run_id, map)).await;
        }
    }

    fn format_node(
        node: &TreeNode,
        depth: usize,
        status_map: &HashMap<String, NodeStatus>,
        parent_status: Option<&NodeStatus>,
        out: &mut String,
    ) {
        let indent = "  ".repeat(depth);
        let liveness = if node.pane_alive { "alive" } else { "dead" };

        let mut status_bits = Vec::new();

        // 0. Model label (e.g. "kimi") for a node on a non-default brain via a launch profile.
        if let Some(label) = &node.model_label {
            status_bits.push(label.as_str());
        }

        // 1. Busy bit (from parent's view)
        if let Some(ps) = parent_status {
            if let Some(cs) = ps.children.iter().find(|c| c.name == node.name) {
                status_bits.push(if cs.busy { "busy" } else { "idle" });
            }
        }

        // 2. Branch & Shutdown (from node's own view)
        let mut shutdown_flag = "";
        if let Some(s) = status_map.get(&node.name) {
            status_bits.push(&s.branch);
            if s.shutdown_pending {
                shutdown_flag = " [SHUTDOWN PENDING]";
            }
        }

        let extra = if status_bits.is_empty() {
            String::new()
        } else {
            format!(" — {}", status_bits.join("; "))
        };

        use std::fmt::Write;
        writeln!(
            out,
            "{indent}• {} ({}) [{liveness}]{extra}{shutdown_flag}",
            node.name, node.pane
        )
        .unwrap();

        let my_status = status_map.get(&node.name);
        for child in &node.children {
            Self::format_node(child, depth + 1, status_map, my_status, out);
        }
    }

    fn enrich_json(
        val: &mut serde_json::Value,
        status_map: &HashMap<String, NodeStatus>,
        parent_status: Option<&NodeStatus>,
    ) {
        if let Some(obj) = val.as_object_mut() {
            let name = obj
                .get("name")
                .and_then(|v| v.as_str())
                .unwrap_or_default()
                .to_string();

            // Add node's own status
            if let Some(status) = status_map.get(&name) {
                if let Ok(val) = serde_json::to_value(status) {
                    obj.insert("status".to_string(), val);
                }
            }

            // Add busy bit from parent
            if let Some(ps) = parent_status {
                if let Some(cs) = ps.children.iter().find(|c| c.name == name) {
                    obj.insert("busy".to_string(), serde_json::Value::Bool(cs.busy));
                }
            }

            let my_status = status_map.get(&name);
            if let Some(children) = obj.get_mut("children").and_then(|v| v.as_array_mut()) {
                for child in children {
                    Self::enrich_json(child, status_map, my_status);
                }
            }
        }
    }
}

/// Count `(total, alive)` over a subtree, inclusive of the node itself.
fn count(node: &TreeNode) -> (usize, usize) {
    node.children
        .iter()
        .fold((1, usize::from(node.pane_alive)), |(t, a), c| {
            let (ct, ca) = count(c);
            (t + ct, a + ca)
        })
}

#[async_trait::async_trait]
impl<R: Topology + Fs + Send + Sync> Tool<R> for Tree {
    fn name(&self) -> &str {
        "tree"
    }

    fn description(&self) -> &str {
        "Show your place in the swarm tree: your subtree (every descendant, folded from the \
         on-disk ledgers) plus your parent, with a per-node liveness flag (`pane_alive` = the \
         node's tmux pane still exists). Read-only; takes no arguments."
    }

    fn schema(&self) -> serde_json::Value {
        schema_json::<TreeArgs>()
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
