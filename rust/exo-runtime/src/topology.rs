//! `impl Topology for Runtime` — fold this node's place in the swarm tree from the on-disk
//! ledgers + a tmux pane-liveness probe.
//!
//! The runtime owns identity (`node_path`, `working_dir`, `own_pane`), so it does the walk; the
//! policy `tree` tool is a thin shim. The recursive ledger read is sync fs work, run inside
//! `spawn_blocking` so it never blocks the tokio executor (the crate's HARD RULE); the one tmux
//! probe is async and best-effort.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{
    fold_children, ChildKind, ChildRecord, Topology, TopologyError, TopologyView, TreeNode,
};
use std::collections::HashSet;
use std::path::Path;

/// Defensive bound on recursion depth. The worktree tree is physically acyclic (a child's
/// worktree nests under its parent's `.exo/worktrees`), so this is pure insurance.
const MAX_DEPTH: usize = 32;

#[async_trait]
impl Topology for Runtime {
    async fn topology(&self) -> Result<TopologyView, TopologyError> {
        let alive = live_panes().await;
        let wd = self.working_dir().to_path_buf();
        let children = tokio::task::spawn_blocking(move || subtree(&wd, &alive, MAX_DEPTH))
            .await
            .map_err(|e| TopologyError::Failed {
                op: "subtree",
                detail: e.to_string(),
            })?;

        let node = TreeNode {
            name: self.name().as_str().to_string(),
            kind: None,
            pane: self.own_pane().as_str().to_string(),
            // Self is, definitionally, alive — it's the node answering the call. Deliberately NOT
            // derived from the `alive` probe set: a best-effort `tmux list-panes` failure would
            // then mis-report this node as dead, which is strictly wrong (it's clearly running).
            pane_alive: true,
            children,
        };

        let parent = self
            .node_path()
            .parent()
            .map(|p| p.name().as_str().to_string());
        let path = self
            .node_path()
            .segments()
            .iter()
            .map(|s| s.as_str().to_string())
            .collect();

        Ok(TopologyView { node, parent, path })
    }
}

/// Fold `{working_dir}/.exo/children.jsonl` into [`TreeNode`]s, recursing into each **Worktree**
/// child's own ledger at `{working_dir}/.exo/worktrees/{name}/.exo/children.jsonl`. Inline
/// children share the parent worktree and spawn nothing, so they're leaves.
fn subtree(working_dir: &Path, alive: &HashSet<String>, depth: usize) -> Vec<TreeNode> {
    if depth == 0 {
        return Vec::new();
    }
    let records = read_records(&working_dir.join(".exo/children.jsonl"));
    fold_children(&records)
        .into_values()
        .map(|c| {
            let pane = c.pane.as_str().to_string();
            let children = match c.kind {
                ChildKind::Worktree => {
                    let child_wd = working_dir.join(".exo/worktrees").join(c.name.as_str());
                    subtree(&child_wd, alive, depth - 1)
                }
                ChildKind::Inline => Vec::new(),
            };
            TreeNode {
                name: c.name.as_str().to_string(),
                kind: Some(c.kind),
                pane_alive: alive.contains(&pane),
                pane,
                children,
            }
        })
        .collect()
}

/// Read + tolerantly parse a `children.jsonl`. A **missing** file is expected (a node with no
/// children) → empty, silently. A real IO error (permissions, etc.) and a malformed line are
/// each `warn!`-ed and skipped — best-effort like the bus/inbound parse, but never silent about
/// a genuine problem (which would otherwise make the topology view quietly wrong).
fn read_records(path: &Path) -> Vec<ChildRecord> {
    let content = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Vec::new(),
        Err(e) => {
            tracing::warn!("topology: could not read {}: {e}", path.display());
            return Vec::new();
        }
    };
    content
        .lines()
        .filter(|l| !l.trim().is_empty())
        .filter_map(|l| match serde_json::from_str::<ChildRecord>(l) {
            Ok(r) => Some(r),
            Err(e) => {
                tracing::warn!(
                    "topology: skipping malformed children.jsonl line in {}: {e}",
                    path.display()
                );
                None
            }
        })
        .collect()
}

/// The set of currently-existing tmux pane ids. **Best-effort**: a tmux failure yields an empty
/// set (every node then reads as not-alive) plus a warning — liveness is a proxy, never fatal.
async fn live_panes() -> HashSet<String> {
    match tokio::process::Command::new("tmux")
        .args(["list-panes", "-a", "-F", "#{pane_id}"])
        .output()
        .await
    {
        Ok(out) if out.status.success() => String::from_utf8_lossy(&out.stdout)
            .lines()
            .map(|l| l.trim().to_string())
            .filter(|l| !l.is_empty())
            .collect(),
        _ => {
            tracing::warn!("topology: `tmux list-panes` failed; reporting all nodes as not-alive");
            HashSet::new()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_caps::{AgentName, Branch, InboxPath, NodePath, PaneId};

    fn write_ledger(dir: &Path, records: &[ChildRecord]) {
        std::fs::create_dir_all(dir.join(".exo")).unwrap();
        let body: String = records
            .iter()
            .map(|r| format!("{}\n", serde_json::to_string(r).unwrap()))
            .collect();
        std::fs::write(dir.join(".exo/children.jsonl"), body).unwrap();
    }

    fn spawned(name: &str, kind: ChildKind, pane: &str) -> ChildRecord {
        ChildRecord::Spawned {
            child: AgentName::new(name.into()).unwrap(),
            kind,
            pane: PaneId::new(pane.into()).unwrap(),
            inbox: InboxPath::new(format!("/tmp/{name}.jsonl").into()),
        }
    }

    #[test]
    fn subtree_folds_recursively_into_worktree_children() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        // root's direct children: worktree "a", inline "b"
        write_ledger(
            root,
            &[
                spawned("a", ChildKind::Worktree, "%1"),
                spawned("b", ChildKind::Inline, "%2"),
            ],
        );
        // a's nested ledger: worktree grandchild "c"
        let a_wd = root.join(".exo/worktrees/a");
        write_ledger(&a_wd, &[spawned("c", ChildKind::Worktree, "%3")]);

        let alive: HashSet<String> = ["%1".to_string()].into_iter().collect();
        let tree = subtree(root, &alive, MAX_DEPTH);

        assert_eq!(tree.len(), 2);
        let a = tree.iter().find(|n| n.name == "a").unwrap();
        let b = tree.iter().find(|n| n.name == "b").unwrap();
        assert!(a.pane_alive, "a's pane %1 is in the alive set");
        assert!(!b.pane_alive, "b's pane %2 is not in the alive set");
        // recursion: a (worktree) descends into "c"; b (inline) is a leaf
        assert_eq!(a.children.len(), 1);
        assert_eq!(a.children[0].name, "c");
        assert!(b.children.is_empty());
    }

    #[test]
    fn missing_ledger_is_empty_not_an_error() {
        let dir = tempfile::tempdir().unwrap();
        assert!(subtree(dir.path(), &HashSet::new(), MAX_DEPTH).is_empty());
    }

    #[tokio::test]
    async fn topology_reports_self_parent_and_path() {
        let dir = tempfile::tempdir().unwrap();
        write_ledger(dir.path(), &[spawned("a", ChildKind::Worktree, "%1")]);

        let node_path = NodePath::new(vec![
            AgentName::new("root".into()).unwrap(),
            AgentName::new("me".into()).unwrap(),
        ])
        .unwrap();
        let rt = Runtime::new(
            node_path,
            Branch::new("root.me".into()).unwrap(),
            dir.path().to_path_buf(),
            None,
            "run".into(),
            "session".into(),
            PaneId::new("%9".into()).unwrap(),
        );

        let view = rt.topology().await.unwrap();
        assert_eq!(view.node.name, "me");
        assert_eq!(view.node.pane, "%9"); // self's pane id (deterministic; no tmux dependency)
        assert!(view.node.pane_alive); // self is always alive (hardcoded, not probe-derived)
        assert_eq!(view.parent.as_deref(), Some("root"));
        assert_eq!(view.path, vec!["root".to_string(), "me".to_string()]);
        assert_eq!(view.node.children.len(), 1);
        assert_eq!(view.node.children[0].name, "a");
    }
}
