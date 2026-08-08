//! `impl Topology for Runtime` — fold this node's place in the swarm tree from the on-disk
//! ledgers + a tmux pane-liveness probe.
//!
//! The runtime owns identity (`node_path`, `working_dir`, `own_pane`), so it does the walk; the
//! policy `tree` tool is a thin shim. The recursive ledger read is **deliberately sync `std::fs`**
//! (not the async `Fs` cap, despite `Topology: Fs`): the whole walk runs inside one
//! `spawn_blocking` so it never blocks the tokio executor (the crate's HARD RULE), and an async
//! cap can't be awaited from a blocking closure. The pane-liveness probe is the `Tmux` supertrait
//! (`list_panes`), async and best-effort.

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
        // For the tree view a probe failure reads as "all panes dead" (the documented best-effort
        // behaviour) — `Err` → empty set, with a WARN so a real tmux failure stays visible. (The
        // idle gate, via `ChildLiveness`, treats a probe failure differently: unknown ⇒ trust the
        // busy-bit; see `liveness.rs`.)
        let alive = match exo_caps::Tmux::list_panes(self).await {
            Ok(set) => set,
            Err(e) => {
                tracing::warn!(error = %e, "topology: pane probe failed; tree view reads all panes as dead");
                HashSet::new()
            }
        };
        let wd = self.working_dir().to_path_buf();
        let children = tokio::task::spawn_blocking(move || subtree(&wd, &alive, MAX_DEPTH))
            .await
            .map_err(|e| TopologyError::Failed {
                op: "subtree",
                detail: e.to_string(),
            })?;

        let node = TreeNode {
            name: self.name().clone(),
            kind: None,
            pane: self.own_pane().clone(),
            // Self is, definitionally, alive — it's the node answering the call. Deliberately NOT
            // derived from the `alive` probe set: a best-effort `tmux list-panes` failure would
            // then mis-report this node as dead, which is strictly wrong (it's clearly running).
            pane_alive: true,
            // Self records no lifecycle state or model about itself — both are parent-side facts,
            // folded from the parent's ledger.
            state: None,
            model: None,
            model_label: None,
            directives_hash: None,
            children,
        };

        let parent = self
            .node_path()
            .parent()
            .map(|p| p.name().as_str().to_string());
        let path = self.node_path().segments().to_vec();

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
            let pane = c.pane.clone();
            let terminal = c.state.is_terminal();
            // A terminal child's recorded pane may since have been recycled by tmux onto a
            // different, live agent — never consult the probe set for it, and never recurse into
            // its worktree dir (it's gone, or about to be reclaimed).
            let children = if terminal {
                Vec::new()
            } else {
                match c.kind {
                    ChildKind::Worktree => {
                        let child_wd = working_dir.join(".exo/worktrees").join(c.name.as_str());
                        subtree(&child_wd, alive, depth - 1)
                    }
                    ChildKind::Inline => Vec::new(),
                }
            };
            TreeNode {
                name: c.name.clone(),
                kind: Some(c.kind),
                pane_alive: !terminal && alive.contains(pane.as_str()),
                pane,
                state: Some(c.state.clone()),
                model: c.model.clone(),
                model_label: c.model_label.clone(),
                directives_hash: c.directives_hash.clone(),
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
    let data = match std::fs::read(path) {
        Ok(d) => d,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Vec::new(),
        Err(e) => {
            tracing::warn!("topology: could not read {}: {e}", path.display());
            return Vec::new();
        }
    };
    crate::spawner::parse_child_ledger(&data, path)
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
            model_label: None,
            model: None,
            directives_hash: None,
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
        let a = tree.iter().find(|n| n.name.as_str() == "a").unwrap();
        let b = tree.iter().find(|n| n.name.as_str() == "b").unwrap();
        assert!(a.pane_alive, "a's pane %1 is in the alive set");
        assert!(!b.pane_alive, "b's pane %2 is not in the alive set");
        // recursion: a (worktree) descends into "c"; b (inline) is a leaf
        assert_eq!(a.children.len(), 1);
        assert_eq!(a.children[0].name.as_str(), "c");
        assert!(b.children.is_empty());
    }

    #[test]
    fn missing_ledger_is_empty_not_an_error() {
        let dir = tempfile::tempdir().unwrap();
        assert!(subtree(dir.path(), &HashSet::new(), MAX_DEPTH).is_empty());
    }

    /// A `Died` child whose recorded pane IS in the alive set (tmux recycled the pane id onto a
    /// different, live agent) still renders `pane_alive: false` and no children — the terminal
    /// state forces the answer without ever consulting the probe set, and the worktree is never
    /// recursed into (even though it has its own nested ledger on disk).
    #[test]
    fn recycled_pane_tombstone_reads_dead() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        write_ledger(
            root,
            &[
                spawned("a", ChildKind::Worktree, "%1"),
                ChildRecord::Died {
                    child: AgentName::new("a".into()).unwrap(),
                    pane: PaneId::new("%1".into()).unwrap(),
                    at: None,
                },
            ],
        );
        // A nested ledger under "a" that would normally be recursed into.
        let a_wd = root.join(".exo/worktrees/a");
        write_ledger(&a_wd, &[spawned("c", ChildKind::Worktree, "%3")]);

        // "%1" IS in the alive set — simulating tmux having recycled the pane id onto a
        // different, live agent.
        let alive: HashSet<String> = ["%1".to_string()].into_iter().collect();
        let tree = subtree(root, &alive, MAX_DEPTH);

        assert_eq!(tree.len(), 1);
        let a = &tree[0];
        assert_eq!(a.name.as_str(), "a");
        assert!(
            !a.pane_alive,
            "a terminal child must read as dead regardless of the probe set"
        );
        assert!(
            a.children.is_empty(),
            "a terminal child's worktree must not be recursed into"
        );
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
            exo_caps::ChildKind::Worktree,
        );

        let view = rt.topology().await.unwrap();
        assert_eq!(view.node.name.as_str(), "me");
        assert_eq!(view.node.pane.as_str(), "%9"); // self's pane id (deterministic; no tmux dependency)
        assert!(view.node.pane_alive); // self is always alive (hardcoded, not probe-derived)
        assert_eq!(view.parent.as_deref(), Some("root"));
        assert_eq!(
            view.path.iter().map(|a| a.as_str()).collect::<Vec<_>>(),
            vec!["root", "me"]
        );
        assert_eq!(view.node.children.len(), 1);
        assert_eq!(view.node.children[0].name.as_str(), "a");
    }
}
