//! The `Runtime` struct — the single concrete type that implements every `exo-caps`
//! capability trait. Policy monomorphizes against this `R`.
//!
//! Identity (the swarm's `EffectContext` analogue) is baked in at construction and is
//! always present — no `Option`, no task-locals. The per-cap `impl` blocks live in sibling
//! modules (`git`, `github`, `tmux`, `bus`, `spawner`, `fs`, `process`, `log`, `kv`); this
//! file owns **only** the struct + its accessors, so cap leaves never collide here.

use exo_caps::{AgentName, Branch, InboxPath, NodePath, PaneId};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

/// One node's runtime. Holds the node's birth identity + the ambient context the cap
/// impls need (worktree dir, parent inbox pointer, run-id namespace, tmux session).
///
/// Fields are `pub(crate)` so each cap `impl` module reads what it needs without a
/// setter; the struct is constructed once at node boot (Wave 2 wires `new`/self-ID via
/// `exo-scry`). All anticipated fields are present up-front so a cap leaf edits only its
/// own impl file, never this struct.
#[derive(Debug, Clone)]
pub struct Runtime {
    /// Full tree address; `name()` = last segment, `parent()` = prefix.
    pub(crate) node_path: NodePath,
    /// This node's git branch (generated safely from `node_path`).
    pub(crate) branch: Branch,
    /// The node's worktree root (where git/log/fs operations are rooted).
    pub(crate) working_dir: PathBuf,
    /// Path to the parent's ingestion inbox (`Bus::deliver(Parent, …)` appends here).
    /// `None` for the root, which has no parent.
    pub(crate) parent_inbox: Option<InboxPath>,
    /// Swarm run-id — namespaces the inbox dir (`…/inboxes/{run_id}/pane-N.jsonl`) so a
    /// fresh swarm gets a clean namespace and pane-ids can't collide across runs.
    pub(crate) run_id: String,
    /// tmux session name (for pane creation + the tmux-paste delivery last-hop).
    pub(crate) tmux_session: String,
    /// This node's own tmux pane id.
    pub(crate) own_pane: PaneId,
    /// Per-direct-child busy/idle bit (`true` = working). Seeded `true` at birth and on every
    /// deliver down to a child (a poke that wakes it); set `false` when the child reports
    /// `ChildIdle`. Shared across the sidecar's loops via `Arc` (the struct is `Clone`, so every
    /// clone sees the same map). Read by the [`ChildLiveness`](exo_caps::ChildLiveness) cap, which
    /// combines it with pane-liveness — a dead pane is idle regardless of a stale bit.
    pub(crate) children_busy: Arc<Mutex<HashMap<AgentName, bool>>>,
}

impl Runtime {
    /// Construct a node runtime from its resolved birth identity + ambient context.
    pub fn new(
        node_path: NodePath,
        branch: Branch,
        working_dir: PathBuf,
        parent_inbox: Option<InboxPath>,
        run_id: String,
        tmux_session: String,
        own_pane: PaneId,
    ) -> Self {
        Runtime {
            node_path,
            branch,
            working_dir,
            parent_inbox,
            run_id,
            tmux_session,
            own_pane,
            children_busy: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Mark a direct child as working — at birth, and whenever this node delivers a message down to
    /// it (a poke that will wake it). Paired with [`mark_child_idle`](Self::mark_child_idle).
    pub(crate) fn mark_child_busy(&self, child: &AgentName) {
        self.children_busy
            .lock()
            .unwrap()
            .insert(child.clone(), true);
    }

    /// Mark a direct child idle — it reported `ChildIdle` (its whole subtree is quiescent). Called
    /// by the sidecar's inbound loop on a `ChildIdle` system message. Until the child is poked
    /// again it counts as not-working.
    pub fn mark_child_idle(&self, child: &AgentName) {
        self.children_busy
            .lock()
            .unwrap()
            .insert(child.clone(), false);
    }

    /// This node's own name (the `NodePath` last segment).
    pub fn name(&self) -> AgentName {
        self.node_path.name()
    }

    /// This node's tree address.
    pub fn node_path(&self) -> &NodePath {
        &self.node_path
    }

    /// This node's branch.
    pub fn branch(&self) -> &Branch {
        &self.branch
    }

    /// The node's worktree root.
    pub fn working_dir(&self) -> &Path {
        &self.working_dir
    }

    /// This node's own pane id.
    pub fn own_pane(&self) -> &PaneId {
        &self.own_pane
    }

    /// This node's own ingestion inbox.
    pub(crate) fn own_inbox(&self) -> InboxPath {
        exo_caps::paths::inbox_path(&home(), &self.run_id, &self.own_pane)
    }
}

fn home() -> PathBuf {
    std::env::var("HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("."))
}
