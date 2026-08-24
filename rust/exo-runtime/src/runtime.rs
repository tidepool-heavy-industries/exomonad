//! The `Runtime` struct — the single concrete type that implements every `exo-caps`
//! capability trait. Policy monomorphizes against this `R`.
//!
//! Identity (the swarm's `EffectContext` analogue) is baked in at construction and is
//! always present — no `Option`, no task-locals. The per-cap `impl` blocks live in sibling
//! modules (`git`, `github`, `tmux`, `bus`, `spawner`, `fs`, `process`, `log`, `kv`); this
//! file owns **only** the struct + its accessors, so cap leaves never collide here.

use exo_caps::{
    Addressee, AgentName, Branch, ChildKind, ChildStatus, InboxPath, NodePath, NodeStatus, PaneId,
};
use std::path::{Path, PathBuf};

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
    /// Whether this node is `Inline` (shares the parent's worktree) or `Worktree` (own dir).
    /// Drives children-ledger access — see `is_inline()`.
    pub(crate) own_kind: ChildKind,
}

impl Runtime {
    /// Construct a node runtime from its resolved birth identity + ambient context.
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        node_path: NodePath,
        branch: Branch,
        working_dir: PathBuf,
        parent_inbox: Option<InboxPath>,
        run_id: String,
        tmux_session: String,
        own_pane: PaneId,
        own_kind: ChildKind,
    ) -> Self {
        Runtime {
            node_path,
            branch,
            working_dir,
            parent_inbox,
            run_id,
            tmux_session,
            own_pane,
            own_kind,
        }
    }

    /// `true` when this node shares its parent's worktree (an inline worker). Inline nodes
    /// report no children (they have no spawn tools) and run without a CC team (their
    /// cwd-resolution would land in the parent's team).
    pub fn is_inline(&self) -> bool {
        self.own_kind == ChildKind::Inline
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

    /// Map a tree-edge agent name to its bus [`Addressee`]: this node's parent, or a current
    /// **non-tombstoned** child (by ledger [`ChildKind`]). `None` if the name is neither, or is a
    /// child whose folded state is terminal — the outbound Teams bridge uses this to drop a
    /// message addressed to a non-edge (e.g. a stale teammate) or a dead child, since messaging is
    /// tree-edges only and a tombstoned child's inbox has no reader.
    pub async fn resolve_edge(&self, name: &AgentName) -> Option<Addressee> {
        if let Some(parent) = self.node_path.parent() {
            if &parent.name() == name {
                return Some(Addressee::Parent);
            }
        }
        let records = match self.read_child_records().await {
            Ok(r) => r,
            Err(e) => {
                tracing::warn!(
                    target = name.as_str(),
                    error = %e,
                    "resolve_edge: child ledger read failed; cannot resolve edge (message may be silently dropped by the caller)"
                );
                return None;
            }
        };
        match exo_caps::fold_children(&records).get(name) {
            Some(c) if c.state.is_terminal() => {
                tracing::warn!(
                    target = name.as_str(),
                    state = ?c.state,
                    "resolve_edge: child is tombstoned; not resolving"
                );
                None
            }
            Some(_) => Some(Addressee::Child(name.clone())),
            None => None,
        }
    }

    /// Build a periodic status snapshot. `role_str` is the node's domain role as its stable string
    /// (the engine no longer knows the domain role enum; the caller passes `D::Role::role_str`).
    /// `children[].busy` is pane-existence (a live probe, via [`exo_caps::Tmux::list_panes`]) — the
    /// one signal that's true regardless of turn boundaries. There used to be a separate busy-bit
    /// derived from Claude Code's `Stop` hook; it was removed (see `rust/exo/CLAUDE.md`) because
    /// `Stop` fires on every turn-end, including a legitimate async-wait yield, so the bit was
    /// routinely wrong. Best-effort, but never silently dishonest: `NodeStatus` has no "probe
    /// failed" slot, so a failure picks the least-false representation instead of defaulting to
    /// empty. A **ledger** read failure means the child set itself is unknown (not
    /// verified-empty) — logged loud, then reported as no children, since there is no known name
    /// to report anything else against. A **pane-probe** failure means the child set IS known but
    /// liveness isn't — logged loud, then every known child is reported `busy: true`, mirroring
    /// `ChildLiveness::any_child_busy`'s "probe failure ⇒ assume busy, never manufacture a false
    /// idle" discipline.
    pub async fn status_snapshot(&self, role_str: &str, shutdown_pending: bool) -> NodeStatus {
        let records = match self.read_child_records().await {
            Ok(r) => r,
            Err(e) => {
                tracing::warn!(
                    error = %e,
                    "status_snapshot: child ledger read failed; reporting no children (unknown, not a verified empty set)"
                );
                Vec::new()
            }
        };
        let folded = exo_caps::fold_children(&records);
        let alive = match exo_caps::Tmux::list_panes(self).await {
            Ok(set) => Some(set),
            Err(e) => {
                tracing::warn!(
                    error = %e,
                    "status_snapshot: pane probe failed; reporting all known children as busy (never manufacture a false idle)"
                );
                None
            }
        };
        let children = folded
            .into_values()
            .filter(|c| !c.state.is_terminal())
            .map(|c| ChildStatus {
                busy: match &alive {
                    Some(set) => set.contains(c.pane.as_str()),
                    None => true,
                },
                name: c.name.as_str().to_string(),
            })
            .collect();

        NodeStatus {
            node: self.node_path.clone(),
            kind: role_str.to_string(),
            branch: self.branch.as_str().to_string(),
            shutdown_pending,
            // Sidecar state, not runtime state: the status publisher overwrites this from its
            // live ListenerSlot before writing the snapshot. The runtime never sees the socket.
            listener_connected: false,
            children,
            ts: chrono::Utc::now(),
        }
    }
}

pub(crate) fn home() -> PathBuf {
    std::env::var("HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("."))
}
