//! `Topology` capability — a node's view of its own place in the swarm tree: the caller's
//! subtree (folded recursively from the per-node `children.jsonl` ledgers), its parent, and a
//! per-node liveness proxy.
//!
//! The filesystem *is* the registry — each node appends a `Spawned` record to its own
//! `.exo/children.jsonl` ([`ChildRecord`](crate::ChildRecord)). This cap walks that, descending
//! into each worktree child's nested ledger, so a Root/Tl can answer "what does my subtree look
//! like, who's my parent, who's still alive?". Liveness is **pane-existence** (the agent's tmux
//! pane is still there) — a true sidecar round-trip ping isn't available (the sidecar is
//! stdio-bound to its agent, not socket-pingable).

use crate::fs::Fs;
use crate::lifecycle::ChildState;
use crate::tmux::Tmux;
use crate::types::{AgentName, ChildKind, PaneId};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// `Topology` failures.
#[derive(Debug, Error)]
pub enum TopologyError {
    #[error("topology {op} failed: {detail}")]
    Failed { op: &'static str, detail: String },
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

/// One node in the tree view. `kind` is `None` for the caller itself (the runtime doesn't store
/// its own `ChildKind`) and `Some(_)` for nodes folded from a parent's ledger.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TreeNode {
    /// The node's name (last segment of its tree address).
    pub name: AgentName,
    /// How the node relates to its parent's worktree; `None` for the caller/self.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub kind: Option<ChildKind>,
    /// The node's tmux pane id.
    pub pane: PaneId,
    /// Liveness proxy: the node's tmux pane still exists. **Forced `false` for a node whose
    /// [`state`](TreeNode::state) is terminal** — tmux recycles pane ids, so probing a tombstoned
    /// node's recorded pane can alias onto a different, live agent and report a dead child alive.
    pub pane_alive: bool,
    /// The node's folded lifecycle state; `None` for the caller itself (a node records no state
    /// about itself — only its parent's ledger does).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub state: Option<ChildState>,
    /// The effective launch model folded from the node's `Spawned` record (e.g. `"sonnet"`);
    /// `None` for the caller itself or a pre-field ledger.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub model: Option<String>,
    /// Cosmetic model tag (e.g. `"kimi"`) for a node launched on a non-default model via a
    /// launch profile; `None` for default Claude / the caller itself.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub model_label: Option<String>,
    /// Hash of the directives bundle the node was launched with, folded from its `Spawned`
    /// record; `None` for the caller itself or a node spawned without directives.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub directives_hash: Option<String>,
    /// Children folded from this node's ledger (recursive; worktree children only — inline
    /// children share the parent's worktree and spawn nothing).
    pub children: Vec<TreeNode>,
}

/// What [`Topology::topology`] returns: the caller (self + its subtree), its parent, and its
/// full tree address.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TopologyView {
    /// The caller node and its recursive subtree.
    pub node: TreeNode,
    /// The parent node's name; `None` for the root.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub parent: Option<String>,
    /// The caller's full tree address (`NodePath` segments, root-first).
    pub path: Vec<AgentName>,
}

/// **Composite cap** — the tree walk reads the per-node ledgers (`Fs`) and probes pane
/// liveness ([`Tmux::list_panes`]); the supertraits name those powers.
#[async_trait]
pub trait Topology: Tmux + Fs {
    /// The caller's subtree + parent + per-node pane-liveness. Liveness is best-effort: a tmux
    /// probe failure marks nodes not-alive rather than failing the whole call.
    async fn topology(&self) -> Result<TopologyView, TopologyError>;
}
