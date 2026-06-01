//! Node self-identification + context assembly — the **real** scaffold the loop leaves
//! build on. See `docs/design/swarm/01-identity.md`.
//!
//! A node boots by reading its **papers** (`--papers <path>`, written by the parent at
//! spawn — the parent never makes the child guess where they are) into [`NodePapers`], then
//! enriching with the ambient run context (`$TMUX_PANE`, `EXOMONAD_SWARM_RUN_ID`,
//! `EXOMONAD_TMUX_SESSION`) and, lazily, CC team membership via `exo-scry`. The result is a
//! [`NodeContext`] holding a real [`Runtime`] (the `R` policy monomorphizes against) plus the
//! [`NodeKind`] needed to pick the role's toolset/hooks via [`exo_policy::role_def`].

use std::path::{Path, PathBuf};
use std::sync::Arc;

use exo_caps::{Branch, InboxPath, NodeKind, NodePapers, NodePath, PaneId};
use exo_runtime::Runtime;

use crate::error::{NodeError, NodeResult};

/// Everything a running node needs: its identity, the concrete [`Runtime`] (all caps), and
/// the ambient context the loops read. `Runtime` does not itself store the [`NodeKind`]
/// (its identity is the tree address + branch), so the context carries it for
/// [`exo_policy::role_def`].
pub struct NodeContext {
    /// The concrete runtime — implements every `exo-caps` capability. Policy monomorphizes
    /// against this `R`; the outbound loop serves `role_def::<Runtime>(kind).tools`.
    pub runtime: Arc<Runtime>,
    /// This node's archetype (from papers). Drives `role_def` + the last-hop agent_type.
    pub kind: NodeKind,
    /// This node's own pane — the inbox key + the tmux-paste target for self-injection.
    pub own_pane: PaneId,
    /// This node's own ingestion inbox (`…/inboxes/{run_id}/pane-N.jsonl`) — what the
    /// inbound loop watches and `InjectMessage` appends to.
    pub own_inbox: InboxPath,
    /// The parent's ingestion inbox (`Bus::deliver(Parent, …)`); `None` for the root.
    pub parent_inbox: Option<InboxPath>,
    /// Swarm run-id namespace.
    pub run_id: String,
}

impl NodeContext {
    /// `true` for the un-parented root (no up-edge).
    pub fn is_root(&self) -> bool {
        self.parent_inbox.is_none()
    }
}

/// Read+parse the node's papers from the `--papers` path.
fn load_papers(papers_path: &Path) -> NodeResult<NodePapers> {
    let bytes = std::fs::read(papers_path).map_err(|e| NodeError::Papers {
        path: papers_path.display().to_string(),
        detail: e.to_string(),
    })?;
    serde_json::from_slice(&bytes).map_err(|e| NodeError::Papers {
        path: papers_path.display().to_string(),
        detail: e.to_string(),
    })
}

fn home() -> PathBuf {
    std::env::var("HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("."))
}

/// Self-ID and assemble the [`NodeContext`].
///
/// Inputs:
/// - `papers_path` — the `--papers <path>` flag value (the node is *told* where its papers
///   are; it never guesses). Required for an exomonad-spawned node.
/// - `working_dir` — the node's cwd (worktree root for a Worktree child).
///
/// Ambient (env): `$TMUX_PANE` (the universal key — must agree with papers.pane),
/// `EXOMONAD_SWARM_RUN_ID`, `EXOMONAD_TMUX_SESSION`.
pub fn bootstrap(papers_path: &Path, working_dir: PathBuf) -> NodeResult<NodeContext> {
    let papers = load_papers(papers_path)?;

    let run_id = std::env::var("EXOMONAD_SWARM_RUN_ID")
        .map_err(|_| NodeError::MissingContext("EXOMONAD_SWARM_RUN_ID"))?;
    let tmux_session = std::env::var("EXOMONAD_TMUX_SESSION")
        .map_err(|_| NodeError::MissingContext("EXOMONAD_TMUX_SESSION"))?;

    // The pane in papers is authoritative; `$TMUX_PANE` is the live cross-check.
    let own_pane = papers.pane.clone();

    let own_inbox = exo_caps::paths::inbox_path(&home(), &run_id, &own_pane);

    let node_path: NodePath = papers.path.clone();
    let branch: Branch = papers.branch.clone();
    let kind: NodeKind = papers.role;
    let parent_inbox: Option<InboxPath> = papers.parent_inbox.clone();

    let runtime = Runtime::new(
        node_path,
        branch,
        working_dir,
        parent_inbox.clone(),
        run_id.clone(),
        tmux_session,
        own_pane.clone(),
    );

    Ok(NodeContext {
        runtime: Arc::new(runtime),
        kind,
        own_pane,
        own_inbox,
        parent_inbox,
        run_id,
    })
}
