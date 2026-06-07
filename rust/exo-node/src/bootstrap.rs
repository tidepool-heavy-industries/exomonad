//! Node self-identification + context assembly — the **real** scaffold the loop leaves
//! build on.
//!
//! A node boots by reading its **papers** (`--papers <path>`, written by the parent at
//! spawn — the parent never makes the child guess where they are) into [`NodePapers`], then
//! enriching with the ambient run context (`$TMUX_PANE`, `EXOMONAD_SWARM_RUN_ID`,
//! `EXOMONAD_TMUX_SESSION`) and, lazily, CC team membership via `exo-scry`. The result is a
//! [`NodeContext`] holding a real [`Runtime`] (the `R` policy monomorphizes against), the
//! the [`NodeContext`] holding a [`Runtime`], plus the node's `D::Role` (typed from papers) used to
//! pick the role's toolset/hooks.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use exo_caps::{Branch, InboxPath, NodePapers, NodePath, PaneId};
use exo_framework::Exomonad;
use exo_runtime::Runtime;
use tracing::warn;

use crate::error::{NodeError, NodeResult};

/// Everything a running node needs: its identity, the concrete [`Runtime`] (all caps), and the
/// ambient context the loops read — generic over the domain `D` ([`Exomonad`]). The engine resolves
/// a role's tools/hooks through `D::role_def` (static dispatch) rather than an injected registry, and
/// reacts to domain system messages through `D::handle_system`. `Runtime` does not itself store the
/// role (its identity is the tree address + branch), so the context carries it for role resolution.
#[derive(Debug)]
pub struct NodeContext<D: Exomonad> {
    /// The concrete runtime — implements every `exo-caps` capability. Policy monomorphizes
    /// against this `R`; the outbound loop serves `D::role_def(kind).tools`.
    pub runtime: Arc<Runtime>,
    /// This node's role (from papers). Drives the role resolution + the last-hop agent_type.
    pub kind: D::Role,
    /// This node's own pane — the inbox key + the tmux-paste target for self-injection.
    pub own_pane: PaneId,
    /// This node's own ingestion inbox (`…/inboxes/{run_id}/pane-N.jsonl`) — what the
    /// inbound loop watches and `InjectMessage` appends to.
    pub own_inbox: InboxPath,
    /// The parent's ingestion inbox (`Bus::deliver(Parent, …)`); `None` for the root.
    pub parent_inbox: Option<InboxPath>,
    /// Swarm run-id namespace.
    pub run_id: String,
    /// Cooperative-shutdown state. `None` until a `Shutdown` is accepted (a cooperative leaf, or a
    /// forced node); `Some(grace_ms)` once pending — the sidecar reaps itself when its subtree is
    /// clear (see `inbound::try_reap`). Read by both the inbound loop and the stop-hook path.
    pub shutdown_pending: std::sync::Mutex<Option<u32>>,
    /// Children (by name) that have sent `ChildExited` — the authoritative "gone" set `try_reap`
    /// uses to decide childlessness without racing pane-death timing.
    pub exited_children: std::sync::Mutex<std::collections::HashSet<String>>,
}

impl<D: Exomonad> NodeContext<D> {
    /// Mark this node as shutting down (cooperative or forced); the sidecar reaps itself once its
    /// subtree is clear. `grace_ms` is the pre-kill backstop applied at the actual reap.
    pub fn set_shutdown_pending(&self, grace_ms: u32) {
        *self.shutdown_pending.lock().unwrap() = Some(grace_ms);
    }

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

/// Self-ID and assemble the [`NodeContext`].
///
/// Inputs:
/// - `papers_path` — the `--papers <path>` flag value (the node is *told* where its papers
///   are; it never guesses). Required for an exomonad-spawned node.
/// - `working_dir` — the node's cwd (worktree root for a Worktree child).
///
/// Ambient (env): `$TMUX_PANE` (the universal key — must agree with papers.pane),
/// `EXOMONAD_SWARM_RUN_ID`, `EXOMONAD_TMUX_SESSION`, `$HOME`.
///
/// Generic over the domain `D` — monomorphized once at the binary (`bootstrap::<exo::ExoDomain>`).
/// The role is read off the papers (recorded erased) and typed back to `D::Role` here — bootstrap
/// is the role's one typed reader. `Caps = Runtime` is inherent: the sidecar builds the concrete
/// `Runtime` here, so the domain's tools monomorphize against it.
pub fn bootstrap<D: Exomonad<Caps = Runtime>>(
    papers_path: &Path,
    working_dir: PathBuf,
) -> NodeResult<NodeContext<D>> {
    let papers = load_papers(papers_path)?;

    let run_id = std::env::var("EXOMONAD_SWARM_RUN_ID")
        .map_err(|_| NodeError::MissingContext("EXOMONAD_SWARM_RUN_ID"))?;
    let tmux_session = std::env::var("EXOMONAD_TMUX_SESSION")
        .map_err(|_| NodeError::MissingContext("EXOMONAD_TMUX_SESSION"))?;
    // `$HOME` is required, NOT silently defaulted: the inbox dir is `$HOME/.claude/exo/...`,
    // and a fallback to `.` would point `own_inbox` under cwd — the node would watch a file
    // the parent never writes and silently receive nothing. Fail loudly instead.
    let home = std::env::var("HOME").map_err(|_| NodeError::MissingContext("HOME"))?;

    // The pane in papers is authoritative; cross-check it against the live `$TMUX_PANE` and warn
    // on mismatch — a stale papers file reused on a recycled pane id would otherwise have the node
    // silently watch the wrong inbox.
    let own_pane = papers.pane.clone();
    if let Ok(live) = std::env::var("TMUX_PANE") {
        if live != own_pane.as_str() {
            warn!(
                "papers pane {} != live $TMUX_PANE {} — using papers pane (possible stale papers)",
                own_pane.as_str(),
                live
            );
        }
    }

    let own_inbox = exo_caps::paths::inbox_path(Path::new(&home), &run_id, &own_pane);

    // Ensure the inbox directory exists before the inbound loop watches it. `notify` cannot
    // watch a missing parent dir, and nothing else creates it for the root (children's dirs
    // are likewise created here at their own bootstrap). The file is created on first write.
    if let Some(inbox_dir) = own_inbox.as_path().parent() {
        std::fs::create_dir_all(inbox_dir)?;
    }

    let node_path: NodePath = papers.path.clone();
    let branch: Branch = papers.branch.clone();
    // The role is recorded erased; type it back to this domain's role enum (the one typed read).
    let kind: D::Role = papers.role.typed::<D::Role>().map_err(|e| NodeError::Papers {
        path: papers_path.display().to_string(),
        detail: format!("role: {e}"),
    })?;
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
        shutdown_pending: std::sync::Mutex::new(None),
        exited_children: std::sync::Mutex::new(std::collections::HashSet::new()),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::TestRole;
    use exo_caps::{AgentName, Branch, InboxPath, NodePapers, NodePath, PaneId};
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_bootstrap_logic() {
        let dir = tempdir().unwrap();
        let papers_path = dir.path().join("papers.json");
        let working_dir = dir.path().join("work");
        fs::create_dir(&working_dir).unwrap();

        let own_pane = PaneId::new("%42".into()).unwrap();
        let parent_inbox = InboxPath::new(dir.path().join("parent-inbox.jsonl"));
        let node_path = NodePath::new(vec![
            AgentName::new("root".into()).unwrap(),
            AgentName::new("me".into()).unwrap(),
        ])
        .unwrap();
        let branch = Branch::new("root.me".into()).unwrap();

        let papers = NodePapers::new(
            node_path.clone(),
            branch.clone(),
            TestRole::Tl,
            own_pane.clone(),
            Some(parent_inbox.clone()),
            NodePapers::DEFAULT_YOLO,
            NodePapers::DEFAULT_WRAP_NIX,
        )
        .unwrap();

        let papers_json = serde_json::to_string(&papers).unwrap();
        fs::write(&papers_path, papers_json).unwrap();

        // 1. Success case
        std::env::set_var("EXOMONAD_SWARM_RUN_ID", "test-run");
        std::env::set_var("EXOMONAD_TMUX_SESSION", "test-session");
        std::env::set_var("HOME", dir.path().to_str().unwrap());

        let ctx = bootstrap::<crate::test_support::TestDomain>(&papers_path, working_dir.clone())
            .unwrap();

        assert_eq!(ctx.kind, TestRole::Tl);
        assert_eq!(ctx.own_pane, own_pane);
        assert_eq!(ctx.parent_inbox, Some(parent_inbox));
        assert_eq!(ctx.run_id, "test-run");
        assert_eq!(ctx.runtime.name(), AgentName::new("me".into()).unwrap());

        // 2. Missing env case
        std::env::remove_var("EXOMONAD_SWARM_RUN_ID");
        let res =
            bootstrap::<crate::test_support::TestDomain>(&papers_path, working_dir.clone());
        assert!(res.is_err());

        // Cleanup env
        std::env::remove_var("EXOMONAD_TMUX_SESSION");
        std::env::remove_var("HOME");
    }
}
