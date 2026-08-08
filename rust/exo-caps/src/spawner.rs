//! The `Spawner` cap — the recursion, collapsed to **one generic op**. A domain hands a
//! [`SpawnSpec`](crate::SpawnSpec) (its `D::Spawn`) carrying the `(role, kind)` it fixed at the
//! tool boundary, and the runtime births it through one private `birth` tail:
//! append `Spawned` record → (`git worktree add` for a Worktree child) → `tmux new-pane` →
//! write child papers (incl. `parent_inbox`) → launch `exo node` (see [`crate::invocation`]).
//!
//! A new archetype is a new domain role + a thin domain tool wrapper that builds a `D::Spawn`,
//! **not** a new `Spawner` method (an `exo-caps` edit). The role-fixing lives in the domain, not
//! the cap.

use crate::fs::Fs;
use crate::git::Git;
use crate::tmux::Tmux;
use crate::types::{AgentName, ChildKind};
use crate::SpawnSpec;
use async_trait::async_trait;
use std::path::Path;
use thiserror::Error;

// `ChildKind` lives in `types` — it's a shared domain enum (used here and by
// `lifecycle::ChildRecord`), not specific to the `Spawner` trait.

#[derive(Debug, Error)]
pub enum SpawnError {
    #[error("spawn {op} failed for {child:?}: {detail}")]
    Failed {
        op: &'static str,
        child: Option<AgentName>,
        detail: String,
    },
    /// The named child is not in this node's `children.jsonl` fold — a typo, a name that belongs to
    /// a different node's subtree, or a child that was never born. Distinct from
    /// [`Failed`](SpawnError::Failed) so a caller can tell "you named the wrong thing" from "the op
    /// itself broke".
    #[error("unknown child {0:?} — not in this node's children ledger")]
    UnknownChild(AgentName),
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

/// The isolation preamble prepended to every child's launch prompt. Lives here — beside the
/// [`ChildKind`] it branches on — rather than inside the runtime's birth tail, so the wording is
/// one shared string and a caller that renders a prompt outside `birth` can't drift from it.
pub fn birth_preamble(kind: ChildKind, child_dir: &Path) -> String {
    match kind {
        ChildKind::Worktree => format!(
            "You are working in an ISOLATED git worktree at `{}` — this is your repo root. ALL file \
             paths are relative to it. Do NOT read or write files outside this directory (never touch \
             the parent repository). Commit your work to your branch here.\n\n",
            child_dir.display()
        ),
        ChildKind::Inline => format!(
            "You are working in the repository at `{}`. ALL file paths are relative to it. \
             Do NOT read or write files outside this directory.\n\n",
            child_dir.display()
        ),
    }
}

/// **Composite cap** — birth orchestrates across the primitives it declares as supertraits:
/// `Git` (worktree add/remove), `Tmux` (holding-shell pane + launch paste + teardown), `Fs`
/// (papers). An impl must also impl those primitives, so a `Spawner` can never quietly
/// re-shell a domain a primitive already owns.
#[async_trait]
pub trait Spawner: Git + Tmux + Fs {
    /// Birth one child from a domain spawn intent. The `(role, kind)` are read off the spec (fixed
    /// by whichever domain tool built it), so an illegal pairing is unnameable at that boundary.
    /// Fully generic over the domain's role — the runtime records the role erased ([`RoleRecord`](crate::RoleRecord)).
    async fn spawn<S: SpawnSpec>(&self, spec: S) -> Result<AgentName, SpawnError>;

    /// Fork a wave — **per-spec results**, so one bad fork doesn't discard the children that did
    /// spawn (the TL converges on what succeeded, re-decomposes the failures). A thin sequential
    /// wrapper over [`spawn`](Spawner::spawn); a domain `fork_wave` tool passes a `Vec<D::Spawn>`.
    async fn fork_wave<S: SpawnSpec>(&self, specs: Vec<S>) -> Vec<Result<AgentName, SpawnError>> {
        let mut out = Vec::with_capacity(specs.len());
        for spec in specs {
            out.push(self.spawn(spec).await);
        }
        out
    }

    /// Teardown is **two independent steps, not one reap**. Worktree reclamation is parent-side, run
    /// at convergence (after the child's branch merges); an `Inline` child has no worktree to reclaim.
    async fn reclaim_worktree(&self, child: &AgentName) -> Result<(), SpawnError>;
    /// Forceful process teardown of a non-responsive child (graceful shutdown is a
    /// `Control(Shutdown)` *message* the child self-applies — not here).
    async fn kill_pane(&self, child: &AgentName) -> Result<(), SpawnError>;
}
