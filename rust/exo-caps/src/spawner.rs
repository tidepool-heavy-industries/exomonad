//! The `Spawner` cap — the recursion, collapsed to **one generic op**. A domain hands a
//! [`SpawnSpec`](crate::SpawnSpec) (its `D::Spawn`) carrying the `(role, kind)` it fixed at the
//! tool boundary, and the runtime births it through one private `birth` tail:
//! append `Spawned` record → (`git worktree add` for a Worktree child) → `tmux new-pane` →
//! write child papers (incl. `parent_inbox`) → launch `exo node` (see [`crate::invocation`]).
//!
//! Replaces the old per-archetype methods (`spawn_worker`/`spawn_gemini`/`spawn_reviewer`/the
//! per-op `fork_wave`): a new archetype is now a new domain role + a thin domain tool wrapper that
//! builds a `D::Spawn`, **not** a new `Spawner` method (an `exo-caps` edit). The role-fixing moved
//! out of the cap and into the domain.

use crate::types::{AgentName, NodeKind};
use crate::SpawnSpec;
use async_trait::async_trait;
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
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

#[async_trait]
pub trait Spawner {
    /// Birth one child from a domain spawn intent. The `(role, kind)` are read off the spec (fixed
    /// by whichever domain tool built it), so an illegal pairing is unnameable at that boundary.
    ///
    /// Transitionally bounded `Role = NodeKind` (the runtime still writes a `NodeKind` into papers);
    /// P5 relaxes this to the fully generic `S: SpawnSpec` once papers carry `D::Role`.
    async fn spawn<S: SpawnSpec<Role = NodeKind>>(
        &self,
        spec: S,
    ) -> Result<AgentName, SpawnError>;

    /// Fork a wave — **per-spec results**, so one bad fork doesn't discard the children that did
    /// spawn (the TL converges on what succeeded, re-decomposes the failures). A thin sequential
    /// wrapper over [`spawn`](Spawner::spawn); a domain `fork_wave` tool passes a `Vec<D::Spawn>`.
    async fn fork_wave<S: SpawnSpec<Role = NodeKind>>(
        &self,
        specs: Vec<S>,
    ) -> Vec<Result<AgentName, SpawnError>> {
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
