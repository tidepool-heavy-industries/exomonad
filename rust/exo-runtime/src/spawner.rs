//! `impl Spawner for Runtime` — the recursion (birth + teardown). **Race-prone; built by
//! the Spawner sub-TL, decomposed S1/S2/S3 — never one leaf.** See docs 03/04, plan 06.
//!
//! Per-op methods each fix their own `(role, agent_type, kind)`; the spec carries only
//! task content. All three ops funnel through one private `birth(BirthCore)` tail:
//!   append `AgentSpawned` record FIRST (so there's never an untracked process)
//!   → (`git worktree add` for a Worktree child — Inline shares the parent's cwd)
//!   → `tmux new_pane`
//!   → write child papers (`node.json`, incl. `parent_inbox` = my inbox)
//!   → launch `exomonad mcp-stdio` in the pane.
//!
//! Decomposition:
//!   - **S1**: safe branch-gen (`Branch::from_path`) + `git worktree add` (Worktree only).
//!   - **S2**: the `birth(BirthCore)` core (record-first ordering is the load-bearing race
//!     guard — log intent before the pane exists).
//!   - **S3**: teardown — `reclaim_worktree` (`git worktree remove`, parent-side at
//!     convergence) + force `kill_pane`.
//!
//! HARD RULE: `tokio::process`/`spawn_blocking`; reuse `Git`/`Tmux` cap impls + the
//! exomonad-core `GitWorktreeService`/`TmuxIpc` — do not re-shell git/tmux by hand where a
//! cap already does it.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{
    AgentName, AgentType, Branch, ChildKind, ForkSpec, GeminiSpec, SpawnError, Spawner, WorkerSpec,
};

/// The fixed triple + identity each op hands to the shared `birth` tail. Constructed by
/// the per-op method (the single place a triple is named); `birth` branches only on `kind`.
#[derive(Debug, Clone)]
pub(crate) struct BirthCore {
    pub kind: ChildKind,
    pub agent_type: AgentType,
    pub name: AgentName,
    pub branch: Branch,
    pub task: String,
}

impl Runtime {
    /// The shared birth tail. **S2.** Record-first, then pane, then papers, then launch.
    pub(crate) async fn birth(&self, _core: BirthCore) -> Result<AgentName, SpawnError> {
        todo!(
            "S2: append AgentSpawned FIRST -> (worktree add if kind==Worktree) -> \
             tmux new_pane -> write child node.json (parent_inbox = mine) -> launch mcp-stdio"
        )
    }
}

#[async_trait]
impl Spawner for Runtime {
    async fn spawn_worker(&self, _spec: WorkerSpec) -> Result<AgentName, SpawnError> {
        todo!("S2: fix (Worker, Gemini, Inline); build BirthCore; self.birth(core).await")
    }

    async fn spawn_gemini(&self, _spec: GeminiSpec) -> Result<AgentName, SpawnError> {
        todo!("S2: fix (Dev, Gemini, Worktree); build BirthCore; self.birth(core).await")
    }

    async fn fork_wave(&self, _specs: Vec<ForkSpec>) -> Vec<Result<AgentName, SpawnError>> {
        todo!("S2: fix (Tl, Claude, Worktree) per spec; birth each; collect per-spec Results")
    }

    async fn reclaim_worktree(&self, _child: &AgentName) -> Result<(), SpawnError> {
        todo!("S3: look up child worktree path; git worktree remove (parent-side, at converge)")
    }

    async fn kill_pane(&self, _child: &AgentName) -> Result<(), SpawnError> {
        todo!("S3: fold children -> child.pane -> tmux kill-pane (forceful teardown)")
    }
}
