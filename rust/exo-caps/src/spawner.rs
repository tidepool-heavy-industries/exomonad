//! The `Spawner` cap — the recursion. **Per-op methods**, each fixing its own
//! `(role, agent_type, kind)`, so illegal triples are *unnameable*. A shared private
//! `birth(BirthCore)` tail (in the runtime impl) does the common sequence:
//! append `AgentSpawned` → (`git worktree add` for a Worktree child) → `tmux new-pane` →
//! write child papers (incl. `parent_inbox`) → launch `exomonad mcp-stdio`. See docs 03/04.

use crate::error::CapResult;
use crate::types::AgentName;
use async_trait::async_trait;

// `ChildKind` lives in `types` — it's a shared domain enum (used here and by
// `lifecycle::ChildRecord`), not specific to the `Spawner` trait.

// Task-content specs — the ONLY thing the caller supplies; `(role, agent_type, kind)`
// are fixed by which method is called. Field lists port field-for-field from the
// Haskell `WorkerSpec` / `SpawnSpec` in Wave 3 — `task` is a placeholder for the full set
// (steps / verify / done_criteria / context / boundary / read_first / …).

/// → Inline / Worker / Gemini.
#[derive(Debug, Clone)]
pub struct WorkerSpec {
    pub name: Option<AgentName>,
    pub task: String,
}

/// → Worktree / Dev / Gemini.
#[derive(Debug, Clone)]
pub struct GeminiSpec {
    pub name: Option<AgentName>,
    pub task: String,
}

/// → Worktree / Tl / Claude.
#[derive(Debug, Clone)]
pub struct ForkSpec {
    pub name: Option<AgentName>,
    pub task: String,
}

#[async_trait]
pub trait Spawner {
    async fn spawn_worker(&self, spec: WorkerSpec) -> CapResult<AgentName>;
    async fn spawn_gemini(&self, spec: GeminiSpec) -> CapResult<AgentName>;
    async fn fork_wave(&self, specs: Vec<ForkSpec>) -> CapResult<Vec<AgentName>>;

    /// Teardown is **two independent steps, not one reap** (see docs 03/04). Worktree
    /// reclamation is parent-side, run at convergence (after the child's PR merges); an
    /// `Inline` child has no worktree to reclaim.
    async fn reclaim_worktree(&self, child: &AgentName) -> CapResult<()>;
    /// Forceful process teardown of a non-responsive child (graceful shutdown is a
    /// `Control(Shutdown)` *message* the child self-applies — not here).
    async fn kill_pane(&self, child: &AgentName) -> CapResult<()>;
}
