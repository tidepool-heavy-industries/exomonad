//! The `Spawner` cap — the recursion. **Per-op methods**, each fixing its own
//! `(role, agent_type, kind)`, so illegal triples are *unnameable*. A shared private
//! `birth(BirthCore)` tail (in the runtime impl) does the common sequence:
//! append `AgentSpawned` → (`git worktree add` for a Worktree child) → `tmux new-pane` →
//! write child papers (incl. `parent_inbox`) → launch `exo node`
//! (see [`crate::invocation`]).

use crate::types::AgentName;
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

// Task-content specs — the ONLY thing the caller supplies; `(role, agent_type, kind)`
// are fixed by which method is called. Field lists port field-for-field from the
// Haskell `WorkerSpec` / `SpawnSpec` in Wave 3 — `task` is a placeholder for the full set
// (steps / verify / done_criteria / context / boundary / read_first / …).

/// → Inline / Worker / Gemini.
#[derive(Debug, Clone)]
pub struct WorkerSpec {
    pub name: Option<AgentName>,
    pub task: String,
    pub steps: Vec<String>,
    pub verify: Vec<String>,
    pub done_criteria: Vec<String>,
    pub context: Option<String>,
    pub boundary: Vec<String>,
    pub read_first: Vec<String>,
}

/// → Worktree / Dev / Gemini.
#[derive(Debug, Clone)]
pub struct GeminiSpec {
    pub name: Option<AgentName>,
    pub task: String,
    pub steps: Vec<String>,
    pub verify: Vec<String>,
    pub done_criteria: Vec<String>,
    pub context: Option<String>,
    pub boundary: Vec<String>,
    pub read_first: Vec<String>,
}

/// → Worktree / Tl / Claude.
#[derive(Debug, Clone)]
pub struct ForkSpec {
    pub name: Option<AgentName>,
    pub task: String,
    pub steps: Vec<String>,
    pub verify: Vec<String>,
    pub done_criteria: Vec<String>,
    pub context: Option<String>,
    pub boundary: Vec<String>,
    pub read_first: Vec<String>,
}

#[async_trait]
pub trait Spawner {
    async fn spawn_worker(&self, spec: WorkerSpec) -> Result<AgentName, SpawnError>;
    async fn spawn_gemini(&self, spec: GeminiSpec) -> Result<AgentName, SpawnError>;
    /// Spawn a short-lived **reviewer** of the caller's branch: a Gemini in its OWN worktree
    /// branched off the *current* branch (the under-review code), `role = Reviewer`. Mirrors
    /// `spawn_gemini` but fixes the role — the reviewer reads the diff, emits a `verdict`, exits.
    async fn spawn_reviewer(&self, spec: GeminiSpec) -> Result<AgentName, SpawnError>;
    /// Fork a wave — **per-spec results**, so one bad fork doesn't discard the children
    /// that did spawn (the TL converges on what succeeded, re-decomposes the failures).
    async fn fork_wave(&self, specs: Vec<ForkSpec>) -> Vec<Result<AgentName, SpawnError>>;

    /// Teardown is **two independent steps, not one reap**. Worktree
    /// reclamation is parent-side, run at convergence (after the child's PR merges); an
    /// `Inline` child has no worktree to reclaim.
    async fn reclaim_worktree(&self, child: &AgentName) -> Result<(), SpawnError>;
    /// Forceful process teardown of a non-responsive child (graceful shutdown is a
    /// `Control(Shutdown)` *message* the child self-applies — not here).
    async fn kill_pane(&self, child: &AgentName) -> Result<(), SpawnError>;
}
