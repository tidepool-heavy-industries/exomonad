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
//!
//! ## Record-first ordering — the load-bearing race guard (read before editing `birth`)
//!
//! The frozen [`ChildRecord::Spawned`] stores the child's `pane` id, yet the parent must
//! log the spawn **before** an *agent* process exists (so a parent crash never leaves an
//! untracked agent). A pane id doesn't exist until tmux creates the pane — so these are
//! reconciled by **two-phase pane creation**:
//!
//!   1. (Worktree only) `git worktree add` — the child's dir.
//!   2. `Tmux::new_pane(cwd, $SHELL)` — a **holding shell**, NOT the agent. Returns `%N`.
//!   3. Append `Spawned { child, kind, pane: %N, inbox }` to `children.jsonl`. ← THE GUARD.
//!   4. Write the child's `node.json` papers (`parent_inbox` = *my* inbox).
//!   5. `Tmux::paste(%N, "<launch cmd>\n")` — inject the agent command into the holding
//!      shell, starting `claude`/`gemini` (+ its `exomonad mcp-stdio` sidecar via .mcp.json).
//!
//! The record precedes the **agent** launch (step 3 before step 5). The holding shell
//! (step 2) carries no agent, so a crash before step 5 leaves only a bare shell — nothing
//! untracked. **Do not collapse steps 2+5 into a one-shot `new_pane(cwd, launch_cmd)`** —
//! that reopens the orphan window the two-phase split closes.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{
    AgentName, AgentType, Branch, ChildKind, ChildRecord, ForkSpec, GeminiSpec, InboxPath, PaneId,
    SpawnError, Spawner, WorkerSpec,
};
use std::path::PathBuf;
use tokio::io::AsyncWriteExt;

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

// ── Shared ledger + inbox-scheme helpers (Spawner-TL scaffold) ───────────────────────────
// Used by S2 (`birth` appends `Spawned`) and S3 (`reclaim_worktree`/`kill_pane` read+fold).
// Self-contained in this file so leaves edit only `spawner.rs`. The inbox-path scheme is
// duplicated by the `Bus` leaf (R4) for resolution; hoisting to a shared module is a
// Runtime-TL converge concern, deliberately not done here (would touch a sibling's file).
impl Runtime {
    /// This node's parent-local child ledger (`{working_dir}/.exo/children.jsonl`).
    pub(crate) fn children_log_path(&self) -> PathBuf {
        self.working_dir.join(".exo/children.jsonl")
    }

    /// Append one lifecycle record. **Single-writer** (this node owns its ledger), so a
    /// plain `append` is race-free — none of the multi-writer-bus PIPE_BUF dance applies.
    pub(crate) async fn append_child_record(&self, rec: &ChildRecord) -> Result<(), SpawnError> {
        let path = self.children_log_path();
        if let Some(dir) = path.parent() {
            tokio::fs::create_dir_all(dir).await?;
        }
        let mut line = serde_json::to_string(rec).map_err(|e| SpawnError::Failed {
            op: "record_encode",
            child: Some(rec.child().clone()),
            detail: e.to_string(),
        })?;
        line.push('\n');
        let mut f = tokio::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&path)
            .await?;
        f.write_all(line.as_bytes()).await?;
        Ok(())
    }

    /// Read + parse the child ledger (fold with [`exo_caps::fold_children`] for the current
    /// child set). A missing file means no children yet → empty, not an error.
    pub(crate) async fn read_child_records(&self) -> Result<Vec<ChildRecord>, SpawnError> {
        let path = self.children_log_path();
        let content = match tokio::fs::read_to_string(&path).await {
            Ok(c) => c,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
            Err(e) => return Err(e.into()),
        };
        content
            .lines()
            .filter(|l| !l.trim().is_empty())
            .map(|l| {
                serde_json::from_str::<ChildRecord>(l).map_err(|e| SpawnError::Failed {
                    op: "record_decode",
                    child: None,
                    detail: e.to_string(),
                })
            })
            .collect()
    }

    /// The child's OWN ingestion inbox, derived from its pane + the run-id namespace:
    /// `~/.claude/exo/inboxes/{run_id}/pane-{N}.jsonl` (pane `%317` → `pane-317.jsonl`).
    /// Stored in the child's `Spawned` record so the parent can address DOWN to it.
    pub(crate) fn child_inbox_path(&self, pane: &PaneId) -> InboxPath {
        let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
        let n = pane.as_str().trim_start_matches('%');
        InboxPath::new(
            PathBuf::from(home)
                .join(".claude/exo/inboxes")
                .join(&self.run_id)
                .join(format!("pane-{n}.jsonl")),
        )
    }
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
