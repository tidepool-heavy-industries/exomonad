//! [`ExoSpawn`] — the `exo` domain's spawn intent (its `D::Spawn`), a plain struct implementing
//! [`SpawnSpec`]. The domain's spawn tools each build one of these with the `(role, kind)` pair
//! fixed (so an illegal pairing is unnameable at the tool boundary); the engine's single generic
//! `Spawner::spawn` consumes it. The rendered prompt is built by the tool (the spec just carries it).
//!
//! Until the `Spawner` collapse (P4) wires the engine to `spawn(D::Spawn)`, this is the concrete
//! `Exomonad::Spawn` type that proves the seam; the per-op tool wrappers populate it in P4.

use exo_caps::{AgentName, ChildKind, NodeKind, SpawnSpec};

/// One spawn intent: which role/kind to birth, the (optional) name, the rendered task body, and the
/// opt-in context-inheritance flag.
#[derive(Debug, Clone)]
pub struct ExoSpawn {
    /// The role the child is born as (fixes its tool set + launch backend).
    pub role: NodeKind,
    /// Own worktree vs inline pane.
    pub kind: ChildKind,
    /// Explicit child name, or `None` to auto-generate from `name_prefix`.
    pub name: Option<AgentName>,
    /// Auto-increment prefix used when `name` is `None`.
    pub name_prefix: &'static str,
    /// The fully-rendered prompt/task body delivered to the child.
    pub task: String,
    /// Opt-in Claude context inheritance (`--resume --fork-session`).
    pub fork_session: bool,
}

impl SpawnSpec for ExoSpawn {
    type Role = NodeKind;

    fn role(&self) -> NodeKind {
        self.role
    }
    fn child_kind(&self) -> ChildKind {
        self.kind
    }
    fn name(&self) -> Option<AgentName> {
        self.name.clone()
    }
    fn name_prefix(&self) -> &str {
        self.name_prefix
    }
    fn fork_session(&self) -> bool {
        self.fork_session
    }
    fn into_task(self) -> String {
        self.task
    }
}
