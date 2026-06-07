//! The **domain seam** traits — the abstractions a concrete domain (the `exo` crate) implements
//! so the engine becomes generic machinery over a domain TYPE. These live in `exo-caps` (not
//! `exo-framework`) because the foundational `exo-caps` types themselves reference them:
//! [`Spawner`](crate::Spawner) spawns a [`SpawnSpec`], [`NodePapers`](crate::NodePapers) records a
//! [`RoleKind`], and the [`MessageKind::Domain`](crate::MessageKind) wire carries a
//! [`DomainSystem`]. `exo-caps` cannot depend on `exo-framework`, so the seam roots here; the
//! [`Exomonad`](../../exo_framework/exomonad/trait.Exomonad.html) trait that ties them together
//! lives in `exo-framework` (it also names `RoleDef`/`PolicyCaps`).
//!
//! See `docs/decisions/exo-trait-refactor.md` for the full design.

use crate::types::{AgentType, ChildKind};
use crate::AgentName;
use serde::{de::DeserializeOwned, Serialize};

/// A domain's **role enum** — the closed set of node archetypes a domain defines (the `exo`
/// domain's is `root`/`tl`/`dev`/`worker`/`reviewer`). Replaces the hard-coded
/// [`NodeKind`](crate::NodeKind): adding a role is a domain edit, not an engine edit.
///
/// The engine reads three things off a role: its `agent_type` (the role→backend mapping — which
/// CLI to launch + how the last-hop delivers), its `role_str` (the wire/papers key), and `all`
/// (for coverage tests + exhaustive iteration). The role is recorded **fully typed** in
/// [`NodePapers`](crate::NodePapers) (only the child's own bootstrap reads it, preserving the
/// validate-on-deserialize invariant), so it must round-trip through serde.
pub trait RoleKind:
    Copy + Eq + std::fmt::Debug + Send + Sync + 'static + Serialize + DeserializeOwned
{
    /// Every role the domain defines — for coverage tests and exhaustive iteration.
    fn all() -> &'static [Self]
    where
        Self: Sized;
    /// The role→backend mapping: which agent runtime backs this role. The engine owns the
    /// launchable [`AgentType`] *set* (a domain can't add a 4th backend without an engine edit —
    /// the documented IoC); a domain only maps its roles onto that set.
    fn agent_type(&self) -> AgentType;
    /// The role's stable wire/papers key (also the `role_def` lookup key).
    fn role_str(&self) -> &'static str;
}

/// A domain's **inter-node system payload** — the typed message a domain's tools send node-to-node
/// over the bus and a domain's [`handle_system`](../../exo_framework/exomonad/trait.Exomonad.html)
/// consumes (the `exo` domain's is the review-verdict enum). Rides the bus erased as
/// [`MessageKind::Domain`](crate::MessageKind)`(Box<RawValue>)` and is deserialized back to the
/// concrete `D::System` at exactly one place (the inbound loop's Domain arm). A new node-to-node
/// behavior is a new variant + a new `handle_system` arm — **zero engine edits**.
///
/// Blanket-implemented for any serde type: a domain never names this trait, it just defines a
/// serializable enum.
pub trait DomainSystem: Serialize + DeserializeOwned + Send + Sync + 'static {}

impl<T> DomainSystem for T where T: Serialize + DeserializeOwned + Send + Sync + 'static {}

/// A domain's **spawn intent** — what the one generic [`Spawner::spawn`](crate::Spawner) needs to
/// birth a child, with the `(role, kind)` pair the domain fixes (so an illegal pairing is
/// unnameable at the domain's tool boundary). Replaces the per-op `WorkerSpec`/`GeminiSpec`/
/// `ForkSpec` methods: the role-fixing moves to thin domain tool wrappers that each build a
/// `D::Spawn`.
pub trait SpawnSpec: Send + Sync + 'static {
    /// The domain role this spec spawns as. Drives the child's papers + tool set, and (via
    /// [`RoleKind::agent_type`]) the launch backend.
    type Role: RoleKind;

    /// The role the child is born as.
    fn role(&self) -> Self::Role;
    /// How the child relates to the parent's worktree (own worktree vs inline pane).
    fn child_kind(&self) -> ChildKind;
    /// An explicit child name, or `None` to auto-generate `{name_prefix}-{n}`.
    fn name(&self) -> Option<AgentName>;
    /// The auto-increment prefix used when [`name`](Self::name) is `None`.
    fn name_prefix(&self) -> &str;
    /// Opt-in Claude context inheritance (`--resume --fork-session`); honored only for a Claude
    /// worktree child, ignored otherwise.
    fn fork_session(&self) -> bool;
    /// The fully-rendered prompt/task body delivered to the child. Consumes the spec — it is the
    /// last thing the birth tail reads.
    fn into_task(self) -> String;
}
