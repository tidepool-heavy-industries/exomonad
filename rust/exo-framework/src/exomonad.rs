//! [`Exomonad`] — the single trait that makes the node-mode engine **generic machinery over a
//! domain TYPE**. A domain is a ZST implementing this trait; the engine (`exo-node`) is generic
//! over `D: Exomonad`, monomorphized once at the binary (`run_node::<exo::ExoDomain>(…)`). This is
//! the seam that replaces the fn-pointer `RoleRegistry`: a domain adds a role / a backend / a tool
//! / a new inter-node behavior with **zero edits** to `exo-framework`/`exo-caps`/`exo-node`/
//! `exo-runtime`.
//!
//! The four associated types (the leaks each closes):
//! - [`Caps`](Exomonad::Caps): the dispatch-boundary cap bundle (replaces a bare `R`). Tools keep
//!   their own narrow `run<C: Git + Bus>` bounds; `D::Caps: PolicyCaps` satisfies any subset, so
//!   least-privilege is intact.
//! - [`Role`](Exomonad::Role): the domain role enum (replaces the closed `NodeKind`) — closes the
//!   role + backend leaks.
//! - [`System`](Exomonad::System): the domain inter-node payload (replaces the `Review*` system
//!   messages) — closes the inter-node-behavior leak.
//! - [`Spawn`](Exomonad::Spawn): the domain spawn intent (one generic `Spawner::spawn`) — closes
//!   the spawn-archetype leak.
//!
//! The seam traits the associated types are bound by ([`RoleKind`](exo_caps::RoleKind),
//! [`DomainSystem`](exo_caps::DomainSystem), [`SpawnSpec`](exo_caps::SpawnSpec)) live in `exo-caps`
//! (the foundational `exo-caps` types reference them, and `exo-caps` can't depend on this crate);
//! `Exomonad` itself lives here because it also names [`RoleDef`](crate::RoleDef) and
//! [`PolicyCaps`](crate::PolicyCaps). See `docs/decisions/exo-trait-refactor.md`.

use async_trait::async_trait;
use exo_caps::{Branch, CapResult, DomainSystem, Message, Persona, RoleKind, SpawnSpec};

use crate::caps::PolicyCaps;
use crate::roles::RoleDef;
use crate::tool::BoxFuture;

/// What the engine should do after a domain's [`handle_system`](Exomonad::handle_system) returns —
/// the lifecycle action the engine still owns (so the domain handler needs no [`Spawner`] access).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SystemOutcome {
    /// Nothing further: the domain handled the message in full.
    Done,
    /// Tear down the message **sender** (`kill_pane` + `reclaim_worktree`) — e.g. a one-shot
    /// reviewer whose verdict was just applied. The engine owns the teardown call so the domain
    /// handler stays free of spawn/teardown caps.
    ReclaimSender,
}

/// The slice of engine context a domain's [`handle_system`](Exomonad::handle_system) is given. The
/// engine (`exo-node`) implements this over its live `NodeContext`; the domain handler operates
/// only through these four operations, so it needs no direct cap access and no knowledge of the
/// last-hop (Teams vs tmux). `deliver_to_self` injects into THIS node's own LLM conversation;
/// `deliver_parent` rides the bus up the tree edge.
#[async_trait]
pub trait SystemCtx: Send + Sync {
    /// This node's own git branch (e.g. to match a review verdict's branch).
    fn own_branch(&self) -> &Branch;
    /// This node's current HEAD sha (e.g. to confirm an approval is for the committed code).
    async fn head_sha(&self) -> CapResult<String>;
    /// Deliver a message up the tree edge to this node's parent.
    async fn deliver_parent(&self, msg: Message) -> CapResult<()>;
    /// Inject a synthetic message into THIS node's own LLM conversation (the last-hop dispatch).
    async fn deliver_to_self(&self, from: &str, summary: &str, text: &str) -> CapResult<()>;
    /// Read a domain-state file by path (e.g. the exo domain's review log). None if missing.
    async fn read_file(&self, path: &std::path::Path) -> CapResult<Option<Vec<u8>>>;
    /// Persist a domain-state file by path (atomic write).
    async fn write_file(&self, path: &std::path::Path, bytes: &[u8]) -> CapResult<()>;
}

/// The domain trait. A ZST domain implements it; the engine is generic over `D: Exomonad`.
pub trait Exomonad: Send + Sync + 'static {
    /// The dispatch-boundary cap bundle (the concrete runtime `R` the engine monomorphizes against).
    type Caps: PolicyCaps;
    /// The domain's role enum.
    type Role: RoleKind;
    /// The domain's inter-node system payload (rides the bus erased; deserialized here).
    type System: DomainSystem;
    /// The domain's spawn intent (one generic `Spawner::spawn`).
    type Spawn: SpawnSpec<Role = Self::Role>;

    /// Resolve a role's served tools + hooks — the domain's whole policy table. Direct
    /// construction in a `match` (struct-first, no builder DSL).
    fn role_def(role: Self::Role) -> RoleDef<Self::Caps>;

    /// React to one domain [`System`](Exomonad::System) message (the relocated review-gate logic
    /// for the `exo` domain). Operates through the engine-provided [`SystemCtx`] `C` (generic, not a
    /// trait object — `C: SystemCtx` is `Sync` via the supertrait, so the returned future is `Send`
    /// for the spawned inbound task); returns the [`SystemOutcome`] the engine acts on (e.g. tear
    /// down a one-shot reviewer). Called at exactly one place — the inbound loop's Domain arm, after
    /// deserializing the erased wire payload to `Self::System`.
    fn handle_system<'a, C: SystemCtx>(
        ctx: &'a C,
        from: &'a Persona,
        system: &'a Self::System,
    ) -> BoxFuture<'a, CapResult<SystemOutcome>>;

    /// Periodic wall-clock self-check (the sidecar's `watchdog` loop calls this on a fixed interval,
    /// `elapsed` since this node's process started). Replaces turn-boundary (`Stop` hook) inference —
    /// Claude Code's `Stop` fires on every turn-end including a legitimate async-wait yield, so it
    /// cannot distinguish "genuinely done" from "paused waiting on a background task"; a wall-clock
    /// timeout can. Takes `&Self::Caps` directly (not `SystemCtx`) since a domain's tick logic may
    /// need caps `SystemCtx` doesn't expose (e.g. `Kv`), and `Self::Caps: PolicyCaps` already
    /// guarantees every cap is present. Default no-op so a domain that doesn't need this incurs zero
    /// cost and no required edit.
    fn handle_tick<'a>(
        _caps: &'a Self::Caps,
        _role: Self::Role,
        _elapsed: std::time::Duration,
    ) -> BoxFuture<'a, CapResult<()>> {
        Box::pin(async { Ok(()) })
    }
}
