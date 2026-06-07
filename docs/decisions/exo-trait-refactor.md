# The `Exomonad` trait — engine as generic machinery over a domain type

Status: in progress (phased migration, see "Phases"). Supersedes the fn-pointer `RoleRegistry`.

## Context

The v2 node-mode swarm split into `exo-framework` (engine) + `exo` (domain), with a fn-pointer
`RoleRegistry<R>` inverting the engine→domain dependency. That inversion covered **role
resolution** but four other domain concerns still leaked into the engine crates as hard-coded,
closed types:

1. **Role enum** — `NodeKind` (`root`/`tl`/`dev`/`worker`/`reviewer`) lived in `exo-caps`. Adding
   a role meant editing `exo-caps`.
2. **Role→backend mapping** — `NodeKind::agent_type()` hard-coded which roles are Claude vs Gemini.
3. **Inter-node behavior** — `SystemMessage::Review*` (the review-gate signals) lived in `exo-caps`
   and the gate logic (`apply_verdict`) lived in `exo-node/inbound.rs`. A new node-to-node signal
   meant editing both engine crates.
4. **Spawn archetypes** — `Spawner` had one method per archetype (`spawn_worker`/`spawn_gemini`/
   `spawn_reviewer`/`fork_wave`). A new archetype meant a new `Spawner` method (an `exo-caps` edit).

## Decision

A single trait, `Exomonad`, makes the engine **generic machinery over a domain TYPE `D`**. A
domain is a ZST implementing `Exomonad`; `exo-node` is generic over `D: Exomonad`, monomorphized
once at the binary (`run_node::<exo::ExoDomain>(…)`). `RoleRegistry` is deleted.

```rust
pub trait Exomonad: Send + Sync + 'static {
    type Caps: PolicyCaps;                       // dispatch-boundary cap bundle (was bare `R`)
    type Role: RoleKind;                         // domain role enum (was closed `NodeKind`)
    type System: DomainSystem;                   // inter-node payload (was `SystemMessage::Review*`)
    type Spawn: SpawnSpec<Role = Self::Role>;    // spawn intent (was per-archetype methods)

    fn role_def(role: Self::Role) -> RoleDef<Self::Caps>;
    fn handle_system<'a>(ctx: &'a dyn SystemCtx, from: &'a Persona, system: &'a Self::System)
        -> BoxFuture<'a, CapResult<SystemOutcome>>;
}
```

Each associated type closes one leak. A domain now adds a role / a per-role backend / a new tool /
a new tool-mediated inter-node behavior with **zero edits** to
`exo-framework`/`exo-caps`/`exo-node`/`exo-runtime`. The acceptance proof (Phase 6) is a
`#[cfg(test)] TestDomain` that links `exo-node` and compiles with no engine edits.

### Where the seam traits live (a necessary deviation from the original plan)

The plan placed all new traits in `exo-framework`. The dependency graph forbids it: `exo-caps` owns
`Spawner` (spawns a `SpawnSpec`), `NodePapers` (records a `RoleKind`), and `MessageKind` (carries a
`DomainSystem`), and **`exo-caps` cannot depend on `exo-framework`**. So:

- **`exo-caps`** holds `RoleKind`, `SpawnSpec`, `DomainSystem`, `Lifecycle`, and the
  `deliver_domain` helper (the seam traits the foundational types reference).
- **`exo-framework`** holds `Exomonad`, `SystemCtx`, `SystemOutcome` (they also name `RoleDef` /
  `PolicyCaps`, which are `exo-framework`).

This is invisible to a domain author (everything re-exports through the usual paths) and respects
the crate graph.

### Wire: HYBRID, `exo-caps` `Message` stays NON-generic

A fully-typed System wire would force `C: Bus` → `C: Bus<D::System>` at every tool, collapsing
per-tool least-privilege (a tool that only sends `Chat` would have to name the domain's System
type). So the wire is hybrid:

- `MessageKind::Lifecycle(Lifecycle)` — CLOSED, engine-owned, **typed**: `ChildIdle` / `ChildExited`
  / `ShutdownResponse`. The engine acts on these directly (`mark_child_idle` / `try_reap` / the
  shutdown matrix).
- `MessageKind::Domain(Box<RawValue>)` — domain-opaque, deserialized to `D::System` at **exactly
  one place** (the inbound loop's Domain arm) before `D::handle_system`.

`Box<RawValue>` is here for **domain-opaque erasure** (the least-privilege property), not for any
byte-compat migration (dropped — pre-release, recreate sessions to test). Papers carry `D::Role`
**fully typed** (only the child's own bootstrap reads them, preserving the validate-on-deserialize
invariant). Only the multi-writer bus payload is erased.

The cost: an unknown domain System tag is a runtime skip (the tolerant inbound parser logs + drops
the line), not a compile error. Mitigated by `Lifecycle` being typed + `RoleKind::all()` coverage +
an exhaustive `handle_system` match.

### Other constraints (held)

- **struct-first** — `RoleDef` stays a plain struct; no builder/combinator DSL.
- **least-privilege** — `Caps` associated type + narrow per-tool `run<C: …>` bounds + the free
  `deliver_domain::<S>(bus, …)` helper (a tool naming `D::System` still only needs `C: Bus`).
- **`AgentType` variant set stays engine-owned** — a domain maps roles onto the launchable backend
  set; it can't add a 4th backend without an engine edit (the documented IoC). The `AgentType`
  enrichment (sum-type-with-config) keeps the variant SET closed while making each variant carry
  launch params.
- **Classic untouched** — `exomonad-core` / `exomonad serve` build + behave identically throughout.

## Phases

- **P0** (additive, zero behavior change): add `RoleKind`/`SpawnSpec`/`DomainSystem`/`Lifecycle` to
  `exo-caps`, `Exomonad`/`SystemCtx`/`SystemOutcome` to `exo-framework`, `impl RoleKind for
  NodeKind`. (`deliver_domain` + the wire variants land in P1, where `MessageKind::Domain` exists.)
- **P1**: split `MessageKind::System` → `Lifecycle(Lifecycle)` + `Domain(Box<RawValue>)`; route
  `Review*` through `Domain`; add `deliver_domain`.
- **P2**: introduce `impl Exomonad for ExoDomain`; make `NodeContext<D>`/`run_node<D>`/`bootstrap`
  generic; relocate the review gate into `ExoDomain::handle_system`; delete `RoleRegistry`; binary
  → `run_node::<ExoDomain>`.
- **P4**: collapse `Spawner` to one generic `spawn(D::Spawn)` + a `fork_wave` wrapper; relocate the
  `.exo/acceptance.md` write into `submit_branch`.
- **P5**: swap `Role = NodeKind` → domain `ExoRole`; delete `NodeKind` from `exo-caps`; the
  `AgentType` sum-type-with-config enrichment.
- **P6**: the `TestDomain` acceptance proof.

(Original plan numbered the gate relocation P3; it is folded into P2 here to avoid a broken
intermediate — a stub `handle_system` between P2 and P3 would break the review e2e.)
