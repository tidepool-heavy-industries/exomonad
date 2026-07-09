# exo-framework — the reusable node-mode engine abstractions

The policy **contract** the node-mode engine is built on: the `Tool<R>` trait, `RoleDef<R>`, the
hook decision enums, the `PolicyCaps` dispatch bound-union, and the **`Exomonad`** trait that
inverts the engine→domain dependency. Everything here is generic over the concrete runtime `R` and
written against the `exo-caps` trait seam — **no IO, no concrete tools/roles/gates**. The concrete
domain (the actual tool set, roster, and gates) lives in the [`exo`](../exo/CLAUDE.md) usage crate.

This is the "big reusable framework" half of the framework/domain split (the Rust analog of
Classic's Haskell-WASM DSL / Rust-host split). See [`docs/decisions/exo-framework-domain-split.md`](../../docs/decisions/exo-framework-domain-split.md).

> Part of the v2 node-mode swarm (`exo`). See `rust/CLAUDE.md`.

## What lives here

| Module | Contents |
|--------|----------|
| `tool` | `Tool<R>` (typed authoring surface — what domain tools implement; const `NAME`/`DESCRIPTION`, assoc `Args`, receiverless async `run`) + `ErasedTool<R>` (object-safe runtime surface — what `RoleDef` stores; today's `Tool` trait renamed) + `tool()` roster constructor (wraps any `T: Tool<R>` in the one generic `Adapter`) + JSON-edge helpers (`parse`/`ok_json`/`schema_json`) + `ToolOutput` + `BoxFuture`. No per-tool adapter, no macro; direct `ErasedTool` impls remain open for runtime-named tools. |
| `hooks` | The hook **contract**: the decision enums (`HookDecision`, `SessionStartOutput`) + the parsed `HookInput`. The concrete gate bodies that produce them are domain code. There used to be a `StopDecision` here too (Claude Code's `Stop` event) — removed; see `rust/exo/CLAUDE.md`. |
| `roles` | `RoleDef<R>` (a role's tools + its two hook fn-pointers) + the fn-pointer type aliases (`PreToolUseFn`/`SessionStartFn`). |
| `caps` | `PolicyCaps` — a static bound-**union** for the dispatch boundary. NOT a god-trait: tools still declare their own narrow per-cap bounds. A blanket impl makes any all-caps type (`Runtime`, a test mock) `PolicyCaps` automatically. |
| `exomonad` | `Exomonad` — the engine-as-generic-machinery trait: the seam that replaces the fn-pointer registry. Four associated types (`Caps`/`Role`/`System`/`Spawn`) + `role_def` + `handle_system` + `handle_tick` (default no-op). `SystemCtx` (the engine context a domain's `handle_system` operates through: `own_branch`/`head_sha`/`deliver_parent`/`deliver_to_self` + the **generic domain-state file** methods `read_file`/`write_file` (the exo domain uses them for its review log; the engine names no review vocabulary)) + `SystemOutcome` (the lifecycle action the engine still owns). `handle_tick(caps, role, elapsed)` is the sidecar's `watchdog` loop's periodic hook (`exo-node`) — a domain's wall-clock self-check (e.g. `exo`'s reviewer-abandonment timeout), the replacement for Claude Code's `Stop` hook (which couldn't distinguish "done" from "paused mid-async-wait"; see `rust/exo/CLAUDE.md`). Takes `&Self::Caps` directly rather than `SystemCtx`, since a domain's tick logic may need caps `SystemCtx` doesn't expose. The seam traits the associated types are bound by (`RoleKind`/`SpawnSpec`/`DomainSystem`) live in `exo-caps`. See [`docs/decisions/exo-trait-refactor.md`](../../docs/decisions/exo-trait-refactor.md). |

## The load-bearing piece: `Exomonad` (dependency inversion)

The engine (`exo-node`) is generic over a domain TYPE `D: Exomonad`. Instead of holding an
injected fn-pointer registry, it is monomorphized once at the binary via
`run_node::<exo::ExoDomain>(…)`. This is the seam that inverts the engine→domain dependency:

```rust
// exo-framework
pub trait Exomonad: Send + Sync + 'static {
    type Caps: PolicyCaps;
    type Role: RoleKind;
    type System: DomainSystem;
    type Spawn: SpawnSpec<Role = Self::Role>;

    fn role_def(role: Self::Role) -> RoleDef<Self::Caps>;
    fn handle_system<'a, C: SystemCtx>(...) -> BoxFuture<'a, CapResult<SystemOutcome>>;
    fn handle_tick<'a>(caps: &'a Self::Caps, role: Self::Role, elapsed: Duration)
        -> BoxFuture<'a, CapResult<()>> { /* default no-op */ }
}
```

The `exo` usage crate implements this on a ZST (`ExoDomain`). Adding a role, a backend, or a
new inter-node behavior requires **zero edits** to `exo-framework`, `exo-caps`, `exo-node`, or
`exo-runtime`. `exo-node` depends only on `exo-framework` for these types and **never on the
domain crate**.

## A tool's cap bounds *are* its least-privilege spec

`async fn run(ctx: &R, args: Self::Args)` where `R: Bus` can only touch the bus; `R: Git` only git.
The bound is compiler-checked in the `impl<R: …> Tool<R> for X` header. `PolicyCaps` exists only
so the roster's `role_def<R: PolicyCaps>` can name one bound that guarantees every cap is present
at the dispatch boundary — it does not weaken the per-tool bounds.
