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
| `tool` | The `Tool<R>` trait (object-safe over the concrete `R`) + JSON-edge helpers (`parse`/`ok_json`/`schema_json`) + `ToolOutput` + `BoxFuture`. |
| `hooks` | The hook **contract**: the decision enums (`HookDecision`, `StopDecision`, `SessionStartOutput`) + the parsed `HookInput`. The concrete gate bodies that produce them are domain code. |
| `roles` | `RoleDef<R>` (a role's tools + its three hook fn-pointers) + the fn-pointer type aliases (`PreToolUseFn`/`StopFn`/`SessionStartFn`). |
| `caps` | `PolicyCaps` — a static bound-**union** for the dispatch boundary. NOT a god-trait: tools still declare their own narrow per-cap bounds. A blanket impl makes any all-caps type (`Runtime`, a test mock) `PolicyCaps` automatically. |
| `exomonad` | `Exomonad` — the engine-as-generic-machinery trait: the seam that replaces the fn-pointer registry. Four associated types (`Caps`/`Role`/`System`/`Spawn`) + `role_def` + `handle_system`. `SystemCtx` (the engine context a domain's `handle_system` operates through) + `SystemOutcome` (the lifecycle action the engine still owns). The seam traits the associated types are bound by (`RoleKind`/`SpawnSpec`/`DomainSystem`) live in `exo-caps`. See [`docs/decisions/exo-trait-refactor.md`](../../docs/decisions/exo-trait-refactor.md). |

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
}
```

The `exo` usage crate implements this on a ZST (`ExoDomain`). Adding a role, a backend, or a
new inter-node behavior requires **zero edits** to `exo-framework`, `exo-caps`, `exo-node`, or
`exo-runtime`. `exo-node` depends only on `exo-framework` for these types and **never on the
domain crate**.

## A tool's cap bounds *are* its least-privilege spec

`fn run<C: Bus>` can only touch the bus; `fn run<C: Git>` only git. The bound is compiler-checked
and surfaced in the hand-written `Tool<R>` adapter's `impl` header. `PolicyCaps` exists only so the
roster's `role_def<R: PolicyCaps>` can name one bound that guarantees every cap is present at the
dispatch boundary — it does not weaken the per-tool bounds.
