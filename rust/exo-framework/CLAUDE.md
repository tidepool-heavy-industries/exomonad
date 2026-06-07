# exo-framework — the reusable node-mode engine abstractions

The policy **contract** the node-mode engine is built on: the `Tool<R>` trait, `RoleDef<R>`, the
hook decision enums, the `PolicyCaps` dispatch bound-union, and the **`RoleRegistry<R>`** that
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
| `registry` | `RoleRegistry<R>` — the injected role resolver. **Being superseded by `exomonad`** (deleted in P2 of the trait refactor). |
| `exomonad` | `Exomonad` — the engine-as-generic-machinery trait: four associated types (`Caps`/`Role`/`System`/`Spawn`) + `role_def` + `handle_system`. `SystemCtx` (the engine context a domain's `handle_system` operates through) + `SystemOutcome` (the lifecycle action the engine still owns, e.g. `ReclaimSender` for reviewer teardown). The seam traits the associated types are bound by (`RoleKind`/`SpawnSpec`/`DomainSystem`) live in `exo-caps`. See [`docs/decisions/exo-trait-refactor.md`](../../docs/decisions/exo-trait-refactor.md). |

## The load-bearing piece: `RoleRegistry` (dependency inversion)

The engine (`exo-node`) must not know the concrete roles. Instead of calling a domain
`role_def(kind)` directly, it holds a `RoleRegistry<R>` **injected by the binary** and asks it to
resolve a `RoleDef<R>` per `NodeKind`:

```rust
// exo-framework
pub struct RoleRegistry<R: Send + Sync> { resolver: fn(NodeKind) -> RoleDef<R> }
impl<R: Send + Sync> RoleRegistry<R> {
    pub const fn new(resolver: fn(NodeKind) -> RoleDef<R>) -> Self { … }
    pub fn role_def(&self, kind: NodeKind) -> RoleDef<R> { (self.resolver)(kind) }
}
```

The `exo` usage crate builds it from its roster (`RoleRegistry::new(exo::role_def::<R>)`), and the
binary injects it into `exo-node` via `bootstrap(papers, cwd, exo::roster())`. After this,
`exo-node` depends only on `exo-framework` for these types and **never on the domain crate**
(`cargo tree -p exo-node` does not list `exo`). The registry is a thin
`fn`-pointer wrapper (monomorphized at the binary's concrete `R`), so it is `Copy` and stateless.

## A tool's cap bounds *are* its least-privilege spec

`fn run<C: Bus>` can only touch the bus; `fn run<C: Git>` only git. The bound is compiler-checked
and surfaced in the hand-written `Tool<R>` adapter's `impl` header. `PolicyCaps` exists only so the
roster's `role_def<R: PolicyCaps>` can name one bound that guarantees every cap is present at the
dispatch boundary — it does not weaken the per-tool bounds.
