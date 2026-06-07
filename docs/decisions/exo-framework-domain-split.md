# exo-framework / exo split: clean engine vs domain separation

**Status:** accepted 2026-06-06 (design interview). Executes the north star in [the memory] — a big reusable Rust framework + a minimal `exo` "usage" (the Rust analog of Classic's Haskell-WASM DSL / Rust-host split).

## Goal

A clean compiler-enforced boundary: the **framework (engine)** is the reusable hylomorphism-over-worktrees machinery; **`exo`** is the minimal domain/orchestration usage of it. v2 node-mode only — Classic stays separate and deprecated.

## Target crate layout

- **`exo-framework`** (NEW) — the policy *abstractions* extracted from `exo-policy`: the `Tool<R>` trait + JSON-edge helpers (`exo-policy/src/tool.rs`), `RoleDef<R>` (the struct from `roles.rs`), the `Hook` trait + dispatch machinery (`hooks.rs`), the `PolicyCaps` cap bundle (`caps.rs`), and a **`RoleRegistry`** abstraction (new — see Inversion). No concrete tools/roles/gates.
- **`exo`** (NEW, bin + small lib) — the domain. Its `lib.rs` holds the concrete `tools/*` (spawn/fork/merge/submit/tree/verdict/messaging), the concrete role roster (today's `role_def(NodeKind)` body), and the concrete gates (clean-gate/stop/review from `hooks.rs`), exposed as a `roster()`/registry builder. `main.rs` is just the node entrypoint: build the roster, hand it to `framework::run_node(roster())`.
- **`exo-policy`** — DELETED once empty.
- Framework stays "big" via the engine crates (`exo-caps`, `exo-runtime`, `exo-node`, `exo-scry`, `exomonad-shared`); `exo` is the comparatively small usage.

## The load-bearing change: dependency inversion

Today the engine reaches into the domain: `exo-node/src/hook.rs:37` calls `exo_policy::role_def::<Runtime>(ctx.kind)` (also `bootstrap.rs:9,23`, and `outbound.rs` serves `role_def(kind).tools`). That is backwards — the engine must not know the concrete roles.

**Flip it to injection.** `exo-node` (and its serve/hook entrypoints) take a `RoleRegistry` (defined in `exo-framework`, generic over `R`) passed in by the caller, instead of calling `exo_policy::role_def`. The **`exo` binary** constructs the registry from its domain roster and injects it (via `NodeContext` / a `run_node(registry)` param). After this, `exo-node` depends on `exo-framework` (for `Tool`/`RoleDef`/`Hook`/`RoleRegistry` types) and **never on the domain**. This inversion is the whole point of the split — do it, don't just move files.

## Move map

| From `exo-policy` | To | Notes |
|---|---|---|
| `tool.rs` (`Tool<R>`, helpers, `BoxFuture`, `ToolOutput`) | `exo-framework` | abstraction |
| `roles.rs` → `RoleDef<R>` struct | `exo-framework` | abstraction |
| `roles.rs` → `role_def(NodeKind)` body (the concrete roster) | `exo` lib | domain; becomes `roster()` |
| `hooks.rs` → `Hook` trait + dispatch | `exo-framework` | abstraction |
| `hooks.rs` → concrete gates (`clean`/`stop`/`pre_tool_use`/`session_start`/review) | `exo` lib | domain |
| `caps.rs` (`PolicyCaps` bundle) | `exo-framework` | abstraction |
| `tools/*` (merge/messaging/spawn/submit/tree/verdict) | `exo` lib | domain |
| `testing.rs` | `exo-framework` (test support) or `exo` per usage | check both consumers |

## Known wrinkles (verified)

- **`exo-caps/Cargo.toml` references `exo-policy`** — the foundational seam cannot depend on policy. Inspect: if it's a `[dev-dependencies]` for test fixtures, repoint to `exo-framework` (or a small test-util); if a real dep, that's a latent layering bug to fix as part of this.
- **`exo-node` tests** (`hook.rs:156+`) construct `RoleDef`/roles directly via `exo_policy::*` — repoint to `exo-framework` + a domain test fixture (the registry can be injected with a test roster).
- **The misleading comment** `"No phases, no DSL, no macros"` in `exo-policy/src/lib.rs:5` was a Claude editorialization, not a design rule (a Rust DSL + optional phases are explicitly on the table). **DELETE it** during the move; do not carry it forward.
- **Phases are NOT part of this task.** Relocate `RoleDef<R>` as-is — it's already the role unit. The builder-vs-trait DSL authoring shape (and optional per-role phases) is a SEPARATE follow-on; the `rust/exo-dsl-spike/` evidence is banked for it (trait+assoc wins iff phases are added).

## Strangler steps (each ends green: `cargo build --workspace`)

1. **Create `exo-framework`**, move the abstractions (`Tool`/`RoleDef`/`Hook`/dispatch/`PolicyCaps` + new `RoleRegistry`) into it. `exo-policy` temporarily re-exports them (`pub use exo_framework::*`) so `exo-node`/`exo-caps` keep compiling unchanged. Build.
2. **Create `exo` (bin+lib)**, move the concretes (`tools/*`, the roster, the gates) into its lib; add `roster()`/registry builder + a `main.rs` node entrypoint. Build.
3. **Invert `exo-node`**: replace `exo_policy::role_def(kind)` calls with the injected `RoleRegistry`; repoint imports to `exo-framework`; repoint the `exo-caps` dep; update `exo-node` tests with an injected test roster. Repoint the binary's node entry to inject `exo::roster()`. **Delete `exo-policy`** (and its workspace member entry). Build + test.

## Verify

```
cargo build --workspace
cargo test -p exo-framework -p exo -p exo-node
! cargo tree -p exo-node | grep -q exo-policy     # exo-policy gone
! cargo tree -p exo-node | grep -q '\bexo\b'      # engine does NOT depend on the domain crate
```

Update `rust/CLAUDE.md` (crate inventory, dependency story), add `rust/exo-framework/CLAUDE.md` and `rust/exo/CLAUDE.md`, remove `rust/exo-policy/CLAUDE.md`.
