# Classic / Experimental split: shared crate + two binaries

**Status:** accepted 2026-06-06. Supersedes the earlier `classic` Cargo-feature idea.

## Decision

Split classic from experimental by a **crate boundary**, not a feature flag, and ship **two binaries**:

- `exomonad-shared` (new crate) — the lean code both sides need.
- `exomonad-core` — stays the **classic** crate; now depends on `exomonad-shared`.
- `exomonad` (binary) — classic, unchanged surface (`serve`, `init`, `recompile`, …).
- `exo` (binary) — lean experimental/node-mode; depends on `exomonad-shared` + `exo-*` crates only; **never links classic**.

### Why a crate boundary, not `--features`
- Compiler-enforced: `exomonad-shared` has no dependency on classic, so it *cannot* reference it. No cfg matrix, no never-compiled `--no-default-features` target (the source of the latent `ui_protocol`→`domain` bug class).
- Two `main.rs` files are each clean — no `#[cfg]` on the `Commands` enum / match arms / `use` lines.
- Deprecation endgame is trivial: when classic dies, delete `exomonad-core` + the `exomonad` binary; `exomonad-shared` becomes the core.

## What moves into `exomonad-shared`

From the boundary analysis, the shared seam is small. Move these out of `exomonad-core/src` into the new crate:

- `domain.rs` — shared newtypes (only prost / exomonad-proto / tokio).
- `protocol/` — `Runtime`, `HookEventType`, `HookEnvelope`, `mcp`, `service`, `hook`.
- `services/tmux_ipc.rs` — clean (tokio/tracing/anyhow/std only).
- `services/agent_control/launch.rs` — `write_prompt_file`, `build_agent_command` (clean).
- The shared types `AgentType` + `ClaudeSpawnFlags` — **extracted out of** `agent_control/mod.rs` (see landmine below) into e.g. `agent_control/types.rs` in the shared crate.
- `error.rs`, `util.rs`, `ffi.rs`, `hooks.rs`, `logging.rs` — shared plumbing.

Stays in `exomonad-core` (classic): `plugin_manager.rs`, `effects/`, `mcp/`, `handlers/`, all classic `services/*` (github, poller, external, delivery, registries, `Services`/`Has*`/`ServicesBuilder`), the rest of `agent_control/{mod,internal,spawn,cleanup,error}.rs`, `ui_protocol.rs`.

## Low-churn strategy (do this, it keeps classic code almost untouched)

Add root re-exports in `exomonad-core/src/lib.rs` so existing classic paths keep resolving:
```rust
pub use exomonad_shared::{domain, protocol, error, util, ffi, hooks, logging};
pub use exomonad_shared::services::tmux_ipc;
// etc.
```
Then `crate::domain::X`, `crate::protocol::Y`, … inside classic code resolve via the re-export — **no per-file import rewrites across the classic tree.**

`exo-runtime` switches its imports to `exomonad_shared::…` and **drops its `exomonad-core` dependency** (`exo-runtime/Cargo.toml:16`). It currently reaches `exomonad_core::services::tmux_ipc::TmuxIpc`, `agent_control::AgentType`, `agent_control::launch::{write_prompt_file, build_agent_command}` (`exo-runtime/src/spawner.rs:410,432,460,474`, `tmux.rs:35`). After the move these come from `exomonad-shared`.

## Landmine: `agent_control/mod.rs` is mixed

It *defines* the shared `AgentType` (`agent_control/mod.rs:174`) and `ClaudeSpawnFlags`, but its header unconditionally imports classic `super::github::{…}` (`:37`) and `claude_teams_bridge::TeamRegistry` (`:40`). The shared types must be **physically extracted** into the shared crate; the classic header + submodule decls stay in `exomonad-core`. `launch.rs` then imports the shared types from the shared crate.

Note: `ui_protocol.rs` stays in classic core and imports `domain` from `exomonad-shared` — this dissolves the pre-existing latent bug (it was only "broken" under the never-built `--no-default-features` path, which no longer exists).

## Verify

```
cargo build -p exomonad-shared
cargo build -p exo-runtime          # must build WITHOUT exomonad-core in its graph
cargo build --workspace             # classic still green
cargo test -p exomonad-shared -p exo-runtime
```

## Sequencing

1. **Crate extraction** (this doc) — its own branch.
2. **Two binaries** — follow-up: add the `exo` binary target (lean `main.rs`, `exo-*` + shared only); keep `exomonad` = classic. Done after the crate lands.
