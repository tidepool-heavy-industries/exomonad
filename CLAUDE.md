# ExoMonad

Type-safe LLM agent orchestration over a tree of context windows. Two coexisting architectures share one model:

- **v2 Node-Mode** (the `exo` binary) — the active track. No central server: one Rust sidecar per agent, the filesystem is the bus, the process tree is the topology. Tool/hook/role logic is plain Rust over a capability seam (`exo-caps`). Convergence is local `git merge`.
- **Classic** (`exomonad serve`) — **deprecated, still supported.** A central MCP server; all tool/hook/event logic compiled to Haskell WASM (a typed config DSL); Rust executes the I/O effects the DSL yields.

This file is a **router**: the shared model below, then pointers into the nested `CLAUDE.md` tree where the per-component detail now lives.

---

## Model

ExoMonad is a **hylomorphism over context windows**. The unfold is plan + scaffold + spawn; the fold is merge + integrate + surface-upward. That recursion scheme is the whole system.

### The Agent Triad

Each node in the tree is three things born, living, and dying together:

- **Worktree** — filesystem state (code isolation via git worktree)
- **Context window** — attention state (what the agent knows and can reason about)
- **Actor** — message-processing entity (receives notifications, yields effects)

These are 1:1:1. You cannot have a worktree without a context window to operate on it, or a context window without an actor to drive it. When the actor shuts down, the worktree is cleaned up and the context window is gone. The triad IS the agent.

### The Hylomorphism

**Unfold — the scaffold commit.** A TL plans one level down, commits the shared foundation (types, interfaces, stubs, CLAUDE.md), and spawns children. The scaffold commit defines the shape of the decomposition — the most important thing a TL does. Children fork from it and cannot see each other.

**Fold — merge + integrate.** As children complete, the TL merges their branches, wires outputs together, and writes an integration commit — accumulating understanding from what children produced to sharpen the next wave's specs. After all waves fold, the TL surfaces its result to its parent, which folds it in turn.

The operational realization is **Scaffold-Fork-Converge** (see [Tech Lead Praxis](#tech-lead-praxis-essence)).

### Depth over Breadth

Sub-TLs are **compression boundaries**. A root TL with 3 sub-TLs, each managing 4 leaves, sees O(3) — not O(12). The sub-TL absorbs implementation detail into its context window, and surfaces only the integrated result upward.

This is why the tree prevents context window drift: each node's cognitive load is proportional to its fan-out, not the total number of leaves beneath it. A 4-level-deep tree with branching factor 3 has 81 leaves, but no single context window ever reasons about more than 3 children.

### Waves as Rhythm

Within a single TL's scope, work proceeds in waves. Wave N produces merged code. Wave N+1 builds on it. The wave boundary is where understanding accumulates — the TL reads the merged diffs, learns what the children actually built, and uses that knowledge to write sharper specs for the next wave.

### Branch Naming as Coordinate System

`{parent}.{name}` (dot separator) encodes tree address, where `name = {slug}-{type}` (e.g., `auth-claude`, `oauth-provider-dev`). `dev.auth-claude.oauth-provider-dev` tells you: root is `dev`, first-level TL is `auth`, leaf is `oauth-provider` (a dev leaf). The last dot-segment IS the `AgentName` — one namespace, zero translation. Branches converge to the parent branch, not main — folded up the tree (v2: local `git merge`; Classic: PR). The git DAG IS the computation trace.

---

## Rules

### Style

ALWAYS update CLAUDE.md files when you make changes. Adding new documentation is critical, as is removing stale documentation.

Comments should always focus on what is or will be. Never leave comments about why you deleted something, its in the git history which is enough.

The repository should be kept clean of dead code, placeholders, and half-done heuristics.

Always prefer failure to an undocumented heuristic or fallback.

### Single Code Path

Never maintain two code paths that do the same thing. Redundant paths cause bug risk. Note: The Classic and v2 architectures are coexisting parallel tracks, not redundant paths; the v2 track builds on the lean `exomonad-shared` crate, while Classic keeps the heavier `exomonad-core`.

### Capability Seam as Boundary (v2 Node-Mode)

**In the v2 Node-Mode architecture, the `exo-caps` Rust crate is the IO boundary.** Tools, hooks, and roles are written as plain Rust in the `exo` domain crate (concrete tools/roles/gates) over the `exo-framework` abstractions, generic over the capability traits in `exo-caps`. A crate that does not link the `exo-runtime` (which implements the caps) cannot perform IO, providing security and testability via the crate graph with zero serialization cost.

The Classic counterpart — *all* MCP tools and hooks defined in Haskell WASM, Rust as the I/O runtime — lives in [`haskell/wasm-guest/CLAUDE.md`](haskell/wasm-guest/CLAUDE.md).

### Crosscutting Rules

When you learn something that applies to a crosscutting context (a programming language, a tool like git worktrees, a pattern that spans directories), **create or update a `.claude/rules/*.md` file** rather than documenting it in a directory-specific CLAUDE.md.

Examples: language idioms (`.claude/rules/haskell.md`, `.claude/rules/rust.md`), tool usage patterns (git, cabal, cargo, tmux), architectural patterns that span the codebase.

Rules files use YAML frontmatter to scope when they load:
```yaml
---
paths:
  - "**/*.hs"
---
```

### Logging

Silent failures are unacceptable. When code shells out to subprocesses, calls external services, or crosses process/container boundaries, **log aggressively**: before the call (command, key params), after (exit code, status, size), on error (stderr, enough context to debug without reproducing), on success (result summary).

**Rust pattern:**
```rust
tracing::info!("Executing: {} {}", cmd, args.join(" "));
let status = Command::new(cmd).args(&args).status()?;
tracing::info!("{} returned: {:?}", cmd, status);
if !status.success() {
    tracing::error!("{} failed with status: {}", cmd, status);
}
```

The Haskell/WASM logging pattern is in [`haskell/wasm-guest/CLAUDE.md`](haskell/wasm-guest/CLAUDE.md).

---

## Getting Started

**v2 Node-Mode (active):**
```bash
exomonad new                  # one-time: .exo/config.toml, .gitignore, rules templates
exo init                      # decentralized swarm session (per-agent sidecars, Monitor wake-channel delivery)
```

**Classic (deprecated):** `exomonad init` creates a central-server tmux session. Full classic getting-started, MCP registration, config, and companions → [`rust/exomonad/CLAUDE.md`](rust/exomonad/CLAUDE.md).

**Build / install:**
```bash
just install-all-dev          # debug build → ~/.cargo/bin/exomonad (fast iteration)
just install-all              # release build
```
WASM build pipeline → [`haskell/wasm-guest/CLAUDE.md`](haskell/wasm-guest/CLAUDE.md). Use `--recreate` on `init` to rebuild a session after binary updates.

---

## Orchestration & Coordination

Spawn a recursive tree of heterogeneous agents:

- **`fork_wave`** — N parallel TL children, each in its own worktree + branch, context optionally forked.
- **`spawn_dev`** (v2; `spawn_gemini` in Classic) — a dev leaf in its own worktree + branch with a self-contained spec; commits and calls `submit_branch` when ready.
- **`spawn_worker`** — ephemeral worker in a tmux pane (no branch, no merge); reports via `notify_parent`.
- **`merge`** — fold a child's submitted branch into yours (v2: local `git merge`; Classic: PR via `merge_pr`).

**Agent types:** v2 node-mode defaults to independent stock Codex TUIs in tmux panes. Exo supplies node-local MCP configuration on the Codex command line, records the real thread UUID from MCP metadata, and uses `codex queue` for delivery. Spawned TLs default to `gpt-5.6-sol` high reasoning; dev/worker/reviewer leaves default to the same model at low reasoning. `backend = "claude"` or `exo init --backend claude` preserves the Opus/Sonnet Claude tree. **Identity** = birth-branch (immutable, deterministic); root = `root`.

**Coordination is push-based** via the sidecar: a child calls `notify_parent` (or `send_message`), the message lands on the durable bus, and the recipient sidecar pushes it into the harness. Codex uses `codex queue`; Claude uses the Monitor-armed `exo listen` channel. The cursor advances only after the last hop accepts the message, so disconnected recipients queue without tmux-paste delivery. tmux survives for spawning, the interactive TUIs, and observability.

Tool/role matrix → [`.claude/rules/exomonad.md`](.claude/rules/exomonad.md). Root protocol → `.exo/roles/devswarm/context/root.md`.

---

## Architecture

Two tracks over shared `exomonad-core` services. Isolation = git worktrees (no Docker); multiplexing = tmux windows (subtrees) and panes (ephemeral workers). Each agent = worktree + window/pane, managed by the Rust runtime.

- **v2 Node-Mode crates:** `exo-caps` (capability seam — traits + types, no IO), `exo-node` (per-agent sidecar; outbound MCP + inbound inbox-watch loops), `exo-runtime` (IO implementations), `exo` (tool/hook/role logic, generic over caps), `exo-scry` (native Teams discovery from live OS state).
- **Classic:** the `exomonad` binary (MCP server + hook handler) hosting Haskell WASM; Rust executes the effects the WASM yields.

Per-architecture detail: classic components + data flows → [`rust/exomonad/CLAUDE.md`](rust/exomonad/CLAUDE.md); WASM guest, MCP tool definitions, DSL → [`haskell/wasm-guest/CLAUDE.md`](haskell/wasm-guest/CLAUDE.md); v2 swarm + observability → [`rust/CLAUDE.md`](rust/CLAUDE.md) and the `exo-*` crate docs. The classic/experimental crate-split plan → [`docs/decisions/classic-shared-crate-split.md`](docs/decisions/classic-shared-crate-split.md).

---

## Tech Lead Praxis (essence)

The TL drives the recursion at each node: unfold (scaffold + spawn), continue useful non-overlapping work while pushed child events are pending, then fold (merge + integrate + surface up). It delegates substantial independent work by default, while directly handling small work, shared scaffolding, integration, conflict resolution, and diagnostics when delegation adds more overhead than value.

- **Depth over breadth** — more than ~4 independent leaves ⇒ interpose a sub-TL. Root reasons about decomposition + integration points, not implementation detail.
- **Intelligence gradient** — expensive context decomposes; cheap leaves implement; review converges. Every line the TL writes is expensive code, every review cycle it runs is an expensive cycle.
- **Push-aware coordination** — decompose → spec → spawn; never poll. Continue useful coordination or local work, and yield only when nothing useful remains because child events are pushed.
- **Spec quality (one shot)** — objective and observable done criteria first, then mechanical paths, small read-first context, concise constraints, optional steps, exact verification, and handoff. Use repository-relative paths and concrete commands.
- **Calibrated escalation** — a leaf that fails repeatedly reports what it tried; the TL re-decomposes, resolves integration or conflict issues in its own worktree, or escalates when authority or scope is missing.

Full operational manual — Scaffold-Fork-Converge protocol, convergence loop, spec template, notification vocabulary → [`.claude/rules/exomonad.md`](.claude/rules/exomonad.md) and `.exo/roles/devswarm/context/root.md`.

---

## Documentation Tree

```
CLAUDE.md  ← YOU ARE HERE (router + model)
├── proto/CLAUDE.md             ← Protocol buffers (FFI boundary types)
├── haskell/CLAUDE.md           ← Haskell package inventory, build/test
│   ├── wasm-guest/CLAUDE.md    ← Classic: MCP tool defs, WASM-as-DSL, build pipeline, tools reference
│   └── proto/CLAUDE.md         ← Generated Haskell types for proto
├── rust/CLAUDE.md              ← Rust workspace overview (Classic + v2), observability, design decisions
│   ├── exomonad/CLAUDE.md      ← Classic: MCP server + hook handler, getting-started, config, data flows
│   ├── exomonad-core/CLAUDE.md ← Shared library: framework, handlers, services
│   ├── exomonad-proto/CLAUDE.md ← Proto-generated types (prost)
│   ├── exo-caps/CLAUDE.md      ← v2 capability seam (traits + types)
│   ├── exo-node/CLAUDE.md      ← v2 per-node sidecar
│   ├── exo-runtime/CLAUDE.md   ← v2 IO implementations
│   ├── exo/CLAUDE.md           ← v2 domain: tools/roles/gates (generic over caps)
│   └── exo-scry/CLAUDE.md      ← v2 native Teams discovery
├── tests/e2e/CLAUDE.md         ← E2E test pattern + harness conventions
└── docs/decisions/             ← Architecture decision records (living docs)
```

| I want to... | Read this |
|--------------|-----------|
| Spawn/orchestrate agents (tool + role matrix) | `.claude/rules/exomonad.md` |
| Understand the Classic server + data flows | `rust/exomonad/CLAUDE.md` |
| Work on the WASM guest / Classic MCP tools | `haskell/wasm-guest/CLAUDE.md` |
| Work on `exomonad-core` framework/services | `rust/exomonad-core/CLAUDE.md` |
| Extend the v2 Node-Mode swarm | `rust/CLAUDE.md` + the `exo-*` crate docs |
| Define v2 capabilities | `rust/exo-caps/CLAUDE.md` |
| Work on v2 node sidecar logic | `rust/exo-node/CLAUDE.md` |
| Add or modify E2E tests | `tests/e2e/CLAUDE.md` |
| Add FFI boundary types | `proto/CLAUDE.md` |
| Understand architectural decisions | `docs/decisions/` |

---

## References

- [`rust/CLAUDE.md`](rust/CLAUDE.md) — Rust workspace (Classic + v2 swarm)
- [`haskell/wasm-guest/CLAUDE.md`](haskell/wasm-guest/CLAUDE.md) — Classic MCP tool definitions
- [freer-simple](https://hackage.haskell.org/package/freer-simple) — Effect system (Classic WASM guest)
- [Anthropic tool use](https://docs.anthropic.com/en/docs/tool-use)
