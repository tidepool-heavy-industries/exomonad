# exo-caps — the capability seam

The trait/type contract that the node-mode swarm forks from. Policy (`exo-policy`) is written **generic over these traits**; the runtime (`exo-runtime`) implements them. This crate has **no IO** — only trait definitions, validated domain newtypes, and the message/identity vocabulary.

This is the seam that replaces the old Haskell-WASM boundary. WASM *physically* prevented the policy layer from doing IO; a crate that simply doesn't link the runtime gets the same separation at zero runtime cost — **except** the wall is soft: policy *may* drop to raw IO as an escape hatch. Good caps make it rarely want to.

> Part of the v2 node-mode swarm (`exomonad experimental`), built beside classic exomonad-core. See `rust/CLAUDE.md` for how the node-mode crates relate.

## What lives here

| Module | Contents |
|--------|----------|
| `types` | Validated domain newtypes + the identity/messaging vocabulary (see below) |
| `error` | `CapError` — source-preserving (`#[from]` per-cap errors), `CapResult` |
| `bus` | `Bus` trait + `Addressee` (tree-edges only) + `BusError` |
| `spawner` | `Spawner` trait + the per-op specs (`WorkerSpec`/`GeminiSpec`/`ForkSpec`) |
| `lifecycle` | `ChildRecord` (append-only `Spawned`/`Started`) + `fold_children` |
| `papers` | `NodePapers` (`node.json`) — a node's immutable birth identity |
| `paths` | Inbox/papers path scheme (`~/.claude/exo/inboxes/{run_id}/pane-N.jsonl`) |
| `invocation` | Single source of truth for a child's `exomonad experimental node/hook` argv |
| `git` `tmux` `fs` `kv` `process` `log` | The IO capability traits (signatures only) |

## The capability traits

Eight caps, each one trait per file. `exo-runtime::Runtime` implements all of them; `exo-policy::testing::MockRuntime` mocks all of them.

- **`Git`** — `current_branch`, `is_clean`, `fetch`, **`merge`** (the local on-disk fold — v2 convergence), `worktree_add`/`worktree_remove`. **No `GitHub` cap** — v2 convergence is local git, no PR/Copilot (cut 2026-06-01; see `reactive-github-layer-stays` memory).
- **`Bus`** — `deliver(Addressee, Message)`. The append half only; the read/cursor/watch half is the sidecar's inbound loop. Delivery mechanism (Teams vs tmux) is the *recipient's* last-hop concern — policy never names it.
- **`Spawner`** — `spawn_worker` / `spawn_gemini` / `fork_wave` (the recursion) + `reclaim_worktree` / `kill_pane` (teardown).
- **`Tmux`** — `new_pane`, `new_window`, `paste`, `kill_pane`.
- **`Fs`** — `read`, `write_atomic`.
- **`Kv`** — `get`, `set`.
- **`Process`** — `run`.
- **`Log`** — `info`, `error` (sync, infallible).
- **`Topology`** — `topology()` → the caller's subtree (folded recursively from the per-node `children.jsonl` ledgers) + parent + per-node pane-liveness. Backs the `tree` tool.

## Domain types (the invariants worth knowing)

- **Validate-at-construction newtypes.** `new(...) -> CapResult`; once built, always valid. **Serde deserializes *through* the constructor** (`#[serde(try_from)]`), so a value read off disk (papers, a bus line) is validated too — no "transparent" hole. `AgentName`, `Branch`, `PaneId`, `MessageBody` (≤4 KiB, no C0 ctrl except `\t\n\r`), `Summary` (≤256 B, single line), `SyntheticName`.
- **`NodePath`** — tree address as a `Vec<AgentName>`, **not** a dot-string (branch segments may contain `.`, so a joined form can't round-trip). `name()` = last segment, `parent()` = prefix, `child()` extends. `Branch::from_path` generates a *safe* branch (sanitize segments to `[A-Za-z0-9_-]`, join `.`) decoupled from the path.
- **`NodeKind`** (`Root`/`Tl`/`Dev`/`Worker`) — the one stored archetype. `agent_type()` **derives** (`Root`/`Tl`→Claude, `Dev`/`Worker`→Gemini) — never stored separately, so `(Root, Gemini)` is unnameable.
- **`Message`** = `{text, summary, kind}` — what *policy* builds. It carries **no `from`/`ts`/`id`**: the runtime stamps the `IngestionEntry` envelope at append, so **a tool cannot spoof its sender** (anti-spoof is structural, not convention). `Persona` = `Agent(AgentName) | Synthetic(SyntheticName)`.
- **`MessageKind`** = `Chat | Event | Control(Shutdown{grace_ms})`. `Event` is delivered like `Chat` (tagged `kind: event` in the last-hop header) — there is no world-event handler in v2.

## Load-bearing principles (encoded in the types)

- **Observe, don't store.** Only genuinely-recorded facts get types: `ChildRecord` is `Spawned`/`Started` only. Running-vs-exited is computed **live** (pane-alive), never written back. `fold_children` folds the append-only log into the current child set (newest `Spawned` wins; `Started` upgrades lifecycle).
- **Identity is assigned at birth, not derived.** `role`/`parent`/tree-position exist in no runtime's live state, so `NodePapers` records them once. Live derivation (`exo-scry`) recovers only runtime-native facts (pane, CC team).
- **Messaging is tree-edges only.** `Addressee` = `Parent | InlineChild(name) | WorktreeChild(name)`. There is no sibling/cross-tree addressee — the messaging structure *is* the process tree. (`Pane` is an internal resolution target, not policy-facing.)

## Gaps / not-yet

- `reclaim_worktree` / `kill_pane` are defined here but have **no policy-facing tool** that calls them (see `exo-policy` gaps — convergence teardown is unwired).
- `ChildKind::Standalone` (fresh-repo child, a classic feature) is folded into `Worktree`; not separately represented.
