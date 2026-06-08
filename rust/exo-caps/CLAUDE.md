# exo-caps — the capability seam

The trait/type contract that the node-mode swarm forks from. The engine abstractions (`exo-framework`) and the domain (`exo`) are written **generic over these traits**; the runtime (`exo-runtime`) implements them. This crate has **no IO** — only trait definitions, validated domain newtypes, and the message/identity vocabulary.

This is the seam that replaces the old Haskell-WASM boundary. WASM *physically* prevented the policy layer from doing IO; a crate that simply doesn't link the runtime gets the same separation at zero runtime cost — **except** the wall is soft: policy *may* drop to raw IO as an escape hatch. Good caps make it rarely want to.

> Part of the v2 node-mode swarm (`exo`), built beside classic exomonad-core. See `rust/CLAUDE.md` for how the node-mode crates relate.

## What lives here

| Module | Contents |
|--------|----------|
| `types` | Validated domain newtypes + the identity/messaging vocabulary (see below) |
| `domain` | The **domain seam** traits the engine is generic over: `RoleKind` (domain role enum), `SpawnSpec` (spawn intent), `DomainSystem` (inter-node payload, blanket-impl'd for any serde type). Rooted here (not `exo-framework`) because `Spawner`/`NodePapers`/`MessageKind` reference them and `exo-caps` can't depend on `exo-framework`. The `Exomonad` trait that ties them together lives in `exo-framework`. See [`docs/decisions/exo-trait-refactor.md`](../../docs/decisions/exo-trait-refactor.md). |
| `error` | `CapError` — source-preserving (`#[from]` per-cap errors), `CapResult` |
| `bus` | `Bus` trait + `Addressee` (tree-edges only) + `BusError` |
| `spawner` | `Spawner` trait + the per-op specs (`WorkerSpec`/`GeminiSpec`/`ForkSpec`) |
| `lifecycle` | `ChildRecord` (append-only `Spawned`/`Started`) + `fold_children` |
| `papers` | `NodePapers` (`node.json`) — a node's immutable birth identity |
| `paths` | Inbox/papers path scheme (`~/.claude/exo/inboxes/{run_id}/pane-N.jsonl`) |
| `invocation` | Single source of truth for a child's `exo node/hook` argv |
| `git` `tmux` `fs` `kv` `process` `log` | The IO capability traits (signatures only) |

## The capability traits

Ten caps, each one trait per file. `exo-runtime::Runtime` implements all of them; `exo::testing::MockRuntime` mocks all of them.

- **`Git`** — `current_branch`, `head_sha` (sha-tag review verdicts), `merge_base` (fork-point base for a reviewer's `git diff`), `is_clean`, `fetch`, **`merge`** (the local on-disk fold — v2 convergence), `worktree_add`/`worktree_remove`. **No `GitHub` cap** — v2 convergence is local git, no PR/Copilot (cut 2026-06-01; see `reactive-github-layer-stays` memory).
- **`Bus`** — `deliver(Addressee, Message)`. The append half only; the read/cursor/watch half is the sidecar's inbound loop. Delivery mechanism (Teams vs tmux) is the *recipient's* last-hop concern — policy never names it.
- **`Spawner`** — ONE generic `spawn(D::Spawn)` (the recursion; the domain's spawn intent fixes `(role, kind)` at the tool boundary) + a `fork_wave` vec wrapper (default method) + `reclaim_worktree` / `kill_pane` (teardown). Replaces the old per-archetype methods: a new archetype is a new domain role + a thin domain tool, not a `Spawner` edit. Fully generic `spawn<S: SpawnSpec>` over the domain role (the runtime records the role erased).
- **`Tmux`** — `new_pane`, `new_window`, `paste`, `kill_pane`.
- **`Fs`** — `read`, `write_atomic`.
- **`Kv`** — `get`, `set`.
- **`Process`** — `run`.
- **`Log`** — `info`, `error` (sync, infallible).
- **`Topology`** — `topology()` → the caller's subtree (folded recursively from the per-node `children.jsonl` ledgers) + parent + per-node pane-liveness. Backs the `tree` tool.
- **`ChildLiveness`** — `any_child_busy()` → is any *direct* child still working? Idle is tracked from messages (busy at birth + on every poke; idle on `ChildIdle`), combined with pane-death as a one-way override. Distinct from `Topology`'s pane-**existence**: a live pane ≠ busy (a Gemini child idles with its `--prompt-interactive` pane alive), but a dead pane ⇒ idle. In-memory, non-persisted (a sidecar restart re-seeds conservatively: unknown ⇒ busy if the pane is alive). Backs the `stop` idle gate.

## Domain types (the invariants worth knowing)

- **Validate-at-construction newtypes.** `new(...) -> CapResult`; once built, always valid. **Serde deserializes *through* the constructor** (`#[serde(try_from)]`), so a value read off disk (papers, a bus line) is validated too — no "transparent" hole. `AgentName`, `Branch`, `PaneId`, `MessageBody` (≤4 KiB, no C0 ctrl except `\t\n\r`), `Summary` (≤256 B, single line), `SyntheticName`.
- **`NodePath`** — tree address as a `Vec<AgentName>`, **not** a dot-string (branch segments may contain `.`, so a joined form can't round-trip). `name()` = last segment, `parent()` = prefix, `child()` extends. `Branch::from_path` generates a *safe* branch (sanitize segments to `[A-Za-z0-9_-]`, join `.`) decoupled from the path.
- **Role** — the role enum is **domain-owned** (`exo::ExoRole`), reached through the [`RoleKind`](domain) seam; the engine never names a variant. Papers record it **erased** as a [`RoleRecord`] (raw JSON), typed back to `D::Role` by the one typed reader (bootstrap). `RoleKind::agent_type` is the domain's role→backend mapping (`root`/`tl`→Claude, `dev`/`worker`/`reviewer`→Gemini in `exo`). `RoleKind::protocol` returns the role's decomposition-steering prose injected at session_start — **defaults to empty** (so non-`exo` `RoleKind` impls compile unchanged), overridden per-variant only by `exo::ExoRole`.
- **`Message`** = `{text, summary, kind}` — what *policy* builds. It carries **no `from`/`ts`/`id`**: the runtime stamps the `IngestionEntry` envelope at append, so **a tool cannot spoof its sender** (anti-spoof is structural, not convention). `Persona` = `Agent(AgentName) | Synthetic(SyntheticName)`.
- **`MessageKind`** = `Chat | Event | Control(Shutdown{grace_ms}) | Lifecycle(Lifecycle) | Domain(String)` (the **HYBRID wire**). `Event` is delivered like `Chat`. Both `Lifecycle` and `Domain` are sidecar-consumed (never rendered to the LLM unless the handler decides to act), but they split by ownership: **`Lifecycle`** (typed, engine-owned, closed — `child_idle` / `child_exited` / `shutdown_response`) is acted on by the sidecar itself (`mark_child_idle` / `try_reap` / shutdown matrix); **`Domain`** is a domain's [`DomainSystem`] payload erased to a raw JSON string (held as a `String`, not `RawValue`, so it survives `#[serde(flatten)]`'s buffered intermediate), deserialized to the concrete `D::System` at exactly one place (the inbound loop's Domain arm) before `D::handle_system`. A fully-typed System wire was rejected because it would force `C: Bus` → `C: Bus<D::System>` and collapse per-tool least-privilege; only the multi-writer bus payload is erased (papers carry `D::Role` fully typed). Build a Domain message with the free [`deliver_domain`] helper (a tool naming `D::System` still needs only `C: Bus`). The `exo` domain's `D::System` is the review-verdict enum (transitionally `exo_caps::ReviewVerdict` until the engine goes generic). An unknown Domain tag is a runtime skip (tolerant parser), not a compile error — mitigated by `Lifecycle` being typed + an exhaustive `handle_system` match.

## Load-bearing principles (encoded in the types)

- **Observe, don't store.** Only genuinely-recorded facts get types: `ChildRecord` is `Spawned`/`Started` only. Running-vs-exited is computed **live** (pane-alive), never written back. `fold_children` folds the append-only log into the current child set (newest `Spawned` wins; `Started` upgrades lifecycle). The one piece of *live* per-child state is the `ChildLiveness` busy-bit — in-memory only, derived from observed messages, never persisted; a restart rebuilds it conservatively.
- **Identity is assigned at birth, not derived.** `role`/`parent`/tree-position exist in no runtime's live state, so `NodePapers` records them once. Live derivation (`exo-scry`) recovers only runtime-native facts (pane, CC team).
- **Messaging is tree-edges only.** `Addressee` = `Parent | InlineChild(name) | WorktreeChild(name)`. There is no sibling/cross-tree addressee — the messaging structure *is* the process tree. (`Pane` is an internal resolution target, not policy-facing.)

## Gaps / not-yet

- `reclaim_worktree` / `kill_pane` are called by the `merge` tool (folded-child reclaim) and the sidecar's reviewer verdict-teardown. Both are **best-effort but bounded-retried** — the runtime impls wrap each op in `exo_runtime::retry_teardown` (3 attempts, linear backoff), logging a loud structured error on final failure and surfacing (never escalating) it. A dirty or nested worktree can still resist reclaim after the retries and linger.
- `ChildKind::Standalone` (fresh-repo child, a classic feature) is folded into `Worktree`; not separately represented.
