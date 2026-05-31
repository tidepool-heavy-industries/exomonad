# Migration & Implementation Plan (as an agent tree)

> **Status: plan.** Build *beside* the working central server + Haskell-WASM,
> non-destructive, dogfooded as an exomonad swarm. Sequenced as
> scaffold-fork-converge; the crate dependency graph forces the wave order.

## Principles

- **Non-destructive.** New `exo-*` crates + new binary modes land beside the old
  `serve`/WASM path. Nothing deleted until its replacement is proven.
- **Dogfood the current tree.** Each wave is a TL scaffolding a commit, forking
  Gemini leaves (one focused change each), converging. Claude TLs decompose;
  Gemini implements; Copilot reviews.
- **The swarm drains Haskell as a side effect** — every ported tool retires its
  WASM twin (Bucket B); the boundary (Bucket A, ~37k LOC) deletes at the end.

## Assets that already exist

- **`exo-scry`** — identity resolution (pane→team/member, papers). Feeds the seam
  + node self-ID. *Done.*
- **`teams-mcp`** — rmcp server + tools (`team_status`/`list_teammates`/`read_inbox`/
  `send_message`). The **seed of the node's outbound half**; will be refactored to
  the generic-ingestion + two-loop model and folded into the binary.

## The tree

```
ROOT TL (human + Claude) ── owns the migration; scaffolds + converges each wave
├── Wave 0  Foundation & spike      (root scaffolds; 1 leaf)
├── Wave 1  Runtime caps            (sub-TL R; 5 leaves)
├── Wave 2  The node / sidecar      (sub-TL N; 4 leaves)   ── depends on W1 + W0 spike
├── Wave 3  Policy content          (sub-TL P; 6 leaves)   ── parallel after caps
└── Wave 4  Cutover & delete        (root TL)              ── last
```

Waves are sequential (deps); leaves within a wave are parallel (no shared files).

---

## Wave 0 — Foundation & spike

**Scaffold (root TL commits):** the workspace skeleton everyone forks from —
`exo-caps` with the *full* domain newtypes + trait signatures from
[03](03-capabilities.md)/[04](04-policy.md) (bodies `todo!()`), stub `exo-runtime`
+ `exo-policy` crates, workspace wiring, the `Caps` super-trait and `RoleDef`
shape. This is the typed contract; all later leaves compile against it.

**Leaf S0 (Gemini) — the CC multi-team spike.** Empirically determine:
- Can one CC session be in two teams at once (member of A + lead of B)?
- Which inbox does InboxPoller watch — does creating B stop delivery to A's inbox?
- Does writing a team config register it, or is `TeamCreate` (the tool) required?

*Method:* create team A; spawn a teammate into A; have it `TeamCreate` B; inspect
its claude process's inotify watches (`/proc/{pid}/fdinfo`) before/after; test
delivery to it via A's inbox vs B's.

*Decision rule:* if a session keeps receiving on the first team after joining a
second → **join-spawner's-team** is fine. If active-team switches (old inbox goes
dark) → **solo-team-per-session** (each node leads its own 1-member team as a pure
push channel). Record the choice in [07](07-open-questions.md). *Either way the
generic-ingestion layer is unaffected — this only sets the CC last-hop wiring.*

**Converge:** skeleton compiles (`cargo check`); decision recorded.

---

## Wave 1 — Runtime caps (`exo-runtime`)

**Sub-TL R (Claude).** Scaffold: the `Runtime` struct stub that will `impl Caps`,
plus per-cap module stubs. Fork (one leaf each, no file overlap):

| Leaf | Cap | Source / notes |
|---|---|---|
| R1 | `Git` + `GitHub` | adapt `GitService`/`GitHubService` from exomonad-core |
| R2 | `Tmux` | adapt `TmuxIpc`; includes tmux-paste delivery |
| R3 | **`Bus`** | ingestion-inbox append/read, ulid + cursor, `Addressee`→`InboxPath` resolve (reuse `exo-scry`). *The core new piece.* |
| R4 | `Spawner` | worktree + pane + write child papers (`parent_inbox`) + launch node mode |
| R5 | `Kv` + `Clock` | trivial (file-backed kv; system clock) |

**Converge:** R wires the leaves into `Runtime: Caps`; integration test that
`Bus::deliver(Parent, …)` appends to a papers-pointed inbox.

---

## Wave 2 — The node / sidecar (binary `mcp-stdio` mode)

**Sub-TL N (Claude).** Depends on W1 + the W0 spike decision. Refactors `teams-mcp`
into the node. Scaffold: the node bootstrap (self-ID via `exo-scry` → build
`Runtime` → assemble loops). Fork:

| Leaf | Piece |
|---|---|
| N1 | **Outbound** — rmcp adapter exposing `exo-policy` `Tool`s; `send_message`/`notify_parent` via `Bus`. (Refactor teams-mcp outbound: write the *ingestion* inbox, not CC Teams directly.) |
| N2 | **Inbound loop** — inotify-watch own ingestion inbox + cursor; route each entry to the agent (Teams write / tmux-paste per `agent_type`); invoke `on_world_event` on `kind=event`, handle `kind=control`. |
| N3 | **Self-poll** — periodic own-PR/CI poll → `WorldEvent` → `on_world_event` → `InjectMessage`/`NotifyParent`. (Per-agent realization of the old central poller.) |
| N4 | **`exomonad hook` mode** — CC payload → `exo-policy` `pre_tool_use`/`stop`/`session_start` → verdict. No server. |

**Converge:** N assembles the three stimuli as tokio tasks in one process; e2e —
spawn a node, round-trip a message parent↔child, fire a synthetic event.

---

## Wave 3 — Policy content (`exo-policy`)

**Sub-TL P (Claude).** Parallel-able once `exo-caps` exists (after W0/W1). One leaf
per tool/area, each retiring its Haskell twin as its Rust lands:

| Leaf | Content |
|---|---|
| P1 | messaging tools (port/confirm from teams-mcp) |
| P2 | `tasks_*` tools |
| P3 | `spawn_*` tools (`fork_wave`/`spawn_gemini`/`spawn_worker`) over `Spawner` |
| P4 | `file_pr` / `merge_pr` |
| P5 | hooks (`pre_tool_use`, `stop` live-query, `session_start`) |
| P6 | events (`PrReview`/`SiblingMerged`/`CiStatus`/`ReviewTimeout`) + `RoleDef` tables |

**Converge:** P wires `role_def(role)`; each ported tool's WASM twin is removed in
the same PR (Bucket B drains here).

---

## Wave 4 — Cutover & delete

**Root TL.**
1. Wire node mode into `exomonad init` + spawn (coexist with old `serve` behind a
   flag/role during transition).
2. Migrate roles to the new path; verify each against the e2e harness.
3. As the last WASM tool retires, **delete `wasm-guest` + Bucket A** (proto,
   proto3-runtime, freer-simple, continuations, FFI — ~37k LOC) in one commit.
4. Remove `serve` once nothing uses it.

---

## Dependency & parallelism summary

- **0 → 1 → 2** strictly (contract → caps → node).
- **3** can start once `exo-caps` is real (end of W0) and run alongside W1/W2; it
  only needs the `Tool`/`Caps` contract, not the runtime impls.
- **4** is last.
- Within each wave, leaves are conflict-free (separate crates/modules) → full
  parallel fork.

## Gates (each wave's converge before the next forks)

- W0: skeleton `cargo check`s; spike decision recorded.
- W1: `Runtime: Caps` + Bus integration test green.
- W2: node e2e (message round-trip + synthetic event) green.
- W3: each tool has parity with its WASM twin (Copilot-reviewed) before the twin is cut.
- W4: full e2e on the new path; old path removed.
