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
- **Reuse tested components, don't rewrite** — adapt exomonad-core's
  `GitService`/`GitHubService`/`TmuxIpc`, the tmux-injection (works for Gemini
  today), CC-inbox delivery, and the `github_poller` timeout logic. Greenfield only
  the genuinely-new pieces (the append-only `Bus`, the inbound loop).
- **Honest scope** — this is **not** a mechanical port. Real Bucket B+C is ~4.5k LOC
  of dense domain logic (`Spawn.hs` ~637, `MergePR.hs` ~364 — retries/guards/
  heuristics; roles/hooks/events ~2k). Budget accordingly; aim for best-practice,
  strongly-typed Rust, executed by a massively-parallel swarm.

## Assets that already exist

- **`exo-scry`** — identity resolution (pane→team/member, papers). Feeds the seam
  + node self-ID. *Done.*
- **`teams-mcp`** — rmcp server + tools (`team_status`/`list_teammates`/`read_inbox`/
  `send_message`). The **seed of the node's outbound half**; will be refactored to
  the generic-ingestion + two-loop model and folded into the binary.

## The tree

**Three Claude TLs; Gemini does the implementation.** Root scaffolds the contract and
converges; each sub-TL runs its own scaffold-fork-converge over Gemini leaves. The
headline is the **two concurrent sub-TLs** — Runtime ∥ Policy — which is what "multiple
Claude TLs with Geminis under them" means in practice.

```
Root TL (Claude, human-facing) ── scaffolds the frozen contract, converges every wave, owns cutover
│
├─ Wave 0 (Root, solo): scaffold exo-caps — the contract everyone forks from (the coalgebra)
│                        + spawn 1 Gemini for the CC multi-team spike (S0)
│
├─ then fork TWO Claude sub-TLs, CONCURRENT (different crates; both fork the caps-freeze commit):
│  ├─ Runtime TL (Claude) ─ exo-runtime cap impls       → 5 Gemini leaves (one per cap file)
│  └─ Policy  TL (Claude) ─ exo-policy tools/hooks/roles → 6 Gemini leaves (one per tool file)
│        Policy unit-tests against MOCK caps, so it does NOT wait on Runtime —
│        the caps seam decouples the two timelines. THIS is the parallelism payoff.
│
├─ Node TL (Claude) ─ Wave 2, after Runtime converges ─ assembles the sidecar binary
│                                                        → 4 Gemini leaves (the loops + hook mode)
│
└─ Root ─ Wave 4 cutover: wire node mode behind a flag, migrate roles, delete WASM/Bucket-A, drop serve
```

Within a sub-TL, leaves are conflict-free by construction — the pinned module layout
([05](05-crates-and-binary.md)) is one cap-trait/file and one tool/file, so N Geminis
never touch the same file. Waves gate on dependencies; siblings fork in parallel.

---

## Wave 0 — Foundation & spike

**Scaffold (root TL commits):** the workspace skeleton everyone forks from —
`exo-caps` with the *full* domain newtypes + trait signatures from
[03](03-capabilities.md)/[04](04-policy.md) (bodies `todo!()`), stub `exo-runtime`
+ `exo-policy` crates, workspace wiring, the **individual cap traits** (`Git`,
`GitHub`, `Bus`, `Spawner`, …) and the `RoleDef<R>` shape. This is the typed
contract; all later leaves compile against it. **Also
pre-populate every anticipated dependency** in each crate's `Cargo.toml` (octocrab,
inotify, ulid, nix, async-trait, schemars, tokio, …) so parallel leaves never
collide on `Cargo.toml` (the review's top build risk).

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

## Wave 1 — Runtime caps (`exo-runtime`) — **Runtime TL**

**Runtime TL (Claude).** Scaffold: the `Runtime` struct stub (holds `EffectContext`:
agent identity) + per-cap module stubs (`impl <Cap> for Runtime`, one file each). Fork
(one leaf per cap file, no overlap):

| Leaf | Cap(s) | Source / notes |
|---|---|---|
| R1 | `Git` + `GitHub` | adapt `GitService`/`GitHubService` from exomonad-core |
| R2 | `Tmux` | adapt `TmuxIpc`; includes the tmux-paste delivery last-hop |
| R3 | `Fs` + `Process` + `Log` + `Kv` + `Clock` | mostly trivial (file kv, system clock, std fs/process, file-at-worktree-root log) — batch into one leaf |
| R4 | **`Bus`** | ingestion-inbox atomic O_APPEND + byte-offset cursor + inotify + `Addressee`→run-id-keyed `InboxPath` resolve. **test-harness-first.** *The core new piece.* |
| R5 | **`Spawner`** | per-op `birth(BirthCore)`: worktree-add (Worktree kind) → pane → write child papers (`parent_inbox`) → launch node mode → append `AgentSpawned`; + `reclaim_worktree`/`kill_pane`. **test-harness-first.** |

**R1/R2/R3 reuse** exomonad-core services — adapt, don't rewrite. **R4 (`Bus`) and R5
(`Spawner`) are systems-heavy** (atomic append, flock-for-compaction, cursor restart,
inotify; worktree+pane+papers spawn races) — Gemini reliably fumbles this class, so the
Runtime TL **writes the failing tests as part of the scaffold** (atomic-append
concurrency, cursor-restart resume, spawn→papers→`AgentStarted`) and the Gemini
implements to green; escalate to a higher-capability agent if it stalls.

**Converge:** Runtime TL wires the leaves into one `Runtime` impl'ing every cap;
integration test that `Bus::deliver(Parent, …)` appends to a papers-pointed inbox and a
restart resumes from the byte-offset cursor.

---

## Wave 2 — The node / sidecar (binary `mcp-stdio` mode) — **Node TL**

**Node TL (Claude).** Depends on the Runtime TL converging + the W0 spike decision. Refactors `teams-mcp`
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

## Wave 3 — Policy content (`exo-policy`) — **Policy TL, concurrent with Runtime**

**Policy TL (Claude).** **Gated only on `exo-caps` signature-freeze (end of Wave 0)** —
NOT on Runtime impls. Policy tools call cap *traits* and unit-test against **mock caps**
(`impl Git for MockGit`, zero IO — the seam's payoff), so the Policy TL runs **fully
concurrent with the Runtime TL**; real-impl integration happens at Wave 2. The
signature-freeze gate is load-bearing: if caps churn, the concurrent leaves break (the
review's #2 risk). One leaf per tool *file* (the type-per-tool layout — [04](04-policy.md)),
each retiring its Haskell twin as its Rust lands:

| Leaf | File(s) | Content |
|---|---|---|
| P1 | `tools/messaging.rs` | `notify_parent`, `send_message` (port/confirm from teams-mcp) over `Bus` |
| P2 | `tools/tasks.rs` | `task_list`/`task_get`/`task_update` |
| P3 | `tools/spawn.rs` | the three **per-op** spawn tools (`spawn_worker`/`spawn_gemini`/`fork_wave`) over `Spawner` — each fixes its `(role, agent_type, kind)` |
| P4 | `tools/file_pr.rs`, `tools/merge_pr.rs` | PR create/update + merge |
| P5 | `hooks.rs` | `pre_tool_use` (guard/PII), `stop` (live PR gate), `session_start` |
| P6 | `events.rs`, `roles.rs` | `WorldEvent` handlers + the `role_def(NodeKind)` table |

Each tool = a type (`Args` + generic-over-caps `run` + hand `Tool<R>` adapter) with
**mock-cap unit tests** in the same PR. **Converge:** Policy TL wires
`role_def(NodeKind)`; each ported tool's WASM twin is removed in the same PR (Bucket B
drains here).

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

- **Wave 0 first** — the caps signature-freeze is the gate everything forks from.
- **Then Runtime TL ∥ Policy TL** — the two concurrent Claude sub-TLs. Policy needs
  only the cap *traits* (tests against mock caps), so it does **not** wait on Runtime
  impls. This is the core parallelism: two TLs, ~11 Gemini leaves in flight.
- **Node (Wave 2) after Runtime** — it assembles real `Runtime` + `exo-policy` into the
  sidecar; needs Runtime's impls and the W0 spike's CC-last-hop decision.
- **Cutover (Wave 4) last.**
- Within a sub-TL, leaves are conflict-free (one cap-trait/file, one tool/file) → full
  parallel fork.

## Gates (each wave's converge before the next forks)

- W0: skeleton `cargo check`s; spike decision recorded.
- W1: `Runtime` impls all cap traits + Bus integration test green.
- W2: node e2e (message round-trip + synthetic event) green.
- W3: each tool has parity with its WASM twin (Copilot-reviewed) before the twin is cut.
- W4: full e2e on the new path; old path removed.
