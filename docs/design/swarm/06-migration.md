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
- **Granularity tracks complexity, not file count.** A Gemini gets exactly one job it
  can hold in one head: adapt-a-service, or one-function-one-invariant. A *complex*
  piece — systems-level races, several invariants at once, retry/guard heuristics — is
  **never handed to a single Gemini**. It becomes a **sub-TL** (a Claude compression
  boundary) that writes the test harness and splits the work into one-invariant-each
  Gemini tasks, then converges them. This is why `Bus` and `Spawner` get their own
  sub-TLs below, not one leaf each.

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
│  │
│  ├─ Runtime TL (Claude) ─ exo-runtime cap impls
│  │   ├─ Gemini: Git + GitHub                  (adapt services — simple)
│  │   ├─ Gemini: Tmux                          (adapt TmuxIpc — simple)
│  │   ├─ Gemini: Fs+Process+Log+Kv             (trivial batch; ts stamped via Utc::now(), no Clock cap)
│  │   ├─ Bus TL (Claude) ─── COMPLEX → its own level:        4 small Geminis
│  │   │     append-primitive · cursor · inotify-watch · Addressee→path resolve
│  │   └─ Spawner TL (Claude) ─ COMPLEX → its own level:      3 small Geminis
│  │         branch-gen+worktree-add · birth-core · teardown
│  │
│  └─ Policy TL (Claude) ─ exo-policy  (unit-tests vs MOCK caps → does NOT wait on Runtime)
│      ├─ Gemini: messaging   ├─ Gemini: tasks        ├─ Gemini: spawn tools (3 per-op)
│      ├─ Gemini: file_pr     ├─ Gemini: merge_pr ←complex, own leaf
│      ├─ Gemini: hooks       └─ Gemini: events + roles
│
├─ Node TL (Claude) ─ Wave 2, after Runtime converges ─ the sidecar binary
│     outbound · inbound{dispatch | loop} ←split · self-poll · hook-mode
│
└─ Root ─ Wave 4 cutover: wire node mode behind a flag, migrate roles, delete WASM/Bucket-A, drop serve
```

Four levels deep on the complex paths (Root → Runtime TL → Bus TL → Gemini) — each
Claude layer is a compression boundary that keeps any one context window reasoning
about ≤~4 children, never the whole subtree.

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

## Wave 0.5 — `exo-mailbox` (Mailbox TL)

**Mailbox TL (Claude).** The durable file-MPSC queue — the one systems-heavy primitive,
in its own crate with **no exo-specific deps**, so it's built and tortured in isolation
*before* Runtime's `Bus` (a thin adapter) and the Node's inbound loop both consume it.
This is why it's its own sub-TL pulled ahead of Wave 1, not a leaf inside it. Harness
first (the crash-consistency suite), then one invariant per Gemini:

| Leaf | One job | Test it must pass |
|---|---|---|
| M1 | atomic append — single `O_APPEND`, assert serialized line ≤ PIPE_BUF (error if over; **no spill**) | N concurrent appenders, zero interleaving; an oversized line errors, never corrupts |
| M2 | byte-offset cursor — persist sibling `.cursor`, advance via **temp+rename** (a small overwrite is NOT crash-atomic), read-to-last-`\n`, missing→EOF | restart resumes; a crash mid-cursor-write never yields a garbage offset |
| M3 | inotify watch — `IN_MODIFY` → wake consumer, re-read from cursor (absorbs coalesced events). **Via the async reactor** (inotify `Stream`/`AsyncFd` or `notify`) — a blocking read stalls the executor | an append wakes the consumer; no executor block |

**Converge:** `Mailbox { append, consume() -> Stream, commit }` — opaque lines, no exo
types; the crash-consistency suite green. `Addressee`→path resolution and the
`IngestionEntry` envelope are **not** here — they're the Runtime `Bus` adapter (R4),
which wraps this. PR up to Root; gates Runtime's R4 and the Node inbound loop.

---

## Wave 1 — Runtime caps (`exo-runtime`) — **Runtime TL**

**Runtime TL (Claude).** Scaffold: the `Runtime` struct stub (holds `EffectContext`:
agent identity) + per-cap module stubs (`impl <Cap> for Runtime`, one file each). Most
caps are direct Gemini leaves (adapt-a-service, or — for `Bus` — a thin wrapper over the
already-built `exo-mailbox`); only `Spawner` (spawn races) stays a sub-TL.

**Cap leaves** (one head each):

| Leaf | Cap(s) | Source |
|---|---|---|
| R1 | `Git` + `GitHub` | adapt `GitService`/`GitHubService` |
| R2 | `Tmux` | adapt `TmuxIpc`; the tmux-paste last-hop |
| R3 | `Fs` + `Process` + `Log` + `Kv` | trivial (std fs/process, file kv, file-at-worktree-root log). `ts` is stamped by the Bus impl via `Utc::now()` — no `Clock` cap |
| R4 | `Bus` | **thin adapter over `exo-mailbox`**: resolve `Addressee`→`InboxPath` (papers `parent_inbox` / child ledger → run-id-keyed path, reuse `exo-scry`), wrap `Message` in an `IngestionEntry` (stamp `from`/`ts`/`v`), `mailbox.append`. No queue internals here — those are `exo-mailbox`. |

> **Async hazard (all of R1–R3): do NOT block the executor.** The services being
> adapted use synchronous `std::process::Command`; calling `.output()` inside an
> `async fn` cap method blocks a tokio worker for the whole git/tmux/gh call. "Adapt"
> means **`tokio::process::Command`** (or wrap in `spawn_blocking`) — never a raw
> sync call. This is the spec's biggest async footgun; front-load it in every leaf.

> **`Bus` (R4) is now thin** because the queue lives in `exo-mailbox`, built earlier as
> its own sub-TL (see *Wave 0.5* below). R4 is just resolve + wrap + `mailbox.append`.

### Spawner TL (Claude) — birth & teardown, decomposed

Spawn races are the other fumble-class. Harness first (spawn→papers-exist→`AgentStarted`
appears; worktree add/remove), then:

| Leaf | One job | Test |
|---|---|---|
| S1 | safe branch-gen + `git worktree add` (Worktree kind only) — generate `Branch` from `NodePath` with no `.`-corruption | branch round-trips; worktree created |
| S2 | `birth(BirthCore)` core — append `AgentSpawned` **first** → `tmux new-pane` → write child papers (`node.json` incl. `parent_inbox`) → launch `exomonad mcp-stdio` | papers exist; record precedes pane |
| S3 | teardown — `reclaim_worktree` (`git worktree remove`) + force `kill_pane` | worktree gone; pane killed |

**Converge:** wire the three per-op birth paths over the core (worker = inline, no
worktree; gemini/fork = worktree via S1); PR up.

**Runtime TL converge:** wires R1–R4 + the Spawner sub-TL PR into one `Runtime` impl'ing
every cap; integration test that `Bus::deliver(Parent, …)` appends to a papers-pointed
inbox (via `exo-mailbox`) and a restart resumes from the cursor.

---

## Wave 2 — The node / sidecar (binary `mcp-stdio` mode) — **Node TL**

**Node TL (Claude).** Depends on the Runtime TL converging + the W0 spike decision. Refactors `teams-mcp`
into the node. Scaffold: the node bootstrap (self-ID via `exo-scry` → build
`Runtime` → assemble loops). Fork:

| Leaf | Piece |
|---|---|
| N1 | **Outbound** — rmcp adapter exposing `exo-policy` `Tool`s; `send_message`/`notify_parent` via `Bus`. (Refactor teams-mcp outbound: write the *ingestion* inbox, not CC Teams directly.) |
| N2a | **Last-hop dispatch** — route one entry by `node_kind.agent_type()`: CC-in-team → Teams inbox write (via `exo-scry` membership); else → tmux-paste. (Reuse exomonad-core delivery.) |
| N2b | **Inbound loop** — drive the Bus read side (B2+B3, already built) → per new entry match `kind`: `Chat`→dispatch(N2a), `Event`→parse `WorldEvent`→`on_world_event`→act, `Control`→shutdown self-kill. |
| N3 | **Self-poll** — periodic own-PR/CI poll → `WorldEvent` → `on_world_event` → `InjectMessage`/`NotifyParent`. (Per-agent realization of the old central poller; reuse `github_poller` timeout logic.) **Needs a tracked `AbortHandle`**: abort the poll task when the PR merges/closes, and re-`file_pr` must not spawn a second poller (dedup/replace) — a bare `tokio::spawn` leaks and double-polls. |
| N4 | **`exomonad hook` mode** — CC payload → `exo-policy` `pre_tool_use`/`stop`/`session_start` → verdict. No server. |

The inbound loop is split (N2a dispatch | N2b loop) and is **lighter than it looks** —
the hard parts (cursor, inotify, atomic read) are the Bus's, already built and tested in
Wave 1; N2b only *consumes* them.

**Converge:** N assembles the stimuli as tokio tasks in one process; e2e — spawn a
node, round-trip a message parent↔child, fire a synthetic event.

---

## Wave 3 — Policy content (`exo-policy`) — **Policy TL, concurrent with Runtime**

**Policy TL (Claude).** **Gated only on `exo-caps` signature-freeze (end of Wave 0)** —
NOT on Runtime impls. Policy tools call cap *traits* and unit-test against **mock caps**
(`impl Git for MockGit`, zero IO — the seam's payoff), so the Policy TL runs **fully
concurrent with the Runtime TL**; real-impl integration happens at Wave 2. The
signature-freeze gate is load-bearing: if caps churn, the concurrent leaves break (the
review's #2 risk). One leaf per tool *file* (the type-per-tool layout — [04](04-policy.md)),
each retiring its Haskell twin as its Rust lands:

| Leaf | File(s) | Content | Complexity |
|---|---|---|---|
| P1 | `tools/messaging.rs` | `notify_parent`, `send_message` over `Bus` (port from teams-mcp) | simple |
| P2 | `tools/tasks.rs` | `task_list`/`task_get`/`task_update` | simple |
| P3 | `tools/spawn.rs` | the three **per-op** spawn tools over `Spawner` — each fixes its `(role, agent_type, kind)`; thin wrappers | simple |
| P4 | `tools/file_pr.rs` | PR create/update | simple |
| P5 | `tools/merge_pr.rs` | merge — **complex** (rebase/retry/guard heuristics, `MergePR.hs` ~364 LOC); own leaf, strong spec, escalate or sub-split if the conflict path is gnarly | **complex** |
| P6 | `hooks.rs` | `pre_tool_use` (guard/PII), `stop` (live PR gate), `session_start` — the `stop` gate is the involved one; sub-split if it grows | moderate |
| P7 | `events.rs`, `roles.rs` | `WorldEvent` handlers + the `role_def(NodeKind)` table | simple |

Each tool = a type (`Args` + generic-over-caps `run` + hand `Tool<R>` adapter) with
**mock-cap unit tests** in the same PR. The complexity column applies the same rule: a
*moderate/complex* leaf (P5, maybe P6) gets a sharper spec and is the first candidate to
sub-split if it stalls; the simple ones are direct Geminis. **Converge:** Policy TL
wires `role_def(NodeKind)`; each ported tool's WASM twin is removed in the same PR
(Bucket B drains here).

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
- **`exo-mailbox` (Wave 0.5) early** — no exo deps, so it can build as soon as Wave 0's
  workspace exists; it gates Runtime's `Bus` (R4) and the Node inbound loop, so pull it
  ahead rather than blocking Runtime mid-wave. (Policy doesn't need it — mock caps.)
- **Then Runtime TL ∥ Policy TL** — the two concurrent Claude sub-TLs. Policy needs
  only the cap *traits* (tests against mock caps), so it does **not** wait on Runtime
  impls. This is the core parallelism: two TLs, ~11 Gemini leaves in flight.
- **Node (Wave 2) after Runtime** — it assembles real `Runtime` + `exo-policy` into the
  sidecar; needs Runtime's impls, `exo-mailbox`, and the W0 spike's CC-last-hop decision.
- **Cutover (Wave 4) last.**
- Within a sub-TL, leaves are conflict-free (one cap-trait/file, one tool/file) → full
  parallel fork.

## Gates (each wave's converge before the next forks)

- W0: skeleton `cargo check`s; spike decision recorded.
- W0.5: `exo-mailbox` crash-consistency suite green (append no-interleave, cursor restart).
- W1: `Runtime` impls all cap traits + Bus integration test green (Bus over `exo-mailbox`).
- W2: node e2e (message round-trip + synthetic event) green.
- W3: each tool has parity with its WASM twin (Copilot-reviewed) before the twin is cut.
- W4: full e2e on the new path; old path removed.
