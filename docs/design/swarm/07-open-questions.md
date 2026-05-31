# Open Questions

> **Status: living.** Empirical unknowns and undecided design points. Some are
> cheap experiments; note them here so they're not lost.

## Empirical (testable now)

- **CC multi-team membership.** → **Assigned to the Wave-0 spike** (Leaf S0) in
  [06](06-migration.md), with method + decision rule. Can one CC session be in two
  teams at once, and **which inbox does InboxPoller watch**? Decides solo-team-per-
  session vs. join-spawner's-team — *the generic-ingestion layer is robust to
  either*, so this only sets the CC last-hop wiring, not the architecture.
- **Pane id reuse.** tmux `%N` ids are monotonic per server; confirm they're not
  reused within a server lifetime (a server restart kills all panes/agents anyway,
  so stale pane-keyed inboxes refer to dead nodes — acceptable, but verify).
- **tmux-paste delivery into CC.** Confirm pasting into a CC pane (the no-team
  fallback) lands usefully in the conversation, not just the input box.

## Design (undecided)

> Message format, cursor, inbox root path (`~/.claude/exo/inboxes/{run-id}/`), and
> event-policy home are now **settled** in [02](02-bus-and-sidecar.md) — removed from
> this list. What remains:

- **`exo-caps` exact signatures** — `Git`/`GitHub`/`Tmux`/`Fs`/`Process`/`Log`/
  `Clock`/`Kv` method bodies (mechanical; `Bus`/`Addressee`/`Spawner`/newtypes are
  pinned). ([03](03-capabilities.md))
- **Per-role toolsets, hooks, events** — the Bucket-C *content*, filled in
  incrementally as each Haskell tool ports (no phases). ([04](04-policy.md))
- **Readability index** for pane-keyed inboxes (a `{name} → pane` map/symlink, so a
  human can tell which `pane-NNN.jsonl` is whom). Optional.
- **Crate names** — `exo-caps`/`exo-policy`/`exo-runtime` provisional.

## Mechanical TODO (build-time, not design)

- `Git` / `GitHub` / `Tmux` / `Clock` / `Kv` cap signatures — adapt from
  exomonad-core services.
- `Spawner` — **decided: per-op methods** (`spawn_worker`/`spawn_gemini`/`fork_wave`),
  each fixing its `(role, agent_type, kind)`, sharing a private `birth(BirthCore)`
  tail. Remaining mechanical bit: port the narrow per-op spec field lists from the
  Haskell `WorkerSpec`/`SpawnSpec`.
- Sidecar concurrency: the three stimuli (outbound MCP, inbound inbox-watch,
  self-poll) as tokio tasks in one process.
- `exomonad hook` mode wiring (CC payload → `pre_tool_use`/`stop`/`session_start`).

## Resolved (recorded so we don't relitigate)

- **No phases / state machine** — dropped. The stop-gate is a live query
  (`stop(ctx)` asks GitHub "open PR with unaddressed changes?"), not a persisted phase.
- **Policy form:** `Tool` trait (impl per tool) + pure fns for hooks/events + a
  `RoleDef` struct + hand-written `role_def(role)` table. No DSL, no macros, no HList.
- **Message:** plain-text body + `kind` tag (`Chat`/`Event`/`Control`); `id`+`ts`
  stamped at append.
- **Addressee:** `Parent` + `InlineChild(name)` + `WorktreeChild(name)` (tree-edges
  only; the two child variants share delivery, differ in spawn/papers/reap by
  `ChildKind`); `Pane` is internal resolution only.
- **Polling:** per-sidecar self-poll of own PR; parent owns sibling-merge.

- Singletons: **none required.** Poller decomposes to per-agent self-poll + parent
  owns sibling-merge; mutex → FS lock; GC → lazy liveness; OTel → per-session. The
  `.git`-dir root is an *optional* distinguished node, not needed.
- Parent pointer: a **path to the parent's ingestion inbox** (not a name to resolve,
  not the CC Teams inbox).
- Inbox key: **pane**, not `(team, member)` — team-free, collision-proof for
  co-located CC-spawned agents.
- Identity: **assigned-at-birth papers** (immutable) for the tree; **pane** for the
  universal key; `exo-scry` for root-bootstrap + CC-membership + probing.
- No central server, no HList, no macros, IO escape hatch (not a sandbox).

## Resolved by the adversarial review (7 angles, folded in)

- **Bus = build-not-validated.** What was validated is the CC *last-hop*, not a
  durable bus. Implement it properly: O_APPEND jsonl (<PIPE_BUF, atomic, no lock),
  `flock` only for compaction (no stale-lock deadlock), persisted ulid cursor (no
  count-snapshot → no restart message-loss), inotify (no poll). Current
  `inbox.rs`/`inbox_watcher.rs` are jank to replace.
- **Polling stays decentralized, with discipline:** every 3 min, only while an open
  PR exists; ~15-min review-timeout nudge resetting per Copilot round; sibling-merge
  owned by the parent. Sparse load → no polling singleton (reuses `github_poller`).
- **Child tracking = parent-local append-only birth ledger** (`children.jsonl`);
  status never written back, computed live. (Replaces the dropped `TLPhase` map.)
- **Reuse over rewrite** (tmux injection, CC delivery, exomonad-core services).
- **Type system fully used:** `Persona`/`WorldEvent`/`AgentName` enums + validated
  newtypes, `TypedTool` wrapper, no `pub` fields, illegal states unrepresentable.
- **Honest port scope:** ~4.5k LOC of dense domain logic, not "hundreds."
- **Build-plan fixes:** pre-scaffold all deps (Cargo.toml), gate Wave 3 on
  signature-freeze, route `Bus`/`Spawner` leaves to higher-capability + test harnesses.

## Still genuinely open (need a test, not a decision)

- **tmux-paste into a *CC* conversation** — the gemini-paste path is proven & reused,
  but pasting into an un-teamed *Claude* pane (the no-team fallback) is untested.
- **CC multi-team / which inbox InboxPoller watches** (Wave-0 spike).
- **`send_message` delivered *mid-tool-call*** — `teams-mcp.send_message` round-trips
  when the recipient is idle (proven); does CC handle a `<teammate-message>` that
  arrives while the recipient is mid-tool-call (queue & deliver after, or drop)?
  Deferred edge case — test later.

## Parity gaps (vs native Claude Teams teammates)

Messaging + lifecycle reach parity. After triage, **one real gap remains:**

- **Idle / presence notifications (the lone TODO).** Native teammates emit
  `idle_notification` on turn-end. We don't yet — needed mainly for a **sub-TL
  waiting on its children** (converge). Addable later (sidecar emits when its agent
  goes quiet). *Known missing.*

Triaged as non-gaps / out-of-scope:
- **Receive** — *fine, not a ceiling.* The input box **is** the receive channel for
  non-CC runtimes; a `[from: X, kind: Y]` header on the paste is all it takes for the
  agent to read it as a message.
- **Roster / UI** — the worker has a tmux window; that's its visibility.
- **Peer-DM visibility** — N/A: **messaging *is* the tree structure** (parent ↔ own
  children only; no out-of-band / cross-tree / sibling messaging), so the lead is
  always on the path. Consequence: routing needs **no global worktree scan** — only
  up (parent pointer) or down (child ledger), both O(1). This resolves the
  architecture review's scan-latency / partial-papers-race concern.
- **Plan-approval**, **task-workflow** — out of scope (messaging + lifecycle only).

Where we *exceed* native: ground-truth liveness (vs stale `isActive`/phantoms),
cross-runtime teaming, no drifting registry.

## Round-2 review outcome (folded / dismissed)

Folded (real gaps):
- **Capability set widened** — `Fs`, `Process`, `Log` added to the cap set; `Spawner`
  gains `reap(child)` (parent-side worktree removal); `pre_tool_use` is generic over
  `Kv` (data-dependent guards). Copilot-review = the self-poll's job, not a cap. ([03](03-capabilities.md))
- **Idle/presence is REQUIRED, not optional** — a parent waiting on a wave
  *deadlocks* without it (convergence). Elevate from "lone TODO" to converge
  prerequisite (sidecar emits a heartbeat/idle when its agent goes quiet).
- **Logging** — drop OTel (broken OTLP export); log to a file at the **git
  worktree-root** (the one dir not removed with worktrees), via the `Log` cap.
- **Spawn atomicity** — write the birth-ledger entry before/atomically with pane
  creation + a startup recovery scan, so a crash mid-spawn can't leave an invisible
  ghost node. Orphaned subtrees / dead-parent black holes are accepted for now
  (human-driven box — the human reaps panes); revisit if it bites.

Dismissed (threat model / scope):
- **Security (file injection, unauth shutdown, persona spoofing)** — non-issue:
  same trust model as Claude Code's own file-based inboxes, single-user personal dev
  box. No crypto/auth.
- **Force-kill / emergency-stop, swarm dashboard / new UI mode** — out of scope;
  human-driven (the human kills panes directly).
- **Pane identity across reboot** — accepted limitation (reboot kills agents anyway).

Flagged for decision:
- **SQLite (WAL) vs hand-rolled jsonl bus** — a reviewer argues SQLite gives the same
  serverless durability with far less bespoke systems code. Tension with the
  chosen append-only-jsonl direction (simplicity/inspectability/`tail`-ability). Open.

## Deep-review pass — type-shape hygiene (folded)

Two ultrathink passes hunting the unused-type / illegal-combo / parallel-enum /
conflation class. Fixed in place across passes: the `id`-vs-byte-offset cursor
contradiction ([02](02-bus-and-sidecar.md)/[03](03-capabilities.md)), the papers
self-ID chicken-and-egg (told via launch flag, not guessed — [01](01-identity.md)),
the `reap` conflation (split into process-teardown vs parent-side worktree-reclamation
— [03](03-capabilities.md)/[04](04-policy.md)), and the "event log" mislabel
(→ "record log"). The type-shape findings, all now **folded**:

- **`NodeRef` cut** (unused; its fields didn't even match the one job it could do —
  missing `kind` for teardown, carrying `agent_type` the sender never needs since the
  *recipient* picks its own last-hop). The parent's child-handle is just the folded
  `AgentSpawned` record. A future probe/`list_agents` surface can add a purpose-built
  node-view *then* — no speculative floating type now. ([03](03-capabilities.md))
- **`SpawnSpec { role, agent_type, kind }` → per-op narrow specs.** The unified struct
  admitted illegal combos (`(Inline, Tl, Claude)`) and was a *regression* vs the
  ported Haskell, where `SpawnSpec` is task-content-only and the triple is fixed by
  which core you call. Now: `spawn_worker`/`spawn_gemini`/`fork_wave` each fix their
  own `(role, agent_type, kind)`; the spec carries only task content; a shared private
  `birth(BirthCore)` is the common tail. Illegal combos are *unnameable*. ([03](03-capabilities.md))
- **`EventType` cut — duplicated `WorldEvent`.** Two enums with identical variant
  lists (`PrReview`/`SiblingMerged`/`CiStatus`/`ReviewTimeout`), one as the message
  tag, one as the handler input — guaranteed to drift. Single source of truth:
  `WorldEvent` (04) is the typed enum; `MessageKind::Event` is a bare tag and the
  event detail rides the plain-text body (parsed into `WorldEvent` at the handler),
  preserving the plain-text-body / CC-last-hop principle. ([03](03-capabilities.md)/[04](04-policy.md))
- **`MessageBody` defined** — was referenced by `Message.text` but absent from the
  newtype block (same gap `SyntheticName` had). Now a validated newtype.

Still-open minor flags (low stakes; decide at impl):

- **`AgentSpawned` record — derivable fields resolve by a *rule*, not uniformly.**
  Two of `{ child, kind, pane, path, inbox }` are recomputable, but for *different*
  reasons, so they go opposite ways:
  - **`path` → drop.** Its derivation (`parent.path ++ child`) is **scheme-stable**
    (tree composition never evolves) and the child's identity has a **canonical home
    elsewhere** (the child's own papers). Re-storing it in the parent's record only
    creates a second home that can disagree.
  - **`inbox` → keep (store).** Its derivation (`pane + run-id`) is **scheme-coupled**:
    deriving it couples every reader to the inbox-path layout, and under a
    mixed-version swarm (which the `v` field anticipates) a reader on a *new* layout
    computes the *wrong* path for an *old* child. Stored, it's a birth-time snapshot —
    self-describing, `tail`-able, correct-per-child across layout evolution — the same
    reasoning that stores `parent_inbox` in papers.

  **Rule: store a derivable field iff its derivation is scheme-coupled *or* it has no
  canonical home in the reader's reach; otherwise derive.** Minimal record →
  `{ child, kind, pane, inbox }`.
- **`Role` ↔ `ChildKind` correlate** (`Worker`⟺`Inline`; `Tl`/`Dev`⟺`Worktree`). Both
  are parent-written (papers / record), never free user fields, so no construction
  hazard — but don't ever re-introduce a free `kind` field beside `role`. (Note: `kind`
  is a property of the *spawn relationship*, recorded in the parent's ledger — not in
  the child's own papers, which is already correct.)

## Type-system idiom assessment

Does the design make good expressive use of the type system? **Largely yes** — and
the gaps are now closed or are conscious boundary tradeoffs, not oversights.

**Strong (keep):**
- **Generic-over-caps, no `dyn Caps`** — per-tool bounds (`fn file_pr<C: Git + GitHub>`)
  make least-privilege a *compiler-checked* property. The best type-system use here.
- **`Addressee` encodes the tree topology** — `Parent`/`InlineChild`/`WorktreeChild`
  makes sibling/cross-tree messaging *unrepresentable*; the constraint is in the type,
  not a runtime check.
- **`Persona` (anti-spoof), per-op spawn (illegal triples unnameable), `WorldEvent`
  single-source, validated newtypes** (`PaneId %N`, `NodePath` non-empty, …).
- **Errors-as-data** — `ScryError` is the bar: every failure a distinct inspectable
  variant, `#[from]` source-chaining, no stringly soup. The new caps' `Result<_>`
  error types are TBD — **specify them to the `ScryError` standard** (per-cap or a
  shared `CapError`), not `anyhow`.

**Principled *non*-use of the type system (correct call):**
- **Phases/typestate dropped.** rust.md says "express state transitions in types," but
  agent lifecycle is driven by *external* events (Copilot review, CI) not knowable at
  compile time. Typestate would fight reality; "the state lives in the world, observe
  it" is the right model. A deliberate, defensible *omission*, not a gap.

**Conscious boundary tradeoffs (name them, don't fix):**
- **Event payload is text-on-bus, parsed to `WorldEvent` at the handler** — chosen for
  the plain-text body / CC-last-hop. The parse is the boundary (parse-don't-validate
  *at* the edge), but an unparseable `kind=event` body is representable — handle it.
- **`HookDecision::Modify(serde_json::Value)` + MCP arg-erasure + `dyn Tool<R>`** — the
  inherently-dynamic MCP/JSON edge and Rust's lack of existentials over varying bounds.
  Erase *arguments*, never *caps*; the `TypedTool` wrapper confines it. Correct.

**`role` + `agent_type` correlation — RESOLVED: collapsed to `NodeKind`.** They were
two independent fields though only ~4 pairs are legal (Root→Claude, Tl→Claude,
Dev→Gemini, Worker→Gemini). The per-op-spawn fix had already closed the *construction*
hazard (the spawn op is the only writer and fixes legal pairs), so this collapse is
*representational* — self-documenting, `(Root, Gemini)`/`(Worker, Claude)` unnameable.
`enum NodeKind { Root, Tl, Dev, Worker }` is now the single stored identity enum;
`role` (the `role_def` key) is the variant, `agent_type` derives via `NodeKind::
agent_type()`. **`AgentType` survives only as a delivery-routing concern** (the
last-hop's Claude/Gemini/Shoal switch), fed by `node_kind.agent_type()` for tree
nodes. The Shoal worry dissolved on inspection: **Shoal is a companion/external-rmcp
participant, not a per-op spawn archetype**, so it never needed a free `agent_type`
on a tree node — it lives in the delivery `AgentType` only. ([01](01-identity.md)/[03](03-capabilities.md)/[04](04-policy.md))

## Full review vs the goal (expressive sidecar · clean tool modules · lightweight role specs · one binary)

Reviewed end-to-end against the stated goal. **Sidecar (facet 1)** and **one binary
(facet 4)** are strong and settled. The thin part was the **tool/role layer** (facets
2–3, the goal's heart) — now pinned:

- **Tool shape — DECIDED: a type per tool + a hand-written `Tool<R>` adapter, no
  macros** ([04](04-policy.md)). The free-fn sketch hid an object-safety wall:
  `dyn Tool<R>` needs a non-generic method while `run` is generic over its caps, and
  Rust has no blanket impl across that — so each tool carries a ~6-line mechanical
  adapter. Chosen over a `tool!` macro (would keep the no-macros rule) and over
  free-fn+closures (would push schema/closure noise *into* the role table, hurting the
  "lightweight role specs" goal). A role is a clean list of tool types + 3 hook fns.
- **Schema derives from `Args`** (`#[derive(JsonSchema)]`) — single source; the author
  writes only `Args` + `run`.
- **Module layout pinned** ([05](05-crates-and-binary.md)) — one cap-trait/file, one
  tool/file, `hooks.rs`/`events.rs`/`roles.rs`; `Runtime`'s cap impls one-per-file.
- **Caps seam ⇒ tools are unit-testable with mock caps, zero IO** — named as a
  first-class payoff (every tool ships mock-cap tests); the WASM guest couldn't.
- **Cap error types: hold to the `ScryError` bar** (distinct inspectable variants,
  `#[from]` chaining), not `anyhow`. Still TBD in signatures.
- **Idle/presence reaffirmed as a Wave-2 prerequisite** (a parent can't detect wave
  convergence without it) — not a post-hoc nicety.

## Behavioral review (metacog round 2 — crash-consistency / async / AOF)

After the type trio (jonhoo/dtolnay/niko) hardened the *types*, three authorities who've
*built* this class of system reviewed *runtime behavior* (aphyr / Alice Ryhl / antirez).
Two design bugs, three impl hazards, one simplification — all folded:

- **Cursor must be temp+rename, not "tiny ⇒ atomic"** (aphyr). A small in-place
  overwrite is not crash-atomic → a garbage offset → silent skip/data-loss. The one
  real loss window; fixed in [02](02-bus-and-sidecar.md).
- **An inbox line is invariantly ≤ PIPE_BUF** (aphyr). `O_APPEND` atomicity only holds
  to PIPE_BUF, but `MessageBody` allows 1 MiB → medium bodies corrupt the log under
  concurrent append. Spill threshold tied to the *line* size (PIPE_BUF), not the body
  cap; once it holds, torn lines can't occur. [02](02-bus-and-sidecar.md).
- **`MessageId` dropped** (aphyr + antirez converged). Ordering = append order (free);
  dedup is the *sink's* job (a sidecar dedup record has the cursor's crash window), and
  the id was even dropped at the CC hop — vestigial. Removed from the contract +
  `Clock::new_id`. At-least-once duplicates are benign (agent re-reads one line).
- **Cap impls must not block the executor** (Ryhl) — the adapted `std::process`
  services must become `tokio::process`/`spawn_blocking`. The plan's biggest async
  footgun; written into every Runtime-TL leaf ([06](06-migration.md)).
- **inotify via the async reactor**, not blocking reads (Ryhl) — Bus-TL B3.
- **Self-poll needs a tracked `AbortHandle`** (Ryhl) — abort on PR-close, dedup on
  re-`file_pr`; a bare `tokio::spawn` leaks. Node-TL N3.
- **Ghost-spawn reap = `Spawned ∧ ¬Started ∧ ¬pane-alive`** (aphyr) — check pane
  liveness, don't reap a slow boot. [04](04-policy.md).
- **Durability stated: no fsync, no fsync-policy knob** (aphyr + antirez) — survives
  crash/`kill -9` (bytes in page cache), not power-loss (which kills the swarm anyway).

Validated as correct (recorded so we don't relitigate): deferring compaction (antirez —
the most bug-prone part of Redis AOF; run-id namespacing also bounds per-run growth);
JSON over binary for `tail`-ability; single-task cursor needs no lock (Ryhl); at-least-once
makes the `select!` loop cancellation-tolerant; tail recovery mirrors AOF `aof-load-truncated`.

## Review-process note

Adversarial reviewers must get a **viewpoint/category only** ("systems-level review
from a security / types / rust-idioms lens — read the code, report findings"), never
pre-baked conclusions/concerns — leading prompts produce confirmation bias + scope
creep. Independent discovery is the whole value.

## Conflation/complection pass (decisions)

- **`dyn Caps` cut** — policy is generic over the caps it needs (per-tool bounds,
  compiler-enforced least-privilege); no god-trait. ([03](03-capabilities.md))
- **"Event" disambiguated** — `MessageKind::Event` = world events (messages); the
  lifecycle log holds **records** (`AgentSpawned`/`AgentStarted`).
- **exo-scry resolvers + `ActiveTeamSignal`: KEEP all** (possible future portability),
  but **mark** the ones the swarm model doesn't use (transcript / session-uuid / the
  trait) as unused-with-rationale in the code — don't delete.
- **Inline-vs-worktree child disambiguated** — not one monolithic "child." A
  `ChildKind { Inline, Worktree }` rides the birth record and the `InlineChild` /
  `WorktreeChild` address variants: **shared** delivery (name → pane → inbox),
  **distinct** spawn, papers location ([01](01-identity.md)), and reap. Messaging
  stays uniform (both have a pane → pane-keyed inbox); the split lives only where
  behavior actually diverges. (Not called "isolation" — it's a *kind*.)
- **Pane-as-inbox-key complection dissolved** — the worry was a fresh swarm reusing a
  pane `%N` and colliding with a dead swarm's leftover `pane-N.jsonl` across a tmux
  **server** restart. Fix: **namespace inboxes (and inline-worker papers) by
  `swarm-run-id`** (`~/.claude/exo/inboxes/{run-id}/…`). Pane-keying is rock-solid
  *within* a run (ids are monotonic for a server's life); run-ids isolate *across*
  runs. Pane stays the key; run-id removes the only cross-restart hazard. ([02](02-bus-and-sidecar.md))
