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
- `Spawner` method breakdown (one method + `ChildKind` vs N) — spawn ops stay separate per `ChildKind`.
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
- **Type system fully used:** `Persona`/`EventType`/`AgentName` enums + validated
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

## Deep-review pass — flagged for the Wave-0/1 signature freeze

A second ultrathink pass (post-conflation) fixed three things in place — the
`id`-vs-byte-offset cursor contradiction ([02](02-bus-and-sidecar.md)/[03](03-capabilities.md)),
the papers self-ID chicken-and-egg (told via launch flag, not guessed —
[01](01-identity.md)), and the `reap` conflation (split into process-teardown vs
parent-side worktree-reclamation — [03](03-capabilities.md)/[04](04-policy.md)). Two
type-shape loose ends remain — **non-blocking** (they sit in already-TBD `exo-caps`
signatures) but **decide before Wave 1 freezes the traits**:

- **`NodeRef` is currently unused**, and its fields (`path, pane, inbox, agent_type`)
  don't match the `AgentSpawned` record (`child, kind, pane, path, inbox`) a parent
  folds to learn its children. Decide: make the folded child-handle *be* `NodeRef`
  (then it needs `kind` for teardown and likely drops `agent_type` — the *sender*
  never needs it; the *recipient* picks its own last-hop), or cut `NodeRef` and let
  the fold yield the record type directly. Don't keep a floating unused type.
- **`SpawnSpec { role, agent_type, kind }` admits illegal combinations** — e.g.
  `(Inline, Tl, Claude)` — the same stringly-typed sin the type-elegance review
  flagged, in a design that prizes illegal-states-unrepresentable. Only ~3 combos are
  real (`spawn_worker`→Inline/Worker/Gemini; `spawn_gemini`→Worktree/Dev/Gemini;
  `fork_wave`→Worktree/Tl/Claude). Reconcile with "spawn ops are separate per
  `ChildKind`": prefer **per-op narrow specs** feeding a shared `birth(core)` helper
  over one unified `SpawnSpec` with independent enums.

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
