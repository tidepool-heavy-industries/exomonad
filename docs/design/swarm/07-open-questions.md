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

- **Ingestion message format** — fields + mapping to CC's Teams-inbox shape and to
  the tmux-paste rendering. ([02](02-bus-and-sidecar.md))
- **Cursor / high-water-mark format** for restart-safe ingestion. ([02](02-bus-and-sidecar.md))
- **Ingestion inbox root path** — `~/.claude/exo/inboxes/…` vs. project `.exo/` vs.
  beside CC's team dir. Pane-keyed regardless. ([02](02-bus-and-sidecar.md))
- **`exo-caps` signatures** — `Bus`, `Addressee`, `Spawner`, domain newtypes.
  ([03](03-capabilities.md))
- **Per-role toolsets, phases, hooks, events** — the Bucket-C content. ([04](04-policy.md))
- **Event/transform policy home** — the sidecar inbound loop runs it; exact shape TBD.
- **Readability index** for pane-keyed inboxes (a `{member} → pane` map/symlink, so a
  human can tell which `pane-NNN.jsonl` is whom). Optional.
- **Crate names** — `exo-caps`/`exo-policy`/`exo-runtime` provisional.

## Mechanical TODO (build-time, not design)

- `Git` / `GitHub` / `Tmux` / `Clock` / `Kv` cap signatures — adapt from
  exomonad-core services.
- `Spawner` method breakdown (one + `Isolation` vs N) — spawn ops stay separate.
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
- **Addressee:** `Parent` + `Member(name)` (covers real/synthetic/children); `Pane`
  is internal resolution only.
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
- **Type system fully used:** `Persona`/`EventType`/`MemberName` enums + validated
  newtypes, `TypedTool` wrapper, no `pub` fields, illegal states unrepresentable.
- **Honest port scope:** ~4.5k LOC of dense domain logic, not "hundreds."
- **Build-plan fixes:** pre-scaffold all deps (Cargo.toml), gate Wave 3 on
  signature-freeze, route `Bus`/`Spawner` leaves to higher-capability + test harnesses.

## Still genuinely open (need a test, not a decision)

- **tmux-paste into a *CC* conversation** — the gemini-paste path is proven & reused,
  but pasting into an un-teamed *Claude* pane (the no-team fallback) is untested.
- **CC multi-team / which inbox InboxPoller watches** (Wave-0 spike).
