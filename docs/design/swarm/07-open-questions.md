# Open Questions & Decision Ledger

> **Status: living.** What's still open (needs a test, a decision, or build work), plus a
> one-line ledger of settled decisions pointing to where each lives. Spec detail belongs
> in 01–06; this file is the index of what's *not yet* pinned and a guard against
> relitigating what is.

## Open — needs a test (empirical)

- **Hand-written member registration** (residual of the resolved Wave-0 spike — minor,
  Wave-2 wiring-time, not blocking). The spike's architectural question is **settled** (see
  ledger: solo-team-per-session). `exo-scry`'s live-validated findings already answer the
  core: a CC session **leads exactly one team — a second `TeamCreate` errors** — and members
  are keyed by `tmuxPaneId` in the team's `members[]`. The one detail to confirm when the CC
  last-hop (N2a) is wired: whether writing a `members[]` entry *by hand* (rather than via the
  Agent-tool spawn) is enough for InboxPoller to deliver to that teammate, or whether the
  spawn flow registers something more. Robust either way — it only picks the registration call.
- **tmux-paste into a *CC* conversation** — the gemini-paste path is proven; pasting into
  an un-teamed *Claude* pane (the no-team fallback) is untested.
- **`send_message` mid-tool-call** — round-trips when the recipient is idle; unverified
  whether CC queues or drops a `<teammate-message>` that arrives mid-tool-call.
- **Pane-id reuse** — `%N` ids are monotonic per tmux server; confirm no reuse within a
  server's life (a server restart kills all agents, so stale inboxes refer to dead nodes
  — acceptable, but verify).

## Open — needs a decision

- **Crate names** — `exo-caps` / `exo-policy` / `exo-runtime` provisional.
- **Readability index** (optional) — a `{name} → pane` map/symlink so a human can tell
  which `pane-NNN.jsonl` is whom.

## Cap-completion wave (gaps the real code surfaced — do BEFORE Node)

The Wave-1/Wave-3 code is merged, but a post-merge review found three `exo-caps`
*contract* gaps the original plan assumed away. Because they change the frozen contract,
they land as a small dedicated wave **before** the Node TL forks against it — not during
Node assembly (which would re-introduce the churn the freeze prevents).

- **Spawner spec fields never ported.** `WorkerSpec`/`GeminiSpec`/`ForkSpec` shipped as
  `{ name, task }` placeholders ([spawner.rs]); the structured TL-praxis fields
  (`steps`/`verify`/`done_criteria`/`context`/`boundary`/`read_first`) the spawn tools need
  to carry a real spec were never added. Port them field-for-field from the Haskell
  `SpawnSpec`. (Contract change → pre-Node.)
- **`GitHub` cap too thin to feed the self-poll.** `on_world_event` (consumer) is done, but
  *nothing produces a `WorldEvent`*, and the N3 self-poll can't: the cap has only
  `has_unaddressed_changes`. Add review-state, CI-status, and review-comment-timestamp reads
  (adapt exomonad-core `GitHubService`/`github_poller`) so the poll can construct
  `PrReview`/`CiStatus`/`ReviewTimeout`. (Contract change → pre-Node.)
- **`pre_tool_use` shipped inverted.** It must be **default-allow** — a tool-specific
  *antipattern nudge* ("this fits a common antipattern, try X"), NOT a security/allowlist
  gate. The merged version is a deny-by-default KV allowlist (the literal inverse) and the
  [04](04-policy.md) description ("guards + PII-rewrite") is also wrong. Rewrite to
  default-allow heuristics; fix the doc. (Policy-only, but bundle it here.)
- **`merge_pr` lacks `force`.** Both its own message and the `ReviewTimeout` event tell
  agents to "use `force: true`," but `MergePrArgs` has no such field. Add it.

## Open — build work (remaining)

- **`exomonad experimental init` — the node-mode entry point (NOT YET BUILT).** The only
  thing standing between "merged" and "a human can run it." Today the experimental surface
  is child-side only (`experimental node` / `experimental hook`, which a parent writes into
  a child's `.mcp.json`/settings). There is no command to bootstrap a node-mode *root* from
  scratch: write the root's papers (`node.json`), an `.mcp.json` pointing CC at
  `experimental node --papers`, the experimental hooks in settings, and launch the root
  pane. Lighter than production `init` (no central server to stand up). Reuses the spawner's
  config-writing logic for the root. Needs a short design pass (root papers schema, no-server
  tmux setup) then a leaf.
- **Per-role toolset content** ([04](04-policy.md)) — remaining Bucket-C tools/hooks/events,
  ported one at a time as each Haskell twin retires (incremental, demand-driven).

## Open — optional liveness feature (NOT a convergence blocker)

- **Idle / presence notifications.** Native teammates emit `idle_notification` on
  turn-end; we don't. This is a **liveness nicety, not a convergence requirement** —
  convergence is event-driven: the N3 self-poll produces `CiStatus`/`PrReview`/`ReviewTimeout`
  `WorldEvent`s that wake the parent (`[PR READY]`/`[FIXES PUSHED]`/`[REVIEW TIMEOUT]`), and
  children signal completion explicitly via `notify_parent`. A TL waiting on a wave wakes on
  those GitHub events, not on idle pings — so the fold does **not** deadlock without this.
  Where it would help: a human watching liveness, or detecting a child that went quiet
  without filing a PR or notifying. Undesigned; if built, the trigger is the Stop hook
  (turn-end), emitting presence to the parent inbox. Needs a design pass before any leaf.

## Done (merged to main — Wave 1 + Wave 3)

- **Cap-completion wave (#912–#914, #921).** All four gaps below closed: spawner spec
  fields ported; `GitHub` review-state/CI-status reads added (feed the self-poll);
  `pre_tool_use` corrected to default-allow nudges; `merge_pr` `force`. The "do BEFORE Node"
  section above is historical.
- **Wave 2 — `exo-node` sidecar (#920).** Three concurrent loops (outbound MCP / inbound
  watch / self-poll) as tokio tasks with outbound-closure shutdown; `exomonad experimental
  node` + `experimental hook` wiring; `session_start` identity bootstrap (N4 wrapper injects
  role/branch/parent — real, not a no-op); all four `WorldEvent` variants have live producers.
- **Experimental namespace + invocation single-source (#921, e238ceeb).** Child wiring goes
  through `exomonad experimental node`/`hook`; the invocation strings live once in
  `exo_caps::invocation`.
- **Wave 1 hardening (#922, #923).** `merge_pr` now selects merge strategy + does best-effort
  post-merge `git fetch` (agent teardown stays parent-side by design); `exo-node` gained
  bootstrap/hook(Stop+Deny)/inbound-cursor-restart/dispatch coverage + honest module docs.
  **Still open:** `poll_once` edge-trigger test (needs a pure-fn seam) and the
  `exo-runtime` bus/spawner deterministic integration tests.


- **`exo-caps`** — full contract: validated newtypes, `Message`/`IngestionEntry` split,
  `NodePapers`, per-cap errors, `fold_children` lifecycle, `Spawner`/`Bus` seam.
- **`exo-runtime`** — `Runtime` impls all 9 caps. `Bus` = jsonl append (PIPE_BUF assert,
  no-spill, flush-not-fsync). `Spawner` = record-first / two-phase-pane birth + teardown.
  Hardening landed: `own_pane`-derived `parent_inbox` (no silent `None`), `resolve_child_name`
  (derive-unique / error-on-duplicate), single `exo_caps::paths` site.
- **`exo-policy`** — `Tool<R>` (object-safe over concrete `R`, hand adapter, no macro),
  `role_def(NodeKind)` table, all 7 tool groups, hooks, `on_world_event`. 0 `dyn Caps`,
  0 macros, 0 phases. 56 tests across the three crates; fmt/clippy clean.
- **Provisional caps resolved — none cut.** `PolicyCaps` (the dispatch-boundary bound-union)
  requires all 9, and the runtime uses `Tmux`/`Fs`/`Process`/`Log` internally (Bus paste +
  side-files, Spawner panes). The "cut if no consumer" question is closed: all have one.

## Settled (ledger — don't relitigate)

Each points to where the detail lives. Recorded so these don't get reopened.

**Architecture & identity**
- No central server, no singletons, no registry that drifts — per-node local action +
  filesystem primitives. ([README](README.md))
- Pane is the universal key (team-free, collision-proof for co-located agents); identity
  is assigned-at-birth immutable papers; `exo-scry` for root-bootstrap + CC-membership.
  ([01](01-identity.md))
- Inboxes + inline-worker papers namespaced by `swarm-run-id` (isolates across tmux-server
  restarts; pane-keying is stable within a run). ([01](01-identity.md)/[02](02-bus-and-sidecar.md))
- Messaging is tree-edges-only (parent ↔ own children); routing is O(1) (parent pointer +
  child ledger), no global scan, no sibling/cross-tree channel. ([02](02-bus-and-sidecar.md))
- **CC last-hop = solo-team-per-session** (each node leads its own 1-member team as a pure
  push channel). Resolved at Wave 0 from `exo-scry`'s live-validated **one-team-per-leader**
  finding — a CC session leads exactly one team (a second `TeamCreate` errors) and holds the
  `tasks/{team}` inotify watch only for the team it *leads*; a joined member watches none.
  No fresh CC-internals spike needed. ([01](01-identity.md))

**The bus**
- A jsonl file: append + read-from-cursor + `notify`-watch. No queue abstraction, no
  `exo-mailbox` crate. ([02](02-bus-and-sidecar.md))
- Line invariantly ≤ PIPE_BUF (no spill); cursor is a byte-offset advanced via temp+rename;
  at-least-once, dedup is the sink's job; no fsync (survives crash, not power-loss; no
  message-id — ordering is append order). ([02](02-bus-and-sidecar.md))
- Bulk content = a sender-written file referenced by path, never inlined. `MessageBody`
  ≤ 4 KiB, `Summary` ≤ 256 B. ([02](02-bus-and-sidecar.md)/[03](03-capabilities.md))

**Types & caps**
- Generic-over-caps, no `dyn Caps` god-trait; demand-driven cap set. ([03](03-capabilities.md))
- `NodeKind { Root, Tl, Dev, Worker }` is the one identity enum; `agent_type` derives;
  `AgentType` is delivery-routing only. ([03](03-capabilities.md))
- Per-op spawn fixes `(role, agent_type, kind)` → illegal combos unnameable. ([03](03-capabilities.md))
- `Message` (policy) vs `IngestionEntry` (wire): runtime stamps `from`/`ts`/`v` →
  spoofing structurally impossible. Validated newtypes (serde via `try_from`). Per-cap
  error enums `#[from]` `CapError` (source chain preserved). ([03](03-capabilities.md))
- Teardown is two steps: process (pane-kill, graceful self-kill or forceful) + worktree
  reclamation (parent-side at convergence). ([03](03-capabilities.md)/[04](04-policy.md))
- Time is not a capability — `Utc::now()` in the runtime. No `MessageId`. No `Clock` cap.

**Policy**
- No phases / state machine — the stop-gate is a live GitHub query. ([04](04-policy.md))
- Tool = a type (`Args` + generic `run` + hand-written `Tool<R>` adapter, no macros); a
  role is a list of tool types + 3 hook fns. ([04](04-policy.md))
- Child tracking = parent-local append-only `children.jsonl` (`AgentSpawned`/`AgentStarted`
  records), folded to state; status computed live; reap predicate `Spawned ∧ ¬Started ∧
  ¬pane-alive`. ([04](04-policy.md))
- Polling decentralized: per-sidecar self-poll of own PR (3 min, only while a PR is open,
  tracked `AbortHandle`); parent owns sibling-merge. ([04](04-policy.md))

**Build**
- Reuse over rewrite (tmux injection, CC delivery, exomonad-core git/gh/tmux services);
  the cap impls must not block the executor (`tokio::process`/`spawn_blocking`). ([06](06-migration.md))
- Honest port scope: ~4.5k LOC of dense domain logic. ([06](06-migration.md))

## Dismissed (threat model / scope)

- **Security** (file injection, unauth shutdown, persona spoofing) — non-issue: same trust
  model as CC's own file-based inboxes, single-user dev box. No crypto/auth.
- **Force-kill / dashboard / new UI mode** — out of scope; human-driven (the human kills
  panes).
- **Pane identity across reboot** — accepted limitation (reboot kills agents anyway).
- **SQLite vs jsonl** — jsonl chosen for inspectability/`tail`-ability; the bus is a `>>`,
  not a database.
- **Plan-approval, task-workflow parity** — out of scope (messaging + lifecycle only).

## Principles (process)

- **Prefer well-understood primitives** over bespoke machinery — stdlib + a mature crate
  (`notify`, `serde_json`, `octocrab`) before hand-rolling. When a piece starts to look
  like a subsystem, check whether it's one line of stdlib or one dep first. Don't fear
  complexity; do avoid reinventing it.
- **Demand-driven, not coverage-driven** — add a type/cap/field when a consumer needs it,
  not because it might be useful. (This guards against the unused-type class.)
- **Adversarial review = viewpoint only** — give a reviewer a category/lens, never a
  pre-baked conclusion; verify its findings against the actual code, not at face value.
