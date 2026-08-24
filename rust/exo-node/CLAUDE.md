# exo-node — the per-node sidecar

Assembles `exo-runtime` (all caps) + a domain `D: Exomonad` (the domain's tools/hooks/roles/system, monomorphized once by the binary as `run_node::<exo::ExoDomain>`) into a running **sidecar, one process per agent**. This is the binary's `exo node` and `exo hook` modes — there is **no central server**: each agent has its own sidecar, the filesystem is the bus, the process tree is the topology.

**Generic over the domain (`Exomonad`):** `exo-node`'s `NodeContext<D>`, `run_node<D>`, `bootstrap<D>`, and every loop are generic over `D: Exomonad`. It resolves a role's tools/hooks through `D::role_def` (static dispatch — the fn-pointer `RoleRegistry` is **deleted**) and reacts to a domain inter-node payload through `D::handle_system` (the inbound Domain arm deserializes `D::System`, runs the domain handler via a `SystemCtx`, then performs the `SystemOutcome` — e.g. `ReclaimSender` tears the sender down). So it depends only on [`exo-framework`](../exo-framework/CLAUDE.md) for the trait seam and **never on the [`exo`](../exo/CLAUDE.md) domain crate** (`cargo tree -p exo-node` shows neither). The binary is the composition root that names both. The engine bounds only `D: Exomonad<Caps = Runtime>` (the sidecar builds the concrete `Runtime`); the role is fully domain-defined (`D::Role`), read off papers (recorded erased as a `RoleRecord`) and typed back at bootstrap — its one typed reader.

> Part of the v2 node-mode swarm. See `rust/CLAUDE.md`. Classic exomonad (`serve`/`mcp-stdio`) is untouched and lives in `exomonad-core`.

## The loops

`run_node(ctx)` runs several concurrent loops; `handle_hook` is a separate short-lived mode.

| Module | Loop | Role |
|--------|------|------|
| `outbound` (N1) | serve | Serve `role_def(kind).tools` as MCP over stdio (`tools/list` emits each tool's `name`/`description`/`inputSchema`, so the toolset is self-documenting). **Owns stdin/stdout → the node's lifetime anchor** (when it closes, the node ends). |
| `inbound` (N2b) | watch | Watch the node's own ingestion inbox (byte-offset cursor + `notify` watch), route each new entry. Missing/malformed cursor ⇒ replay from 0 (boot-window messages are backlog, not history). |
| `hooksock` (N5) | serve | Per-agent UDS hook-RPC channel — runs the role hook fn on the live runtime and shapes the verdict as the Claude hook-output JSON (`{"continue":true}` / `{"decision":"block",...}`). Only `pre_tool_use` and `session_start` route here now (`Stop` was removed — see Gaps history / `rust/exo/CLAUDE.md`). |
| `watchdog` | watch | Periodic wall-clock self-check (fixed interval, tracks elapsed time since this loop started ≈ node boot). Each tick, in order: `D::handle_tick(caps, role, elapsed)` (a domain's abandonment-timeout logic — e.g. the reviewer's `handle_review_tick`), then the **child death scan** (`Runtime::detect_child_deaths` → a `[CHILD DIED: name]` note self-appended to this node's own inbox; see below), then an unconditional `try_reap` re-check. Replaces the old `Stop`-triggered reap check: `Stop` fires on every turn-end including a legitimate async-wait yield, so it couldn't tell "done" from "paused"; a wall-clock interval can. |
| `listen` (N6) | serve | The **wake channel** socket (`paths::listen_sock`, streaming newline-JSON — a second UDS beside the hooksock). Accepts the agent's `exo listen` Monitor client **latest-wins** into `NodeContext.listener` (`ListenerSlot`), pings `inbox_wake` on attach so queued backlog drains at once. |
| `dispatch` (N2a) | — | The **last hop**: render one entry with a `[from: X, kind: Y, id: Z]` header (`, re: W` appended when the message carries a `reply_to`) and deliver it over the listen channel (`ListenerSlot::try_deliver` — write frame, await the client's flushed-to-stdout ack). Full render inline ≤2048 B and ≤12 lines; larger spills to `.exo/tmp/inbox-{pid}-{n}.md` behind a one-line `@`-ref. **No listener / failed ack ⇒ `Err`** (`NodeError::NoListener` logged quietly) — the cursor pins and the entry queues until a client attaches. There is no tmux-paste delivery. |
| `hook` (N4) | one-shot | `exo hook <event>` → bootstrap from papers → run the role's `pre_tool_use`/`session_start` → print the verdict. No server. On SessionStart it appends the node identity **and the WAKE CHANNEL arming instruction** (`listen_instruction` — the exact `Monitor { command: "exo listen --papers <abs>" }` call, papers path shell-escaped) to the `additionalContext`, for every role including root, re-fired on every resume/clear; the role protocol is delivered via the launch-time `--append-system-prompt` instead. |
| `bootstrap` | — | Self-ID: read `--papers` → `NodePapers`, enrich with ambient env (`$TMUX_PANE`, `EXOMONAD_SWARM_RUN_ID`, `EXOMONAD_TMUX_SESSION`, `$HOME`), build `NodeContext<D> { runtime, kind, own_inbox, parent_inbox, ... }`. Arms `PR_SET_PDEATHSIG` (Linux) as its first act so the sidecar self-terminates if its parent `claude` process dies — stdin-EOF alone is not a reliable lifetime anchor. |
| `error` | — | `NodeError`. |

`run_node` spawns `inbound`, `hooksock`, `listen`, `watchdog`, and a periodic status publisher (which stamps `NodeStatus.listener_connected` off the live `ListenerSlot`) as background tasks and awaits `outbound::serve`; when serve returns (agent gone) it aborts all of them. A background loop erroring is logged, not fatal.

## Persistent Logging

The sidecar initializes a persistent file subscriber at startup (in the binary composition root). Logs are written to the **project root** (the main repo's `.exo/` dir, NOT the worktree's) so they survive worktree teardown:

- **Path:** `<project-root>/.exo/logs/sidecar/<run_id>/<branch>.log`
- **Configuration:** Respects `RUST_LOG` (default: `info`).
- **Mechanism:** Uses `tracing-subscriber` with a non-blocking `tracing-appender`.
- **Instrumentation:** The inbound loop (`Domain`/`Lifecycle` arms) and `handle_system` outcomes are span-instrumented. Spans carry **log-friendly labels, never payloads** — `kind` is the one-word discriminant (`kind_label`), `from` is the bare name (`persona_label`); recording a `MessageKind` via Debug would splat the whole `Domain` findings JSON onto every nested line. Inner spans (`handle_lifecycle`/`handle_domain`) don't re-record `node` (the per-node log filename already names it). Success is logged **once at the authoritative layer** — `Bus::deliver` for sends, `dispatch` for last-hop — so the `deliver_parent`/`deliver_to_self` wrappers log only on failure, not a redundant start+OK pair.

## The inbound → dispatch path

`inbound::watch` resumes from a `pane-N.cursor` byte-offset (missing/malformed cursor → **replay from 0**: a fresh node's inbox holds exactly the boot-window messages the parent sent before the sidecar existed, and full replay after cursor loss is benign duplication under at-least-once), reads only up to the last `\n` (torn lines re-read once complete), advances the cursor **after** successful delivery (at-least-once; a duplicate line is benign), **resolves any claim-check `spill` pointer** to the full entry (`resolve_spilled` loads the bus's `.spill/` side-file — how an oversized payload like a rich verdict arrives), and routes by `kind`. The wake `Notify` is hoisted onto `NodeContext.inbox_wake` — fed by the filesystem watcher, by the listen server on client-attach, and by `deliver_synthetic`'s own-inbox append. A `NodeError::NoListener` from dispatch is logged at debug, not error — an unarmed agent queuing messages is the expected boot-window state, not a routing fault. The watch loop wakes on a coalesced `tokio::sync::Notify` fed by the `notify`-crate filesystem watcher, **or** a 15s periodic tick — the tick is a backstop: a routing failure leaves the cursor unadvanced by design, and without it that entry would only ever retry on a *later* filesystem write, which may never come.

- **`Chat` / `Event`** → `dispatch::dispatch` (last-hop deliver, rendered with a `[from: X, kind: Y]` header).
- **`Control(Shutdown{grace_ms, force})`** → the cooperative/forced matrix (`decide`):
  - *cooperative (`force=false`)* — **leaf**: deliver a "wrap up now" note to the agent, mark `shutdown_pending`, and reap once the watchdog loop's next tick finds the subtree clear (`try_reap`, re-checked unconditionally every tick — no longer triggered by the `Stop` hook, which was removed); **has live children**: bounce a `[shutdown deferred] you have N live children — re-send force:true` message back to the requester (the parent) and do nothing else (the "are you sure").
  - *forced (`force=true`)* — **leaf**: reap now (grace backstop); **has live children**: the sidecar cascades `Shutdown{force:true}` to every live child and reaps itself once they've all exited. Control-plane teardown — note it hard-kills the subtree (uncommitted work in a busy descendant is lost); revisit if that bites in dogfooding.
  - The actual self-reap (`try_reap`) only fires when `shutdown_pending` AND the subtree is clear — **pane-liveness (`Topology`) is the sole authority** for "children gone"; there is no separate exited-set. Before killing its own pane it sends an **advisory** `Lifecycle::Exiting` poke up (receipt does not prove the pane is dead); the parent's `handle_lifecycle(Exiting)` re-runs *its* `try_reap` immediately and once more after a 5s detached delay, so a forced teardown drains bottom-up without racing the child's own pane-kill. The watchdog tick is the unconditional backstop either way.
- **`Lifecycle(Lifecycle)`** → `handle_lifecycle` (engine-owned, sidecar-consumed; it takes the whole entry, not just its `from`, because an arm that both records a fact *and* re-shows the message needs the original envelope). `Exiting` / `ShutdownResponse` drive the reap / shutdown-render paths; a child is **never** torn down just for finishing a turn. **`Submitted`** (a child reporting `branch@sha` awaiting this node's merge) appends a `ChildRecord::Submitted` to this node's own ledger **and then still re-dispatches the child's `[READY]` prose as chat** — recording never replaces showing: the ledger is for the machine (it outlives a context window), the delivered prose is what actually makes the agent act. Guards: the sender must be a `Persona::Agent` **and** one of this node's own direct children (checked against `Topology`'s children, deliberately *not* `resolve_edge`, which now returns `None` for a tombstone — a tombstoned child's submission is still a real fact worth recording); anything else is warned about and rendered only, never granted an invented ledger row. An append failure **returns `Err`** so the cursor stays unadvanced and the entry is retried — a retried `Submitted` folds to the same `ChildState`, so the duplicate is harmless while a lost submission is not. (There used to be a third variant, `ChildIdle`, sent whenever a child's `Stop` hook fired — it flipped a busy-bit `ChildLiveness` read. Removed along with `Stop` itself: `Stop` fires on every turn-end including a legitimate async-wait yield, so the bit was routinely wrong. `ChildLiveness` now reads pane-existence directly instead.)
- **`Domain(DomainPayload)`** → `handle_domain` (domain-opaque, sidecar-consumed, never shown to the LLM unless the domain decides to act). The **one place** the erased wire payload is deserialized to the concrete `D::System` and handed to `D::handle_system` — for the `exo` domain that's `ReviewSystem` (decision derived from structured findings; the round is **best-effort RMW-appended to `.exo/reviews/{safe-branch}.json`** via the generic `read_file`/`write_file` `SystemCtx` methods). The engine then acts on the returned `SystemOutcome`: `ReclaimSender` tears the sender down (`kill_pane` + `reclaim_worktree` — how a one-shot reviewer dies; teardown is **verdict-only**). An undeserializable payload is logged + skipped (tolerant, like a malformed bus line).

## Child death is announced, not just detected

A child's pane can die without anyone deciding it should — a crash, an OOM, a closed window, a
provider cutting it off. Nothing in the tree reports that on its own, so a parent would idle forever
waiting for a `[READY]` that will never arrive, while the dead child's branch may hold real committed
work and its worktree uncommitted work.

The watchdog closes that: every tick it calls `Runtime::detect_child_deaths()`, which appends a
`ChildRecord::Died` per newly-dead un-reaped child (see `exo-runtime/CLAUDE.md`) and hands them back,
and the sidecar delivers a `[CHILD DIED: {name}]` synthetic note to its own agent (via
`deliver_synthetic`'s own-inbox append → the listen wake channel) naming the dead pane and telling
the TL to run `tree`, then merge what the branch holds or respawn the work.

The order is **record-then-enqueue**: the `Died` record is durable *before* the note is appended, so
an append failure is a `warn!` and the tick continues — the fact is not lost, and the announcement is
not retried into a duplicate. Once appended, the note itself has cursor-backed at-least-once delivery
(it queues if the agent hasn't armed its monitor). Re-announcement is prevented structurally, not by
a bookkeeping set: a child recorded `Died` folds to a terminal state and is excluded from every later
scan.

## Message ids: reference-only, never dedup

Every bus-delivered entry carries a UUID `id` (stamped by `Bus::deliver`; the two entries this crate
*authors* — `deliver_synthetic` and `render_shutdown_response` — mint their own, while the `Submitted`
re-dispatch **preserves** the incoming id, since it is the same message shown a second way). The
header renders it so an agent and a log can name a specific message, and `Message::reply_to` points at
one.

**Nothing anywhere deduplicates on an id, and nothing may start.** The inbound cursor advances only
*after* a successful last-hop delivery — that is what makes delivery at-least-once — so a redelivered
line arrives with its **original** id. An "already seen this id" check would silently swallow exactly
the retry the protocol exists to guarantee. The comment at the cursor-advance site says so.

## Delivery: the listen wake channel (exo owns its channel, harness-native)

`dispatch` has a single last hop for every node kind: hand the rendered `[from: X, kind: Y]`
entry to the agent's attached **`exo listen` client** over the node's listen socket
(`~/.claude/exo/sockets/{run_id}/pane-{n}.listen.sock`). The agent arms that client under Claude
Code's `Monitor` tool as its **first action** (the SessionStart hook injects the exact command,
papers path interpolated — see `hook.rs::listen_instruction`); each message the client prints to
stdout becomes a harness notification that wakes the agent between turns. The client acks each
frame **after** flushing it to stdout, and only an acked frame advances the inbound cursor — so
`Ok` from dispatch means "reached the harness's notification stream".

**No listener ⇒ messages queue.** The cursor pins, the bus holds the backlog durably, and the
listen server pings `inbox_wake` when a client attaches so it drains immediately. This is the
whole boot-window story: a message sent between spawn and Monitor-arm (or after a monitor died
and before a re-arm) is delivered late, never dropped. Senders see the state in their tool
responses (`Bus::wake_status` → a ⚠ note from `notify_parent`/`send_message`/`submit_branch`),
in `tree` (`wake:listen` / `wake:-`), and in the status snapshot (`listener_connected`).

Protocol (`listen/mod.rs`): newline-delimited JSON both ways — `ListenFrame { seq, text }` down,
`ListenAck { seq }` up. Ack correlation is by connection-local `seq`, never message id (ids are
reference-only; a redelivered entry keeps its original id). Connections are **latest-wins**
(`ListenerSlot`, generation-guarded): re-arming after a `/clear`/resume/auto-stop always
succeeds, and the replaced client sees EOF and exits — its Monitor watch ends with a visible
message telling the agent to re-arm. The client itself (`listen/client.rs`, `exo listen
--papers <p>`) is deliberately dumb: bounded connect retry (30s budget — it races the sidecar
bind at cold start), print + flush + ack per frame, exit on EOF (sidecar dead means the agent is
dying too via PDEATHSIG; replaced means a successor owns the slot), loud non-zero exit on bad
papers/env or a protocol violation.

**Why not tmux-paste?** It was the previous channel — typing into the agent's TUI with heavy
hardening (Rewind dismissal, SIGWINCH wakes, verified submission by pane-scraping). It was
fragile by nature, hit the human mid-typing at the root pane, and made agent messages
indistinguishable from user input. **Cut entirely as a delivery mechanism**; `Tmux::paste`
survives only for launch-command injection into holding shells (`spawner.rs`) and the root
launch (`init.rs`). **Why not CC Agent Teams?** As of Claude Code 2.1.178 teammates are
in-process and a solo session-lead never drains its teammate inbox (reproduced; CC GH#26426), so
Teams delivery was removed even earlier. `exo-scry` survives only for `fork_session` context
inheritance (`spawner.rs`), not delivery.

Outbound is symmetric and exo-owned: a child reaches its parent via the **`notify_parent` MCP tool**
(→ Bus → the parent's inbound → its listen channel); `send_message` addresses a child. No native CC
team tools are used.

**Synthetic notes ride the same path.** `deliver_synthetic` (shutdown prompts, watchdog death
notes, the domain's `deliver_to_self`) no longer calls dispatch directly — it appends the
pre-stamped entry to the node's **own inbox** (`Runtime::append_entry`, the same append+spill
discipline as `Bus::deliver`, preserving `Persona::Synthetic`) and pings `inbox_wake`. One
cursor-backed delivery path for everything: a `[CHILD DIED]` that lands while the agent is
unarmed queues and replays instead of being warn-and-dropped.

## Inline-child shutdown

Tearing down an inline worker is a **parent-side `Spawner::kill_pane(child_name)`** — done by the
`dismiss_worker` MCP tool (the same `kill_pane` path, no cooperative handshake; the inline worker has
no sidecar inbound loop to receive a `Control(Shutdown)`). Worktree-child shutdown is the cooperative
bus path (`Control(Shutdown)` → bus → the child's inbound loop → `try_reap`), and a folded/reviewed
worktree child is reclaimed by `merge` / verdict-side teardown.

## Gaps / not-yet

- **Shutdown has structured response.** A structured `shutdown_response` is written back to the requester (status accepted/deferred + live_children + busy), and the requester's `handle_system` renders it to chat.
- **Forced teardown is a hard kill.** `force:true` cascades pane-kills through the subtree with no per-node commit/wrap-up — a busy descendant loses uncommitted work. Deliberate (force = "tear it down"); revisit if it bites in dogfooding. **It also never reclaims worktrees — by design**: the cascade only kills panes; the subtree's worktree directories (and branches) are left on disk for post-mortem inspection and are cleaned up later by `exo doctor --fix` (or the next `merge` of an ancestor). Force-kill is an abnormal path — preserving the filesystem state it leaves behind is a feature, not a leak.
- **`outbound` is a hand-written minimal MCP/JSON-RPC stdio server** — `initialize`/`tools/list`/`tools/call`; no capability negotiation beyond that.
- **Convergence teardown is wired, best-effort, now retried.** `merge` (the `exo` domain tool) reclaims the folded child (`kill_pane` + `reclaim_worktree`), and a one-shot reviewer is torn down verdict-side in `handle_system`. The reclaim path is now **bounded-retried** (3 attempts, linear backoff) inside the `Spawner` impls (`exo_runtime::retry_teardown`), so the reviewer teardown and the `merge` tool both inherit it for free; `try_reap`'s own-pane self-kill uses the same helper. A final failure logs a **loud structured error** (`op` + `child` + `attempts`) and the error is surfaced — but teardown stays **best-effort**: it never aborts the merge/teardown flow, and a child whose worktree is dirty or holds a nested worktree (e.g. a still-live reviewer) can still fail to reclaim after retries and linger (self-heals via the liveness reap).
