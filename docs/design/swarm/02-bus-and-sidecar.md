# The Bus & the Two-Loop Sidecar

> **Status: settled** (message format + cursor details stubbed, see end).

## The bus = per-node ingestion inboxes

Each node has one **generic ingestion inbox**: an append-only file the sidecar
owns. Senders drop a message there knowing nothing about the recipient's runtime.
The recipient's sidecar consumes it and does the runtime-specific last hop into
its own agent.

- **Pane-keyed within a run, team-free.** `pane-317.jsonl` (sanitize `%`). The pane
  is the key because it's unique per agent (even co-located), needs no CC team, and
  is stable *for a tmux server's lifetime*. `AgentName` is display metadata.
- **Namespaced by `swarm-run-id`.** The inbox dir is keyed by `EXOMONAD_SWARM_RUN_ID`
  (`.exo/run_id`), so a fresh swarm gets a clean namespace and a tmux **server**
  restart (which kills every agent) can't make a new pane `%N` collide with a dead
  swarm's leftover `pane-N.jsonl`. Pane ids are stable within a run; run-ids isolate
  across runs.
- **Append-only + inotify.** Appends are atomic; the sidecar watches via inotify
  (event-driven, not polling).
- **Exomonad-namespaced**, not written into CC's managed dirs:
  ```
  ~/.claude/exo/inboxes/{swarm-run-id}/pane-317.jsonl   ← the bus mailbox (run-id-namespaced)
  ~/.claude/teams/{team}/inboxes/{member}.json          ← CC's push channel (sidecar's CC last hop)
  ```

## The sidecar has two loops

```
            ┌─────────────────────────── exomonad (node mode) ───────────────────────────┐
  agent ───▶│ OUTBOUND: serve MCP tools.  send_message(to) → append to TO's ingestion inbox│
            │                                                                              │
  inbox ───▶│ INBOUND: watch own ingestion inbox → route each new msg INTO this agent:     │
 (inotify)  │   • CC + in a team → write its Teams inbox  → InboxPoller → <teammate-message>│
            │   • else (gemini, or CC w/o team) → tmux-paste into its pane                  │
            └──────────────────────────────────────────────────────────────────────────────┘
```

- **Outbound** is what `teams-mcp` already does: the agent calls a tool; the tool
  appends to the *target's* ingestion inbox. `Bus::deliver(target, msg)` = append.
  Runtime-agnostic — policy never mentions Teams or tmux.
- **Inbound** is the new half and answers the old "how does the sidecar get
  triggered" question: the ingestion inbox *is* the inbound path. External events
  (a poller, a peer sidecar) just append. It's also the home for **event/transform
  policy** (the Bucket-C `EventAction` logic: "should this become a message,
  reformatted how").

The sidecar is a child of its agent, so sidecar-liveness ≈ agent-liveness — you
only need delivery when the agent is up, and then the sidecar is too.

## Delivery last hop

| Target | Mechanism | Needs a team? |
|---|---|---|
| CC, in a team | write Teams inbox → InboxPoller → `<teammate-message>` | yes (nice) |
| CC, no team | tmux-paste into its pane | **no** |
| gemini | tmux-paste into its pane | no |

The sidecar picks based on its *own* `agent_type` + CC membership (resolved via
`exo-scry`). For the tmux-paste path the entry is rendered with a `[from: X,
kind: Y]` header — the input box **is** the receive channel for non-CC runtimes, so
the header is all the agent needs to read it as a message (full receive, not a
degraded floor). CC's Teams inbox is demoted to "the CC sidecar's pretty last hop" —
the bus itself is team-free. (Verified: CC's InboxPoller delivers an inbox entry
from an *arbitrary, unregistered* `from` — the `oracle` test — so the sidecar's
last-hop write needs no member registration for the *sender*.)

## Routing

**Messaging is tree-edges-only** — a node messages its **parent** (up) and its own
**children** (down). There is **no out-of-band / cross-tree / sibling** messaging:
the messaging structure *is* the tree structure, so the lead is always on the path
of anything that concerns it (observability is inherent).

- **Up (notify_parent):** append to `parent_inbox` (a path held in your papers;
  whoever spawned you set it — robust to unplanned spawns). O(1).
- **Down (to a child):** append to the child's inbox — its path is in the parent's
  birth ledger (the parent spawned it). O(1).

So routing needs **no global worktree scan** — the parent pointer + child ledger
are the whole table, both O(1), with no scan-latency or partial-papers race (the
architecture review's concern, dissolved by the tree-only constraint).

## Ingestion entry format (settled)

One JSON object per line (append-only). The runtime flattens the
[`Message`](03-capabilities.md) under a stamped envelope (`v` + `ts` + `from`):

```jsonc
{ "v": 1,                    // schema version — parsed tolerantly (serde defaults; no deny_unknown_fields)
  "ts": "2026-05-30T22:…Z",  // stamped on append
  "from": "github",          // agent name or synthetic persona — STAMPED by the runtime (a tool can't set it → no spoofing)
  "kind": "event",           // chat | event | control
  "summary": "PR #5 approved",
  "text": "…" }              // plain body
```

**No message-id.** Ordering is the **append order** (line order in the file) — free,
so no ulid is carried. At-least-once redelivery (see *Cursor & restart*) may show the
agent a duplicate line; that's benign (it re-reads one message). Dedup, if ever wanted,
is the **sink's** job (the agent / CC InboxPoller), never the sidecar's — a sidecar
dedup record would have the same crash window as the cursor.

**CC last-hop mapping:** entry → CC Teams entry `{from, text, summary, timestamp,
read:false}`. `kind` is exomonad-side and dropped (or folded into `text`) at the CC
write. **tmux-paste rendering:** `from` + `summary` + `text` formatted into the pane.

## Cursor & restart (settled)

The node's **own sidecar is the sole reader** of its inbox — senders never touch the
cursor, so there is no concurrency on it; restart is a pure single-process resume.

- **Cursor = byte-offset** into the append-only file (sibling `pane-317.cursor`).
  Valid because the common path **never** rewrites/renames — appends only grow the
  file — so the offset stays correct and resume is **O(1)** (seek + read forward).
- **Advance after a successful last-hop delivery**, written with **temp + rename**
  (atomic replace) — **NOT** an in-place overwrite. "Small" is not "atomic": a crash
  mid-overwrite of the offset can leave a garbage value → seek to the wrong place →
  silently skip or land mid-line. `rename(2)` is atomic, so the cursor is always a
  whole valid offset. A crash between delivery and advance re-delivers the one
  in-flight message on restart: **at-least-once, never dropped, never corrupted.**
- **Read only up to the last `\n`** — a torn trailing line is re-read once complete.
- **Missing cursor** (fresh node, or lost) → start at current EOF; don't replay
  history.
- **No compaction initially** — the file just grows (cheap for text), which keeps
  the cursor a pure offset with no invalidation case. If a long-lived inbox ever
  gets large, compaction is a `flock`-guarded rewrite that also resets the cursor —
  deferred until it's a real problem.

(Fixes the current `inbox_watcher.rs` count-snapshot, which drops messages on
restart.)

## Inbound event/transform policy

`kind=event` entries are passed to `exo-policy`'s event handler
(`on_world_event → InjectMessage | NotifyParent | NoAction`) before delivery;
`kind=control` (e.g. shutdown) is handled by the loop; `kind=chat` passes straight
through. See [policy](04-policy.md).

## Implementation requirements (fully realized — not the current jank)

The current `rust/exo-scry/src/inbox.rs` (read-modify-write of a JSON array under a
manual `O_EXCL` lock) and `exomonad-core/.../inbox_watcher.rs` (500 ms poll + a
message-count snapshot) are **NOT** this design — they are the jank to be
replaced. The bus must be built solid at the systems level:

- **A line is *invariantly* ≤ `PIPE_BUF`.** Atomicity of `O_APPEND` only holds up to
  `PIPE_BUF` (4 KiB on Linux); a larger write **interleaves under concurrent
  appenders and corrupts the jsonl**. So the spill threshold is tied to the **line
  size (PIPE_BUF)**: any entry whose serialized line would exceed PIPE_BUF spills its
  body to a side file (written temp+rename **before** the small pointer line is
  appended) and the line carries a pointer. `MessageBody` is itself capped at **64 KiB**
  (message-sized, not a document — bulk content references a file/PR, it isn't inlined),
  so the spill only ever covers the PIPE_BUF…64 KiB band; the common message is one
  atomic line and never spills. With
  this invariant every line is one atomic `write(2)` — no interleaving, no lock, and
  no torn lines (a single atomic append is all-or-nothing, so "read to last `\n`" only
  ever discards a *missing* final line, never a garbage one). Assumes a **local fs**
  (`O_APPEND`≤PIPE_BUF isn't atomic on NFS; fine for a dev box). **Never**
  read-modify-write the whole file.
- **Durability level (stated, not implied): no `fsync`.** A bus append survives an
  agent crash / `kill -9` (the bytes are in the kernel page cache the moment
  `write(2)` returns — process death doesn't lose them). It does **not** survive
  power-loss / kernel panic — but that kills the whole swarm anyway, so there's
  nothing to recover *to*. So **no fsync, and no fsync *policy* knob** (we don't need
  Redis's `everysec`/`always`/`no` trichotomy — plain OS-flush is correct here).
- **Kernel `flock` only where a non-append op is unavoidable** (compaction), never
  a manual lockfile — `flock` releases on process death, so no stale-lock deadlock
  (the CRITICAL the concurrency review found).
- **Persisted cursor** = byte-offset (see *Cursor & restart*), advanced after a
  successful last hop via **temp+rename** (atomic replace — a "small" overwrite is
  *not* crash-atomic). **Never** a count-snapshot (which drops messages on restart).
- **Schema-versioned** — every jsonl entry + papers file carries a `v` field, parsed
  tolerantly (serde defaults), so a mixed-version swarm (rolling `cargo install`)
  doesn't crash on an unknown field.
- **inotify `IN_MODIFY`**, event-driven; on each wake re-read from the cursor
  (absorbs coalesced events). **Never** a poll loop.
- **Reuse, don't rewrite:** the tested tmux-injection (buffer pattern) and
  CC-inbox delivery from exomonad-core are the last-hop mechanisms — adapt them.
