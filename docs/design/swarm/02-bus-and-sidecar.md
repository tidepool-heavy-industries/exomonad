# The Bus & the Two-Loop Sidecar

> **Status: settled** (message format + cursor details stubbed, see end).

## The bus = per-node ingestion inboxes

Each node has one **generic ingestion inbox**: an append-only file the sidecar
owns. Senders drop a message there knowing nothing about the recipient's runtime.
The recipient's sidecar consumes it and does the runtime-specific last hop into
its own agent.

- **Pane-keyed, team-free.** `inbox/pane-317.jsonl` (sanitize `%`). The pane is the
  key because it's unique per agent (even co-located), needs no CC team, and is
  stable. Member name is *display metadata*, not the key.
- **Append-only + inotify.** Appends are atomic; the sidecar watches via inotify
  (event-driven, not polling).
- **Exomonad-namespaced**, not written into CC's managed dirs:
  ```
  ~/.claude/exo/inboxes/{…}/pane-317.jsonl     ← the bus mailbox (exomonad owns)
  ~/.claude/teams/{team}/inboxes/{member}.json ← CC's push channel (sidecar's CC last hop)
  ```
  (Exact root path is a [stub](07-open-questions.md) — pane-keyed regardless.)

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
`exo-scry`). CC's Teams inbox is demoted to "the CC sidecar's pretty last hop" —
the bus itself is team-free. (Verified: CC's InboxPoller delivers an inbox entry
from an *arbitrary, unregistered* `from` — the `oracle` test — so the sidecar's
last-hop write needs no member registration for the *sender*.)

## Routing

- **Up (notify_parent):** append to `parent_inbox` (a path held in your papers).
  Whoever spawned you set it — robust to unplanned spawns.
- **Down (to a child) / peer (sibling) / arbitrary:** resolve the target's
  ingestion-inbox path. For exomonad nodes: their papers (pane → inbox path). For
  unplanned CC sub-claudes: CC team config (`tmuxPaneId`) or pane enumeration.
- **The worktree directory is the routing table** — no separately-maintained
  central registry. Glob `.exo/worktrees/*/.exo/node.json`.

## Ingestion entry format (settled)

One JSON object per line (append-only). Serializes the [`Message`](03-capabilities.md)
plus runtime-stamped `id` + `ts`:

```jsonc
{ "id": "01J8…",            // ulid/monotonic, stamped on append — the cursor key
  "ts": "2026-05-30T22:…Z",  // stamped on append
  "from": "github",          // member name or synthetic persona
  "kind": "event",           // chat | event | control
  "summary": "PR #5 approved",
  "text": "…" }              // plain body
```

**CC last-hop mapping:** entry → CC Teams entry `{from, text, summary, timestamp,
read:false}`. `id`/`kind` are exomonad-side and dropped (or folded into `text`) at
the CC write. **tmux-paste rendering:** `from` + `summary` + `text` formatted into
the pane.

## Cursor (settled)

A sibling `pane-317.cursor` holding the **last-delivered `id`** (ulids sort, so
"deliver everything after this id"). On (re)start the sidecar reads the cursor,
skips already-delivered entries, resumes; it advances the cursor **only after a
successful last-hop delivery**, so a crash mid-delivery re-delivers at most one
message rather than dropping it. Mirrors CC's `tasks/.highwatermark`.

## Inbound event/transform policy

`kind=event` entries are passed to `exo-policy`'s event handler
(`on_world_event → InjectMessage | NotifyParent | NoAction`) before delivery;
`kind=control` (e.g. shutdown) is handled by the loop; `kind=chat` passes straight
through. See [policy](04-policy.md).
