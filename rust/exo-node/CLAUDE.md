# exo-node — the per-node sidecar

Assembles `exo-runtime` (all caps) + a domain `D: Exomonad` (the domain's tools/hooks/roles/system, monomorphized once by the binary as `run_node::<exo::ExoDomain>`) into a running **sidecar, one process per agent**. This is the binary's `exo node` and `exo hook` modes — there is **no central server**: each agent has its own sidecar, the filesystem is the bus, the process tree is the topology.

**Generic over the domain (`Exomonad`):** `exo-node`'s `NodeContext<D>`, `run_node<D>`, `bootstrap<D>`, and every loop are generic over `D: Exomonad`. It resolves a role's tools/hooks through `D::role_def` (static dispatch — the fn-pointer `RoleRegistry` is **deleted**) and reacts to a domain inter-node payload through `D::handle_system` (the inbound Domain arm deserializes `D::System`, runs the domain handler via a `SystemCtx`, then performs the `SystemOutcome` — e.g. `ReclaimSender` tears the sender down). So it depends only on [`exo-framework`](../exo-framework/CLAUDE.md) for the trait seam and **never on the [`exo`](../exo/CLAUDE.md) domain crate** (`cargo tree -p exo-node` shows neither). The binary is the composition root that names both. The engine bounds only `D: Exomonad<Caps = Runtime>` (the sidecar builds the concrete `Runtime`); the role is fully domain-defined (`D::Role`), read off papers (recorded erased as a `RoleRecord`) and typed back at bootstrap — its one typed reader.

> Part of the v2 node-mode swarm. See `rust/CLAUDE.md`. Classic exomonad (`serve`/`mcp-stdio`) is untouched and lives in `exomonad-core`.

## The loops

`run_node(ctx)` runs several concurrent loops; `handle_hook` is a separate short-lived mode.

| Module | Loop | Role |
|--------|------|------|
| `outbound` (N1) | serve | Serve `role_def(kind).tools` as MCP over stdio (`tools/list` emits each tool's `name`/`description`/`inputSchema`, so the toolset is self-documenting). **Owns stdin/stdout → the node's lifetime anchor** (when it closes, the node ends). |
| `inbound` (N2b) | watch | Watch the node's own ingestion inbox (byte-offset cursor + `notify` watch), route each new entry. |
| `hooksock` (N5) | serve | Per-agent UDS hook-RPC channel — runs the role hook fn on the live runtime and shapes the verdict as the Claude hook-output JSON (`{"continue":true}` / `{"decision":"block",...}`). Only `pre_tool_use` and `session_start` route here now (`Stop` was removed — see Gaps history / `rust/exo/CLAUDE.md`). |
| `watchdog` | watch | Periodic wall-clock self-check (fixed interval, tracks elapsed time since this loop started ≈ node boot). Calls `D::handle_tick(caps, role, elapsed)` (a domain's abandonment-timeout logic — e.g. the reviewer's `handle_review_tick`) and unconditionally re-checks `try_reap` every tick. Replaces the old `Stop`-triggered reap check: `Stop` fires on every turn-end including a legitimate async-wait yield, so it couldn't tell "done" from "paused"; a wall-clock interval can. |
| `dispatch` (N2a) | — | The **last hop**: deliver one entry into the agent's pane via tmux-paste (buffer pattern), rendered with a `[from: X, kind: Y]` header. |
| `hook` (N4) | one-shot | `exo hook <event>` → bootstrap from papers → run the role's `pre_tool_use`/`session_start` → print the verdict. No server. On SessionStart it appends the node identity to the `additionalContext`; the role protocol is delivered via the launch-time `--append-system-prompt` instead. |
| `bootstrap` | — | Self-ID: read `--papers` → `NodePapers`, enrich with ambient env (`$TMUX_PANE`, `EXOMONAD_SWARM_RUN_ID`, `EXOMONAD_TMUX_SESSION`, `$HOME`), build `NodeContext<D> { runtime, kind, own_inbox, parent_inbox, ... }`. Arms `PR_SET_PDEATHSIG` (Linux) as its first act so the sidecar self-terminates if its parent `claude` process dies — stdin-EOF alone is not a reliable lifetime anchor. |
| `error` | — | `NodeError`. |

`run_node` spawns `inbound`, `hooksock`, `watchdog`, and a periodic status publisher as background tasks and awaits `outbound::serve`; when serve returns (agent gone) it aborts all of them. A background loop erroring is logged, not fatal.

## Persistent Logging

The sidecar initializes a persistent file subscriber at startup (in the binary composition root). Logs are written to the **project root** (the main repo's `.exo/` dir, NOT the worktree's) so they survive worktree teardown:

- **Path:** `<project-root>/.exo/logs/sidecar/<run_id>/<branch>.log`
- **Configuration:** Respects `RUST_LOG` (default: `info`).
- **Mechanism:** Uses `tracing-subscriber` with a non-blocking `tracing-appender`.
- **Instrumentation:** The inbound loop (`Domain`/`Lifecycle` arms) and `handle_system` outcomes are span-instrumented. Spans carry **log-friendly labels, never payloads** — `kind` is the one-word discriminant (`kind_label`), `from` is the bare name (`persona_label`); recording a `MessageKind` via Debug would splat the whole `Domain` findings JSON onto every nested line. Inner spans (`handle_lifecycle`/`handle_domain`) don't re-record `node` (the per-node log filename already names it). Success is logged **once at the authoritative layer** — `Bus::deliver` for sends, `dispatch` for last-hop — so the `deliver_parent`/`deliver_to_self` wrappers log only on failure, not a redundant start+OK pair.

## The inbound → dispatch path

`inbound::watch` resumes from a `pane-N.cursor` byte-offset (missing cursor → start at EOF, no history replay), reads only up to the last `\n` (torn lines re-read once complete), advances the cursor **after** successful delivery (at-least-once; a duplicate line is benign), **resolves any claim-check `spill` pointer** to the full entry (`resolve_spilled` loads the bus's `.spill/` side-file — how an oversized payload like a rich verdict arrives), and routes by `kind`:

- **`Chat` / `Event`** → `dispatch::dispatch` (last-hop deliver, rendered with a `[from: X, kind: Y]` header).
- **`Control(Shutdown{grace_ms, force})`** → the cooperative/forced matrix (`decide`):
  - *cooperative (`force=false`)* — **leaf**: deliver a "wrap up and yield" note to the agent, mark `shutdown_pending`, and reap once the watchdog loop's next tick finds the subtree clear (`try_reap`, re-checked unconditionally every tick — no longer triggered by the `Stop` hook, which was removed); **has live children**: bounce a `[shutdown deferred] you have N live children — re-send force:true` message back to the requester (the parent) and do nothing else (the "are you sure").
  - *forced (`force=true`)* — **leaf**: reap now (grace backstop); **has live children**: the sidecar cascades `Shutdown{force:true}` to every live child and reaps itself once they've all exited. Control-plane teardown — note it hard-kills the subtree (uncommitted work in a busy descendant is lost); revisit if that bites in dogfooding.
  - The actual self-reap (`try_reap`) only fires when `shutdown_pending` AND the subtree is clear (live children minus the authoritative `exited_children` set). Before killing its own pane it sends `ChildExited` up; the parent's `handle_system(ChildExited)` re-runs *its* `try_reap`, so a forced teardown drains bottom-up.
- **`Lifecycle(Lifecycle)`** → `handle_lifecycle` (engine-owned, sidecar-consumed). `ChildExited` / `ShutdownResponse` drive the reap / shutdown-render paths; a child is **never** torn down just for finishing a turn. (There used to be a third variant, `ChildIdle`, sent whenever a child's `Stop` hook fired — it flipped a busy-bit `ChildLiveness` read. Removed along with `Stop` itself: `Stop` fires on every turn-end including a legitimate async-wait yield, so the bit was routinely wrong. `ChildLiveness` now reads pane-existence directly instead.)
- **`Domain(DomainPayload)`** → `handle_domain` (domain-opaque, sidecar-consumed, never shown to the LLM unless the domain decides to act). The **one place** the erased wire payload is deserialized to the concrete `D::System` and handed to `D::handle_system` — for the `exo` domain that's `ReviewSystem` (decision derived from structured findings; the round is **best-effort RMW-appended to `.exo/reviews/{safe-branch}.json`** via the generic `read_file`/`write_file` `SystemCtx` methods). The engine then acts on the returned `SystemOutcome`: `ReclaimSender` tears the sender down (`kill_pane` + `reclaim_worktree` — how a one-shot reviewer dies; teardown is **verdict-only**). An undeserializable payload is logged + skipped (tolerant, like a malformed bus line).

## Delivery: tmux-paste (exo owns its channel)

`dispatch` has a single last hop for every node kind (Claude, Shoal companion, inline worker):
`Tmux::paste` the rendered `[from: X, kind: Y]` entry into the agent's own pane (buffer pattern —
`load-buffer`/`paste-buffer`, no bracketed `-p`). The durable bus carries the message
sidecar→sidecar; this module injects it into the live `claude`.

**Why not CC Agent Teams?** exo used to write the agent's CC team lead inbox so its `InboxPoller`
rendered a native `<teammate-message>`. As of **Claude Code 2.1.178** that channel is dead for a
multi-process orchestrator: teammates are now **in-process** (`Agent({name})`), one-team-per-session,
no cross-session sharing — and a **solo team-lead with no live in-process teammate never drains its
inbox**. Every exo node is a separate `claude` process = a solo lead, so every Teams write silently
stranded (reproduced; CC GH#26426). There is no supported external/file-based registration or
push-injection path. So Teams delivery, the `teamout` outbound bridge, and the `TeamCreate`
SessionStart instruction were all **removed**; tmux-paste (the former floor) is now the only channel.
`exo-scry` survives only for `fork_session` context inheritance (`spawner.rs`), not delivery.

The paste path (`exomonad_shared::services::tmux_ipc::inject_input`) is hardened: per-target async
lock, **Rewind/modal dismissal** (capture pane → if it looks like the Rewind menu, send Escape — else
the modal swallows the paste), copy/scroll-mode exit, temp-file `load-buffer`, 150ms debounce, 3×
Enter retry, SIGWINCH wake, session-qualified target, and spill-to-file for payloads >480B.

Outbound is symmetric and exo-owned: a child reaches its parent via the **`notify_parent` MCP tool**
(→ Bus → the parent's inbound→paste); `send_message` addresses a child. No native CC team tools are
used.

## Inline-child shutdown

Tearing down an inline worker is a **parent-side `Spawner::kill_pane(child_name)`** — done by the
`dismiss_worker` MCP tool (the same `kill_pane` path, no cooperative handshake; the inline worker has
no sidecar inbound loop to receive a `Control(Shutdown)`). Worktree-child shutdown is the cooperative
bus path (`Control(Shutdown)` → bus → the child's inbound loop → `try_reap`), and a folded/reviewed
worktree child is reclaimed by `merge` / verdict-side teardown.

## Gaps / not-yet

- **Shutdown has structured response.** A structured `shutdown_response` is written back to the requester (status accepted/deferred + live_children + busy), and the requester's `handle_system` renders it to chat.
- **Forced teardown is a hard kill.** `force:true` cascades pane-kills through the subtree with no per-node commit/wrap-up — a busy descendant loses uncommitted work. Deliberate (force = "tear it down"); revisit if it bites in dogfooding. **It also never reclaims worktrees — by design**: the cascade only kills panes; the subtree's worktree directories (and branches) are left on disk for post-mortem inspection and are cleaned up later by `exo doctor --fix` (or the next `merge` of an ancestor). Force-kill is an abnormal path — preserving the filesystem state it leaves behind is a feature, not a leak.
- **`outbound` hand-rolls JSON-RPC** over stdio (despite the "rmcp/stdio" framing) — minimal `initialize`/`tools/list`/`tools/call`; no capability negotiation beyond that.
- **Convergence teardown is wired, best-effort, now retried.** `merge` (the `exo` domain tool) reclaims the folded child (`kill_pane` + `reclaim_worktree`), and a one-shot reviewer is torn down verdict-side in `handle_system`. The reclaim path is now **bounded-retried** (3 attempts, linear backoff) inside the `Spawner` impls (`exo_runtime::retry_teardown`), so the reviewer teardown and the `merge` tool both inherit it for free; `try_reap`'s own-pane self-kill uses the same helper. A final failure logs a **loud structured error** (`op` + `child` + `attempts`) and the error is surfaced — but teardown stays **best-effort**: it never aborts the merge/teardown flow, and a child whose worktree is dirty or holds a nested worktree (e.g. a still-live reviewer) can still fail to reclaim after retries and linger (self-heals via the liveness reap).
