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
| `hooksock` (N5) | serve | Per-agent UDS hook-RPC channel — runs the role hook fn on the live runtime and shapes the verdict as the Claude hook-output JSON (`{"continue":true}` / `{"decision":"block",...}`). |
| `teamout` (N6) | watch | **Outbound Teams bridge (Claude-only, Linux-only).** Watches this node's own CC team inboxes for messages the agent *sent* to a teammate (native `SendMessage` / `shutdown_request`), maps the recipient name → tree-edge `Addressee`, and forwards onto the bus. The reverse of `dispatch`. No roster authored (a child self-registers as a teammate when it first messages up); sidecar-owned processed-count cursor, never writes CC's inboxes. |
| `dispatch` (N2a) | — | The **last hop**: deliver one entry into the agent's native interface (Teams inbox or tmux paste). |
| `hook` (N4) | one-shot | `exo hook <event>` → bootstrap from papers → run the role's `pre_tool_use`/`stop`/`session_start` → print the verdict. No server. On SessionStart it appends the node identity + team lines to the `additionalContext`; the role protocol is delivered via the launch-time `--append-system-prompt` instead. |
| `bootstrap` | — | Self-ID: read `--papers` → `NodePapers`, enrich with ambient env (`$TMUX_PANE`, `EXOMONAD_SWARM_RUN_ID`, `EXOMONAD_TMUX_SESSION`, `$HOME`), build `NodeContext<D> { runtime, kind, own_inbox, parent_inbox, ... }`. Arms `PR_SET_PDEATHSIG` (Linux) as its first act so the sidecar self-terminates if its parent `claude` process dies — stdin-EOF alone is not a reliable lifetime anchor. |
| `error` | — | `NodeError`. |

`run_node` spawns `inbound`, `hooksock`, and `teamout` as background tasks and awaits `outbound::serve`; when serve returns (agent gone) it aborts all three. A background loop erroring is logged, not fatal.

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
  - *cooperative (`force=false`)* — **leaf**: deliver a "wrap up and yield" note to the agent, mark `shutdown_pending`, and reap on its next idle (the stop hook drives `try_reap`); **has live children**: bounce a `[shutdown deferred] you have N live children — re-send force:true` message back to the requester (the parent) and do nothing else (the "are you sure").
  - *forced (`force=true`)* — **leaf**: reap now (grace backstop); **has live children**: the sidecar cascades `Shutdown{force:true}` to every live child and reaps itself once they've all exited. Control-plane teardown — note it hard-kills the subtree (uncommitted work in a busy descendant is lost); revisit if that bites in dogfooding.
  - The actual self-reap (`try_reap`) only fires when `shutdown_pending` AND the subtree is clear (live children minus the authoritative `exited_children` set). Before killing its own pane it sends `ChildExited` up; the parent's `handle_system(ChildExited)` re-runs *its* `try_reap`, so a forced teardown drains bottom-up.
- **`Lifecycle(Lifecycle)`** → `handle_lifecycle` (engine-owned, sidecar-consumed). A **`ChildIdle`** from a child finishing a turn ONLY flips that child's busy-bit to idle (`runtime.mark_child_idle` — what the `stop` idle gate reads); it is **NOT rendered to the LLM**. CC's Stop fires on every turn-end (e.g. a child pausing on a long bash command mid-task), so `[child idle]` is high-frequency noise, not a done-signal — the meaningful signals (`notify_parent`, `[READY]`, a reviewer verdict, `ChildExited`) carry the real state. The child is **never** torn down. `ChildExited` / `ShutdownResponse` drive the reap / shutdown-render paths.
- **`Domain(DomainPayload)`** → `handle_domain` (domain-opaque, sidecar-consumed, never shown to the LLM unless the domain decides to act). The **one place** the erased wire payload is deserialized to the concrete `D::System` and handed to `D::handle_system` — for the `exo` domain that's `ReviewSystem` (decision derived from structured findings; the round is **best-effort RMW-appended to `.exo/reviews/{safe-branch}.json`** via the `read_reviews`/`persist_reviews` `SystemCtx` methods). The engine then acts on the returned `SystemOutcome`: `ReclaimSender` tears the sender down (`kill_pane` + `reclaim_worktree` — how a one-shot reviewer dies; teardown is **verdict-only**). An undeserializable payload is logged + skipped (tolerant, like a malformed bus line).

## Native Teams delivery (the hard-won part)

`dispatch` decides the last hop by the node's `agent_type`:

- **Claude in a team** → write the team's **lead inbox**; the agent's own CC `InboxPoller` renders it as a native `<teammate-message>`. The team is resolved via **`exo_scry::resolve_self_or_portable()`** (solo-team-per-session): primary path is `resolve_self()`, which walks the sidecar → parent `claude` process and reads that process's inotify-bound team dir — without a `tmux_pane_id` (which CC omits from team config — that's why `resolve_by_pane` never fired and native delivery used to fail; see `node-native-teams-delivery` memory). On `resolve_self`'s failure (no team, or a transient `/proc`/config race) it falls back to the portable `resolve_via_transcript` path (cwd→transcript→`resolve_by_session`). **This portable fallback is WIRED but UNTESTED on non-Linux:** its cwd reader is currently Linux-only, so off-Linux `resolve_self_or_portable()` returns `None` and the node degrades to tmux paste. On Linux, `resolve_self` succeeds in the common case, so the fallback rung is rarely hit and is itself effectively untested.
- **Claude with no team (or a Shoal companion)** → `Tmux::paste` into the pane.
- **Inline worker (any agent type)** → always `Tmux::paste`. `dispatch` short-circuits team resolution when `ctx.runtime.is_inline()` is true, because the inline node's cwd is the parent's worktree — `resolve_self_or_portable()`'s cwd→transcript fallback would resolve into the **parent's** team, leaking the message into the parent's conversation. The `SessionStart` hook also skips the `TeamCreate` instruction for inline nodes.

Delivery always works, only degrades — Teams is a *nicety*, tmux-paste is the floor. The `SessionStart` hook tells a Claude **worktree** node to `TeamCreate` if it doesn't already lead a team.

The bridge is **bidirectional** for every worktree node (all Claude): `dispatch` is the inbound last hop (bus → the agent's lead inbox → native `<teammate-message>`), and `teamout` (N6) is the outbound one (the agent's native `SendMessage`/`shutdown_request` → bus → the addressed tree edge). `run_node` does **not** spawn the `teamout` task for inline nodes. So a worktree node can just use its native team tools to talk to its parent/children — no exomonad-specific tool required. The MCP `send_message`/`notify_parent` tools remain as a fallback (and for inline workers, Shoal companions, or any teamless node).

## Inline-child shutdown (FIX C)

When a **parent's** `teamout` bridge receives a `shutdown_request` addressed to an inline child (resolved as `Addressee::InlineChild` via `resolve_edge`), it calls `Spawner::kill_pane(&*ctx.runtime, child_name)` directly — **parent-side pane kill** — instead of forwarding a `Control(Shutdown)` onto the bus. The inline worker has no teamout bridge (not spawned) and can't receive cooperative-shutdown messages, so the parent just kills its pane. Worktree-child shutdown continues the cooperative path (`Control(Shutdown)` → bus → the child's inbound loop → `try_reap`).

## Gaps / not-yet

- **Shutdown has structured response.** A structured `shutdown_response` is written back to the requester (status accepted/deferred + live_children + busy), and the requester's `handle_system` renders it to chat. The native CC `shutdown_request` has no force field, so a bridged request is always cooperative.
- **Forced teardown is a hard kill.** `force:true` cascades pane-kills through the subtree with no per-node commit/wrap-up — a busy descendant loses uncommitted work. Deliberate (force = "tear it down"); revisit if it bites in dogfooding.
- **`outbound` hand-rolls JSON-RPC** over stdio (despite the "rmcp/stdio" framing) — minimal `initialize`/`tools/list`/`tools/call`; no capability negotiation beyond that.
- **Portable team resolution is wired but untested off-Linux.** `dispatch` resolves via `exo_scry::resolve_self_or_portable()`, which falls back from the inotify `resolve_self` to the portable cwd→transcript path. That fallback is portable *by design* but its cwd reader is Linux-only, so on non-Linux it yields `None` and the node degrades to tmux paste — native delivery there has never run. Verify off-Linux (or with a portable cwd reader) before relying on it.
- **Convergence teardown is wired, best-effort, now retried.** `merge` (the `exo` domain tool) reclaims the folded child (`kill_pane` + `reclaim_worktree`), and a one-shot reviewer is torn down verdict-side in `handle_system`. The reclaim path is now **bounded-retried** (3 attempts, linear backoff) inside the `Spawner` impls (`exo_runtime::retry_teardown`), so the reviewer teardown and the `merge` tool both inherit it for free; `try_reap`'s own-pane self-kill uses the same helper. A final failure logs a **loud structured error** (`op` + `child` + `attempts`) and the error is surfaced — but teardown stays **best-effort**: it never aborts the merge/teardown flow, and a child whose worktree is dirty or holds a nested worktree (e.g. a still-live reviewer) can still fail to reclaim after retries and linger (self-heals via the liveness reap).
