# exo-node — the per-node sidecar

Assembles `exo-runtime` (all caps) + `exo-policy` (tools/hooks/roles) into a running **sidecar, one process per agent**. This is the binary's `exomonad experimental node` and `exomonad experimental hook` modes — there is **no central server**: each agent has its own sidecar, the filesystem is the bus, the process tree is the topology.

> Part of the v2 node-mode swarm. See `rust/CLAUDE.md`. Classic exomonad (`serve`/`mcp-stdio`) is untouched and lives in `exomonad-core`.

## The loops

`run_node(ctx)` runs two concurrent stimuli; `handle_hook` is a separate short-lived mode.

| Module | Loop | Role |
|--------|------|------|
| `outbound` (N1) | serve | Serve `role_def(kind).tools` as MCP over stdio (`tools/list` emits each tool's `name`/`description`/`inputSchema`, so the toolset is self-documenting). **Owns stdin/stdout → the node's lifetime anchor** (when it closes, the node ends). |
| `inbound` (N2b) | watch | Watch the node's own ingestion inbox (byte-offset cursor + `notify` watch), route each new entry. |
| `hooksock` (N5) | serve | Per-agent UDS hook-RPC channel — runs the role hook fn on the live runtime and shapes the verdict per agent_type (never a Gemini Stop deny). |
| `dispatch` (N2a) | — | The **last hop**: deliver one entry into the agent's native interface (Teams inbox or tmux paste). |
| `hook` (N4) | one-shot | `exomonad experimental hook <event>` → bootstrap from papers → run the role's `pre_tool_use`/`stop`/`session_start` → print the verdict. No server. |
| `bootstrap` | — | Self-ID: read `--papers` → `NodePapers`, enrich with ambient env (`$TMUX_PANE`, `EXOMONAD_SWARM_RUN_ID`, `EXOMONAD_TMUX_SESSION`, `$HOME`), build `NodeContext { runtime, kind, own_inbox, parent_inbox, ... }`. |
| `error` | — | `NodeError`. |

`run_node` spawns `inbound` as a background task and awaits `outbound::serve`; when serve returns it aborts inbound. A background loop erroring is logged, not fatal.

## The inbound → dispatch path

`inbound::watch` resumes from a `pane-N.cursor` byte-offset (missing cursor → start at EOF, no history replay), reads only up to the last `\n` (torn lines re-read once complete), advances the cursor **after** successful delivery (at-least-once; a duplicate line is benign), and routes by `kind`:

- **`Chat` / `Event`** → `dispatch::dispatch` (last-hop deliver, rendered with a `[from: X, kind: Y]` header).
- **`Control(Shutdown{grace_ms})`** → sleep the grace, then `Tmux::kill_pane` on the node's **own** pane — reaping pane + agent + sidecar in one shot.
- **`System(SystemMessage)`** → `handle_system` (sidecar-consumed, never shown to the LLM unless it decides to act). A **review verdict** (`ReviewApproved`/`Denied`/`Changes`) from a one-shot reviewer is applied via `apply_verdict`, then that reviewer is torn down (`kill_pane` + `reclaim_worktree`) — teardown is **verdict-only**. A **`ChildIdle`** from a child finishing a turn flips that child's busy-bit to idle (`runtime.mark_child_idle` — what the `stop` idle gate reads) and is rendered as a concise line (`render_child_idle`, preserving the child's `from`); the child is **never** torn down. This render is the parent-side seam where idle-signal refinement (dedupe, richer state) will land.

## Native Teams delivery (the hard-won part)

`dispatch` decides the last hop by the node's `agent_type`:

- **Claude in a team** → write the team's **lead inbox**; the agent's own CC `InboxPoller` renders it as a native `<teammate-message>`. The team is resolved via **`exo_scry::resolve_self()`** (solo-team-per-session): it walks the sidecar → parent `claude` process and reads that process's inotify-bound team dir. This works without a `tmux_pane_id` (which CC omits from team config — that's why `resolve_by_pane` never fired and native delivery used to fail; see `node-native-teams-delivery` memory).
- **Claude with no team, or Gemini** → `Tmux::paste` into the pane.

Delivery always works, only degrades — Teams is a *nicety*, tmux-paste is the floor. The `SessionStart` hook tells a Claude node to `TeamCreate` if it doesn't already lead a team.

## Gaps / not-yet

- **Shutdown is the Gemini path for everyone.** `inbound` handles `Control(Shutdown)` by kill-pane for all agent types; the cooperative CC `shutdown_request` forward (graceful ack) is not implemented.
- **`outbound` hand-rolls JSON-RPC** over stdio (despite the "rmcp/stdio" framing) — minimal `initialize`/`tools/list`/`tools/call`; no capability negotiation beyond that.
- **No convergence teardown driver.** Nothing in the node calls `Spawner::reclaim_worktree`/`kill_pane` after a `merge`, so folded children's panes/worktrees linger (the `exo-policy` lifecycle gap surfaces here).
