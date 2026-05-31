# Policy Layer (`exo-policy`)

> **Status: settled** for the *form* of policy; the *content* (which tools/hooks/
> events per role) is filled in incrementally as the swarm absorbs each from the
> Haskell guest. **No phases / state machine** (dropped — recompute from live
> state). No DSL, no macros, no HList.

`exo-policy` is the Bucket-C decision logic that genuinely ports from the Haskell
DSL. Everything else in the old guest was WASM-boundary tax that *deletes* — see
[migration](06-migration.md).

## How policy is defined — plain Rust, three forms

1. **Tools = functions generic over the caps they need** (no `dyn Caps` — see [03](03-capabilities.md)):
   ```rust
   #[derive(Deserialize, JsonSchema)] struct FilePrArgs { /* … */ }
   async fn file_pr<C: Git + GitHub>(ctx: &C, args: FilePrArgs) -> Result<ToolOutput>;
   ```
   The per-tool bounds *are* the least-privilege spec, compiler-checked. Tool authors
   never touch `serde_json::Value`: a `TypedTool { type Args; execute<C>(&C, Args) }`
   wrapper handles the JSON edge; dispatch is monomorphized at the concrete runtime `R`.
2. **Hooks & events = functions generic over the caps they need** (no `dyn Caps`):
   ```rust
   enum HookDecision { Allow, Deny { reason: String }, Modify(serde_json::Value) }
   enum StopDecision { Allow, Block { reason: String } }
   enum EventAction  { InjectMessage{text,summary}, NotifyParent{text,summary}, NoAction }

   fn pre_tool_use<C: Kv>(ctx: &C, input: &HookInput) -> HookDecision; // guards/PII — &C for data-dependent checks (Kv allowlists)
   fn stop<C: GitHub>(ctx: &C) -> StopDecision;                       // LIVE query — no phase
   fn on_world_event<C: GitHub>(ctx: &C, e: &WorldEvent) -> EventAction; // RETURNS an action (loop delivers); bound is per-handler — GitHub here to inspect PR/CI state
   ```
   In the `RoleDef` table below these are stored as `fn(&R, …)` monomorphized at the
   concrete runtime `R` (which impls the cap traits) — the generic bound *is* the
   per-hook least-privilege spec, compiler-checked, exactly as for tools.
3. **A role = a data struct bundling them, wired in a hand-written table:**
   ```rust
   // parameterized by the concrete runtime R (which impls the cap traits) — no dyn Caps
   struct RoleDef<R> {
       tools:        Vec<Box<dyn Tool<R>>>,            // dyn over the CONCRETE R, not over Caps
       pre_tool_use: fn(&R, &HookInput) -> HookDecision,
       stop:         fn(&R) -> StopDecision,
       on_event:     fn(&R, &WorldEvent) -> EventAction,
   }
   fn role_def<R: Runtime>(kind: NodeKind) -> RoleDef<R> { match kind { /* … */ } }
   ```

`RoleDef` literals *read* like declarative config but are plain, greppable,
unit-testable Rust. (A `Role` trait with methods is the alternative; the
struct-of-fn-pointers is more table-like — current lean.) The only infra dep is
`async-trait` (or boxed futures) for the object-safe async `Tool::call`. Policy is
transport-agnostic; the sidecar adapts `Tool` to rmcp.

## No phases

The `StateMachine`/`Phase` abstraction is dropped — low value for the ceremony.
The one thing it backed, the **stop-gate**, becomes a live query: `stop(ctx)` asks
the `GitHub` cap "do I have an open PR with unaddressed `ChangesRequested`?" and
returns `Block`/`Allow`. No persisted phase, consistent with observe-don't-store.

**Child tracking** (the *other* thing the old `TLPhase` held — a `Map ChildHandle`)
is a **parent-local append-only record log** (`children.jsonl`), not a state machine
(these are lifecycle *records* — distinct from `MessageKind::Event` world-events on
the bus). Two record kinds:
- **`AgentSpawned { child, kind, pane, path, inbox }`** (`kind: ChildKind`) — appended
  by the parent **first, before it creates the pane**, so there's never an untracked
  process. `kind` drives papers-location + teardown: **Inline** → pane-keyed papers in
  the shared run dir, torn down by pane-kill; **Worktree** → own-worktree `node.json`,
  torn down by pane-kill + parent-side `git worktree remove` at convergence (the
  two-step teardown — see [03](03-capabilities.md)).
- **`AgentStarted { child }`** — the **child appends this to the record log** on
  startup (its boot check-in; it knows the log path from its papers). A *record*,
  not a message and not a `MessageKind::Event` — never delivered to anyone's
  conversation, just folded for tracking.

"Who are my children" = fold the log. A `Spawned` with **no `Started`** after a
timeout is a **failed/ghost spawn** → the parent reaps/retries. Running-vs-done
status is computed **live** (pane-alive check), never written back. Append-only +
fold-to-state is the same discipline as the bus (a retry is a new append; no mutable
per-child record; no orphan-before-papers gap — the parent logged the intent before
the pane existed). Converge / can-I-exit is computed from the ledger +
live pane/PR checks, not a phase. (Chosen over a live worktree scan, which is racy
and O(N), and over CC-team membership, which re-introduces the multi-team coupling.)

## Where each runs

- **Tools** — served over MCP by the sidecar (outbound loop). `tools = role_def(role).tools`.
- **Hooks** — `exomonad hook` *mode* reads the CC hook payload, self-IDs, calls
  `pre_tool_use` / `stop` / `session_start`, emits the verdict. No central server.
- **Events** — `on_world_event` is invoked from (a) the sidecar **inbound loop** on
  a `kind=event` ingestion entry, and (b) the sidecar's **own PR self-poll**.
  `InjectMessage` → append to own inbox; `NotifyParent` → append to parent inbox.

  **Self-poll discipline** (bounds API load — no central poller needed): poll
  **every 3 min, only while this agent has an open PR** (no PR → no polling, so a
  swarm is *sparse*, not N×/min). A **~15 min review-timeout** nudges/notifies the
  parent if no Copilot review arrived, and **resets on each round of Copilot
  feedback**. Sibling-merge is handled by the **parent** (it has the child ledger),
  not by each sibling polling. This adapts (reuses) the existing
  `exomonad-core/.../github_poller.rs` timeout logic, per-sidecar.

## Shutdown (worker lifecycle) — just a message

Requesting shutdown is **not a special mechanism** — it's a
`Control(Shutdown { grace_ms })` message **appended to the target's ingestion
mailbox via the ordinary send path** (`Bus::deliver` / `send_message`), exactly
like messaging any teammate. (Same as Claude Teams, where `shutdown_request` is
just a `SendMessage` payload, not a separate tool.) Any `shutdown(member)` surface
is mere sugar over that one append; the `kind` tag is what carries it.

The recipient's sidecar **inbound loop** dispatches on `kind=Control(Shutdown)` by
`agent_type`:
- **CC** → forward to CC's native `shutdown_request` (cooperative ack).
- **gemini/shoal** → optionally inject a graceful "finish & exit", then after
  `grace_ms` run `tmux kill-pane` on **its own** `$TMUX_PANE` — reaping the whole
  worker (pane + agent + sidecar) in one shot. The worker **self-terminates** (it
  knows its own pane); no parent force-kill, no separate channel.
- No cleanup write — the child just stops passing the live-filter (pane gone); its
  birth-ledger entry is a harmless stale record (status computed live).
- For a **`Worktree` child** the worktree *dir* outlives the pane; the **parent**
  reclaims it with `git worktree remove` at **convergence** (after merging the
  child's PR). Process teardown (here) and worktree reclamation are separate steps
  with separate owners — see [03](03-capabilities.md).

So non-cooperative runtimes get the clean teardown CC has for free, **reusing the
mailbox + `kind` we already built** — and the phantom-member problem closes at the
root, because the sidecar reliably reaps its own pane on the control message.

## Content (filled in incrementally — not "design")

- **Tools:** `file_pr`, `merge_pr`, `spawn_*`, `tasks_*`, messaging (already in
  `teams-mcp`). Per-tool `Args`/handlers ported one at a time.
- **Hooks kept:** `pre_tool_use` (guards + PII-rewrite), `stop` (live PR gate),
  `session_start` (root identity bootstrap).
- **Events:** `WorldEvent { PrReview, SiblingMerged, CiStatus, ReviewTimeout }` →
  `EventAction`. Behavior ported from the current poller/event handlers. **This is
  the single typed event enum** — there is no parallel `EventType` on the message
  envelope (`MessageKind::Event` is a bare tag, [03](03-capabilities.md)); a
  `kind=event` ingestion entry has its body parsed into a `WorldEvent` before
  `on_world_event` runs, and the in-process self-poll constructs one directly.
- **Per-role toolsets** in `role_def`.
