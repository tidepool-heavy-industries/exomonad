# Policy Layer (`exo-policy`)

> **Status: settled** for the *form* of policy; the *content* (which tools/hooks/
> events per role) is filled in incrementally as the swarm absorbs each from the
> Haskell guest. **No phases / state machine** (dropped — recompute from live
> state). No DSL, no macros, no HList.

`exo-policy` is the Bucket-C decision logic that genuinely ports from the Haskell
DSL. Everything else in the old guest was WASM-boundary tax that *deletes* — see
[migration](06-migration.md).

## How policy is defined — plain Rust, three forms

1. **Tools = a trait you implement** (one polymorphic thing):
   ```rust
   #[async_trait]
   trait Tool: Send + Sync {
       fn name(&self) -> &'static str;
       fn schema(&self) -> serde_json::Value;              // schemars on its Args
       async fn call(&self, ctx: &dyn Caps, args: serde_json::Value)
           -> Result<serde_json::Value>;                   // Args erased to JSON at the edge
   }
   ```
   Tool *authors* never touch `serde_json::Value`: they implement a strongly-typed
   `TypedTool { type Args: DeserializeOwned; async fn execute(&self, &dyn Caps, Self::Args) }`,
   and a blanket impl provides the object-safe `Tool` (deserialize → `execute`). The
   JSON erasure is confined to the one inherent MCP-boundary seam.
2. **Hooks & events = pure functions.**
   ```rust
   enum HookDecision { Allow, Deny { reason: String }, Modify(serde_json::Value) }
   enum StopDecision { Allow, Block { reason: String } }
   enum EventAction  { InjectMessage{text,summary}, NotifyParent{text,summary}, NoAction }

   fn pre_tool_use(input: &HookInput) -> HookDecision;            // guards, PII-rewrite
   fn stop(ctx: &dyn Caps) -> StopDecision;                       // LIVE query — no phase
   fn on_world_event(ctx: &dyn Caps, e: &WorldEvent) -> EventAction;
   ```
3. **A role = a data struct bundling them, wired in a hand-written table:**
   ```rust
   struct RoleDef {
       tools:        &'static [&'static dyn Tool],
       pre_tool_use: fn(&HookInput) -> HookDecision,
       stop:         fn(&dyn Caps) -> StopDecision,
       on_event:     fn(&dyn Caps, &WorldEvent) -> EventAction,
   }
   fn role_def(role: Role) -> RoleDef { match role { /* … */ } }
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
becomes a **parent-local birth ledger**, not a state machine: at spawn, the parent's
sidecar appends the child (`{pane, path, inbox}`) to its own `children.jsonl`. The
ledger is **append-only and immutable** — an entry is a *birth fact*, never
modified; a child's status (alive / done / failed) is **never written back**, only
computed live (pane-alive + PR checks). That's exactly what justifies jsonl here
(same discipline as the bus): a retry is a *new* append, "current children" =
live-filter at read, and dead entries are trimmed only by occasional compaction. If
we stored mutable per-child status this would be the wrong format — we deliberately
don't (observe-don't-store). "Who are my children" = read the ledger (authoritative
— the parent recorded each birth, so there's **no orphan-before-papers gap** the
live-scan would have); per-child liveness = a pane-alive check. Converge / can-I-exit is computed from the ledger +
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

## Content (filled in incrementally — not "design")

- **Tools:** `file_pr`, `merge_pr`, `spawn_*`, `tasks_*`, messaging (already in
  `teams-mcp`). Per-tool `Args`/handlers ported one at a time.
- **Hooks kept:** `pre_tool_use` (guards + PII-rewrite), `stop` (live PR gate),
  `session_start` (root identity bootstrap).
- **Events:** `WorldEvent { PrReview, SiblingMerged, CiStatus, ReviewTimeout }` →
  `EventAction`. Behavior ported from the current poller/event handlers.
- **Per-role toolsets** in `role_def`.
