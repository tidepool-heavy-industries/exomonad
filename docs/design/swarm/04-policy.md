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

## Where each runs

- **Tools** — served over MCP by the sidecar (outbound loop). `tools = role_def(role).tools`.
- **Hooks** — `exomonad hook` *mode* reads the CC hook payload, self-IDs, calls
  `pre_tool_use` / `stop` / `session_start`, emits the verdict. No central server.
- **Events** — `on_world_event` is invoked from (a) the sidecar **inbound loop** on
  a `kind=event` ingestion entry, and (b) the sidecar's **own PR self-poll** (the
  per-agent realization of the old central poller; sibling-merge handled by the
  parent, which knows its children). `InjectMessage` → append to own inbox;
  `NotifyParent` → append to parent inbox.

## Content (filled in incrementally — not "design")

- **Tools:** `file_pr`, `merge_pr`, `spawn_*`, `tasks_*`, messaging (already in
  `teams-mcp`). Per-tool `Args`/handlers ported one at a time.
- **Hooks kept:** `pre_tool_use` (guards + PII-rewrite), `stop` (live PR gate),
  `session_start` (root identity bootstrap).
- **Events:** `WorldEvent { PrReview, SiblingMerged, CiStatus, ReviewTimeout }` →
  `EventAction`. Behavior ported from the current poller/event handlers.
- **Per-role toolsets** in `role_def`.
