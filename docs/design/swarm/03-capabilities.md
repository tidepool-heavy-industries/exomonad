# Capabilities (`exo-caps`)

> **Status: settled** for the seam shape (`Message`, `Addressee`, `Bus`); domain
> newtypes and the `Spawner` method breakdown are pinned-enough to build against,
> with details flagged.

The capability layer is the seam between **policy** (`exo-policy`: the
role/tool/hook/event decision logic — Bucket C, no phases) and **runtime**
(`exo-runtime`: the IO impls). Policy is written generic over the capabilities it
needs; the runtime implements them. Unlike the old WASM boundary this is **not
enforced** — policy *may* drop to raw IO as an escape hatch; good capabilities make
it rarely want to.

## Domain newtypes

```rust
// All newtypes VALIDATE AT CONSTRUCTION: private field + `fn new(..) -> Result<Self,_>`
// (per .claude/rules/rust.md). Once constructed, always valid — no `pub` fields.
struct NodePath(Vec<String>);   // tree address; AgentName = last segment; parent = prefix
struct Branch(String);           // git branch, generated (decoupled from NodePath)
struct PaneId(String);           // "%317" — validated %N form
struct InboxPath(PathBuf);
struct AgentName(String);        // a node's name = NodePath.last(); non-empty, no path separators
struct SyntheticName(String);    // a non-node persona ("github", "ci"); non-empty
enum  Role      { Root, Tl, Dev, Worker }
enum  AgentType { Claude, Gemini, Shoal }
enum  Persona   { Agent(AgentName), Synthetic(SyntheticName) } // who a message is "from"
enum  EventType { PrReview, SiblingMerged, CiStatus, ReviewTimeout } // not a raw String
struct NodeRef  { path: NodePath, pane: PaneId, inbox: InboxPath, agent_type: AgentType }
```

Make-illegal-states-unrepresentable is a first-class goal here (the type-elegance
review flagged the original stringly-typed draft): `Persona`/`EventType`/
`AgentName` replace raw `String`s so a tool can't spoof a persona or fabricate an
event type, and the compiler enforces it.

## Message — plain text + a kind tag

```rust
struct Message {
    from:    Persona,        // Agent(name) or Synthetic(name) — not a raw String (no spoofing)
    text:    MessageBody,    // plain body, validated newtype (length / control-char checks)
    summary: String,         // short preview
    kind:    MessageKind,    // lets the inbound loop route/transform without parsing text
}
enum MessageKind {
    Chat,                          // peer/agent message
    Event { event_type: EventType }, // typed world event — not a raw String
    Control(ControlKind),          // lifecycle (exomonad-internal) — see Shutdown in 04
}
enum ControlKind { Shutdown { grace_ms: u32 } } // a directed control MESSAGE; lifecycle RECORDS (spawned/started) live in the json record log, not here
// id (ulid) + timestamp stamped by the runtime at append — for ordering/dedup, not set by policy. (The cursor is a byte-offset, not the id — see 02.)
```

The body stays plain text; `kind` is the only structure, and it's exomonad-side
(CC's Teams entry has no equivalent — see the CC last-hop mapping in
[02](02-bus-and-sidecar.md)).

## Addressee — how policy names a target

```rust
enum Addressee {
    Parent,                       // my parent's inbox (up)
    InlineChild(AgentName),       // a worker spawned in MY worktree (ephemeral pane, no PR)
    WorktreeChild(AgentName),     // a child spawned in its OWN worktree (branch + PR)
}
```

Messaging is **tree-edges-only**: `Parent` (up) and the two child variants (down,
resolved from the parent's record log). No out-of-band / cross-tree / sibling
addressing — the messaging structure *is* the tree. **`InlineChild` and
`WorktreeChild` share the delivery path** (both resolve name → pane → run-id-keyed
inbox) but **differ in spawn / papers / teardown** (their `ChildKind`). `Pane` is not
policy-facing — an internal resolution target.

## Bus — runtime-agnostic delivery

```rust
trait Bus {
    async fn deliver(&self, to: Addressee, msg: Message) -> Result<()>; // = append to target's ingestion inbox
    // resolve(Addressee) -> InboxPath is INTERNAL to the runtime impl, not exposed to policy.
}
```

`deliver` just appends to the target's pane-keyed ingestion inbox. The
Teams-vs-tmux choice lives in the *recipient's* inbound loop — so policy never
mentions a delivery mechanism. This is the one cap that makes `notify_parent` /
`send_message` trivial and runtime-blind.

## Spawner — the recursion

```rust
struct SpawnSpec { name: Option<AgentName>, role: Role, agent_type: AgentType,
                   task: String, kind: ChildKind }
enum ChildKind { Inline, Worktree } // Standalone (own fresh repo) = a Worktree flavor; revisit if needed
trait Spawner { /* spawn ops — see note */ }
```

Spawn operations are **separate per `ChildKind`** — `spawn_worker` → `Inline`
(ephemeral pane in the parent's worktree, no PR), `spawn_gemini`/`fork_wave` →
`Worktree` (own worktree + branch + PR). They map to the `InlineChild` /
`WorktreeChild` address variants: **shared** messaging/delivery, **distinct** spawn,
papers location, and teardown. `spawn` births a child node: (`git worktree add` for
a `Worktree` child) → `tmux new-pane` → write child papers (incl. `parent_inbox` = my
inbox) → launch `exomonad` node mode. See [05](05-crates-and-binary.md).

**Teardown is two independent steps, not one `reap`** (a conflation the review
caught):
- **Process teardown** — the pane (agent + sidecar) dies. *Graceful*: a
  `Control(Shutdown)` message → the child **self-kills its own pane** (it knows
  `$TMUX_PANE`; see [04](04-policy.md)). *Forceful*: the parent `tmux kill-pane`s a
  non-responsive child. Applies to **both** `ChildKind`s.
- **Worktree reclamation** — `git worktree remove`, **only** for a `Worktree` child,
  **parent-side**, run at **convergence** (after the child's PR merges, when the
  worktree is no longer needed). The worktree *dir* outlives the pane, so this is
  decoupled from process teardown. An `Inline` child has no worktree — nothing to
  reclaim.

## No god-trait — policy is generic over the caps it needs

There is **no `Caps` super-trait and no `&dyn Caps`**. Each tool/hook is generic over
exactly the cap traits it uses, so least-privilege is enforced by the compiler:
```rust
async fn file_pr<C: Git + GitHub>(ctx: &C, args: FilePrArgs) -> Result<ToolOutput>;
```
The concrete **runtime type `R`** implements the individual cap traits it provides
(`Git`, `GitHub`, `Tmux`, `Fs`, `Process`, `Log`, `Bus`, `Spawner`, `Clock`, `Kv` —
`Fs`/`Process`/`Log` added per the caps-coverage review). A role's dispatch is built
by **monomorphizing** its tools at `R`; the MCP edge erases *arguments* to JSON (the
`TypedTool` wrapper) but **never erases the caps**. (Exact shaping — a `Tool<R>`
trait object over the concrete `R`, or a per-role fn-table — is an impl detail; the
invariant is generic-over-caps, per-tool bounds, no `dyn Caps`.)

## Still TBD

- `Git` / `GitHub` / `Tmux` / `Fs` / `Process` / `Log` / `Clock` / `Kv` method
  signatures (mechanical; adapt from exomonad-core services).
- `Spawner` exact method signatures (`spawn` per `ChildKind`; `reclaim_worktree`
  parent-side; force-`kill_pane` — the two-step teardown above, not one `reap`).
- Copilot-review is **not** a cap — it's the sidecar **self-poll**'s job (poll own
  PR → `WorldEvent` → action), replacing the old blocking `wait_for_copilot_review`.
