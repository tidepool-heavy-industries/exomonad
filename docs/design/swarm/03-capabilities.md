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
struct MessageBody(String);      // plain message body; validated (length / control-char checks)
enum  NodeKind  { Root, Tl, Dev, Worker } // the node's archetype — the ONE stored identity enum; role + runtime derive from it (no illegal (role, runtime) pair representable)
enum  AgentType { Claude, Gemini, Shoal }  // runtime — used by the DELIVERY last-hop only; for a tree node = node_kind.agent_type(). Shoal is a companion/external-rmcp participant, NOT a per-op spawn archetype, so it never forces a free agent_type field on a tree node.
enum  Persona   { Agent(AgentName), Synthetic(SyntheticName) } // who a message is "from"
// (no EventType here — the single typed event enum is `WorldEvent` in 04; see Message below)

// NodeKind is the single source of a node's correlated identity. role (the role_def key,
// = the variant: "root"/"tl"/"dev"/"worker") and agent_type both DERIVE; neither is stored separately.
impl NodeKind {
    fn agent_type(&self) -> AgentType { /* Root | Tl => Claude ;  Dev | Worker => Gemini */ }
}
```

Make-illegal-states-unrepresentable is a first-class goal here (the type-elegance
review flagged the original stringly-typed draft): `Persona`/`AgentName` and the
typed `WorldEvent` (04) replace raw `String`s so a tool can't spoof a persona or
fabricate an event, and the compiler enforces it. **`NodeKind` collapses the
correlated `(role, agent_type)` pair into one enum** — only the four real archetypes
are representable, `agent_type` derives, and `(Root, Gemini)` / `(Worker, Claude)`
become unnameable (closing the representational gap the idiom review flagged; the
*construction* hazard was already closed by per-op spawn specs). (`NodeRef` was cut
as an unused type — a future probe/`list_agents` surface can introduce a
purpose-built node-view then; the parent's child-handle is just the folded
`AgentSpawned` record.)

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
    Event,                         // a world event — routed to on_world_event, which parses the body into a typed `WorldEvent` (04). Bare tag: the detail rides the plain-text body, not the enum (keeps the body CC-last-hop-friendly).
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

**Per-op narrow specs — `(role, agent_type, kind)` are fixed by the op, never free
caller fields** (so illegal combos like `(Inline, Tl, Claude)` are *unnameable*).
This mirrors the existing Haskell exactly: there `SpawnSpec` is *task content only*
(`steps`/`verify`/`done_criteria`/`context`/…), and the triple is fixed by **which
core you call** (`spawnWorkerToolCore`/`spawnGeminiCore`/`forkWaveCore`). One op ↔
one MCP tool ↔ one fixed triple:

```rust
enum ChildKind { Inline, Worktree } // internal; set by the op, drives birth/papers/teardown. Standalone = a Worktree flavor.

trait Spawner {
    // each op fixes its own (role, agent_type, kind); the spec carries ONLY task content
    async fn spawn_worker(&self, spec: WorkerSpec) -> Result<AgentName>;   // → Inline / Worker / Gemini
    async fn spawn_gemini(&self, spec: GeminiSpec) -> Result<AgentName>;   // → Worktree / Dev / Gemini
    async fn fork_wave(&self, specs: Vec<ForkSpec>) -> Result<Vec<AgentName>>; // → Worktree / Tl / Claude
}
// WorkerSpec / GeminiSpec / ForkSpec = narrow task-content structs (name?, task, steps, verify, …),
// ported field-for-field from the Haskell WorkerSpec / SpawnSpec. No role/agent_type/kind inside them.
```

All three ops share one private tail — **`birth(BirthCore { kind, agent_type, name,
branch })`** — which the op constructs with its fixed triple: (`git worktree add` for
a `Worktree` child) → `tmux new-pane` → write child papers (incl. `parent_inbox` = my
inbox) → launch `exomonad` node mode → append the `AgentSpawned` record. The shared
tail branches only on `kind`; the per-op methods are the single place each triple is
named. They back the `InlineChild` / `WorktreeChild` address variants: **shared**
delivery, **distinct** spawn / papers / teardown. See [05](05-crates-and-binary.md).

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
by **monomorphizing** its tools at `R`; the MCP edge erases *arguments* to JSON but
**never erases the caps**. Shaping (decided — see [04](04-policy.md)): each tool is a
**type** with a generic-over-caps `run` + a hand-written `Tool<R>` adapter (no macro);
a role is a `Vec<Box<dyn Tool<R>>>` over the concrete `R`. The invariant is
generic-over-caps, per-tool bounds, no `dyn Caps`.

## Still TBD

- `Git` / `GitHub` / `Tmux` / `Fs` / `Process` / `Log` / `Clock` / `Kv` method
  signatures (mechanical; adapt from exomonad-core services).
- `Spawner` narrow per-op spec field lists (`WorkerSpec`/`GeminiSpec`/`ForkSpec`,
  ported from the Haskell) + teardown method names (`reclaim_worktree` parent-side;
  force-`kill_pane`) — the per-op methods + two-step teardown are settled above.
- Copilot-review is **not** a cap — it's the sidecar **self-poll**'s job (poll own
  PR → `WorldEvent` → action), replacing the old blocking `wait_for_copilot_review`.
