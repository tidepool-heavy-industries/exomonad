# Capabilities (`exo-caps`)

> **Status: settled** for the seam shape (`Message`, `Addressee`, `Bus`); domain
> newtypes and the `Spawner` method breakdown are pinned-enough to build against,
> with details flagged.

The capability layer is the seam between **policy** (`exo-policy`: the
role/tool/phase/hook/event decision logic — Bucket C) and **runtime**
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
struct MemberName(String);       // non-empty, no path separators
enum  Role      { Root, Tl, Dev, Worker }
enum  AgentType { Claude, Gemini, Shoal }
enum  Persona   { Member(MemberName), Synthetic(SyntheticName) } // who a message is "from"
enum  EventType { PrReview, SiblingMerged, CiStatus, ReviewTimeout } // not a raw String
struct NodeRef  { path: NodePath, pane: PaneId, inbox: InboxPath, agent_type: AgentType }
```

Make-illegal-states-unrepresentable is a first-class goal here (the type-elegance
review flagged the original stringly-typed draft): `Persona`/`EventType`/
`MemberName` replace raw `String`s so a tool can't spoof a persona or fabricate an
event type, and the compiler enforces it.

## Message — plain text + a kind tag

```rust
struct Message {
    from:    Persona,        // Member(name) or Synthetic(name) — not a raw String (no spoofing)
    text:    MessageBody,    // plain body, validated newtype (length / control-char checks)
    summary: String,         // short preview
    kind:    MessageKind,    // lets the inbound loop route/transform without parsing text
}
enum MessageKind {
    Chat,                          // peer/agent message
    Event { event_type: EventType }, // typed world event — not a raw String
    Control(ControlKind),          // shutdown / lifecycle (exomonad-internal)
}
// id + timestamp are stamped by the runtime at append (for the cursor) — not set by policy.
```

The body stays plain text; `kind` is the only structure, and it's exomonad-side
(CC's Teams entry has no equivalent — see the CC last-hop mapping in
[02](02-bus-and-sidecar.md)).

## Addressee — how policy names a target

```rust
enum Addressee {
    Parent,                // my parent_inbox
    Member(MemberName),    // any teammate by name: real, synthetic (gemini worker), or a child
}
```

`Member` is the universal "by name" case — children and synthetic workers are just
members. `Pane` is **not** policy-facing; it's an internal resolution target the
`Bus` derives (member name → pane → inbox path). Cross-tree `Node(path)` is
deferred until something needs it.

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
struct SpawnSpec { name: Option<String>, role: Role, agent_type: AgentType,
                   task: String, isolation: Isolation }
enum Isolation { Worktree, Inline, Standalone }
trait Spawner { /* spawn ops — see note */ }
```

Spawn operations are kept **separate** (mirroring today's `fork_wave` =
worktree-claude, `spawn_gemini` = worktree, `spawn_worker` = inline pane) rather
than collapsed into one `Isolation`-parameterized call — that unification is
deferred. Whether the `Spawner` cap is one method + `Isolation` or N methods is a
detail; the *tools* stay distinct regardless. Either way, `spawn` births a child
node: `git worktree add` (if isolated) → `tmux new-pane` → write child papers
(incl. `parent_inbox` = my inbox) → launch `exomonad` node mode in the pane. See
[05](05-crates-and-binary.md).

## The runtime super-trait

```rust
trait Caps: Git + GitHub + Tmux + Bus + Spawner + Clock + Kv + Send + Sync {}
```

Makes `Tool` objects work; the one runtime implements all of it.

## Still TBD

- `Git` / `GitHub` / `Tmux` / `Clock` / `Kv` method signatures (mechanical; adapt
  from exomonad-core services).
- `Spawner` method breakdown (one + `Isolation` vs N).
