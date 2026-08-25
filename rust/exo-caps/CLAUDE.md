# exo-caps — the capability seam

The trait/type contract that the node-mode swarm forks from. The engine abstractions (`exo-framework`) and the domain (`exo`) are written **generic over these traits**; the runtime (`exo-runtime`) implements them. This crate has **no IO** — only trait definitions, validated domain newtypes, and the message/identity vocabulary.

This is the seam that replaces the old Haskell-WASM boundary. WASM *physically* prevented the policy layer from doing IO; a crate that simply doesn't link the runtime gets the same separation at zero runtime cost — **except** the wall is soft: policy *may* drop to raw IO as an escape hatch. Good caps make it rarely want to.

> Part of the v2 node-mode swarm (`exo`), built beside classic exomonad-core. See `rust/CLAUDE.md` for how the node-mode crates relate.

## What lives here

| Module | Contents |
|--------|----------|
| `types` | Validated domain newtypes + the identity/messaging vocabulary (see below) |
| `domain` | The **domain seam** traits the engine is generic over: `RoleKind` (domain role enum), `SpawnSpec` (spawn intent), `DomainSystem` (inter-node payload, blanket-impl'd for any serde type). Rooted here (not `exo-framework`) because `Spawner`/`NodePapers`/`MessageKind` reference them and `exo-caps` can't depend on `exo-framework`. The `Exomonad` trait that ties them together lives in `exo-framework`. See [`docs/decisions/exo-trait-refactor.md`](../../docs/decisions/exo-trait-refactor.md). |
| `error` | `CapError` — source-preserving (`#[from]` per-cap errors), `CapResult` |
| `bus` | `Bus` trait + `Addressee` (tree-edges only) + `BusError` |
| `spawner` | `Spawner` trait + the generic `SpawnSpec` (one `spawn<S>`; the domain's per-op spec fixes role/kind) + `SpawnError::UnknownChild` + the free `birth_preamble(kind, child_dir)` (the worktree/inline isolation prose every child's launch prompt opens with — one home, beside the `ChildKind` it branches on, so a caller rendering a prompt outside `birth` can't drift from it). Historical per-op method names `WorkerSpec`/`GeminiSpec`/`ForkSpec` are gone. |
| `lifecycle` | `ChildRecord` (append-only: `Spawned` / `Reaped` / `Died` / `Submitted`) + `fold_children` → `Child { …, state: ChildState, model }`. See [The child lifecycle ledger](#the-child-lifecycle-ledger) below. |
| `papers` | `NodePapers` (`node.json`) — a node's immutable birth identity. Gains a `kind: ChildKind` field (`#[serde(default = "Worktree")]`) so old papers still parse. `NodePapers::new` hardcodes `Worktree`; only `birth_finish`'s struct literal sets `kind: core.kind`. Also carries `parent_branch: Option<Branch>` (`#[serde(default)]`) — the parent's REAL git branch, stamped at birth from the spawner's own `current_branch` (`None` for the root); backs `submit_branch`'s `needs_rebase` gate at every depth instead of a string-derived (and often unresolvable) coordinate. |
| `paths` | Inbox/papers path scheme (`~/.claude/exo/inboxes/{run_id}/pane-N.jsonl`), the per-node sockets (`hook_sock`, `listen_sock` — the streaming wake channel), and `pane_from_inbox` (recover a recipient's pane, hence its sibling status file, from an inbox path a sender already holds) |
| `invocation` | Single source of truth for a child's `exo node/hook/listen` argv (`listen_command` is what the SessionStart hook interpolates into the arm-Monitor instruction) |
| `git` `tmux` `fs` `kv` `process` | The IO capability traits (signatures only) |

## The capability traits

Nine caps, each one trait per file, in **two tiers**: *primitive* caps own one external resource each; *composite* caps orchestrate across resources and declare the primitives they stand on as **supertraits** (`Spawner: Git + Tmux + Fs`, `Bus: Fs`, `Topology: Tmux + Fs`, `ChildLiveness: Tmux + Fs`). An impl of a composite must also impl its primitives, so a composite can never quietly re-shell a domain a primitive already owns. `exo-runtime::Runtime` implements all of them; `exo::testing::MockRuntime` mocks all of them. (There is **no `Log` cap** — sidecar code logs via `tracing` directly; a separate `Log` cap was a redundant unbounded file channel and was removed.)

**Primitives:**

- **`Git`** — `current_branch`, `head_sha` (sha-tag review verdicts), `merge_base` (fork-point base for a reviewer's `git diff`), `is_clean`, `status_porcelain` (the raw `git status --porcelain` lines — `is_clean` answers *whether*, this answers *what*, so a gate can NAME the offending files), `is_ahead_of` / `is_behind` (branch-vs-base commit-count checks — `is_behind` backs `submit_branch`'s rebase gate; both fail-open to `false` on an unresolvable base), `commits_between(base, head)` → `Vec<CommitFiles { sha, files }>` newest-first (unlike the fail-open predicates this **errs** on an unresolvable base — a caller enumerating commits must not read "empty" as "none"), `fetch`, **`merge`** (the local on-disk fold — v2 convergence), `worktree_add`/`worktree_remove` (**force/reclaim semantics**: the worktree directory's state is discarded, the branch ref survives). **No `GitHub` cap** — v2 convergence is local git, no PR/Copilot (cut 2026-06-01; see `reactive-github-layer-stays` memory).
- **`Tmux`** — `new_pane`, `new_window`, `paste`, `kill_pane`, `list_panes` (the liveness probe: `Err` = probe *failure*, never "no panes" — each consumer applies its own default).
- **`Fs`** — `read`, `write_atomic` (temp+rename, creates parent dirs), `read_dir` (a directory's immediate entry paths; errors on a missing/unreadable directory — no recursion, no metadata, filtering stays with the caller). Deliberately **no `append`**: the two append disciplines (single-writer ledger, multi-writer PIPE_BUF bus) live inside the `Spawner`/`Bus` impls, out of policy's reach.
- **`Kv`** — `get`, `set`.
- **`Process`** — `run` (no timeout, no kill) + `run_with_timeout` (returns `ProcessOutcome::Completed`/`TimedOut`; no default impl — a naive `tokio::time::timeout` wrapped around `run` can't kill anything it doesn't hold a handle to, so every impl provides its own kill-capable body). `exo-runtime`'s impl spawns the child in a **new process group** (`process_group(0)`) and, on expiry, `killpg`s that whole group rather than just the direct child — a gate command that forks grandchildren (a build wrapper, a shell pipeline) doesn't leak them. Backs `merge`'s optional `gate_timeout_ms`.

**Composites:**

- **`Bus: Fs`** — `deliver(Addressee, Message)` + `wake_status(&Addressee) -> WakeStatus`. The append half only; the read/cursor/watch half is the sidecar's inbound loop. The last-hop delivery mechanism (the listen wake channel) is the *recipient's* concern — policy never names it; `wake_status` is the one advisory peephole (read off the recipient's status snapshot: `Listening`/`NotListening`/`Unknown`, never errs, default-impl `Unknown`) so a *sender* can surface "your recipient can't hear yet — message queued" in its tool response. Resolving a **tombstoned** child (`Reaped`/`Died`) is `BusError::Tombstoned`, not a silently-successful append: nothing reads a dead child's inbox, and its recorded pane may since have been recycled onto a different live agent.
- **`Spawner: Git + Tmux + Fs`** — ONE generic `spawn(D::Spawn)` (the recursion; the domain's spawn intent fixes `(role, kind)` at the tool boundary) + a `fork_wave` vec wrapper (default method) + `reclaim_worktree` / `kill_pane` (teardown). Replaces the old per-archetype methods: a new archetype is a new domain role + a thin domain tool, not a `Spawner` edit. Fully generic `spawn<S: SpawnSpec>` over the domain role (the runtime records the role erased). NB: `Spawner::kill_pane(&AgentName)` and `Tmux::kill_pane(&PaneId)` collide on a composite receiver — call them UFCS-qualified.
- **`Topology: Tmux + Fs`** — `topology()` → the caller's subtree (folded recursively from the per-node `children.jsonl` ledgers) + parent + per-node pane-liveness. Backs the `tree` tool. Each `TreeNode` carries its folded `state` + effective `model` (both `None` for the caller itself, which its own ledger records nothing about). A node in a **terminal** state reports `pane_alive: false` unconditionally — the probe is never consulted for it (see the pane-id-reuse note below).
- **`ChildLiveness: Tmux + Fs`** — `any_child_busy()` → does any *direct* child's pane currently exist? Used to be a genuinely separate question from `Topology`'s pane-existence — a busy-bit tracked from messages (busy at birth + on every poke; idle on a Claude Code `Stop`-hook-derived `ChildIdle` report), combined with pane-death as a one-way override. The bit was removed: `Stop` fires on every turn-end, including a legitimate async-wait yield, so it was routinely wrong (see `rust/exo/CLAUDE.md`). `any_child_busy` is now literally a `Tmux::list_panes` probe over direct children — coarser (can't distinguish "actively working" from "idle but its pane is open"), but honest. Its one remaining caller is the cooperative-shutdown `Defer` response's cosmetic wording, not a safety-critical gate.

Tool bounds in `exo` stay **explicit per-cap** (`C: Git + Spawner`, not just `C: Spawner`): a tool's bound documents what it *directly* calls — its least-privilege spec — while the supertraits encode what an *implementation* tier needs. Redundant bounds are harmless; don't narrow them.

## Domain types (the invariants worth knowing)

- **Validate-at-construction newtypes.** `new(...) -> CapResult`; once built, always valid. **Serde deserializes *through* the constructor** (`#[serde(try_from)]`), so a value read off disk (papers, a bus line) is validated too — no "transparent" hole. `AgentName`, `Branch`, `PaneId`, `MessageBody` (≤4 KiB, no C0 ctrl except `\t\n\r`), `Summary` (≤256 B, single line), `SyntheticName`, `ToolName` (non-empty, no path sep), `Reason` (non-empty, multi-line allowed — backs the hook `Deny`/`Block` reasons + `HookInput.tool_name`).
- **`NodePath`** — tree address as a `Vec<AgentName>`, **not** a dot-string (branch segments may contain `.`, so a joined form can't round-trip). `name()` = last segment, `parent()` = prefix, `child()` extends. `Branch::from_path` generates a *safe* branch (sanitize segments to `[A-Za-z0-9_-]`, join `.`) decoupled from the path.
- **Role** — the role enum is **domain-owned** (`exo::ExoRole`), reached through the [`RoleKind`](domain) seam; the engine never names a variant. Papers record it **erased** as a [`RoleRecord`] (raw JSON), typed back to `D::Role` by the one typed reader (bootstrap). `RoleKind::agent_type` is the domain's role→backend mapping (**every role→Claude** in `exo`; `Shoal` is a companion backend, not a spawnable tree node). `RoleKind::model` selects the per-role model: `Some("sonnet")` for `dev`/`worker`/`reviewer`, `Some("opus")` for a spawned `tl`, `None` (inherit the launcher's default) for `root` only — root is never spawned via `birth` (it's the human's own top-level `exo init` session, so inheriting is the human's own choice); every node that *is* spawned gets an explicit model rather than inheriting whatever tier the launcher happens to default to. **Defaults to `None`** so non-`exo` impls compile unchanged. `RoleKind::launch_profile_env_prefix` (**defaults to `None`**) names an env-var prefix for an optional per-role *launch profile* — a backend-agnostic redirect of that role's Claude to a non-default Anthropic-compatible endpoint/model via a local proxy (`exo` returns `Some("EXO_REVIEWER")` for the reviewer → e.g. Kimi). The runtime reads `{prefix}_{BASE_URL,MODEL,AUTH_TOKEN,LABEL}` live and translates to `ANTHROPIC_*` at launch; the auth token is memory-only (never papers), only the non-secret `LABEL` is recorded. `RoleKind::protocol` returns the role's decomposition-steering prose injected at session_start — **defaults to empty** (so non-`exo` `RoleKind` impls compile unchanged), overridden per-variant only by `exo::ExoRole`.
- **`Message`** = `{text, summary, kind, reply_to}` — what *policy* builds. It carries **no `from`/`ts`/`id`**: the runtime stamps the `IngestionEntry` envelope at append, so **a tool cannot spoof its sender** (anti-spoof is structural, not convention). `Persona` = `Agent(AgentName) | Synthetic(SyntheticName)`. The envelope also carries an optional **`spill`** claim-check pointer (`#[serde(default)]`, omitted when `None`): an entry whose serialized line would exceed `PIPE_BUF` is written to a side-file and the inbox line becomes a small pointer the reader resolves — so a payload (e.g. a rich verdict) is **unbounded in size** while every line stays one atomic append.
- **Message ids are reference-only, never a dedup key.** The envelope carries `id: Option<String>` (a UUID v4 the runtime stamps at append; a spilled entry and its pointer share ONE id, since they are one logical message). `Message::reply_to` points at another message's `id` — it sits on the *policy* half, so a tool can populate it without changing `Bus::deliver`'s signature; nothing populates it yet, the last hop only renders it. **Never deduplicate on an id.** The inbound cursor advances only *after* a successful last-hop delivery, which makes redelivery at-least-once **by design**, and a redelivered line arrives with its ORIGINAL id — an "already seen" check would swallow exactly the retry the protocol exists to guarantee.
- **`MessageKind`** = `Chat | Event | Control(Shutdown{grace_ms}) | Lifecycle(Lifecycle) | Domain(String)` (the **HYBRID wire**). `Event` is delivered like `Chat`. Both `Lifecycle` and `Domain` are sidecar-consumed (never rendered to the LLM unless the handler decides to act), but they split by ownership: **`Lifecycle`** (typed, engine-owned, closed — `exiting` / `shutdown_response` / `submitted`) is acted on by the sidecar itself (`try_reap` / the shutdown matrix / appending a `ChildRecord::Submitted` to the recipient's own ledger). A `Lifecycle` arm may act *and* still render: `submitted` records the fact durably **and** re-dispatches the child's `[READY]` prose to the LLM, because the ledger is for the machine (it survives a context window) while the pasted prose is what actually makes the agent act; **`Domain`** is a domain's [`DomainSystem`] payload erased to a raw JSON string (held as a `String`, not `RawValue`, so it survives `#[serde(flatten)]`'s buffered intermediate), deserialized to the concrete `D::System` at exactly one place (the inbound loop's Domain arm) before `D::handle_system`. A fully-typed System wire was rejected because it would force `C: Bus` → `C: Bus<D::System>` and collapse per-tool least-privilege; only the multi-writer bus payload is erased (papers carry `D::Role` fully typed). Build a Domain message with the free [`deliver_domain`] helper (a tool naming `D::System` still needs only `C: Bus`). The `exo` domain's `D::System` is `exo::ReviewSystem` (domain-owned — this crate carries zero review vocabulary). An unknown Domain tag is a runtime skip (tolerant parser), not a compile error — mitigated by `Lifecycle` being typed + an exhaustive `handle_system` match.

## The child lifecycle ledger

Each node's `.exo/children.jsonl` is an append-only log of what it knows about its own direct
children. `ChildRecord` is serde-tagged on `record`, and every field added since the first version
is `#[serde(default)]` — so a ledger line written by an older binary still parses (pinned by test in
`tests/serde_roundtrip.rs`).

| Record | Who appends it | Meaning |
|--------|----------------|---------|
| `Spawned { child, kind, pane, inbox, model_label, model, directives_hash }` | the parent, **before** the pane exists | the birth guard — there is never an untracked agent. `model` is the *effective* launch model (after launch-profile-over-role-default precedence); `model_label` stays the non-secret cosmetic tag, never the auth token |
| `Reaped { child, at }` | the **runtime** teardown paths (`Spawner::kill_pane` / `reclaim_worktree`) — never a tool | the parent deliberately tore this child down |
| `Died { child, pane, at }` | the watchdog's death scan | the pane was observed gone while the child was still un-reaped. **At most once** — the record itself is the dedup guard |
| `Submitted { child, branch, sha, reviewed, at }` | the parent's **sidecar**, on a `Lifecycle::Submitted` | the child is waiting on this node's merge; makes the pending-merge queue outlive a context window |

`fold_children` applies records **in order** into `Child { …, state: ChildState, model }`:

```
ChildState = Live | Submitted { sha, reviewed } | Reaped | Died
ChildState::is_terminal() == matches!(self, Reaped | Died)
```

A `Spawned` inserts a fresh `Live` child (newest-spawn-wins — a respawn under the same name *resets*
its state); the other three mutate the existing entry, and a later record overwrites an earlier one,
so the benign race "the watchdog wrote `Died` a moment before the runtime wrote `Reaped`" self-heals
to `Reaped`. A state record naming an unknown child is skipped silently (this is a tolerant pure
function — no tracing lives in `exo-caps`).

**`Submitted` is not terminal** — a submitted child is still a running agent with a live pane,
waiting on its parent.

**Dead children never vanish from the fold.** The parent still needs to see them: a `Died` child may
hold unmerged committed work on its branch, or uncommitted work in its worktree. The consumers'
obligation is therefore not to *filter* tombstones out but to ask `is_terminal()` before they
**probe, resolve, or deliver**. That guard is what closes the **pane-id-reuse** hole: tmux recycles
`%N` pane ids, so a dead child's recorded pane can later belong to a *different, live* agent — and a
liveness probe, a pane-keyed status file, or a message delivery aimed at it would then report or
reach the wrong node entirely.

## Load-bearing principles (encoded in the types)

- **Observe, don't store.** Only genuinely-recorded facts get types: what this node DID (`Spawned`, `Reaped`), what it OBSERVED once and must not re-observe (`Died`), and what a child REPORTED that must outlive a context window (`Submitted`). Liveness of a still-live child is computed **live** (pane-alive), never written back — a tombstone is written only when the child is gone for good. `ChildLiveness` used to carry an in-memory busy-bit derived from observed messages; it's gone now — liveness is a direct pane probe, nothing to seed or restart-recover.
- **Identity is assigned at birth, not derived.** `role`/`parent`/tree-position exist in no runtime's live state, so `NodePapers` records them once. Live derivation (`exo-scry`) recovers only runtime-native facts (pane, CC team).
- **Messaging is tree-edges only.** `Addressee` = `Parent | Child(name)`. There is no sibling/cross-tree addressee — the messaging structure *is* the process tree. (Inline vs worktree children deliver identically, so the edge carries only the name; the `ChildKind` distinction lives at spawn/teardown. `Pane` is an internal resolution target, not policy-facing.)

## Gaps / not-yet

- `reclaim_worktree` / `kill_pane` are called by the `merge` tool (folded-child reclaim) and the sidecar's reviewer verdict-teardown. Both are **best-effort but bounded-retried** — the runtime impls wrap each op in `exo_runtime::retry_teardown` (3 attempts, linear backoff), logging a loud structured error on final failure and surfacing (never escalating) it. A dirty or nested worktree can still resist reclaim after the retries and linger.
- `ChildKind::Standalone` (fresh-repo child, a classic feature) is folded into `Worktree`; not separately represented.
