# exo — the v2 node-mode binary (CLI) + the domain (tools / roles / gates)

`exo` is the **standalone v2 node-mode binary** — it owns the whole node-mode CLI surface
(`exo init` / `exo node` / `exo hook`; classic `exomonad` is server-only). The lib half is the minimal **domain usage** of
[`exo-framework`](../exo-framework/CLAUDE.md): the genuinely domain-specific Bucket-C logic that
ports from the old Haskell DSL — the MCP tool set, the per-role roster, and the CC hook gates. This
is the "small usage" half of the framework/domain split (the Rust analog of Classic's Haskell-WASM
config DSL). Written **generic over the `exo-caps` traits** (no `dyn Caps`), so least-privilege is
compiler-checked and every tool is **unit-testable against mock caps with zero IO**.

The engine never depends on this crate's lib. The binary builds a `RoleRegistry` from `roster()`
and injects it into `exo-node`; that injection is the whole point of the split. See
[`docs/decisions/exo-framework-domain-split.md`](../../docs/decisions/exo-framework-domain-split.md).

The CLI modules (`main.rs` / `init.rs` / `hook.rs` / `config.rs`) are **bin-only** — they link the
v2/shared seam (`exo-node`, `exo-runtime`, `exomonad-shared`) but never classic `exomonad-core`. The
lib (`lib.rs` + `tools/` + `gates.rs` + `roles.rs`) stays generic over the caps and links neither.

> Part of the v2 node-mode swarm (the `exo` binary). See `rust/CLAUDE.md`.

## Shape

| File | Contents |
|------|----------|
| `lib.rs` | Re-exports `role_def` / `roster`. Generic over `R`, depends only on `exo-framework` + `exo-caps`. |
| `main.rs` | The CLI dispatcher (bin): clap `Cli` → `init` / `node` / `hook`. `node` is the composition root — `exo node --papers <path>` → build the roster → `exo_node::bootstrap(papers, cwd, roster())` → `run_node`. |
| `init.rs` | `exo init [--session <s>] [--recreate]` — bootstrap a node-mode ROOT (own tmux session, root papers, no server). Reuses `exo-runtime`/`exomonad-shared`. |
| `hook.rs` | `exo hook <event> --papers <path>` — handle a CC/Gemini hook via the node's `exo` gate (SessionStart in-process; everything else routes to the sidecar hook socket, fail-open). |
| `config.rs` | Minimal node-mode init config read (`tmux_session`, `model` only) — classic `exomonad` owns the full `Config`. |
| `tools/` | One module per tool — a type + `Args` (derives `Deserialize + JsonSchema`) + generic-over-caps `run` + a ~6-line hand-written `Tool<R>` adapter (NO macro). Each ships mock-cap unit tests. |
| `gates.rs` | The concrete hook bodies: `pre_tool_use` (antipattern nudges), `stop` (the convergence gate) + per-role variants (`stop_allow`/`stop_notify`/`stop_reviewer`), `session_start`. Functions generic over the caps they need. |
| `roles.rs` | `role_def(NodeKind)` — the hand-written table (the single place a role's tool list + hooks are named) — and `roster()`, which wraps it as the `RoleRegistry` the binary injects. |
| `testing.rs` | `MockRuntime` — impls every cap, records calls, returns canned values. Every tool tests against this one shared mock. |

The `Tool<R>` trait, `RoleDef<R>`, the hook decision enums, and `PolicyCaps` are the framework
contract ([`exo-framework`](../exo-framework/CLAUDE.md)); this crate provides the concrete instances.

## The tools

| Tool | Caps | Roles | What it does |
|------|------|-------|--------------|
| `fork_wave` | `Spawner` | root, tl | Fork N Claude TL children (own worktrees). |
| `spawn_gemini` | `Spawner` | root, tl | Spawn a Gemini dev in its own worktree. |
| `spawn_worker` | `Spawner` | root, tl | Spawn an ephemeral Gemini worker (inline pane). |
| `merge` | `Git`+`Spawner` | root, tl | **The local fold:** `git merge <child-branch>`, followed by best-effort teardown (`kill_pane` + `reclaim_worktree`) of the child. |
| `submit_branch` | `Git`+`Process`+`Spawner`+`Fs`+`Bus` | tl, dev | **Request review.** Runs the precondition checks (committed + `.exo/checks/pre-merge/*` scripts), then spawns a **reviewer** off this branch (fork-point `git diff` base via `Git::merge_base`) and returns "stop & wait". It does NOT deliver `[READY]` — only the sidecar does, on an approve-verdict (the structural gate). Escape hatch: `dangerously_skip_reviewer: true`. |
| `verdict` | `Bus` | reviewer | A reviewer's one output → a `System(SystemMessage)` to its parent: `approve` / `deny`+msg / `changes`+branch. Triggers reviewer teardown (handled in `exo-node`). |
| `notify_parent` | `Bus` | tl, dev, worker, reviewer | Status/failure update to `Addressee::Parent` (NOT the done-signal). |
| `send_message` | `Bus` | root, tl | Deliver to a child (`Inline`/`Worktree`) — **tree-edges only**. |
| `tree` | `Topology` | root, tl | Read-only: the caller's subtree (recursive ledger fold) + parent + per-node `pane_alive` liveness. |

Every tool implements `Tool::description()`; `exo-node`'s `tools/list` emits it, so the toolset is
self-documenting — an agent learns the local-merge loop (commit → `submit_branch` → parent `merge`,
no PR/remote) from the tools it has. `submit_branch`'s preconditions are an **ordered, extensible
fn-pointer list** (`tools/submit.rs`) mirroring the role hook fn-pointers.

## Roles

`role_def(kind)` returns a `RoleDef<R> { tools, pre_tool_use, stop, session_start }`; `roster()`
wraps `role_def` as the injected `RoleRegistry`. Hooks compose by pointing several roles at the same fn.

| Role | agent | tools | stop gate |
|------|-------|-------|-----------|
| **Root** | Claude | fork_wave, spawn_gemini, spawn_worker, merge, send_message, tree | `stop_allow` (never gate the human's session; no parent to notify) |
| **Tl** | Claude | spawns, merge, notify_parent, send_message, submit_branch, tree | `stop` (clean-gate + notify parent on clean-allow) |
| **Dev** | Gemini | notify_parent, submit_branch | `stop_notify` (notify parent, always allow — never block Gemini) |
| **Worker** | Gemini | notify_parent | `stop_notify` (inline child, no branch to fold, but still signals on yield) |
| **Reviewer** | Gemini | verdict, notify_parent | `stop_reviewer` (ephemeral; its `verdict` is its done-signal; emits `ReviewAborted` if it exits without one) |

## The review gate (how `submit_branch` → `merge` is gated)

A node commits, then calls `submit_branch`. It runs the checks, then spawns a **reviewer** (a full
Gemini in its own worktree branched off the under-review code) handed the diff + `.exo/acceptance.md`.
The reviewer calls `verdict`, which rides the bus as a `System` message to the submitter's
**sidecar**:
- **approve** & sha==HEAD → the sidecar escalates `[READY]` to the parent — *no LLM turn*.
- **deny** / **changes** → delivered into the submitter's LLM to fix / `merge` the reviewer's
  branch, then re-submit (new sha → fresh reviewer).

`submit_branch` never delivers `[READY]` itself, so the gate is **structural** — the LLM has no
tool that skips review. The reviewer is torn down (best-effort) as soon as the `verdict` is processed.

## The gates

- **`pre_tool_use`** — default-**ALLOW** antipattern *nudge* (NOT a security gate). Currently one rule: deny `git add .` / `git add -A` (stage by path). Can `Deny` with guidance or `Modify` to rewrite.
- **`stop`** (tl) — the **local convergence gate** (`R: Git + Log + Bus + ChildLiveness`). Blocks exit while the worktree is dirty (a parent folds a child by merging its *branch* off disk, so uncommitted work is invisible). On a **clean** exit (Allow) it delivers a `System(ChildIdle)` to the parent — but **only when the subtree is idle** (`ChildLiveness::any_child_busy`). **Fails OPEN** on any git error.
- **`stop_notify`** (dev, worker) — Gemini turn-end hook (`R: Bus + Log`): deliver `System(ChildIdle)`, then **always Allow**. **Never blocks** — Gemini's `AfterAgent` `deny` can infinite-loop (gemini-cli #20426).
- **`stop_allow`** (root) — unconditional Allow. Root has no parent.
- **`stop_reviewer`** (reviewer) — silent on the happy path (verdict produced); emits a loud `ReviewAborted` to the parent if the reviewer exits without a verdict. Always allows exit.
- **`session_start`** — identity bootstrap (the node-identity context is prepended by `exo-node`).

## Gaps / not-yet

- **Reviewers:** review is currently always-on (no config to disable); a two-way colleague back-channel (submitter→reviewer reply) needs `send_message` on dev.
- `pre_tool_use` is intentionally minimal (one nudge); classic exomonad's richer antipattern set + PII rewrite are not ported.
- `stop`'s dirty-gate can wedge an agent that holds untracked artifacts it won't commit.
- **Phases / authoring-DSL polish** are a deliberate follow-on — `RoleDef<R>` is relocated as-is, not yet reshaped into a builder/trait.
