# exo-policy — the decision logic (tools / roles / hooks)

The Bucket-C logic that genuinely ports from the old Haskell DSL: MCP tool definitions, the per-role tables, and the CC hooks. Written **generic over the `exo-caps` traits it needs** (no `dyn Caps`), so least-privilege is compiler-checked and every tool is **unit-testable against mock caps with zero IO** — the payoff the WASM guest couldn't have. No phases, no DSL, no macros.

> Part of the v2 node-mode swarm (`exomonad experimental`). See `rust/CLAUDE.md`.

## Three forms, all plain Rust

| File | Contents |
|------|----------|
| `tool` | The `Tool<R>` trait (object-safe over the concrete `R`) + JSON-edge helpers (`parse`/`ok_json`/`schema_json`) + `ToolOutput` + `BoxFuture`. |
| `tools/` | One module per tool — a type + `Args` (derives `Deserialize + JsonSchema`) + generic-over-caps `run` + a ~6-line hand-written `Tool<R>` adapter (NO macro). |
| `hooks` | `pre_tool_use` (antipattern nudges), `stop` (the convergence gate), `session_start`. Functions generic over the caps they need. |
| `roles` | `RoleDef<R>` + the hand-written `role_def(NodeKind)` table — the single place a role's tool list + hooks are named. |
| `caps` | `PolicyCaps` — a static bound-**union** for the dispatch boundary. NOT a god-trait: tools still declare their own narrow per-cap bounds. A blanket impl makes any all-caps type (`Runtime`, `MockRuntime`) `PolicyCaps` automatically. |
| `testing` | `MockRuntime` — impls every cap, records calls, returns canned values. Every tool tests against this one shared mock. |

**A tool's cap bounds *are* its least-privilege spec.** `fn run<C: Bus>` can only touch the bus; `fn run<C: Git>` only git. The bound is compiler-checked and surfaced in the adapter's `impl` header.

## The tools

| Tool | Caps | Roles | What it does |
|------|------|-------|--------------|
| `fork_wave` | `Spawner` | root, tl | Fork N Claude TL children (own worktrees). |
| `spawn_gemini` | `Spawner` | root, tl | Spawn a Gemini dev in its own worktree. |
| `spawn_worker` | `Spawner` | root, tl | Spawn an ephemeral Gemini worker (inline pane). |
| `merge` | `Git` | root, tl | **The local fold:** `git merge <child-branch>`. No PR, no remote, no GitHub. |
| `submit_branch` | `Git`+`Bus` | tl, dev | **The done-signal** (local analogue of file_pr): runs preconditions (v1: committed), then delivers `[READY] branch X` to the parent for it to `merge`. |
| `notify_parent` | `Bus` | tl, dev, worker | Status/failure update to `Addressee::Parent` (NOT the done-signal). |
| `send_message` | `Bus` | root, tl | Deliver to a child (`Inline`/`Worktree`) — **tree-edges only**. |
| `tree` | `Topology` | root, tl | Read-only: the caller's subtree (recursive ledger fold) + parent + per-node `pane_alive` liveness. |

Every tool implements `Tool::description()` (added to the trait); `exo-node`'s `tools/list`
emits it, so the toolset is self-documenting — an agent learns the local-merge loop (commit →
`submit_branch` → parent `merge`, no PR/remote) from the tools it has, not out-of-band.
`submit_branch`'s preconditions are an **ordered, extensible fn-pointer list** (`tools/submit.rs`)
mirroring the role hook fn-pointers — adding a gate (ahead-of-base, tests, a reviewer verdict) is
one entry.

## Roles

`role_def(kind)` returns a `RoleDef<R> { tools, pre_tool_use, stop, session_start }`. Hooks compose by pointing several roles at the same fn.

| Role | agent | tools | stop gate |
|------|-------|-------|-----------|
| **Root** | Claude | fork_wave, spawn_gemini, spawn_worker, merge, send_message | `stop_allow` (never gate the human's session) |
| **Tl** | Claude | spawns, merge, notify_parent, send_message, submit_branch | `stop` (clean-gate) |
| **Dev** | Gemini | notify_parent, submit_branch | `stop` (clean-gate) |
| **Worker** | Gemini | notify_parent | `stop_allow` (inline child, no own branch to submit) |

## The hooks

- **`pre_tool_use`** — default-**ALLOW** antipattern *nudge* (NOT a security gate). Currently one rule: deny `git add .` / `git add -A` (stage by path). Can `Deny` with guidance or `Modify` to rewrite.
- **`stop`** — the **local convergence gate** (`R: Git + Log`). Blocks exit while the worktree is dirty: a parent folds a child by merging its *branch* off disk, so uncommitted work is invisible to that merge. **Fails OPEN** on any error — a hook must never wedge an agent's turn-loop (that bricks the session). Root/Worker use `stop_allow` (nothing to fold).
- **`session_start`** — identity bootstrap (the node-identity context is prepended by `exo-node`).

## Gaps / not-yet

- **No convergence teardown.** `merge` folds the branch but nothing reclaims the child's worktree or kills its pane afterward (`Spawner::reclaim_worktree`/`kill_pane` exist but no tool calls them). After a fold the child pane + worktree linger. This is the main open lifecycle gap.
- **No reviewers.** The planned short-lived adversarial Gemini reviewer loop is not built — `merge` is an unguarded fold. The natural seam is `submit_branch`'s ordered check list (a reviewer-verdict gate) and/or a gate in `merge`.
- `pre_tool_use` is intentionally minimal (one nudge); classic exomonad's richer antipattern set + PII rewrite are not ported.
- `stop`'s dirty-gate can wedge an agent that holds untracked artifacts it won't commit (mitigated only by the agent being told to commit) — watch for this in smokes.
