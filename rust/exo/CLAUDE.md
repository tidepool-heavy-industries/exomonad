# exo — the v2 node-mode binary (CLI) + the domain (tools / roles / gates)

`exo` is the **standalone v2 node-mode binary** — it owns the whole node-mode CLI surface
(`exo init` / `exo node` / `exo hook`; classic `exomonad` is server-only). The lib half is the minimal **domain usage** of
[`exo-framework`](../exo-framework/CLAUDE.md): the genuinely domain-specific Bucket-C logic that
ports from the old Haskell DSL — the MCP tool set, the per-role roster, and the CC hook gates. This
is the "small usage" half of the framework/domain split (the Rust analog of Classic's Haskell-WASM
config DSL). Written **generic over the `exo-caps` traits** (no `dyn Caps`), so least-privilege is
compiler-checked and every tool is **unit-testable against mock caps with zero IO**.

The engine never depends on this crate's lib. The binary's bin-only `domain.rs` defines `ExoDomain`
(the [`Exomonad`](../exo-framework/CLAUDE.md) impl: `Caps = Runtime`, `Role = ExoRole`, `System =
ReviewSystem`, `Spawn = ExoSpawn`) and monomorphizes the engine once as `run_node::<ExoDomain>`;
that's the seam (the fn-pointer `RoleRegistry` is gone). See
[`docs/decisions/exo-framework-domain-split.md`](../../docs/decisions/exo-framework-domain-split.md)
and [`docs/decisions/exo-trait-refactor.md`](../../docs/decisions/exo-trait-refactor.md).

The CLI modules (`main.rs` / `init.rs` / `hook.rs` / `config.rs`) are **bin-only** — they link the
v2/shared seam (`exo-node`, `exo-runtime`, `exomonad-shared`) but never classic `exomonad-core`. The
lib (`lib.rs` + `tools/` + `gates.rs` + `roles.rs`) stays generic over the caps and links neither.

> Part of the v2 node-mode swarm (the `exo` binary). See `rust/CLAUDE.md`.

## Shape

| File | Contents |
|------|----------|
| `lib.rs` | Re-exports `role_def`, `ExoRole`, `ReviewSystem`/`handle_review_system` (the domain `System` + relocated gate: findings-based), `ExoSpawn` (the domain `Spawn`). Generic over `R`, depends only on `exo-framework` + `exo-caps` (+ `tracing`). |
| `review.rs` | The domain's inter-node behavior: `ReviewSystem` (`D::System`) + `handle_review_system` (decision derived from structured findings; IO-free via the `SystemCtx` seam — unit-tested against a mock context). **Now persists each round to a durable `ReviewLog` (`ReviewRound`) at `.exo/reviews/{safe-branch}.json`** using the `safe_branch` helper. |
| `spawn.rs` | `ExoSpawn` (`D::Spawn`) implementing `SpawnSpec`, the role-fixing the per-op tools do; `render_spec_prompt` (moved from the runtime) + `write_acceptance` (the `.exo/acceptance.md` write via `Fs`, relocated out of birth). |
| `domain.rs` | **Bin-only.** `ExoDomain` — the `Exomonad` impl that fixes `Caps = Runtime` and points `role_def`/`handle_system` at the lib. The one place that links `exo-runtime`. |
| `main.rs` | The CLI dispatcher (bin): clap `Cli` → `init` / `node` / `hook`. `node` is the composition root — `exo node --papers <path>` → `exo_node::bootstrap::<ExoDomain>(papers, cwd)` → `run_node::<ExoDomain>`. |
| `init.rs` | `exo init [--session <s>] [--recreate]` — bootstrap a node-mode ROOT (own tmux session, root papers, no server). Reuses `exo-runtime`/`exomonad-shared`. |
| `doctor.rs` | `exo doctor [--fix] [--include-unmerged]` — health-check + cleanup tool for worktrees. |
| `hook.rs` | `exo hook <event> --papers <path>` — handle a CC hook via the node's `exo` gate (SessionStart in-process; everything else routes to the sidecar hook socket, fail-open). |
| `config.rs` | Minimal node-mode init config read (`tmux_session`, `model`, the child-launch policy `yolo`/`wrap_nix`, and `[launch_profile.<role>]` tables flattened to `EXO_<ROLE>_*` for the reviewer-brain redirect) — classic `exomonad` owns the full `Config`. |
| `tools/` | One module per tool — a type + `Args` (derives `Deserialize + JsonSchema`) + `impl Tool<R>` (typed authoring trait; cap bounds in the impl header are the tool's least-privilege spec). The framework's `Adapter` handles JSON erasure; no per-tool adapter, no macro. Each ships mock-cap unit tests. |
| `gates.rs` | The concrete hook bodies: `pre_tool_use` (antipattern nudges), `stop` (the convergence gate) + per-role variants (`stop_allow`/`stop_notify`/`stop_reviewer`), `session_start`. Functions generic over the caps they need. |
| `roles.rs` | `ExoRole` (the domain's `D::Role`, impl `RoleKind`) + `role_def(ExoRole)` — the hand-written table (the single place a role's tool list + hooks are named), resolved through `ExoDomain`'s `Exomonad::role_def`. `RoleKind::protocol` is overridden here to map each variant to its `protocol.rs` const. |
| `protocol.rs` | Per-role **decomposition-steering protocol** consts (`ROOT`/`TL`/`DEV`/`WORKER`/`REVIEWER`) — the prose the engine injects at session_start. The **source of truth** (ported from `.exo/roles/devswarm/context/*.md`, translated to v2 mechanics: local `merge` + `submit_branch`, no PRs/Copilot); an optional on-disk `.md` override wins during prompt-tuning. |
| `testing.rs` | `MockRuntime` — impls every cap, records calls, returns canned values. Every tool tests against this one shared mock. |

The `Tool<R>` trait, `RoleDef<R>`, the hook decision enums, and `PolicyCaps` are the framework
contract ([`exo-framework`](../exo-framework/CLAUDE.md)); this crate provides the concrete instances.

## The tools

| Tool | Caps | Roles | What it does |
|------|------|-------|--------------|
| `fork_wave` | `Spawner` | root, tl | Fork N Claude TL children (own worktrees). Per-child opt-in `fork_session: bool` (default false) inherits the parent's context via `--resume --fork-session`; default-false launches fresh. |
| `spawn_dev` | `Spawner` | root, tl | Spawn a Sonnet Claude dev in its own worktree. |
| `spawn_worker` | `Spawner` | root, tl | Spawn an ephemeral Sonnet Claude worker (inline pane). |
| `dismiss_worker` | `Spawner` | root, tl | Dismiss an inline worker by name: unconditional parent-side `kill_pane` resolved via the children ledger. Matched to `spawn_worker`; the reliable teardown primitive for workers that never registered as a teammate. |
| `merge` | `Git`+`Spawner` | root, tl | **The local fold:** `git merge <child-branch>`, followed by best-effort teardown (`kill_pane` + `reclaim_worktree`) of the child. |
| `submit_branch` | `Git`+`Process`+`Spawner`+`Fs`+`Bus` | tl, dev | **Request review.** Runs the precondition checks (committed + `.exo/checks/pre-merge/*` scripts), then spawns a **reviewer** off this branch (fork-point `git diff` base via `Git::merge_base`) and returns "stop & wait". It does NOT deliver `[READY]` — only the sidecar does, on an approve-verdict (the structural gate). **Continuity:** reads the latest `ReviewLog` and appends unresolved Error findings from the prior round to the reviewer task. Escape hatch: `dangerously_skip_reviewer: true`. |
| `verdict` | `Bus`+`Kv` | reviewer | A reviewer's one output → a `System(Reviewed)` message to its parent: `summary` + structured `findings` {`file`, `line`, `severity`, `body`, `suggestion`?}. Triggers reviewer teardown (handled in `exo-node`). |
| `notify_parent` | `Bus` | tl, dev, worker, reviewer | Status/failure update to `Addressee::Parent` (NOT the done-signal). |
| `send_message` | `Bus` | root, tl | Deliver to a child (`Inline`/`Worktree`) — **tree-edges only**. |
| `tree` | `Topology`+`Fs` | root, tl | Read-only: the caller's subtree (recursive ledger fold) + parent + per-node `pane_alive` liveness, plus a `(label)` for any launch-profiled node (e.g. a Kimi reviewer). |

## exo doctor

`exo doctor` is a health-check and cleanup tool for node-mode workspaces. It audits the `.exo/worktrees/` directory and identifies stale (merged) worktrees.

- **Dry-run (default)**: `exo doctor` reports merged and unmerged worktrees but removes nothing.
- **Reclaim**: `exo doctor --fix` reclaims merged worktrees by running `git worktree remove --force` and deleting their associated branches.
- **Force**: `exo doctor --fix --include-unmerged` reclaims even unmerged worktrees (dangerous).

Worktrees are considered reclaimable if their HEAD is an ancestor of the current branch's HEAD. The current worktree (repo root) is never removed.

Every tool implements `Tool::description()`; `exo-node`'s `tools/list` emits it, so the toolset is
self-documenting — an agent learns the local-merge loop (commit → `submit_branch` → parent `merge`,
no PR/remote) from the tools it has. `submit_branch`'s preconditions are an **ordered, extensible
fn-pointer list** (`tools/submit.rs`) mirroring the role hook fn-pointers.

## Roles

`role_def(kind)` returns a `RoleDef<R> { tools, pre_tool_use, stop, session_start }`; `ExoDomain::role_def`
resolves through it (the domain's `Exomonad` impl), replacing the deleted `RoleRegistry`. Hooks compose by pointing several roles at the same fn.

Every role is a Claude instance; the **model** varies per role via `ExoRole::model()` (the `RoleKind::model` seam): `Some("sonnet")` for dev/worker/reviewer leaves, `None` (session default — the human's Opus) for root/tl. The model flows `RoleKind::model()` → `BirthCore.model` → `ClaudeSpawnFlags.model` → `build_agent_command`'s `--model`.

| Role | agent | tools | stop gate |
|------|-------|-------|-----------|
| **Root** | Claude (session default) | fork_wave, spawn_dev, spawn_worker, merge, send_message, tree | `stop_allow` (never gate the human's session; no parent to notify) |
| **Tl** | Claude (session default) | spawns, merge, notify_parent, send_message, submit_branch, tree | `stop` (clean-gate + notify parent on clean-allow) |
| **Dev** | Claude (Sonnet) | notify_parent, submit_branch | `stop_notify` (notify parent, always allow) |
| **Worker** | Claude (Sonnet) | notify_parent | `stop_notify` (inline child, no branch to fold, but still signals on yield) |
| **Reviewer** | Claude (Sonnet, or a launch-profile brain) | verdict, notify_parent | `stop_reviewer` (ephemeral; its `verdict` is its done-signal; emits `ReviewAborted` if it exits without one) |

The **reviewer** and ephemeral in-pane **worker** roles carry a **launch profile** (`ExoRole::launch_profile_env_prefix` → `Some("EXO_REVIEWER")` / `Some("EXO_WORKER")`): their Claude can be redirected to a non-default Anthropic-compatible endpoint/model (e.g. Kimi via a local `claude-code-proxy`) — still a Claude process, so Teams/hooks/MCP are unchanged (the old Gemini-worker slot, now Kimi). Configure per-role in `.exo/config.toml` (the convenient path) — the **named-brain shorthand** is the common case:
```toml
[launch_profile]
reviewer = "kimi"          # built-in named brain → proxy endpoint + model + label
worker   = "kimi"          # no auth_token needed: the proxy holds the OAuth
```
`"kimi"` is the one built-in brain today (`config.rs::named_brain` — the **only place a vendor is named**; the runtime/seam stays backend-agnostic). For a custom/unknown backend, the **full-table form** still works and overrides the shorthand:
```toml
[launch_profile.reviewer]
base_url = "http://localhost:18765"
model = "kimi-for-coding"
auth_token = "sk-…"        # OPTIONAL — omit for a local proxy; a real key can also live in the shell env (env wins)
label = "kimi"             # tags the window + tree
```
`config.rs` resolves each profile (`Named` shorthand via `named_brain`, an unknown name is a loud skip) and flattens to `EXO_<ROLE_UPPER>_*`; `init.rs` embeds them in the root launch (a matching shell `EXO_*` overrides), the tree propagates, and `exo-runtime` resolves them (see its CLAUDE.md — the profile **activates on `BASE_URL`**, the token is optional). Omit a role ⇒ it stays the default Sonnet. Adding another role/backend = one arm in `launch_profile_env_prefix` (+ optionally a `named_brain` entry).

## The review gate (how `submit_branch` → `merge` is gated)

A node commits, then calls `submit_branch`. It runs the checks, then spawns a **reviewer** (a full
Sonnet Claude in its own worktree branched off the under-review code) handed the diff + `.exo/acceptance.md`.
**Cross-round continuity:** `submit_branch` reads the latest `.exo/reviews/{safe-branch}.json` and
appends any unresolved Error findings from the prior round to the reviewer's task string.
The reviewer calls `verdict`, which rides the bus as a `System` message to the submitter's
**sidecar**:
- **Reviewed** (no Error-severity findings) & sha==HEAD → the sidecar escalates `[READY]` to the parent — *no LLM turn*.
- **Reviewed** (with Error-severity findings) → findings are rendered and delivered into the submitter's LLM to address, then re-submit (new sha → fresh reviewer). **The verdict handler persists the round to the log.**

`submit_branch` never delivers `[READY]` itself, so the gate is **structural** — the LLM has no
tool that skips review. The reviewer is torn down (best-effort) as soon as the `verdict` is processed.

## The gates

- **`pre_tool_use`** — default-**ALLOW** antipattern *nudge* (NOT a security gate). Currently one rule: deny `git add .` / `git add -A` (stage by path). Can `Deny` with guidance or `Modify` to rewrite.
- **`stop`** (tl) — the **local convergence gate** (`R: Git + Log + Bus + ChildLiveness`). Blocks exit while the worktree is dirty (a parent folds a child by merging its *branch* off disk, so uncommitted work is invisible). On a **clean** exit (Allow) it delivers a `System(ChildIdle)` to the parent — but **only when the subtree is idle** (`ChildLiveness::any_child_busy`). **Fails OPEN** on any git error.
- **`stop_notify`** (dev, worker) — leaf turn-end hook (CC `Stop`; `R: Bus + Log + ChildLiveness`): deliver `System(ChildIdle)` (but only when the subtree is idle; skip if a child is busy), then **always Allow**.
- **`stop_allow`** (root) — unconditional Allow. Root has no parent.
- **`stop_reviewer`** (reviewer) — silent on the happy path (verdict produced); emits a loud `ReviewAborted` to the parent if the reviewer exits without a verdict. Always allows exit.
- **`session_start`** — identity bootstrap (the node-identity context is prepended by `exo-node`). The role's **steering protocol** (`RoleKind::protocol`, mapped to a `protocol.rs` const, override-or-const) is delivered via the launch-time `--append-system-prompt` flag at spawn; the SessionStart hook only appends the node-identity + team lines to `additionalContext`.

## Gaps / not-yet

- **Reviewers:** review is currently always-on (no config to disable); a two-way colleague back-channel (submitter→reviewer reply) needs `send_message` on dev.
- `pre_tool_use` is intentionally minimal (one nudge); classic exomonad's richer antipattern set + PII rewrite are not ported.
- `stop`'s dirty-gate can wedge an agent that holds untracked artifacts it won't commit.
- **Authoring-DSL Phase A LANDED** — typed `Tool` + `ErasedTool` flip: 9 hand-adapters deleted, roster uses `tool(X)`. Phase B (gate/observer stop pipelines, #20426 structural) still pending; see [`docs/decisions/exo-authoring-dsl.md`](../../docs/decisions/exo-authoring-dsl.md).
