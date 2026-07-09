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
| `review.rs` | The domain's inter-node behavior: `ReviewSystem` (`D::System`) + `handle_review_system` (decision derived from structured findings; IO-free via the `SystemCtx` seam — unit-tested against a mock context). **Now persists each round to a durable `ReviewLog` (`ReviewRound`) at `.exo/reviews/{safe-branch}.json`** using the `safe_branch` helper. Also `handle_review_tick` — a reviewer's wall-clock abandonment timeout (`REVIEW_ABANDON_TIMEOUT`, 30 min), called from `ExoDomain::handle_tick` by the sidecar's watchdog loop instead of a Stop hook. |
| `spawn.rs` | `ExoSpawn` (`D::Spawn`) implementing `SpawnSpec`, the role-fixing the per-op tools do; `render_spec_prompt` (moved from the runtime) + `write_acceptance` (the `.exo/acceptance.md` write via `Fs`, relocated out of birth). |
| `domain.rs` | **Bin-only.** `ExoDomain` — the `Exomonad` impl that fixes `Caps = Runtime` and points `role_def`/`handle_system` at the lib. The one place that links `exo-runtime`. |
| `main.rs` | The CLI dispatcher (bin): clap `Cli` → `init` / `node` / `hook`. `node` is the composition root — `exo node --papers <path>` → `exo_node::bootstrap::<ExoDomain>(papers, cwd)` → `run_node::<ExoDomain>`. |
| `init.rs` | `exo init [--session <s>] [--recreate]` — bootstrap a node-mode ROOT (own tmux session, root papers, no server). Reuses `exo-runtime`/`exomonad-shared`. |
| `doctor.rs` | `exo doctor [--fix] [--include-unmerged]` — health-check + cleanup tool for worktrees. |
| `hook.rs` | `exo hook <event> --papers <path>` — handle a CC hook via the node's `exo` gate (SessionStart in-process; everything else routes to the sidecar hook socket, fail-open). |
| `config.rs` | Minimal node-mode init config read (`tmux_session`, `model`, the child-launch policy `yolo`/`wrap_nix`, and `[launch_profile.<role>]` tables flattened to `EXO_<ROLE>_*` for the reviewer-brain redirect) — classic `exomonad` owns the full `Config`. A missing `.exo/config.toml` defaults silently; a config file that exists but fails to read or parse is a loud error (`discover()` returns `anyhow::Result`) — a typo'd config must never be mistaken for an absent one. |
| `tools/` | One module per tool — a type + `Args` (derives `Deserialize + JsonSchema`) + `impl Tool<R>` (typed authoring trait; cap bounds in the impl header are the tool's least-privilege spec). The framework's `Adapter` handles JSON erasure; no per-tool adapter, no macro. Each ships mock-cap unit tests. |
| `gates.rs` | The concrete hook bodies: `pre_tool_use` (antipattern nudges), `session_start`. Functions generic over the caps they need. (There used to be a `stop` gate + per-role variants here — removed; see "The gates" below.) |
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
| `submit_branch` | `Git`+`Process`+`Spawner`+`Fs`+`Bus` | tl, dev | **Request review** — if reviewers are enabled (`review_enabled` in `.exo/config.toml`, off by default; read from the node's own papers). Runs the ordered precondition checks (committed → **needs_rebase** → `.exo/checks/pre-merge/*` scripts); the rebase gate blocks + prompts `git rebase <parent>` when the branch is behind its parent's current commit (fails open when the parent name isn't a live ref, e.g. root's `root`). Then spawns a **reviewer** off this branch (fork-point `git diff` base via `Git::merge_base`) and returns "stop & wait". It does NOT deliver `[READY]` itself except via the skip path — only the sidecar does, on an approve-verdict (the structural gate). **Continuity:** reads the latest `ReviewLog` and appends unresolved Error findings from the prior round to the reviewer task. Explicit escape hatch regardless of config: `dangerously_skip_reviewer: true`. |
| `verdict` | `Bus`+`Kv` | reviewer | A reviewer's one output → a `System(Reviewed)` message to its parent: `summary` + structured `findings` {`file`, `line`, `severity`, `body`, `suggestion`?}. Triggers reviewer teardown (handled in `exo-node`). |
| `notify_parent` | `Bus` | tl, dev, worker, reviewer | Status/failure update to `Addressee::Parent` (NOT the done-signal). |
| `send_message` | `Bus` | root, tl | Deliver to a child by name (`to: <child>`) — **tree-edges only**; inline vs worktree is transparent. |
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
fn-pointer list** (`tools/submit.rs`) mirroring the role hook fn-pointers — currently `committed`
(clean tree), `needs_rebase` (branch not behind its parent — prompts `git rebase <parent>`, keeps
the parent's fold conflict-free by resolving in the child's own context), and `pre_merge_checks`
(project `.exo/checks/pre-merge/*` scripts). Any check failing surfaces as a tool error the agent
acts on, before either the review-spawn or the skip-forward path.

## Roles

`role_def(kind)` returns a `RoleDef<R> { tools, pre_tool_use, session_start }`; `ExoDomain::role_def`
resolves through it (the domain's `Exomonad` impl), replacing the deleted `RoleRegistry`. Hooks compose by pointing several roles at the same fn.

Every role is a Claude instance; the **model** varies per role via `ExoRole::model()` (the `RoleKind::model` seam): `Some("sonnet")` for dev/worker/reviewer leaves, `Some("opus")` for a spawned tl, `None` (inherit the launcher's default) for root only. Every *spawned* node (everything but root) gets an explicit cap — never `None` — because "inherit the launcher's default" means whatever model tier the human's own top-level session happens to be set to, which is the human's choice for their own interactive use, not a choice made for subagent work (e.g. a human running a cheap/fast model for chat must not have that silently propagate onto a spawned TL's decomposition work). The model flows `RoleKind::model()` → `BirthCore.model` → `ClaudeSpawnFlags.model` → `build_agent_command`'s `--model`.

There is no per-role "stop gate" column anymore — Claude Code's `Stop` hook is no longer wired at
all (see "The gates" below for why). What each role needs from convergence/liveness now comes from
explicit tool calls (`submit_branch`, `verdict`) and the watchdog loop's wall-clock checks, not a
turn-boundary hook.

| Role | agent | tools |
|------|-------|-------|
| **Root** | Claude (inherits the launcher's default — the human's own session) | fork_wave, spawn_dev, spawn_worker, merge, send_message, tree |
| **Tl** | Claude (Opus) | spawns, merge, notify_parent, send_message, submit_branch, tree |
| **Dev** | Claude (Sonnet) | notify_parent, submit_branch |
| **Worker** | Claude (Sonnet) | notify_parent |
| **Reviewer** | Claude (Sonnet, or a launch-profile brain) | verdict, notify_parent |

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

**Reviewers are opt-in, off by default** — `review_enabled` in `.exo/config.toml` (inherited down
the tree onto every node's papers exactly like `yolo`/`wrap_nix`; unset ⇒
`NodePapers::DEFAULT_REVIEW_ENABLED = false`). Reviewers aren't a fully-cooked feature yet (see the
abandonment-timeout and nested-teardown history in this file), so a project turns them on
deliberately rather than getting them by surprise. `submit_branch` reads its own `review_enabled`
(`.exo/node.json`, via `Fs`) at call time; when it's off (or the agent explicitly passes
`dangerously_skip_reviewer: true`), it forwards `[READY]` straight to the parent, flagged as
unreviewed, with wording that differs by *why*: plain "reviewers are disabled for this project" when
it's the config default, vs. the loud "dangerously skipped, be suspicious" framing only when the
agent itself opted out of a normally-on gate.

When reviewers ARE enabled: a node commits, then calls `submit_branch`. It runs the checks, then
spawns a **reviewer** (a full Sonnet Claude in its own worktree branched off the under-review code)
handed the diff + `.exo/acceptance.md`. Its task prompt is explicit that review is **read-only** —
judge the diff, don't re-run the build/test suite — because a reviewer has a 30-minute wall-clock
abandonment timeout (`REVIEW_ABANDON_TIMEOUT`) and a cold build routinely blows well past that,
burning the whole round for nothing (see the tidepool forensics in "The gates" below).
**Cross-round continuity:** `submit_branch` reads the latest `.exo/reviews/{safe-branch}.json` and
appends any unresolved Error findings from the prior round to the reviewer's task string.
The reviewer calls `verdict`, which rides the bus as a `System` message to the submitter's
**sidecar**:
- **Reviewed** (no Error-severity findings) & sha==HEAD → the sidecar escalates `[READY]` to the parent — *no LLM turn*.
- **Reviewed** (with Error-severity findings) → findings are rendered and delivered into the submitter's LLM to address, then re-submit (new sha → fresh reviewer). **The verdict handler persists the round to the log.**
- **Aborted** (the reviewer never produced a verdict — see `handle_review_tick` below) → the
  submitter is told explicitly NOT to spawn another reviewer (a second one is likely to hit the same
  wall) and to re-submit with `dangerously_skip_reviewer: true` instead.

When reviewers are enabled, `submit_branch` never delivers `[READY]` itself except through the
skip path, so the gate is **structural** — the LLM has no other tool that fabricates approval. The
reviewer is torn down (best-effort) as soon as the `verdict` (or the abandonment timeout) is processed.

## The gates

- **`pre_tool_use`** — default-**ALLOW** antipattern *nudge* (NOT a security gate). Currently one rule: deny `git add .` / `git add -A` (stage by path). Can `Deny` with guidance or `Modify` to rewrite.
- **`session_start`** — identity bootstrap (the node-identity context is prepended by `exo-node`). The role's **steering protocol** (`RoleKind::protocol`, mapped to a `protocol.rs` const, override-or-const) is delivered via the launch-time `--append-system-prompt` flag at spawn; the SessionStart hook only appends the node-identity + team lines to `additionalContext`.

There used to be a third gate, `stop` (Claude Code's `Stop` event), with per-role variants
(`stop_allow`/`stop_notify`/`stop_dev`/`stop_reviewer`) — a TL/dev dirty-worktree exit-block and a
reviewer verdict-or-abort check. **It was removed entirely** (not neutered to `Allow` — a node's CC
settings no longer register `Stop` at all, so it's never invoked). Root cause, found live in a
production swarm: `Stop` fires on **every turn-end**, including a node legitimately yielding to wait
on a backgrounded async task (e.g. a reviewer polling a `cargo build`). It cannot distinguish
"genuinely done" from "paused" — confirmed against Claude Code's own docs, which offer no signal that
can (`SessionEnd` is the only turn-boundary-independent event, but it can't gate/block and has
undocumented gaps around hard kills). Every decision built on `Stop` was provably wrong some of the
time: a reviewer got killed ~1 second into a build wait, three submit rounds in a row, on the same
branch, before ever producing a verdict; a TL/dev got nagged "commit first" mid-async-wait; and the
`ChildIdle` busy-bit it fed produced false "subtree idle" reports that propagated up the tree.

What replaced it — each protection moved to a signal that's actually true regardless of turn
boundaries, not a hook:
- Reviewer "done" → the `verdict` tool (unchanged) — it was always the real signal; `Stop` was only
  ever consulted for the *negative* case.
- Reviewer "abandoned" → `review.rs`'s `handle_review_tick`, a wall-clock timeout
  (`REVIEW_ABANDON_TIMEOUT`, 30 min) run by `exo-node`'s watchdog loop (`Exomonad::handle_tick`),
  checked against real elapsed time, not a turn count. Delivers the same `ReviewAborted` the old
  `stop_reviewer` sent; the parent-side handling (`handle_review_system`) is unchanged.
- "Uncommitted work before converging" → already independently enforced by `submit_branch`'s own
  precondition check (`tools/submit.rs`) at the moment it actually matters (tool-call time, not
  turn-boundary time) — no Stop-time backstop needed.
- "Is my subtree still working" (`ChildLiveness`) → collapsed to pure pane-existence
  (`Tmux::list_panes`), dropping the busy-bit entirely. Coarser than the old claim, but the old claim
  was false; this one isn't. Its only remaining consumer is the cooperative-shutdown `Defer`
  response's cosmetic wording — the actual clear-to-reap gate was always `Topology`'s recursive pane
  walk, unaffected.
- Cooperative-shutdown reap-on-idle (`try_reap`) lost its `Stop`-triggered check point; the watchdog
  loop now calls it unconditionally every tick instead (it's idempotent and independently gated on
  `shutdown_pending` + subtree-clear, so this is a strict improvement, not a new heuristic).

## Gaps / not-yet

- **Reviewers:** now config-gated (`review_enabled`, off by default) instead of always-on. Still missing: a two-way colleague back-channel (submitter→reviewer reply) needs `send_message` on dev.
- `pre_tool_use` is intentionally minimal (one nudge); classic exomonad's richer antipattern set + PII rewrite are not ported.
- **Authoring-DSL Phase A LANDED** — typed `Tool` + `ErasedTool` flip: 9 hand-adapters deleted, roster uses `tool(X)`. Phase B (gate/observer stop pipelines, #20426 structural) is moot now that Stop-hook gating is gone; see [`docs/decisions/exo-authoring-dsl.md`](../../docs/decisions/exo-authoring-dsl.md).
