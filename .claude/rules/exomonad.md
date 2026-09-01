---
description: "ExoMonad agent orchestration rules — loaded into every agent's context in projects using exomonad"
---

# ExoMonad Agent Rules

> This file describes the v2 Node-Mode loop (the `exo` binary). Classic (`exomonad serve`)
> specifics — `file_pr`/`merge_pr`, Copilot review, PR-based convergence — live in
> [`rust/exomonad/CLAUDE.md`](../../rust/exomonad/CLAUDE.md).

## Model

ExoMonad is a hylomorphism over context windows. Unfold = plan + scaffold + spawn. Fold = merge + integrate + surface upward. Each agent is a triad: worktree (filesystem) + context window (attention) + actor (messages). See `CLAUDE.md` § Model for the full conceptual framework.

## MCP Tools

Use exomonad MCP tools for orchestration. Git operations use the `git` CLI directly, NOT MCP tools — v2 has no GitHub cap (convergence is local `git merge`, no PR/Copilot).

| Tool | Role | What it does |
|------|------|-------------|
| `fork_wave` | root, tl | Fork N parallel Claude TL children, each in its own worktree. Per-child `fork_session` (default **false**) opts into inheriting the parent's context |
| `spawn_dev` | root, tl | Spawn a Sonnet Claude dev in its own worktree+branch |
| `spawn_worker` | root, tl | Spawn an ephemeral Sonnet Claude worker in a tmux pane (no branch) |
| `dismiss_worker` | root, tl | Tear down an inline worker by name (parent-side `kill_pane`) |
| `request_review` | root, tl | Mid-flight flip of a named child's review gate to ON (for its next `submit_branch`); one-way, no un-flip. Refuses on an unknown or terminal (reaped/died) child. Idempotent |
| `amend_boundary` | root, tl | Fix a wrong recorded file boundary (see [`rust/exo/CLAUDE.md`](../../rust/exo/CLAUDE.md) § Fold-time file boundary) — full-replace `allowed` list for a child, parent-side only (`.exo/boundaries/{child}.json` in YOUR OWN worktree). Amends an existing boundary only — refuses if the child was never spawned with `file_boundary` |
| `merge` | root, tl | The local fold: `git merge <child-branch>` + best-effort child teardown. Optional `gate` command runs post-merge, pre-teardown — on success its output tail rides in `data.gate.output_tail`. Optional `gate_timeout_ms` bounds the gate and kills its whole process group on expiry, rendered like a gate failure but labeled TIMED OUT |
| `submit_branch` | tl, dev | Request review/convergence — see "Convergence Protocol" below |
| `verdict` | reviewer | A reviewer's one output: `summary` + structured `findings` → a message to its parent |
| `notify_parent` | tl, dev, worker, reviewer | Status/failure update to the parent (NOT the done-signal) |
| `send_message` | root, tl | Deliver to a named child (tree-edges only) |
| `broadcast` | root, tl | Flat fan-out: the same `text` to every LIVE direct child (no role filtering) — same delivery path as `send_message`, one call per child |
| `tree` | root, tl | Read-only: the caller's subtree + parent + per-node pane-liveness |

`file_pr` and `merge_pr` do not exist in v2 — they are Classic-only.

## Agent Hierarchy

Every role runs as a Claude instance; the **model** varies per role (`RoleKind::model`):

- **Root**: inherits the launcher's default (the human's own top-level `exo init` session — never spawned via `birth`, so this is the human's own model choice).
- **Tl (spawned Tech Lead)**: Opus. Decomposes, specs, scaffolds, spawns, and merges. Delegates substantial independent work by default; may directly handle small work, integration, conflicts, diagnostics, or work where delegation costs more than execution.
- **Dev (leaf)**: Sonnet. Implements a focused spec, commits, `submit_branch`. No spawning.
- **Worker**: Sonnet. Ephemeral pane, no branch. Research or in-place edits. May run on an alternate launch-profile brain (e.g. Kimi) if configured.
- **Reviewer**: Sonnet (or a launch-profile brain). Short-lived, spawned by `submit_branch` when reviewers are enabled; reads the diff read-only and calls `verdict`.

## The TL Protocol: Scaffold-Fork-Converge

Every TL at every level of the tree follows this protocol:

### 1. Scaffold

Before spawning any children, commit the shared foundation they'll build against:

- **Types and interfaces** that children implement
- **Test harness and fixtures** children will use
- **Stub files** showing where children put their code
- **CLAUDE.md additions** scoping this TL's domain

Commit. Children fork from this commit.

### 2. Fork (spawn wave)

Spawn children for wave N. Zero dependencies between siblings in the same wave.

- **Sub-TLs**: `fork_wave` (Claude, Opus). `fork_session` defaults to **false** — context inheritance is opt-in per child, not automatic.
- **Devs**: `spawn_dev` (Sonnet Claude, worktree). They get a self-contained spec. The CLAUDE.md from the scaffolding commit gives them project context.
- **Workers**: `spawn_worker` (Sonnet Claude, ephemeral pane). Research, boilerplate, or non-conflicting edits.

### 3. Converge (merge wave)

Wait for children to complete (notifications arrive from your `exo listen` monitor and wake you between turns). Merge their branches sequentially with `merge`. Then write an **integration commit**:

- Wire children's outputs together
- Run integration tests
- Fix integration bugs

### 4. Next wave (if any)

Wave N+1 depends on merged wave N. Repeat from step 2.

### 5. Submit to parent

After all waves are merged and integrated, call `submit_branch` against the parent's branch.

## Spec Quality

Specs are self-contained — the leaf has no context from previous attempts. Every spec must include:

1. **Anti-patterns** (FIRST) — known failure modes as explicit DO NOT rules
2. **Read first** — exact files to read (CLAUDE.md, source files)
3. **Steps** — numbered, each step = one concrete action with code snippets
4. **Verify** — exact build/test commands
5. **Done criteria** — what "done" looks like

Include complete code snippets. Name every file by full path. Include exact commands, not "run the tests."

## Convergence Protocol

The TL does NOT iterate on children's work. Convergence is **leaf + reviewer**, not TL:

1. Leaf commits, calls `submit_branch`.
2. `submit_branch` runs its precondition checks in order: working tree clean → **rebase gate** (blocks with a `git rebase <parent>` prompt if the branch is behind its parent's current commit; fails open when the parent isn't a live ref, e.g. root) → project `.exo/checks/pre-merge/*` scripts.
3. **If `review_enabled` is set** in `.exo/config.toml` (inherited down the tree; **off by default**), `submit_branch` spawns a one-shot Sonnet reviewer in its own worktree off the under-review diff. The reviewer reads the diff + `.exo/acceptance.md` + (when passed) the submitter's receipts, read-only (no build/test), and calls `verdict` with structured findings. Its task carries three prompt-level lenses beyond plain correctness — RECEIPTS (audit `deviations` against the diff), SCOPE (an undeclared out-of-scope file vs. any ALLOWED PATHS list is an Error), and DUPLICATION (an undeclared reimplementation of an existing mechanism, checked via the touched directories' CLAUDE.md, is an Error) — prompting only, no new mechanical scope/dup check. Severity is calibrated to the parent's actual fold decision: `error` means the parent would be right to REFUSE the fold; when unsure, the reviewer picks `warning` — a false block costs a full round-trip, a missed nit costs nothing.
   - No Error-severity findings, sha matches HEAD → the sidecar escalates `[READY]` to the parent directly — no LLM turn.
   - Error-severity findings → rendered into the submitter's context to address, then re-submit.
   - Reviewer abandoned (30-min wall-clock timeout via the watchdog tick, not a hook) → submitter is told to re-submit with `dangerously_skip_reviewer: true` instead of spawning another reviewer.
4. **If reviewers are disabled** (the default) or `dangerously_skip_reviewer: true` is passed, `submit_branch` forwards `[READY]` straight to the parent, flagged as unreviewed.
5. TL calls `merge` when `[READY]` arrives.

The structured reviewer owns branch review. A TL owns integration, conflict resolution, and verification after folds. There is no Copilot in v2 — Copilot review and `file_pr`/`merge_pr` are Classic-only.

`merge` accepts any local ref, not just a tracked child's branch — this is the supported succession escape hatch for dead-TL recovery (folding an orphaned descendant's branch back into a live ancestor); pane/worktree reclaim only works for your own ledger children and is best-effort otherwise. An optional `gate` command runs after the merge commits and before teardown; a failed gate leaves the child alive (merge stays committed, teardown is skipped) so it can fix its work and be re-merged.

## Branch Naming

`{parent_branch}.{slug}` (dot separator). The last dot-segment IS the `AgentName` — one namespace, zero translation. Branches converge to the parent branch, not main — via local `git merge`, folded up the tree.

## State Machines

Review round-tripping is tracked via a durable `ReviewLog` (`ReviewRound`) persisted to `.exo/reviews/{safe-branch}.json`, appended by the sidecar's `handle_domain` on each `verdict`. There is no `Stop` hook and no stop-hook-based state machine — Claude Code's `Stop` event is not wired at all in v2 (it fired on every turn-end, including legitimate async-wait yields, and couldn't distinguish "done" from "paused"). Time-based logic (e.g. the reviewer's 30-minute abandonment timeout) is handled by each node's **watchdog tick loop** instead, running on wall-clock elapsed time.

## Communication

- `notify_parent` for completion/failure/status updates to parent (not the convergence signal — that's `submit_branch` → `verdict` → `[READY]`)
- `send_message` for peer-to-peer messaging to a named child (tree-edges only)
- Messages arrive as `[from: X, kind: Y]` notifications from the recipient's Monitor-armed `exo listen` client, delivered by its own sidecar off the durable bus (large payloads arrive as a one-line `@`-file reference to read). Every node arms that monitor as its **first action** — the SessionStart hook injects the exact command. An unarmed node's messages **queue durably** (senders see a ⚠ "no active listener" note in their tool responses; `tree` shows `wake:-`) and drain the moment the monitor connects. CC Agent Teams native delivery was retired (as of Claude Code 2.1.178 a solo session-lead never drains its own Teams inbox), and its tmux-paste successor was cut as delivery too (fragile TUI typing, indistinguishable from user input) — tmux survives for spawning and human observability only. `exo` owns its delivery channel end to end; no native CC team tools are used.
- The notification vocabulary is `[READY]` (converged, parent should merge), `[idle]`, `[FAILED: id]`, `[CHILD DIED: name]` (the watchdog observed a child's pane dead while un-reaped — its ledger state is `Died`; check `tree`, then merge what its branch holds or respawn), and `[CHILDREN DIED: N]` (the batched form — one message per watchdog tick when several deaths land in the same scan, e.g. a mass teardown or the first scan over a pre-lifecycle ledger). Acknowledge `Died` tombstones with `exo doctor --fix`, which records `Reaped` for reclaimed corpses and for dead children with no worktree left. There is no Copilot-era vocabulary (`[FIXES PUSHED]`, `[PR READY]`, `[REVIEW TIMEOUT]`) in v2.
- Child events are pushed. A TL never polls; it continues useful non-overlapping work and yields only when nothing useful remains.
