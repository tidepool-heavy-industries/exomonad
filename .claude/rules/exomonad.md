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
| `fork_wave` | root, tl | Fork N subtree-manager children, each in its own worktree. Per-child `fork_session` (default **false**) opts into inheriting the parent's context |
| `spawn_dev` | root, tl | Spawn a dev leaf in its own worktree+branch |
| `spawn_worker` | root, tl | Spawn an ephemeral worker in a tmux pane (no branch) |
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

- **Root** owns the user's intent, overall vision, and final result.
- **Tl** owns a subtree, its children, integration on its branch, and a complete submission upward.
- **Dev** owns a focused branch slice, commits it, and calls `submit_branch` when merge-ready.
- **Worker** performs a bounded task in its parent's worktree and reports with `notify_parent`.
- **Reviewer**, when enabled, independently judges a submitted slice and calls `verdict`.

Backend and model defaults are deployment configuration, not role doctrine; see
`rust/exo/CLAUDE.md` for the current mappings.

## The TL Protocol: Scaffold-Fork-Converge

Every TL at every level of the tree follows this protocol:

### 1. Scaffold

When it improves the handoff, commit the shared foundation children will build against:

- **Types and interfaces** that children implement
- **Test harness and fixtures** children will use
- **Stub files** showing where children put their code
- **CLAUDE.md additions** scoping this TL's domain
- **Inline TODO prompts** that state the next action near the relevant interface

This scaffold is a decomposition artifact. It may contain explicit placeholders or failing tests and
need not compile or be globally green; its purpose and current lifecycle stage must be legible.
Children fork from the commit, so uncommitted files are invisible to worktree children.

### 2. Fork (spawn wave)

Spawn children for wave N. Zero dependencies between siblings in the same wave.

- **Sub-TLs**: `fork_wave`. `fork_session` defaults to **false** — context inheritance is opt-in per child, not automatic.
- **Devs**: `spawn_dev` (worktree). They receive compact task information; the scaffold and repository guidance provide the richest context.
- **Workers**: `spawn_worker` (ephemeral pane). Use for bounded research or explicitly authorized in-place edits.

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

Spawn prompts carry task information, not a second persona or generic lifecycle ritual. Start with
the objective and observable done criteria. Add only useful context not already legible from the
scaffold, mechanically checked scope, a small read-first list, task-specific constraints, optional
step hints, and relevant verification. Empty sections disappear. Prefer repository-relative paths
and concrete commands; do not demand line-by-line implementation when the child can exercise
judgment within its scope.

## Convergence Protocol

Submission has a strong meaning: the child offers a complete, merge-ready assigned subtree. The
parent owns review at fold time; an independent reviewer is an optional additional gate:

1. Leaf commits, calls `submit_branch`.
2. `submit_branch` runs its precondition checks in order: working tree clean → **rebase gate** (blocks with a `git rebase <parent>` prompt if the branch is behind its parent's current commit; fails open when the parent isn't a live ref, e.g. root) → project `.exo/checks/pre-merge/*` scripts.
3. **If `review_enabled` is set** in `.exo/config.toml` (inherited down the tree; **off by default**), `submit_branch` spawns a one-shot reviewer in its own worktree off the under-review diff. The reviewer reads the diff + `.exo/acceptance.md` + (when passed) the submitter's receipts, read-only (no build/test), and calls `verdict` with structured findings. Its task carries three prompt-level lenses beyond plain correctness — RECEIPTS (audit `deviations` against the diff), SCOPE (an undeclared out-of-scope file vs. any ALLOWED PATHS list is an Error), and DUPLICATION (an undeclared reimplementation of an existing mechanism, checked via the touched directories' CLAUDE.md, is an Error) — prompting only, no new mechanical scope/dup check. Severity is calibrated to the parent's actual fold decision: `error` means the parent would be right to REFUSE the fold; when unsure, the reviewer picks `warning`.
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
- Messages arrive as `[from: X, kind: Y]` notifications from the recipient's sidecar over the durable bus (large payloads may arrive as a one-line `@`-file reference to read). Codex delivery uses `codex queue`; Claude delivery uses the Monitor-armed `exo listen` client. If the harness delivery path is unavailable, messages remain queued and drain when it reconnects. tmux remains for spawning and observability, not message delivery.
- The notification vocabulary is `[READY]` (converged, parent should merge), `[idle]`, `[FAILED: id]`, `[CHILD DIED: name]` (the watchdog observed a child's pane dead while un-reaped — its ledger state is `Died`; check `tree`, then merge what its branch holds or respawn), and `[CHILDREN DIED: N]` (the batched form — one message per watchdog tick when several deaths land in the same scan, e.g. a mass teardown or the first scan over a pre-lifecycle ledger). Acknowledge `Died` tombstones with `exo doctor --fix`, which records `Reaped` for reclaimed corpses and for dead children with no worktree left. There is no Copilot-era vocabulary (`[FIXES PUSHED]`, `[PR READY]`, `[REVIEW TIMEOUT]`) in v2.
- Child events are pushed, so repeated status polling is unnecessary. A manager may continue useful coordination, integration, investigation, or direct work while children run.
