# Phase 1: End-to-End Hylo Loop

Get a working recursive decomposition → implementation → PR fold loop with two levels.

## What Already Exists

| Component | Status |
|-----------|--------|
| `spawn_subtree` (Claude, worktree + tab) | Done |
| `spawn_workers` (Gemini, batch panes) | Done |
| Session forking (`--resume --fork-session`) | Done (just shipped) |
| `file_pr` (auto-detect parent branch) | Done |
| `note` / `question` / `answer_question` | Done (messaging system) |
| `wait_for_event` / `notify_parent` | Done (EventQueue) |
| Zellij STDIN injection effect | Done |
| Stop hooks (check uncommitted, PR status) | Done |
| Copilot review polling (`wait_for_copilot_review`) | Done |
| `merge_pr` tool | Not built |
| Event router (event → Zellij injection to parent) | Not built |
| GitHub poller (PR status → events) | Not built |
| Stop-hook rebase check | Not built |
| Dormant parent pattern (events as synthetic user msgs) | Not built |

## Implementation Waves

### Wave 1: Event Router (enables dormant parents)

The keystone piece. Without this, parents must poll.

**Deliverables:**
1. **Pane registry** — Server stores `(session_id → zellij_pane_id)` mapping. Populated at spawn time (child pane ID is returned by Zellij), and at init time (root TL pane).
2. **Event templates** — Haskell functions rendering structured events to natural language. Template per event type (child_complete, question, pr_ready, copilot_review, ci_status).
3. **Injection endpoint** — Server route that accepts an event, resolves target pane, renders template, calls `zellij action write-chars` + Enter.
4. **Hook wiring** — Child SessionEnd hook calls the injection endpoint (instead of/in addition to notify_parent).

**Files:**
- `haskell/wasm-guest/src/ExoMonad/Guest/Tools/Events.hs` — event template rendering
- `rust/exomonad-core/src/services/event_router.rs` — pane registry + injection logic
- `rust/exomonad-core/src/handlers/events.rs` — wire notify_event to injection
- `rust/exomonad/src/serve.rs` — injection HTTP endpoint

**Verification:** Spawn a subtree, have it call `notify_parent`. Parent's Zellij pane receives a natural language message. Parent Claude responds to it.

### Wave 2: `merge_pr` Tool + Rebase Check

**Deliverables:**
1. **`merge_pr` MCP tool** — Haskell tool definition + GitHub effect. Calls `gh pr merge`. Supports squash/merge strategy. Pulls merged changes into local worktree.
2. **Stop-hook rebase check** — In the existing stop-hook chain, add: fetch parent branch, check if ahead, rebase if needed. Conflict handling: attempt auto-resolve, send question to parent if stuck.

**Files:**
- `haskell/wasm-guest/src/ExoMonad/Guest/Tools/PR.hs` — merge_pr tool
- `haskell/wasm-guest/src/ExoMonad/Guest/Effects/GitHub.hs` — merge effect
- `proto/effects/github.proto` — MergePullRequest message
- `.exomonad/lib/StopHook.hs` — rebase check in stop hook

**Verification:** Parent merges child PR via `merge_pr`. Sibling's next stop-hook check detects the change and rebases.

### Wave 3: GitHub Poller + Copilot Feedback Loop

**Deliverables:**
1. **GitHub PR poller** — Background task in MCP server. Tracks PRs filed by agents. Polls for status changes (Copilot review, CI). Generates events routed via Wave 1 infrastructure.
2. **Child Copilot iteration** — When a child receives a Copilot review event (injected into its pane), it addresses the comments, pushes, and waits for the next review.

**Files:**
- `rust/exomonad-core/src/services/github_poller.rs` — polling loop + PR tracking
- `rust/exomonad/src/serve.rs` — start poller as background task
- Event templates for copilot_review and ci_status events

**Verification:** Child files PR. Copilot posts review. Child receives review as synthetic message, addresses comments, pushes. Repeat until green.

### Wave 4: Workers in Worktrees

Currently `spawn_workers` creates panes in the parent worktree (shared filesystem). For the hylo model, workers need their own worktrees + branches for PR filing.

**Deliverables:**
1. **Worker worktree isolation** — `spawn_workers` creates worktree per worker (like `spawn_leaf_subtree`)
2. **Worker PR filing** — Workers file PRs against parent branch on completion
3. **Worker stop hook** — Ensures commit, push, PR before exit

**Files:**
- `rust/exomonad-core/src/services/agent_control.rs` — worker worktree creation
- `.exomonad/roles/dev/Role.hs` — stop hook for workers

**Verification:** Spawn 3 workers. Each gets isolated worktree. Each files PR. PRs target parent branch.

## End-to-End Verification Scenario

```
1. Human gives root TL a multi-component task
2. Root writes spec commit (types, tests, ADR)
3. Root spawns 2 subtrees (session fork, each inherits context)
4. Subtrees each spawn 2 workers (Gemini, concrete tasks)
5. Workers implement, file PRs, iterate with Copilot
6. Workers go dormant when PRs are green
7. Subtree parents receive "[PR READY]" events
8. Subtrees review and merge worker PRs
9. Sibling workers rebase on stop-hook
10. Subtrees file PRs against root branch
11. Root reviews and merges subtree PRs
12. Root files final PR against main
13. Human reviews on GitHub
```

## Non-Goals (Phase 1)

- GitHub webhooks (polling is fine for v1)
- jj migration (git works, jj is a future optimization)
- Cloud-hosted MCP server (local Zellij only)
- Automatic depth adjustment (human approves depth > 2)
- Researcher/explorer node types
- Sleeptime evolution integration
