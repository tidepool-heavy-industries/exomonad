# Phase 1: Two-Level Hylomorphism

Get a working recursive decomposition with one level of subtrees and worker nodes. Everything uses existing primitives with minimal changes.

## Delta from Current

| What exists | What changes |
|-------------|-------------|
| `spawn_agents` branches off main | `spawn_subtree` branches off current branch |
| `spawn_gemini_teammate` creates pane | `spawn_worker` — same mechanics (in-place pane), clearer intent |
| Stop hooks check PR status | Stop hooks file PR against parent branch (not main) |
| `get_messages` returns immediately | `get_messages` long-polls with timeout |
| Copilot reviews exist | Copilot + CI as first-pass gate before parent reviews |

## Implementation Steps

### 1. `spawn_subtree` tool

Haskell tool definition + effect. Reuses existing worktree creation machinery.

Key differences from `spawn_agents`:
- `base_branch` param (current branch, not main)
- Launches with tl role (has spawn tools)
- Context via inline prompt, not just GitHub issue
- Branch naming: `{current_branch}.{subtask_name}` (uses `.` hierarchy)

Proto: extend `SpawnAgentRequest` with `base_branch` field.
Rust: `agent_control.rs` — worktree creation already supports arbitrary base, just need to plumb it.
Haskell: new tool in `ExoMonad.Guest.Tools.Spawn` or similar.

### 2. `spawn_worker` tool

Essentially `spawn_gemini_teammate` with a clearer name and tool description that teaches the base case.

May be a rename + description update, or a new tool that wraps the same machinery. Either way, the description is the important part: "Use when work is concrete enough to execute without further decomposition." Runs in-place in parent's worktree.

### 3. Long-poll `get_messages`

Modify the messaging effect handler to support blocking.

Rust side: `MessagingHandler::get_agent_messages` watches the inbox directory with `notify` (or simple poll loop) and returns when a message appears or timeout expires.

Proto: add `timeout_secs` field to `GetAgentMessagesRequest`.

### 4. PR against parent branch

Modify stop hook or add `file_pr` tool.

The branch naming convention (`parent.child`) makes parent branch detection mechanical: strip the last component.

Stop hook change: `gh pr create --base {parent_branch}` instead of `--base main`.

### 5. Deprecate `spawn_agents` / `spawn_gemini_teammate`

Remove from tool definitions. Keep internal machinery as shared code.

## Verification

1. Root node decomposes a task into 2 subtrees
2. Each subtree spawns 1-2 worker nodes
3. Worker nodes complete, file PRs against subtree branch
4. Copilot reviews, CI runs
5. Subtree node reviews + merges child PRs
6. Subtree node files PR against root branch
7. Root reviews + merges
8. Final PR against main

## Open Questions

- **Context vehicle for `spawn_subtree`**: Inline prompt? GitHub issue? Markdown file in worktree? Inline is simplest for phase 1.
- **jj in phase 1 or phase 2?** Git works, jj is better. Could go either way.
- **Depth limit:** Phase 1 = max 2 levels (root → subtree → worker). Enforce in tool or just convention?

## Non-Goals (Phase 1)

- Cloud-hosted MCP server
- Automatic evolution / learned facts moving into code
- jj migration (unless trivially easy)
- Researcher / code explorer node types
- TUI for human observation (human uses GitHub + terminal)
