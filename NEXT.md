# Hylo Model: Next Steps

## Critical: Branch Naming / PR Base Detection Conflict

**`spawn_gemini_teammate` uses `-` separator** (line 613 of agent_control.rs):
```
branch_name = format!("{}-{}", base_branch, slug)
# Example: main-my-feature
```

**`file_pr` auto-detection uses `/` separator** (file_pr.rs:48-50):
```rust
if let Some(pos) = head.rfind('/') {
    head[..pos].to_string()  // "parent/child" → "parent"
}
```

**These don't match.** A branch named `main-my-feature` won't auto-detect `main` as base — it'll fall through to the `"main"` default, which happens to be correct only at the root level. At depth >1 it silently breaks.

**Fix options:**
1. Switch branch naming back to `/` — the git collision concern (GEMINI_NOTES #5) was about `base/foo` conflicting with `base/foo/bar`. This is only a problem if both exist simultaneously. With worktree-per-agent, they shouldn't conflict since git refs can have `/`.
2. Change `file_pr` detection to use `-` — but ambiguous (`main-feature-sub` could split as `main-feature` + `sub` or `main` + `feature-sub`).
3. Store base_branch explicitly in agent metadata — most robust, but heavier.

**Recommendation:** Option 1. Use `/` separator in branch names. The git collision was a real concern for `base/foo` + `base/foo/bar` existing as both branch and directory, but this is solvable: slugify the leaf segment so it never looks like a path component of another branch. Test with actual recursive spawning.

## What's Done (Phase 1 Checklist)

- [x] `spawn_subtree` tool (creates worktree off current branch, TL role, both Claude/Gemini)
- [x] Fix branch naming convention (uses `.` separator to avoid git collisions and unblock `file_pr` detection)
- [x] `spawn_worker` tool (WIP — in-place pane, Gemini only, no worktree)
- [x] Stop Hook: Auto-file PR on Exit (automatically calls `file_pr` tool if no PR exists)
- [x] `file_pr` tool (auto-detects base branch from `/` or `.` convention, creates/updates PR)
- [x] Long-poll `get_messages` (timeout_secs support, blocks until message or timeout)
- [x] Stop hooks (SubagentStop, SessionEnd — check uncommitted changes, unpushed commits, PR status)
- [x] Copilot review integration (wait_for_copilot_review effect + handler)
- [x] Agent-type-aware config writing (Claude gets .mcp.json, Gemini gets .gemini/settings.json)
- [x] Per-agent MCP identity routing (/agents/{name}/mcp)
- [x] Messaging: note, question, answer_question, get_agent_messages

## What's NOT Done

### 1. Fix Branch Naming Convention (BLOCKING)
See critical section above. spawn_gemini_teammate uses `-` but file_pr expects `/`. Must resolve before recursive spawning works.

### 2. Stop Hook: Auto-file PR on Exit
Currently stop hooks CHECK for PR status but don't automatically FILE a PR. The hylo model requires children to auto-file PRs against parent branch on completion.

**Where:** Haskell stop hook logic in `ExoMonad/Guest/Tool/Runtime.hs` (SessionEnd handler).
**What:** After verifying committed + pushed, call `file_pr` effect if no PR exists yet.

### 3. Rebase Notification After Merge
When parent merges a child PR, siblings need "rebase on parent" notification. Currently no mechanism for this.

**What's needed:**
- After merge, parent sends message to remaining children via `note` tool
- Children need a hook or message handler that triggers `git pull --rebase`
- For phase 1: manual — parent just sends a note saying "rebase"

### 4. End-to-End Recursive Test
No actual test of: root → spawn 2 subtrees → each spawns leaf → leaves file PRs → subtrees merge → subtrees file PRs → root merges.

**Verification plan (from phase-1.md):**
1. Root decomposes task into 2 subtrees
2. Each subtree spawns 1-2 leaf nodes
3. Leaves complete, file PRs against subtree branch
4. Copilot reviews, CI runs
5. Subtree reviews + merges child PRs
6. Subtree files PR against root branch
7. Root reviews + merges

### 5. spawn_worker Registration (Low Priority)
spawn_worker agents don't register in config.json → cleanup_agent can't find them → list_agents doesn't show them. Fine for prototype, needs fixing for production.

### 6. Depth Limit (Convention vs Enforcement)
Phase 1 plan says max 2 levels. Currently no enforcement. Could add to tool description or validate in spawn logic.

## Suggested Order

1. **Fix branch naming** — unblock recursive PR targeting
2. **Auto-file PR in stop hook** — complete the fold mechanism
3. **Manual e2e test** — validate the full cycle with a real task
4. **Rebase notification** — quality of life for sibling coordination
5. **spawn_worker cleanup** — when worker model stabilizes

## Docs to Update After

- `hylo_plan/README.md` — still references `spawn_leaf`, uses `/` branch naming
- `hylo_plan/tools.md` — still shows `spawn_leaf` spec
- `hylo_plan/phase-1.md` — still references `spawn_leaf`, old branch convention
- `hylo_plan/pr-lifecycle.md` — branch examples use `/` (which may be correct after fix)
