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
- [x] End-to-End Recursive Test (verified via unit tests and integration tests for hierarchy separator and PR detection)

## What's NOT Done

### 1. Rebase Notification After Merge
When parent merges a child PR, siblings need "rebase on parent" notification. Currently no mechanism for this.

**What's needed:**
- After merge, parent sends message to remaining children via `note` tool
- Children need a hook or message handler that triggers `git pull --rebase`
- For phase 1: manual — parent just sends a note saying "rebase"

### 2. spawn_worker Registration (Low Priority)
spawn_worker agents don't register in config.json → cleanup_agent can't find them → list_agents doesn't show them. Fine for prototype, needs fixing for production.

### 3. Depth Limit (Convention vs Enforcement)
Phase 1 plan says max 2 levels. Currently no enforcement. Could add to tool description or validate in spawn logic.

## Suggested Order

1. **Rebase notification** — quality of life for sibling coordination
2. **spawn_worker cleanup** — when worker model stabilizes
3. **Docs update** — sync hylo_plan with current state

## Docs to Update After

- `hylo_plan/README.md` — still references `spawn_leaf`, uses `/` branch naming
- `hylo_plan/tools.md` — still shows `spawn_leaf` spec
- `hylo_plan/phase-1.md` — still references `spawn_leaf`, old branch convention
- `hylo_plan/pr-lifecycle.md` — branch examples use `/` (which may be correct after fix)
