---
name: tl-sprint-planning
description: Use when planning sprints, organizing parallel work, or dispatching multiple agents. Covers wave planning, collision detection, and dependency analysis.
---

# Tech Lead Sprint Planning

## Core Principles

1. **Parallel work that touches the same files creates rebase hell.** Always check for file collisions before dispatching parallel agents.

2. **Waves are heterogeneous.** A single wave can mix Claude subtrees (expensive, architectural) and Gemini leaves (cheap, focused implementation), as long as files don't overlap.

## Sprint Planning Protocol

### 1. Identify Ready Work

```bash
gh issue list --label "ready" # Show unblocked items
gh issue list                 # Identify bottlenecks
```

### 2. Classify Tasks

Before dispatch, classify each task:

| Classification | Agent | Spawn Tool | Examples |
|----------------|-------|------------|----------|
| **Focused implementation** | Gemini leaf | `spawn_leaf_subtree` | single feature, bug fix, docs |
| **Multi-step decomposition** | Claude subtree | `spawn_subtree` | architecture, refactors requiring sub-spawns |
| **Investigation / research** | Gemini worker | `spawn_workers` | hypothesis testing, codebase exploration |

### 3. Collision Check (CRITICAL)

Map files each task will touch across ALL agents:

```
| Task | Agent Type | Subsystem | Files | Collision Risk |
|------|------------|-----------|-------|----------------|
| auth | leaf       | handlers  | events.rs, handler.rs | Group A |
| perms| leaf       | handlers  | handler.rs, auth.rs   | Group A ⚠️ |
| proto| leaf       | proto     | agent.proto, types.rs | Group B |
| docs | leaf       | docs      | CLAUDE.md             | Group C |
```

**Rules:**
- Same files → SERIALIZE, don't parallelize
- Different subsystems, shared file → Merge first PR before second
- Completely disjoint files → Safe to parallelize

### 4. Wave Organization

```
Wave N:
├── Gemini leaves (parallel, 3-5x)
│   ├── proto-plumbing (proto/, exomonad-proto/)
│   ├── docs-update (CLAUDE.md, haskell/CLAUDE.md)
│   └── test-coverage (tests/)
│
└── Claude subtree (1x, if needed)
    └── architecture refactor (may spawn its own leaves)
```

### 5. Dispatch

```
# Spawn leaves for focused tasks
spawn_leaf_subtree(task="Implement X", branch_name="feature-x")
spawn_leaf_subtree(task="Implement Y", branch_name="feature-y")

# Spawn workers for investigation
spawn_workers(specs=[
  { name: "h1", task: "Investigate hypothesis 1" },
  { name: "h2", task: "Investigate hypothesis 2" }
])
```

After spawning, **return immediately**. Idle until `[PR READY]`, `[FIXES PUSHED]`, or `[from: agent]` messages arrive.

## Anti-Patterns

### Parallel agents on same subsystem
```
Wave 1: auth-handler + perms-handler  # Both touch handler.rs
Result: Painful rebase conflicts
```

### Correct Pattern
```
Wave 1a: auth-handler (handlers/)
Wave 1b: perms-handler (handlers/) — after 1a merges
Wave 1 parallel: proto-plumbing (proto/) — different subsystem
```

## Merge Order Strategy

When parallel PRs exist:

1. **Merge smallest/most isolated first** — less conflict surface
2. **Rebase next PR on fresh branch** (or let Copilot flag conflicts)
3. **Repeat**

## Commands Reference

```bash
# Planning
gh issue list                # Available work

# Dispatch
spawn_leaf_subtree           # Gemini in worktree, files PR
spawn_subtree                # Claude in worktree, can sub-spawn
spawn_workers                # Gemini panes, ephemeral

# Monitoring — idle until messages arrive
# [PR READY] — Copilot approved, merge
# [FIXES PUSHED] — leaf addressed review, merge if CI passes
# [REVIEW TIMEOUT] — no review after timeout, merge if CI passes
```
