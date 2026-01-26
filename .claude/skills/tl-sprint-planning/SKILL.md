---
name: tl-sprint-planning
description: Use when planning sprints, organizing parallel work, or dispatching multiple agents. Covers wave planning, collision detection, and dependency analysis.
---

# Tech Lead Sprint Planning

## Core Principles

1. **Parallel work that touches the same files creates rebase hell.** Always check for file collisions before dispatching parallel agents.

2. **Waves are heterogeneous.** A single wave can mix tactical (haiku) and strategic (opus) work, as long as files don't overlap.

## Sprint Planning Protocol

### 1. Identify Ready Work

```bash
gh issue list --label "ready" # Show unblocked items
gh issue list                 # Identify bottlenecks
```

### 2. Classify Issues (Tactical vs Strategic)

Before dispatch, classify each issue:

| Classification | Model | Harness | Oversight | Examples |
|----------------|-------|---------|-----------|----------|
| **Tactical** | Haiku | Minimal (plain claude) | Light - batch review | docs, tests, formulaic tools |
| **Strategic** | Opus | Full (start-augmented.sh) | Heavy - active collab | architecture, DSL, refactors |

**Strategic agents can also provide UX feedback** on the tooling itself, creating a feedback loop for improvement.

### 3. Collision Check (CRITICAL)

Map files each issue will touch across ALL agents (tactical + strategic):

```
| Issue | Type | Subsystem | Files | Collision Risk |
|-------|------|-----------|-------|----------------|
| 123   | tactical | PMTools | PMTools.hs, Export.hs | Group A |
| 456   | tactical | PMTools | PMTools.hs, Export.hs | Group A ⚠️ |
| 789   | tactical | LSPTools | LSPTools.hs | Group B |
| 101   | strategic | Effects | Effect/Types.hs | Group C |
```

**Rules:**
- Same files → SERIALIZE, don't parallelize (regardless of tactical/strategic)
- Different subsystems, shared file (Export.hs) → Merge first PR before second
- Completely disjoint files → Safe to parallelize

### 4. Wave Organization (Heterogeneous)

```
Wave N:
├── Tactical agents (haiku, 5-6x)
│   ├── issue-123: docs update (CLAUDE.md)
│   ├── issue-456: add test (tests/)
│   ├── issue-789: simple tool (LSPTools.hs)
│   └── ... all disjoint files
│
└── Strategic agent (opus, 1x)
    └── issue-101: effects refactor (Effect/*.hs)
        └── Uses full harness, provides UX feedback
```

### 5. Dispatch (Current: Manual)

```bash
# Create worktrees
spawn_agents(["123", "456", "789"])

# Human opens Zellij tabs manually
# In each tab:
cd /path/to/worktree
# Context is automatically loaded from .claude/context/issue.md
claude                        # Or: gemini, or ./start-augmented.sh for opus
```

**Model selection is manual** - TL decides per-tab based on classification.

## Anti-Patterns

### ❌ Parallel agents on same subsystem
```
Wave 1: pm_status + pm_review_dag  # Both touch PMTools.hs
Result: Painful rebase conflicts
```

### ❌ Headless background agents
```
Task tool with run_in_background=true  # No human oversight
Result: Risky, can't intervene, context competition
```

### ✅ Correct Pattern
```
Wave 1a: pm_status (PMTools)
Wave 1b: pm_review_dag (PMTools) - after 1a merges
Wave 1 parallel: find_callees (LSPTools) - different subsystem
```

## Merge Order Strategy

When parallel PRs exist with potential conflicts:

1. **Merge smallest/most isolated first** - less conflict surface
2. **Rebase next PR on fresh main**
3. **Repeat**

Or use integration branch if conflicts are mechanical.

## Commands Reference

```bash
# Planning
gh issue list                # Available work
gh issue status              # Project health

# Dispatch
spawn_agents(["123", "456"]) # Create worktrees (MCP tool)

# Monitoring
gh pr list                   # Open PRs
```