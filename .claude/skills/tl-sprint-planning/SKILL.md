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
bd ready                    # Show unblocked items
bd blocked                  # Identify bottlenecks
bd show <epic>              # Check epic children
```

### 2. Classify Beads (Tactical vs Strategic)

Before dispatch, classify each bead:

| Classification | Model | Harness | Oversight | Examples |
|----------------|-------|---------|-----------|----------|
| **Tactical** | Haiku | Minimal (plain claude) | Light - batch review | docs, tests, formulaic tools |
| **Strategic** | Opus | Full (start-augmented.sh) | Heavy - active collab | architecture, DSL, refactors |

**Strategic agents can also provide UX feedback** on the tooling itself, creating a feedback loop for improvement.

### 3. Collision Check (CRITICAL)

Map files each bead will touch across ALL agents (tactical + strategic):

```
| Bead | Type | Subsystem | Files | Collision Risk |
|------|------|-----------|-------|----------------|
| abc  | tactical | PMTools | PMTools.hs, Export.hs | Group A |
| def  | tactical | PMTools | PMTools.hs, Export.hs | Group A ⚠️ |
| ghi  | tactical | LSPTools | LSPTools.hs | Group B |
| jkl  | strategic | Effects | Effect/Types.hs | Group C |
```

**Rules:**
- Same files → SERIALIZE, don't parallelize (regardless of tactical/strategic)
- Different subsystems, shared file (Export.hs) → Merge first PR before second
- Completely disjoint files → Safe to parallelize

### 4. Wave Organization (Heterogeneous)

```
Wave N:
├── Tactical agents (haiku, 5-6x)
│   ├── bead-a: docs update (CLAUDE.md)
│   ├── bead-b: add test (tests/)
│   ├── bead-c: simple tool (LSPTools.hs)
│   └── ... all disjoint files
│
└── Strategic agent (opus, 1x)
    └── bead-z: effects refactor (Effect/*.hs)
        └── Uses full harness, provides UX feedback
```

### 5. Dispatch (Current: Manual)

```bash
# Create worktrees
spawn_agents(["bead1", "bead2", "bead3"])

# Mark as in_progress
bd update <id> --status=in_progress

# Human opens Zellij tabs manually
# In each tab:
cd /path/to/worktree
./scripts/bead-context        # Bootstrap bead context
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

## Epic Management

Epics often block children unnecessarily. Two patterns:

### Scope Epic (close early)
Epic defines scope only, children can proceed independently.
```bash
bd close <epic> --reason="Scope complete - children can proceed"
```

### Tracking Epic (keep open)
Epic tracks completion of all children.
```bash
# Keep open until all children done
bd show <epic>  # Check child status
```

## Commands Reference

```bash
# Planning
bd ready                     # Available work
bd blocked                   # Bottleneck analysis
bd stats                     # Project health

# Dispatch
spawn_agents(["id1", "id2"]) # Create worktrees (MCP tool)
bd update <id> --status=in_progress

# Unblocking
bd close <epic> --reason="..." # Close scope epics
bd dep add <a> <b>            # Add dependency

# Monitoring
bd list --status=in_progress  # Active work
gh pr list                    # Open PRs
```
