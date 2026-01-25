# TL Sprint Planning

Guidelines for organizing parallel subagent work to minimize merge conflicts.

## Core Principle: Dependency Chains Beat Parallel Branches

When work touches overlapping files (same module, shared types, common infrastructure), **sequential commits on one branch** are better than **parallel worktrees** that require rebasing.

### When to Use Parallel Worktrees

✅ **Independent domains** - Different packages, no shared types
✅ **Leaf features** - New files only, no modifications to shared code
✅ **Documentation** - READMEs, CLAUDE.md updates, ADRs

### When to Use Sequential Work (Single Branch)

❌ **Shared effect types** - Multiple features adding to same Effect module
❌ **Graph extensions** - Multiple stages added to same graph definition
❌ **Type evolution** - Changes that ripple through imports/exports
❌ **Infrastructure scaffolding** - Base types that later features depend on

## The Effector Lesson (2026-01-25)

**What happened:** Wave 1 spawned 3 parallel worktrees (test-stage, docs-stage, pr-stage) all adding to:
- `Effector.hs` - effect type definitions
- `StopHook/Graph.hs` - graph node definitions
- `StopHook/Types.hs` - shared type definitions
- `StopHook/Handlers.hs` - handler implementations

**Result:** PR #337 merged cleanly. PRs #338 and #339 required painful rebases due to overlapping changes in the same files.

**Better approach:** Single branch with sequential commits:
```
effector-stages
├── commit 1: test stage (cabal test)
├── commit 2: docs stage (git status/diff)
└── commit 3: pr stage (gh pr-status/pr-create)
```

Each commit builds on the previous, no rebasing needed.

## Planning Checklist

Before spawning parallel worktrees, ask:

1. **Do the tasks modify the same files?** → Sequential
2. **Do the tasks add to the same type/module?** → Sequential
3. **Does task B need types defined in task A?** → Sequential
4. **Are the tasks in completely separate packages?** → Parallel OK

## Wave Structure

**Wave 0 (Scaffold):** Always sequential - establishes shared types/infrastructure

**Wave N (Features):** Parallel OK only if:
- Each feature touches disjoint file sets
- No shared type evolution
- Clear package boundaries

## Collision Detection

Before dispatching parallel work, run:
```bash
# List files each task will likely touch
# If intersection > 0, use sequential
```

Or use the `blast-radius` tool to identify overlapping impact zones.
