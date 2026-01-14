# Execution Plan: Record-Based Node DSL Refactor

## Dependency Graph

```
                    ┌─────────────────┐
                    │  01-core-types  │
                    │   (foundation)  │
                    └────────┬────────┘
                             │
           ┌─────────────────┼─────────────────┐
           │                 │                 │
           ▼                 ▼                 ▼
   ┌───────────────┐ ┌───────────────┐ ┌───────────────┐
   │ 02-goto-      │ │ 03-validation │ │ 04-handler-   │
   │ refactor      │ │               │ │ generation    │
   └───────┬───────┘ └───────┬───────┘ └───────┬───────┘
           │                 │                 │
           │                 └────────┬────────┘
           │                          │
           ▼                          ▼
   ┌───────────────┐         ┌───────────────┐
   │ 06-actor-     │         │ 05-dispatch   │
   │ runtime       │         │               │
   └───────┬───────┘         └───────┬───────┘
           │                          │
           └────────────┬─────────────┘
                        │
                        ▼
               ┌───────────────┐
               │ 07-migration  │
               │   (final)     │
               └───────────────┘
```

## Dependency Matrix

| Plan | Depends On | Blocks |
|------|------------|--------|
| 01-core-types | - | 02, 03, 04, 05, 06, 07 |
| 02-goto-refactor | 01 | 06, 07 |
| 03-validation | 01 | 07 |
| 04-handler-generation | 01 | 05, 07 |
| 05-dispatch | 01, 04 | 07 |
| 06-actor-runtime | 02 | 07 |
| 07-migration | 01, 02, 03, 04, 05, 06 | - |

## Execution Waves

### Wave 1: Foundation
```bash
# Single worktree - must complete first
git worktree add ../tidepool-01 -b refactor/record-nodes-01-core-types
cd ../tidepool-01
# Execute 01-core-types.md
# PR, review, merge to main
```

### Wave 2: Parallel Implementation (3 worktrees)
```bash
# After 01 merges, create 3 parallel worktrees from updated main
git fetch origin && git checkout main && git pull

git worktree add ../tidepool-02 -b refactor/record-nodes-02-goto-refactor
git worktree add ../tidepool-03 -b refactor/record-nodes-03-validation
git worktree add ../tidepool-04 -b refactor/record-nodes-04-handler-generation

# Execute in parallel:
# - tidepool-02: 02-goto-refactor.md
# - tidepool-03: 03-validation.md
# - tidepool-04: 04-handler-generation.md

# PR all three, can merge in any order (no inter-dependencies)
```

### Wave 3: Parallel Integration (2 worktrees)
```bash
# After 02 and 04 merge
git fetch origin && git checkout main && git pull

git worktree add ../tidepool-05 -b refactor/record-nodes-05-dispatch
git worktree add ../tidepool-06 -b refactor/record-nodes-06-actor-runtime

# Execute in parallel:
# - tidepool-05: 05-dispatch.md (needs 04)
# - tidepool-06: 06-actor-runtime.md (needs 02)

# PR both, can merge in any order
```

### Wave 4: Migration
```bash
# After ALL previous plans merge
git fetch origin && git checkout main && git pull

git worktree add ../tidepool-07 -b refactor/record-nodes-07-migration

# Execute 07-migration.md
# Final PR - clean break complete
```

## Parallel Execution Commands

For spawning Claude sessions in parallel worktrees:

```bash
# Wave 2 (after 01 merges)
claude --worktree ../tidepool-02 "Execute refactor-plans/02-goto-refactor.md" &
claude --worktree ../tidepool-03 "Execute refactor-plans/03-validation.md" &
claude --worktree ../tidepool-04 "Execute refactor-plans/04-handler-generation.md" &
wait

# Wave 3 (after 02, 04 merge)
claude --worktree ../tidepool-05 "Execute refactor-plans/05-dispatch.md" &
claude --worktree ../tidepool-06 "Execute refactor-plans/06-actor-runtime.md" &
wait
```

## Critical Path

The minimum sequential path is:

```
01 → 04 → 05 → 07
```

- **01-core-types**: Foundation (all depend on this)
- **04-handler-generation**: Defines handler types needed by dispatch
- **05-dispatch**: Uses handler types for execution
- **07-migration**: Final integration

Plans 02, 03, 06 are off critical path and can be parallelized.

## Merge Order Constraints

1. **01** must merge before anything else
2. **02** must merge before **06**
3. **04** must merge before **05**
4. **03** can merge any time after **01**
5. **07** must be last

Valid merge orders (after 01):
- 02 → 03 → 04 → 05 → 06 → 07
- 03 → 02 → 04 → 06 → 05 → 07
- 04 → 02 → 03 → 05 → 06 → 07
- 02 → 04 → 03 → 06 → 05 → 07
- ... (many valid orderings)

## Estimated Timeline

| Wave | Plans | Parallelism | Est. Time |
|------|-------|-------------|-----------|
| 1 | 01 | 1 | 2-3 hours |
| 2 | 02, 03, 04 | 3 | 3-4 hours |
| 3 | 05, 06 | 2 | 2-3 hours |
| 4 | 07 | 1 | 4-6 hours |

**Total**: ~12-16 hours of work, reduced to ~8-10 hours wall time with parallelization.

## Rollback Strategy

Each plan is a separate branch. If issues discovered:

1. Revert the problematic PR
2. Fix in isolation
3. Re-merge

Plan 07 (migration) is the point of no return for the clean break.
