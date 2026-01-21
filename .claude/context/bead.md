# Task: Update spawn_agents to use <hangar>/worktrees/ path

**ID:** tidepool-kg6
**Status:** open
**Priority:** 2
**Branch:** bd-kg6/update-spawnagents-to-use-hangarworktrees-path

## Description

## Intent
spawn_agents should create worktrees in the Hangar worktrees directory, not ~/dev/.worktrees/tidepool/.

## Current
```
~/dev/.worktrees/tidepool/bd-xxx-.../
```

## Target
```
<hangar>/worktrees/bd-xxx-.../
```

## Acceptance Criteria
- [ ] spawn_agents reads hangar root from env/config
- [ ] Worktrees created in <hangar>/worktrees/
- [ ] Bootstrap scripts work from new location

## Files
- haskell/control-server/src/Tidepool/Control/ExoTools/SpawnAgents.hs

## Dependencies

- tidepool-b0p: Design: Hangar directory schema for multi-project tooling (closed)


## Workflow

1. Implement changes
2. Commit: [tidepool-kg6] <description>
3. Push: git push -u origin bd-kg6/update-spawnagents-to-use-hangarworktrees-path
4. File PR: gh pr create --title "[tidepool-kg6] Update spawn_agents to use <hangar>/worktrees/ path"
