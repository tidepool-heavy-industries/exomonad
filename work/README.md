# Workstream Coordination: Haiku Teaching System

Implementation broken into 7 parallel-where-possible workstreams.

## Dependency Graph

```
01-foundation (no deps)
    │
    ├──→ 02-anthropic-client ──┐
    ├──→ 03-format-conversion ─┤ (PARALLEL OK)
    └──→ 04-recording ─────────┘
            │
            └──→ 05-execute-wrapper
                    │
                    └──→ 06-scout-integration
                            │
                            └──→ 07-cli-integration
```

## Workstream Status

| Task | Assignee | Status | Dependencies | Blocking |
|------|----------|--------|--------------|----------|
| 01-foundation | TBD | pending | none | 02,03,04 |
| 02-anthropic-client | TBD | pending | 01 | 05 |
| 03-format-conversion | TBD | pending | 01 | 05 |
| 04-recording | TBD | pending | 01 | 05 |
| 05-execute-wrapper | TBD | pending | 02,03,04 | 06 |
| 06-scout-integration | TBD | pending | 05 | 07 |
| 07-cli-integration | TBD | pending | 06 | none |

## Critical Path

01 → {02,03,04} → 05 → 06 → 07

**Earliest parallel work:** After 01 completes, assign 02/03/04 to different people.

## Coordination Notes

- **After 01 completes:** Announce to team, kick off 02/03/04
- **Integration point (05):** Requires all of 02/03/04 done
- **Final integration (07):** Requires full stack working

## Files by Task

| Task | New Files | Modified Files |
|------|-----------|----------------|
| 01 | tidepool-teaching.cabal, Types.hs, Teacher.hs, CLAUDE.md | cabal.project |
| 02 | Anthropic.hs | none |
| 03 | Convert.hs | none |
| 04 | Record.hs | none |
| 05 | Execute.hs | none |
| 06 | Scout/Tools.hs, Scout/Teach/Teacher.hs | control-server.cabal |
| 07 | none | Main.hs, Handler/MCP.hs |

## Testing Strategy

- **Unit tests:** Each task has its own test file
- **Integration test:** After 05, test full recording flow
- **E2E test:** After 07, test CLI command

## Getting Started

1. Assign tasks to agents/people
2. Each assignee reads their task file (work/NN-*.md)
3. Mark status in table above when starting/completing
4. Notify team when completing tasks that unblock others
