# Phase 2 Work Items: GraphNode + MCPExport

Work items for implementing GraphNode (graphs containing graphs) and MCPExport (entries as MCP tools).

## Dependency Graph

```
                    ┌──────────────┐
                    │ 07-golden    │ (integration test)
                    │    test      │
                    └──────┬───────┘
                           │ depends on all
        ┌──────────────────┼──────────────────┐
        │                  │                  │
        ▼                  ▼                  ▼
┌──────────────┐   ┌──────────────┐   ┌──────────────┐
│ 06-graphnode │   │ 05-mcp       │   │ 04-mcp       │
│   dispatch   │   │   reify      │   │  detection   │
└──────┬───────┘   └──────┬───────┘   └──────┬───────┘
       │                  │                  │
       │ depends on       │ depends on       │ depends on
       ▼                  ▼                  ▼
┌──────────────┐   ┌──────────────┐   ┌──────────────┐
│ 03-graph     │   │ 02-mcpexport │   │ 02-mcpexport │
│  extraction  │   │    types     │   │    types     │
└──────┬───────┘   └──────────────┘   └──────────────┘
       │
       │ depends on
       ▼
┌──────────────┐
│ 01-graphnode │
│    marker    │
└──────────────┘
```

## Parallel Streams

Three independent streams that can run in parallel worktrees:

### Stream A: GraphNode Infrastructure
```
Worktree: feat/graphnode-core

01-graphnode-marker.md  →  03-graph-extraction.md  →  06-graphnode-dispatch.md
     (10 min)                   (30 min)                    (45 min)
```

### Stream B: MCPExport Types
```
Worktree: feat/mcp-types

02-mcpexport-types.md  →  05-mcp-reify.md
     (15 min)               (45 min)
```

### Stream C: MCP Detection (can start immediately)
```
Worktree: feat/mcp-detection

04-mcp-detection.md
     (30 min)
```

### Integration (after streams merge)
```
Main branch after merge:

07-golden-test.md
     (20 min)
```

## Quick Reference

| Item | Files | Priority | Can Start When |
|------|-------|----------|----------------|
| 01 | Generic/Core.hs | High | Immediately |
| 02 | Types.hs | High | Immediately |
| 03 | Edges.hs | Medium | After 01 |
| 04 | Edges.hs | Medium | After 02 |
| 05 | MCPReify.hs (new) | Medium | After 02, 04 |
| 06 | Interpret.hs, Generic/Core.hs | Medium | After 01, 03 |
| 07 | golden/SemanticScoutGraph.hs | Low | After all |

## Merge Order

1. Merge Stream A (01 → 03) and Stream C (04) in parallel
2. Merge Stream B (02 → 05)
3. Complete 06 (depends on merged 01 + 03)
4. Final: 07 golden test validates everything

## Commands

```bash
# Create worktrees
git worktree add ../tpw-graphnode feat/graphnode-core
git worktree add ../tpw-mcp-types feat/mcp-types
git worktree add ../tpw-mcp-detect feat/mcp-detection

# In each worktree, build and test
cd haskell/dsl/core
cabal build tidepool-core
cabal test tidepool-core:test:graph-tests
```

## Success Criteria (Full Phase 2)

- [ ] `GraphNode` compiles with kind `(Type -> Type) -> Type`
- [ ] `GetGraphEntry`/`GetGraphExit` extract types correctly
- [ ] `MCPExport` annotation compiles on Entry nodes
- [ ] `ReifyMCPTools` generates tool definitions
- [ ] `SemanticScoutGraph` golden test compiles
- [ ] Handler type for GraphNode is `input -> Eff es output`
