# Stream A: GraphNode Core Infrastructure

You're implementing the GraphNode type infrastructure for the Tidepool Graph DSL. This enables graphs to contain other graphs as nodes.

## Context

Read these files for full context:
- `next-work/README.md` - dependency graph
- `next-work/01-graphnode-marker.md` - first task
- `next-work/03-graph-extraction.md` - second task
- `next-work/06-graphnode-dispatch.md` - third task
- Plan: `~/.claude/plans/tranquil-stargazing-scott.md` (Phase 2 section)

## Your Tasks (in order)

1. **01-graphnode-marker**: Add `GraphNode` marker to `Generic/Core.hs`
   - Kind: `(Type -> Type) -> Type` (mirrors LLMNode pattern)
   - Export from module

2. **03-graph-extraction**: Add type families to `Edges.hs`
   - `GetGraphEntry` - extract Entry type from graph
   - `GetGraphExit` - extract Exit type from graph
   - Walk Generic Rep to find Entry/Exit markers

3. **06-graphnode-dispatch**: Add execution support to `Interpret.hs`
   - `NodeHandler` instance for GraphNode
   - `executeGraphNode` function
   - Wire into DispatchGoto

## Branch

```bash
git checkout -b feat/graphnode-core
```

## Verification

After each task:
```bash
cd haskell/dsl/core
cabal build tidepool-core
```

After task 3, test in REPL:
```haskell
:kind GraphNode
-- GraphNode :: (Type -> Type) -> Type

:kind! GetGraphEntry SimpleGraph
-- Should reduce to Entry input type
```

## Key Pattern

GraphNode mirrors LLMNode exactly:
```haskell
-- Existing
type LLMNode :: LLMKind -> Type
data LLMNode subtype

-- You're adding
type GraphNode :: (Type -> Type) -> Type
data GraphNode subgraph
```

Start with task 01.
