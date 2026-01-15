# Stream B: MCPExport Types and Reification

You're implementing MCPExport - the mechanism to expose graph Entry points as MCP tools. This enables graphs to be served as MCP servers.

## Context

Read these files for full context:
- `next-work/README.md` - dependency graph
- `next-work/02-mcpexport-types.md` - first task
- `next-work/05-mcp-reify.md` - second task
- Plan: `~/.claude/plans/tranquil-stargazing-scott.md` (Phase 2 section)

## Your Tasks (in order)

1. **02-mcpexport-types**: Add types to `Types.hs`
   - `MCPExport` marker annotation
   - `ToolMeta` annotation for name/description
   - Extend `NodeKind` with `Graph` constructor

2. **05-mcp-reify**: Create new module `MCPReify.hs`
   - `MCPToolDef` data type
   - `ReifyMCPTools` typeclass
   - Generic traversal to collect MCPExport entries
   - Extract ToolMeta and generate JSON Schema

## Branch

```bash
git checkout -b feat/mcp-types
```

## Verification

After task 1:
```bash
cd haskell/dsl/core
cabal build tidepool-core
```

```haskell
:kind ToolMeta
-- ToolMeta :: (Symbol, Symbol) -> Type
```

After task 2:
```haskell
import Tidepool.Graph.MCPReify
:info ReifyMCPTools
```

## Key Pattern

MCPExport on Entry creates an MCP tool:
```haskell
data MyGraph mode = MyGraph
  { search :: mode :- Entry SearchInput
      :@ MCPExport
      :@ ToolMeta '("search", "Search the codebase")
  }
```

Similar to existing `DecisionTools` pattern in `StructuredOutput/DecisionTools.hs` - use that as reference for Generic traversal.

Start with task 02.
