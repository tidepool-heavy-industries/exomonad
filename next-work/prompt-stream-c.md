# Stream C: MCP Detection Type Families

You're implementing type families to detect MCPExport annotations on Entry nodes. This is a focused task that enables the reification in Stream B.

## Context

Read these files for full context:
- `next-work/README.md` - dependency graph
- `next-work/04-mcp-detection.md` - your task
- Plan: `~/.claude/plans/tranquil-stargazing-scott.md` (Phase 2 section)

## Your Task

**04-mcp-detection**: Add detection type families to `Edges.hs`

- `HasMCPExport` - check if node has MCPExport annotation
- `GetToolMeta` - extract ToolMeta tuple if present
- `GetMCPEntries` - collect all MCPExport entries from graph

## Branch

```bash
git checkout -b feat/mcp-detection
```

## Dependency

You need Stream B's types (MCPExport, ToolMeta) to exist first. Either:
1. Wait for 02-mcpexport-types to merge, OR
2. Add stub types locally for development, then rebase

Stub approach:
```haskell
-- Temporary in Types.hs until Stream B merges
data MCPExport
type ToolMeta :: (Symbol, Symbol) -> Type
data ToolMeta nameAndDesc
```

## Implementation

Add to `Edges.hs`:

```haskell
type HasMCPExport :: Type -> Bool
type family HasMCPExport node where
  HasMCPExport (node :@ MCPExport) = 'True
  HasMCPExport (node :@ MCPExport :@ _) = 'True
  -- ... handle various annotation orderings
  HasMCPExport (node :@ _) = HasMCPExport node
  HasMCPExport _ = 'False
```

## Verification

```bash
cd haskell/dsl/core
cabal build tidepool-core
```

```haskell
:kind! HasMCPExport (Entry Int :@ MCPExport)
-- = 'True

:kind! HasMCPExport (Entry Int)
-- = 'False

:kind! GetToolMeta (Entry Int :@ MCPExport :@ ToolMeta '("test", "desc"))
-- = 'Just '("test", "desc")
```

## Key Insight

MCPExport can appear at any position in the annotation chain:
- `Entry X :@ MCPExport`
- `Entry X :@ MCPExport :@ ToolMeta ...`
- `Entry X :@ SomeOther :@ MCPExport`

Your type families must handle all orderings by recursively stripping annotations.

Start implementation.
