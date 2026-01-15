# Stream A: ToolDef Annotation

You're adding `ToolDef` annotation support for LLMNode, enabling LLM nodes to be exposed as MCP tools.

## Context

The Tidepool DSL uses type-level annotations to describe nodes:
```haskell
myNode :: LLMNode :@ Input Query :@ Schema Response :@ Tools MyTools
```

We need to add `ToolDef` so an LLMNode can carry MCP metadata:
```haskell
myNode :: LLMNode :@ Input Query :@ Schema Response :@ ToolDef "name" "description"
```

## Your Task

Read and implement: `next-work/mcp-server/01-llm-tooldef.md`

## Key Files

- `haskell/dsl/core/src/Tidepool/Graph/Types.hs` - Add/verify ToolDef type
- `haskell/dsl/core/src/Tidepool/Graph/Edges.hs` - Add GetToolDef, HasToolDef

## Reference

Look at existing annotation patterns:
- `GetClaudeCode` / `HasClaudeCode` in Edges.hs
- `MCPExport` / `ToolMeta` that was just merged

## Verification

```haskell
:kind! GetToolDef (LLMNode :@ Input Q :@ ToolDef "scout" "desc")
-- Expected: 'Just '("scout", "desc")
```

## Do Not

- Modify MCP server code (that's Stream B)
- Create example tools (that's Stream C after merge)
