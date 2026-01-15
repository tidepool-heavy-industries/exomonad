# Stream B: MCP Server Harness

You're implementing the MCP server that exposes LLMNode as tools via JSON-RPC.

## Context

We want to expose LLMNode directly as MCP tools without graph wrapper ceremony:
```haskell
scoutTool :: LLMNode :@ Input Query :@ Schema Response :@ ToolDef "scout" "desc"
```

The MCP server needs to:
1. List tools (from ToolDef annotations)
2. Route calls to LLM execution
3. Return structured output

## Your Tasks

1. **First**: Read and execute `next-work/mcp-server/02-mcp-library.md`
   - Evaluate existing MCP/JSON-RPC libraries
   - Document findings in `library-decision.md`
   - Recommend best option

2. **Then**: Read and implement `next-work/mcp-server/03-mcp-harness.md`
   - Create `haskell/effects/mcp-server/` package
   - Implement server using chosen library
   - Wire up tool listing and execution

## Key Constraint

Use existing libraries instead of raw IO. Prefer Haskell, but Rust acceptable if significantly better.

## Reference

- JSON schema derivation already exists for tool defs (look at ToolDef typeclass)
- LLM execution: `haskell/effects/llm-interpreter/`

## Verification

```bash
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | cabal run mcp-server
# Should return tool listing JSON
```

## Do Not

- Implement ToolDef annotation (that's Stream A)
- Create scout example (that's Stream C after merge)
