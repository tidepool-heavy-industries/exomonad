# MCP Server Work Items

Goal: Expose LLMNode directly as MCP tool. No graph wrapper.

```haskell
scoutTool :: LLMNode
    :@ Input ScoutQuery
    :@ Schema ScoutResponse
    :@ Tools LspTools
    :@ ToolDef "scout" "Explore code semantically"
```

## Dependency Graph

```
Stream A: ToolDef Annotation          Stream B: MCP Harness
         │                                     │
    ┌────┴────┐                          ┌─────┴─────┐
    │ 01-llm- │                          │ 02-mcp-   │
    │ tooldef │                          │ library   │
    └────┬────┘                          └─────┬─────┘
         │                                     │
         │                               ┌─────┴─────┐
         │                               │ 03-mcp-   │
         │                               │ harness   │
         │                               └─────┬─────┘
         │                                     │
         └──────────────┬──────────────────────┘
                        │
                  ┌─────┴─────┐
                  │ 04-scout  │  Stream C
                  │ example   │
                  └───────────┘
```

## Parallelization

- **A + B**: Fully parallel (no dependencies)
- **C**: Waits for A + B to merge

## Merge Order

1. Merge A (01)
2. Merge B (02, 03)
3. Merge C (04)
