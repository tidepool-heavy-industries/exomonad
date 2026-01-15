# 02: MCP Server Library Evaluation

## Goal

Find/evaluate existing MCP server libraries. Prefer Haskell, but Rust acceptable if better.

## Requirements

- JSON-RPC over stdio (MCP standard transport)
- Tool listing (`tools/list`)
- Tool execution (`tools/call`)
- Ideally: type-safe tool definition

## Candidates to Evaluate

### Haskell

1. **Search Hackage** for "mcp", "model-context-protocol", "json-rpc"
2. **json-rpc** package - generic JSON-RPC, would need MCP layer on top
3. **Custom** - thin wrapper if nothing suitable exists

### Rust

1. **mcp-rs** or similar - check crates.io
2. **rmcp** - if it exists
3. Leverage existing `rust/mantle-agent` MCP code? (user said out of scope, but code could be referenced)

## Evaluation Criteria

| Criterion | Weight |
|-----------|--------|
| Type-safe tool definitions | High |
| Minimal boilerplate | High |
| Active maintenance | Medium |
| Async support | Medium |
| Documentation | Medium |

## Output

Create `next-work/mcp-server/library-decision.md` with:
1. Libraries evaluated
2. Pros/cons of each
3. Recommendation
4. Example code snippet showing usage

## Research Commands

```bash
# Hackage search
curl "https://hackage.haskell.org/packages/search?terms=json-rpc"

# Crates.io search
curl "https://crates.io/api/v1/crates?q=mcp"

# GitHub search
gh search repos "mcp server haskell"
gh search repos "model context protocol haskell"
```

## Fallback

If no suitable library exists, the harness is simple enough to implement:
- Stdin line reader
- JSON parse → dispatch on method
- Format response → stdout

But prefer library to avoid reinventing wheels.
