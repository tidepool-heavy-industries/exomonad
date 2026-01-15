# Stream C: Scout Example Tool

**WAIT**: This stream depends on A + B merging first.

You're creating the first working example: Semantic Scout exposed as MCP tool.

## Context

With ToolDef annotation (Stream A) and MCP harness (Stream B) merged, we can create an LLMNode that:
- Takes natural language queries about code
- Uses LSP tools to explore
- Returns structured findings

## Your Task

Read and implement: `next-work/mcp-server/04-scout-example.md`

## Key Files to Create

```
haskell/agents/semantic-scout/
├── tidepool-semantic-scout.cabal
├── src/Tidepool/Agents/Scout/
│   ├── Types.hs      -- ScoutQuery, ScoutResponse
│   ├── Tools.hs      -- LspTools record
│   ├── Template.hs   -- Typed template
│   └── Main.hs       -- MCP server entry
├── templates/
│   └── scout.jinja
```

## Key Types

```haskell
data ScoutQuery = ScoutQuery
  { sqQuery :: Text
  , sqTags :: [Text]
  , sqBudget :: Maybe Int
  }

data ScoutResponse = ScoutResponse
  { srSummary :: Text
  , srPointers :: [Pointer]
  , srNodesVisited :: Int
  }
```

## Reference

- LSP effect: `haskell/effects/lsp-interpreter/`
- Tool records: look at existing `Tools` annotation usage
- Template pattern: `$(typedTemplateFile ''Context "template.jinja")`

## Verification

```bash
# Start MCP server
cabal run semantic-scout -- --mcp

# Query via MCP
echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"scout","arguments":{"query":"What uses LLMKind?"}},"id":1}'
```

## Success

- LLM uses LSP tools during execution
- Response contains real file locations
- Structured output matches ScoutResponse schema
