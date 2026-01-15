# 04: Scout Example Tool

## Goal

First working example: Semantic Scout as LLMNode exposed via MCP.

## Depends On

- 01-llm-tooldef (ToolDef annotation)
- 03-mcp-harness (MCP server)
- Existing: LSP effect + interpreter

## The Tool

```haskell
-- | Scout query input
data ScoutQuery = ScoutQuery
  { sqQuery :: Text      -- Natural language question
  , sqTags  :: [Text]    -- Interest signals
  , sqBudget :: Maybe Int -- Max nodes to explore (default 20)
  }
  deriving (Generic, FromJSON, ToJSON)

-- | Scout response
data ScoutResponse = ScoutResponse
  { srSummary :: Text           -- Markdown summary
  , srPointers :: [Pointer]     -- Actionable locations
  , srNodesVisited :: Int       -- How many LSP calls made
  }
  deriving (Generic, FromJSON, ToJSON)

data Pointer = Pointer
  { pLocation :: Text    -- "Edges.hs:89"
  , pWhat :: Text        -- "NodeHandler type family"
  , pRisk :: Text        -- "high" | "medium" | "low"
  , pAction :: Maybe Text -- "add branch for new variant"
  }
  deriving (Generic, FromJSON, ToJSON)

-- | The scout tool definition
scoutTool :: LLMNode
    :@ Input ScoutQuery
    :@ Schema ScoutResponse
    :@ Tools LspTools
    :@ ToolDef "scout" "Explore codebase to answer semantic questions about code"
scoutTool = LLMNode
```

## LspTools Record

```haskell
-- | LSP operations available to the LLM
data LspTools mode = LspTools
  { ltWorkspaceSymbol :: mode :- Tool "workspace_symbol" WorkspaceSymbolParams [SymbolInfo]
  , ltFindReferences :: mode :- Tool "find_references" ReferenceParams [Location]
  , ltHover :: mode :- Tool "hover" HoverParams HoverResult
  , ltIncomingCalls :: mode :- Tool "incoming_calls" CallHierarchyParams [CallHierarchyItem]
  , ltOutgoingCalls :: mode :- Tool "outgoing_calls" CallHierarchyParams [CallHierarchyItem]
  }
  deriving Generic
```

## Template

```jinja
You are a semantic code explorer. Given a question about the codebase, use LSP tools to investigate and provide a structured answer.

## Query
{{ query }}

## Interest Tags
{% for tag in tags %}
- {{ tag }}
{% endfor %}

## Budget
Explore at most {{ budget | default(20) }} locations.

## Instructions

1. Start with workspace_symbol to find relevant symbols
2. Use find_references to see where they're used
3. Use hover to understand what each location does
4. Use incoming_calls/outgoing_calls to trace dependencies
5. Focus on locations matching the interest tags
6. Stop when you have enough to answer, or budget exhausted

## Output

Return a ScoutResponse with:
- summary: 1-3 paragraph answer to the query
- pointers: List of important locations with risk levels and suggested actions
- nodesVisited: How many LSP calls you made
```

## Files to Create

```
haskell/agents/semantic-scout/
├── tidepool-semantic-scout.cabal
├── src/
│   └── Tidepool/
│       └── Agents/
│           └── Scout/
│               ├── Types.hs      -- ScoutQuery, ScoutResponse, Pointer
│               ├── Tools.hs      -- LspTools record
│               ├── Template.hs   -- TypedTemplate for prompt
│               └── Main.hs       -- Wire up MCP server
├── templates/
│   └── scout.jinja
```

## Verification

```bash
# Start MCP server with scout tool
cabal run semantic-scout -- --mcp

# In another terminal, send query:
echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"scout","arguments":{"query":"What breaks if I add a variant to LLMKind?","tags":["exhaustive","pattern-match"]}},"id":1}' | nc localhost 8080

# Should return ScoutResponse JSON
```

## Success Criteria

- [ ] MCP tool listing shows "scout" with correct schema
- [ ] Query returns structured ScoutResponse
- [ ] LLM actually uses LSP tools during execution
- [ ] Response includes real file locations from LSP
