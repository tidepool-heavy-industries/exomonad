---
name: mcp-tool-creation
description: Use when creating a new MCP tool for Claude Code++. Covers the full workflow from graph definition through MCPExport annotation to testing.
---

# Creating MCP Tools

End-to-end guide for creating MCP tools that Claude can call via the control-server.

## Overview

MCP tools in Tidepool are defined as Graph DSL graphs with special annotations:
1. **MCPExport** - Marks entry node for auto-discovery
2. **MCPToolDef** - Provides tool name and description
3. **HasJSONSchema** - Auto-generates input schema from types

The `exportMCPTools` function automatically discovers and registers all annotated graphs.

## Quick Checklist

| Step | File | Action |
|------|------|--------|
| 1 | `LSPTools.hs` | Define graph type with `MCPExport` + `MCPToolDef` |
| 2 | `LSPTools.hs` | Define input/output types with `HasJSONSchema` |
| 3 | `LSPTools.hs` | Implement handlers (entry, logic, exit) |
| 4 | `Export.hs` | Add graph to `reifyMCPTools` list |
| 5 | `Handler/MCP.hs` | Add case for tool dispatch |
| 6 | Terminal | Test via `nc localhost 7432` |

## Step 1: Define Graph Structure

```haskell
-- LSPTools.hs

-- | Graph definition for my_tool.
data MyToolGraph mode = MyToolGraph
  { mtEntry :: mode :- EntryNode MyToolArgs
      :@ MCPExport                      -- ← Marks for discovery
      :@ MCPToolDef '("my_tool", "Description shown to Claude")

  , mtRun :: mode :- LogicNode
      :@ Input MyToolArgs
      :@ UsesEffects '[LSP, Goto Exit MyToolResult]

  , mtExit :: mode :- ExitNode MyToolResult
  }
  deriving (Generic)

instance GGraph MyToolGraph
```

**Key annotations:**
- `MCPExport` - Required for auto-discovery
- `MCPToolDef '(name, description)` - Tool metadata (type-level strings)
- `UsesEffects '[...]` - Effects available in handler

## Step 2: Define Input/Output Types

```haskell
-- | Input arguments (becomes JSON Schema).
data MyToolArgs = MyToolArgs
  { name :: Text           -- Required field
  , maxResults :: Maybe Int  -- Optional field (null allowed)
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, HasJSONSchema)

-- | Tool output.
data MyToolResult = MyToolResult
  { results :: [Text]
  , totalCount :: Int
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
```

**Schema derivation:**
- `Text` → `{"type": "string"}`
- `Int` → `{"type": "integer"}`
- `Maybe a` → allows null
- `[a]` → `{"type": "array", "items": ...}`
- Records → `{"type": "object", "properties": ...}`

## Step 3: Implement Handlers

```haskell
-- | Handlers for MyToolGraph.
myToolHandlers :: GraphHandlers MyToolGraph
myToolHandlers = mkHandlers $ \_ -> MyToolGraph
  { mtEntry = entryHandler    -- Built-in, just forwards input
  , mtRun = handleRun
  , mtExit = exitHandler      -- Built-in, returns output
  }
  where
    handleRun :: MyToolArgs -> Eff es (GotoChoice (To "mtExit" MyToolResult))
    handleRun args = do
      -- Use LSP effects
      symbols <- workspaceSymbol args.name
      let filtered = take (fromMaybe 50 args.maxResults) symbols
          names = map (\(SymbolInformation n _ _ _) -> n) filtered

      let result = MyToolResult
            { results = names
            , totalCount = length names
            }
      pure $ gotoExit result
```

**LSP Effects Available:**
- `workspaceSymbol :: Text -> Eff es [SymbolInformation]`
- `hover :: TextDocumentIdentifier -> Position -> Eff es (Maybe HoverInfo)`
- `references :: TextDocumentIdentifier -> Position -> Eff es [Location]`
- `documentSymbols :: TextDocumentIdentifier -> Eff es [DocumentSymbol]`

## Step 4: Register for Discovery

```haskell
-- Export.hs

exportMCPTools :: Logger -> IO [ToolDefinition]
exportMCPTools logger = do
  -- ... existing tools ...
  let myTools = reifyMCPTools (Proxy @MyToolGraph)  -- ← ADD HERE

  let allTools = concat
        [ fcTools, sfTools, scTools, dgTools
        , myTools                                    -- ← AND HERE
        ]
  -- ...
```

**What `reifyMCPTools` does:**
1. Finds fields with `MCPExport` annotation
2. Extracts `MCPToolDef` name and description
3. Generates JSON Schema from `HasJSONSchema` instance
4. Returns `[MCPToolInfo]` for conversion to `ToolDefinition`

## Step 5: Add MCP Dispatch

```haskell
-- Handler/MCP.hs

handleMcpTool :: LSPSession -> Logger -> Text -> Value -> IO (Either McpError Value)
handleMcpTool session logger toolName args = case toolName of
  "find_callers" -> runFindCallers session logger args
  "show_fields" -> runShowFields session logger args
  "show_constructors" -> runShowConstructors session logger args
  "teach-graph" -> runTeachGraph session logger args
  "my_tool" -> runMyTool session logger args    -- ← ADD HERE
  _ -> pure $ Left $ McpError (-32601) "Unknown tool"
```

**Runner implementation:**
```haskell
runMyTool :: LSPSession -> Logger -> Value -> IO (Either McpError Value)
runMyTool session logger argsValue = do
  case fromJSON argsValue of
    Error err -> pure $ Left $ McpError (-32602) (T.pack err)
    Success args -> do
      result <- runM $ runLSP session $ runGraph myToolHandlers args
      pure $ Right $ toJSON result
```

## Step 6: Test

### Manual Testing
```bash
# Start control-server (if not running via start-augmented.sh)
GEMMA_ENDPOINT=http://localhost:11434 cabal run tidepool-control-server

# List all tools (verify my_tool appears)
echo '{"type":"ToolsListRequest"}' | nc localhost 7432

# Call the tool
echo '{"type":"MCPToolCall","id":"test1","tool_name":"my_tool","arguments":{"name":"runLSP","maxResults":5}}' | nc localhost 7432
```

### Expected Output
```json
{"type":"MCPToolResponse","id":"test1","result":{"results":["runLSP","runLSPSession",...],totalCount":5}}
```

## Common Patterns

### Required vs Optional Fields

```haskell
data Args = Args
  { required :: Text         -- Must be provided
  , optional :: Maybe Int    -- Can be null or omitted
  }
```

### Multiple Effects

```haskell
:@ UsesEffects '[LSP, Log, State MyState, Goto Exit Result]
```

### Error Handling

```haskell
handleRun args = do
  symbols <- workspaceSymbol args.name
  case symbols of
    [] -> pure $ gotoExit $ MyToolResult [] 0  -- Empty result, not error
    syms -> do
      -- ... process symbols ...
```

For actual errors, return `Left McpError` from the runner function.

### Complex Input Schema

For richer schemas, implement `HasJSONSchema` manually:

```haskell
instance HasJSONSchema MyArgs where
  jsonSchema = objectSchema
    [ ("name", describeField "name" "Function name to search for" (emptySchema TString))
    , ("max_results", describeField "max_results" "Maximum results" (emptySchema TNumber))
    ]
    ["name"]  -- Required fields
```

## File Locations

| File | Purpose |
|------|---------|
| `haskell/control-server/src/Tidepool/Control/LSPTools.hs` | Graph + handler definitions |
| `haskell/control-server/src/Tidepool/Control/Export.hs` | Tool discovery (`reifyMCPTools`) |
| `haskell/control-server/src/Tidepool/Control/Handler/MCP.hs` | Tool dispatch |
| `haskell/control-server/src/Tidepool/Control/Protocol.hs` | `ToolDefinition` type |
| `haskell/dsl/core/src/Tidepool/Graph/Types.hs` | `MCPExport`, `MCPToolDef` annotations |
| `haskell/dsl/core/src/Tidepool/Graph/MCPReify.hs` | `ReifyMCPTools` typeclass |

## Debugging

### Tool Not Appearing

1. Check `MCPExport` annotation present on entry node
2. Verify graph added to `exportMCPTools` in `Export.hs`
3. Restart control-server after changes
4. Check logs for discovery output:
   ```
   [MCP Discovery] MyToolGraph: 1 tools
   [MCP Discovery]   my_tool -> mtEntry
   ```

### Schema Issues

1. Ensure `HasJSONSchema` derived or implemented
2. Check JSON Schema matches expected structure:
   ```bash
   echo '{"type":"ToolsListRequest"}' | nc localhost 7432 | jq '.tools[] | select(.name == "my_tool") | .inputSchema'
   ```

### Runtime Errors

1. Check argument parsing: `fromJSON` in runner
2. Check LSP session is initialized
3. Look for error response:
   ```json
   {"type":"MCPToolResponse","id":"...","error":{"code":-32602,"message":"..."}}
   ```

## Advanced: Tier 2 (LLM-Enhanced) Tools

For tools that use LLM nodes (like `teach-graph`):

1. Use `LLMNode` instead of `LogicNode`
2. Add `LLM` to effects
3. Define template for prompt
4. Require `GEMMA_ENDPOINT` environment variable

See `Scout/Graph.hs` for the `DocGenGraph` example.

## See Also

- **control-server docs**: `haskell/control-server/CLAUDE.md`
- **Graph DSL**: `haskell/dsl/core/CLAUDE.md`
- **Protocol types**: `haskell/control-server/src/Tidepool/Control/Protocol.hs`
- **Existing tools**: `haskell/control-server/src/Tidepool/Control/LSPTools.hs`
