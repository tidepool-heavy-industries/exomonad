# 03: MCP Server Harness

## Goal

Implement MCP server that exposes LLMNode as tool. Depends on library choice from 02.

## Architecture

```
┌─────────────────────────────────────────┐
│           MCP Server Harness            │
│                                         │
│  ┌─────────────┐    ┌────────────────┐ │
│  │ JSON-RPC    │───►│ Tool Router    │ │
│  │ (stdio)     │    │                │ │
│  └─────────────┘    └───────┬────────┘ │
│                             │          │
│                     ┌───────▼────────┐ │
│                     │ LLMNode Runner │ │
│                     │                │ │
│                     │ - Build prompt │ │
│                     │ - Call LLM     │ │
│                     │ - Parse output │ │
│                     └────────────────┘ │
└─────────────────────────────────────────┘
```

## Core Types

```haskell
-- | An LLM node packaged for MCP exposure
data McpTool where
  McpTool
    :: (FromJSON i, ToJSON o, HasToolDef node ~ 'True)
    => { mtNode :: node
       , mtRunner :: i -> IO o  -- Simplified; real impl uses Eff
       }
    -> McpTool

-- | MCP server configuration
data McpConfig = McpConfig
  { mcName :: Text
  , mcVersion :: Text
  , mcTools :: [McpTool]
  }

-- | Run MCP server
runMcpServer :: McpConfig -> IO ()
```

## Implementation Steps

### 1. Tool Listing

```haskell
handleToolsList :: McpConfig -> Value
handleToolsList cfg = object
  [ "tools" .= map toolToJson (mcTools cfg)
  ]

toolToJson :: McpTool -> Value
toolToJson (McpTool node _) = object
  [ "name" .= toolName node        -- from ToolDef
  , "description" .= toolDesc node -- from ToolDef
  , "inputSchema" .= inputSchema node -- from Input type
  ]
```

### 2. Tool Execution

```haskell
handleToolsCall :: McpConfig -> Text -> Value -> IO Value
handleToolsCall cfg name args = do
  case findTool name (mcTools cfg) of
    Nothing -> pure $ errorResponse "Tool not found"
    Just (McpTool _ runner) -> do
      case fromJSON args of
        Error e -> pure $ errorResponse e
        Success input -> do
          result <- runner input
          pure $ object ["content" .= toJSON result]
```

### 3. Main Loop

```haskell
runMcpServer :: McpConfig -> IO ()
runMcpServer cfg = do
  -- Use library from 02, or:
  forever $ do
    line <- getLine
    case decode line of
      Nothing -> hPutStrLn stderr "Invalid JSON"
      Just req -> do
        resp <- handleRequest cfg req
        putStrLn (encode resp)
```

## Files to Create

```
haskell/effects/mcp-server/
├── tidepool-mcp-server.cabal
├── src/
│   └── Tidepool/
│       └── MCP/
│           ├── Server.hs      -- Main entry point
│           ├── Types.hs       -- McpTool, McpConfig
│           ├── Protocol.hs    -- JSON-RPC handling
│           └── Reify.hs       -- Extract tool info from types
```

## Dependencies

- `aeson` - JSON
- Library from 02 (or `bytestring` + `text` if rolling own)
- `tidepool-core` - LLMNode types
- `tidepool-llm-interpreter` - Actually run LLM calls

## Verification

```bash
# Start server
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | cabal run mcp-server

# Should return tool listing with "scout" tool
```
