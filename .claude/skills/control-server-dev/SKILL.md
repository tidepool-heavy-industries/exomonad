---
name: control-server-dev
description: Use when working on the Haskell control-server: adding MCP tools, implementing hook logic, LSP queries, or debugging the TCP server.
---

# Control Server Development

The control-server is the Haskell backend for Claude Code++. It receives messages from exomonad via TCP and executes MCP tools, hook handlers, and LSP queries.

## Quick Reference

| Action | Location |
|--------|----------|
| Add MCP tool | `haskell/control-server/src/ExoMonad/Control/LSPTools.hs` or new graph |
| Add hook logic | `haskell/control-server/src/ExoMonad/Control/Handler/Hook.hs` |
| Add protocol type | `haskell/control-server/src/ExoMonad/Control/Protocol.hs` |
| Register tool for export | `haskell/control-server/src/ExoMonad/Control/Export.hs` |
| LSP session management | `haskell/control-server/src/ExoMonad/Control/Server.hs` |
| MCP routing | `haskell/control-server/src/ExoMonad/Control/Handler/MCP.hs` |

## Architecture

```
exomonad (Rust, spawned per-call)
    │
    │ TCP NDJSON (port 7432)
    ▼
control-server (Haskell, long-lived)
    ├── Server.hs     ← TCP listener, LSP session
    ├── Protocol.hs   ← Message types
    ├── Handler.hs    ← Message routing
    │   ├── Handler/Hook.hs  ← Hook events
    │   └── Handler/MCP.hs   ← Tool dispatch
    └── LSPTools.hs   ← Tier 1 tools (LSP-only)
```

## Tool Tiers

| Tier | Description | LLM? | Example |
|------|-------------|------|---------|
| **Tier 1** | Deterministic LSP queries | No | find_callers, show_fields, show_constructors |
| **Tier 2** | LLM-enhanced exploration | Yes | teach-graph (uses Haiku for symbol selection) |

## Adding a Tier 1 Tool (LSP-only)

### Step 1: Define Graph Structure

```haskell
-- LSPTools.hs
data MyToolGraph (mode :: Mode) = MyToolGraph
  { entry    :: mode :- Entry MyInput :@ MCPExport :@ MCPToolDef '("my_tool", "Description of what it does")
  , process  :: mode :- LogicNode :@ Input MyInput :@ UsesEffects '[LSP, Goto Exit MyOutput]
  , exit     :: mode :- Exit MyOutput
  }
  deriving (Generic)

instance GGraph MyToolGraph
```

### Step 2: Define Input/Output Types

```haskell
data MyInput = MyInput
  { name :: Text
  , maxResults :: Maybe Int
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, HasJSONSchema)

data MyOutput = MyOutput
  { results :: [Text]
  , count :: Int
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, HasJSONSchema)
```

### Step 3: Implement Handler

```haskell
myToolHandlers :: GraphHandlers MyToolGraph
myToolHandlers = mkHandlers $ \_ -> MyToolGraph
  { entry = entryHandler
  , process = handleProcess
  , exit = exitHandler
  }
  where
    handleProcess input = do
      -- Use LSP effects
      symbols <- workspaceSymbol input.name
      let filtered = take (fromMaybe 50 input.maxResults) symbols

      let output = MyOutput
            { results = map symbolName filtered
            , count = length filtered
            }
      pure $ gotoChoice @"exit" output
```

### Step 4: Register in Export.hs

```haskell
-- Export.hs
exportMCPTools :: IO [ToolDefinition]
exportMCPTools = do
  let allTools = concat
        [ reifyMCPTools (Proxy @FindCallersGraph)
        , reifyMCPTools (Proxy @ShowFieldsGraph)
        , reifyMCPTools (Proxy @ShowConstructorsGraph)
        , reifyMCPTools (Proxy @DocGenGraph)
        , reifyMCPTools (Proxy @MyToolGraph)  -- ADD HERE
        ]
  pure $ map reifyToToolDef allTools
```

### Step 5: Add to MCP Handler

```haskell
-- Handler/MCP.hs
handleMcpTool :: LSPSession -> Text -> Value -> IO (Either McpError Value)
handleMcpTool session toolName args = case toolName of
  "find_callers" -> runFindCallers session args
  "show_fields" -> runShowFields session args
  "show_constructors" -> runShowConstructors session args
  "teach-graph" -> runTeachGraph session args
  "my_tool" -> runMyTool session args  -- ADD HERE
  _ -> pure $ Left $ McpError (-32601) "Unknown tool"
```

## Adding Hook Logic

Currently hooks are passthrough (all allowed). To add real logic:

### Edit Handler/Hook.hs

```haskell
handleHook :: HookInput -> IO HookOutput
handleHook input = case input.hookType of
  PreToolUse -> handlePreToolUse input
  PostToolUse -> handlePostToolUse input
  _ -> pure allowContinue

handlePreToolUse :: HookInput -> IO HookOutput
handlePreToolUse input = do
  -- Example: Block writes to certain files
  case input.toolName of
    "Write" | isProtectedFile input.arguments -> do
      pure $ HookOutput
        { permissionDecision = "deny"
        , message = Just "Cannot modify protected file"
        }
    _ -> pure allowPreToolUse

isProtectedFile :: Value -> Bool
isProtectedFile args = case args ^? key "file_path" . _String of
  Just path -> "CLAUDE.md" `T.isSuffixOf` path
  Nothing -> False
```

## LSP Query Patterns

### Workspace Symbol Search
```haskell
runLSP session $ workspaceSymbol "MyType"
-- Returns: [SymbolInformation] with name, kind, location
```

### Find References
```haskell
runLSP session $ findReferences doc pos includeDeclaration
-- Returns: [Location] of all uses
```

### Get Hover Info
```haskell
runLSP session $ hover doc pos
-- Returns: Maybe Hover with type info
```

### Document Symbols
```haskell
runLSP session $ documentSymbols doc
-- Returns: [DocumentSymbol] in file
```

## Testing Tools

### Manual Testing via nc
```bash
# List all tools
echo '{"type":"ToolsListRequest"}' | nc localhost 7432

# Call a tool
echo '{"type":"MCPToolCall","id":"1","tool_name":"find_callers","arguments":{"name":"runLSP"}}' | nc localhost 7432
```

### Check Server Health
```bash
curl http://localhost:7434  # Returns: OK
```

### View Logs
Logs appear in the process-compose TUI (pane 2) or stdout if running standalone.

## Protocol Types (Must Match Rust)

The `Protocol.hs` types must serialize identically to `rust/exomonad-shared/src/protocol.rs`.

### ControlMessage (Incoming)
```haskell
data ControlMessage
  = HookEvent { input :: HookInput }
  | McpToolCall { mcpId :: Text, toolName :: Text, arguments :: Value }
  | ToolsListRequest
  | UIInteraction { ... }
```

### ControlResponse (Outgoing)
```haskell
data ControlResponse
  = HookResponse { output :: HookOutput, exitCode :: Int }
  | McpToolResponse { mcpId :: Text, result :: Maybe Value, mcpError :: Maybe McpError }
  | ToolsListResponse { tools :: [ToolDefinition] }
  | UISpec { ... }
```

## Debugging

### Common Issues

1. **Tool not appearing in MCP**
   - Check `exportMCPTools` includes your graph
   - Verify `MCPExport` annotation on entry node
   - Restart control-server after changes

2. **LSP queries failing**
   - Check HLS is running: `pgrep haskell-language-server`
   - Verify project compiles: `cabal build`
   - Check workspace is indexed (first query may be slow)

3. **Protocol mismatch**
   - Symptoms: Parse errors, unexpected EOF
   - Check `Protocol.hs` matches `protocol.rs` exactly
   - Use `jq` to validate JSON: `echo '...' | jq .`

### Verbose Logging

```haskell
-- Add to handler
liftIO $ putStrLn $ "[MCP] tool=" <> T.unpack toolName
liftIO $ putStrLn $ "  args=" <> show args
```

## Environment Variables

| Variable | Purpose |
|----------|---------|
| `EXOMONAD_PROJECT_DIR` | Project root (where .exo/ lives) |
| `GEMMA_ENDPOINT` | Ollama endpoint (required for Tier 2 tools) |

## File Locations

```
haskell/control-server/
├── app/Main.hs                  ← Entry point
├── src/ExoMonad/Control/
│   ├── Server.hs                ← TCP listener + LSP session
│   ├── Protocol.hs              ← Message types (MUST MATCH RUST)
│   ├── Handler.hs               ← Message routing
│   ├── Handler/Hook.hs          ← Hook handlers
│   ├── Handler/MCP.hs           ← MCP tool dispatch
│   ├── Export.hs                ← Tool discovery + registration
│   └── LSPTools.hs              ← Tier 1 tool graphs
└── exomonad-control-server.cabal
```

## See Also

- **Full docs**: `haskell/control-server/CLAUDE.md`
- **Protocol types (Rust)**: `rust/exomonad-shared/src/protocol.rs`
- **exomonad**: `rust/exomonad/CLAUDE.md`
- **Graph DSL**: `haskell/dsl/core/CLAUDE.md`
