# Tidepool Control Server

Unix socket server for Claude Code++ integration. Receives hook events and MCP tool calls from mantle-agent via NDJSON over Unix socket. Provides semantic code exploration via the `scout` MCP tool.

## When to Read This

Read this if you're:
- Understanding the Claude Code++ integration architecture
- Working on hook handling or MCP tool implementation
- Debugging the control server or data flow
- Adding new MCP tools to the system

## Architecture Overview

```
┌────────────────────────────────────────────────────────────────────┐
│ User TTY (Zellij 2-pane layout)                                    │
│  ┌──────────────────────────┐  ┌─────────────────────────────────┐│
│  │ Pane 1: Claude Code      │  │ Pane 2: control-server          ││
│  │ (user interaction)       │  │ (logs, HLS output)              ││
│  └──────────────────────────┘  └─────────────────────────────────┘│
└────────────────────────────────────────────────────────────────────┘
                │                                    ▲
                │ Hooks/MCP                          │ Logs
                ▼                                    │
┌────────────────────────────────────────────────────────────────────┐
│ mantle-agent (Rust)                                                │
│  • hook subcommand: forwards CC hooks → TCP                        │
│  • mcp subcommand: JSON-RPC stdio server → TCP                     │
└────────────────────────────────┬───────────────────────────────────┘
                                 │ TCP (NDJSON)
                                 │ 127.0.0.1:7432
                                 ▼
┌────────────────────────────────────────────────────────────────────┐
│ control-server (Haskell)                                           │
│  • Unix socket: .tidepool/control.sock                             │
│  • Protocol: ControlMessage/ControlResponse                        │
│  • Long-lived LSP session (HLS)                                    │
│  • Routes:                                                         │
│    - HookEvent → Handler.Hook (passthrough)                        │
│    - McpToolCall → Handler.MCP (scout tool)                        │
└────────────────┬───────────────────────────────────┬───────────────┘
                 │                                   │
                 ▼                                   ▼
        ┌────────────────┐              ┌────────────────────┐
        │ Hook Handler   │              │ Scout Handler      │
        │ (passthrough)  │              │ (LSP + Gemma)      │
        └────────────────┘              └──────────┬─────────┘
                                                   │
                                 ┌─────────────────┼─────────────────┐
                                 ▼                 ▼                 ▼
                        ┌────────────┐   ┌──────────────┐  ┌───────────┐
                        │ LSP (HLS)  │   │ Gemma Effect │  │ Frontier  │
                        │ workspace/ │   │ (scorer)     │  │ (BFS)     │
                        │ symbol,    │   └──────────────┘  └───────────┘
                        │ hover,     │
                        │ references │
                        └────────────┘
```

## .tidepool Directory Convention

Project-local configuration in `.tidepool/` (gitignored):

```
<project-root>/
├── .tidepool/              ← gitignored
│   ├── control.sock        ← Unix socket for IPC
│   └── config.json         ← Optional: project-specific config (future)
├── .gitignore              ← Contains: .tidepool/
└── ...
```

## Complete Data Flow

### Hook Flow (PreToolUse Example)

```
1. User interaction
   ┌──────────────────────────────────────────────┐
   │ Claude Code: wants to call Write tool        │
   │ Generates hook JSON on stdin                 │
   └─────────────────┬────────────────────────────┘
                     │ stdin JSON
                     ▼
2. mantle-agent hook pre-tool-use
   ┌──────────────────────────────────────────────┐
   │ Read HookInput from stdin                    │
   │ Connect to control server (TCP)              │
   │ Send ControlMessage::HookEvent               │
   └─────────────────┬────────────────────────────┘
                     │ TCP NDJSON
                     ▼
3. control-server
   ┌──────────────────────────────────────────────┐
   │ Accept connection on Unix socket             │
   │ Parse ControlMessage                         │
   │ Route to handleHook                          │
   │   → Log and return allowPreToolUse           │
   │ Send ControlResponse::HookResponse           │
   └─────────────────┬────────────────────────────┘
                     │ TCP response
                     ▼
4. mantle-agent returns
   ┌──────────────────────────────────────────────┐
   │ Receive HookResponse                         │
   │ Print HookOutput JSON to stdout              │
   │ Exit with exit_code (0 = allow)              │
   └─────────────────┬────────────────────────────┘
                     │ stdout JSON
                     ▼
5. Claude Code processes
   ┌──────────────────────────────────────────────┐
   │ Parse hook response                          │
   │ permissionDecision = "allow"                 │
   │ → Execute Write tool                         │
   └──────────────────────────────────────────────┘
```

### MCP Tool Flow (scout)

```
1. User asks question
   ┌──────────────────────────────────────────────┐
   │ "What breaks if I add a variant to LLMKind?" │
   │ Claude plans to use scout MCP tool           │
   └─────────────────┬────────────────────────────┘
                     │
                     ▼
2. Claude Code calls MCP
   ┌──────────────────────────────────────────────┐
   │ Spawn: mantle-agent mcp                      │
   │ Send JSON-RPC: tools/call                    │
   │   { name: "scout", arguments: {...} }        │
   └─────────────────┬────────────────────────────┘
                     │ stdin (JSON-RPC)
                     ▼
3. mantle-agent mcp
   ┌──────────────────────────────────────────────┐
   │ Parse JSON-RPC request                       │
   │ Connect to control server (TCP)              │
   │ Send ControlMessage::McpToolCall             │
   └─────────────────┬────────────────────────────┘
                     │ TCP NDJSON
                     ▼
4. control-server routes
   ┌──────────────────────────────────────────────┐
   │ Parse ControlMessage::McpToolCall            │
   │ Route to handleMcpTool                       │
   │   case "scout" → handleScoutTool             │
   └─────────────────┬────────────────────────────┘
                     │
                     ▼
5. Scout exploration
   ┌──────────────────────────────────────────────┐
   │ Parse ScoutQuery arguments                   │
   │ Run: runM $ runGemmaHeuristic $              │
   │      runLSP session $ exploreEff query       │
   │                                              │
   │ Exploration steps:                           │
   │  a) Find entry points (LSP workspace/symbol) │
   │  b) BFS loop:                                │
   │     - Fetch context (LSP hover + file read)  │
   │     - Score node (Gemma effect)              │
   │     - Decide expansion (heuristics)          │
   │     - Queue children (LSP references)        │
   │  c) Collect results                          │
   │     - Top pointers (sorted by score)         │
   │     - Training examples (query+node+rubric)  │
   └─────────────────┬────────────────────────────┘
                     │ ScoutResponse
                     ▼
6. LSP communication (within exploration)
   ┌──────────────────────────────────────────────┐
   │ runLSP session $ workspaceSymbol "LLMKind"   │
   │   → executeSession (send to worker thread)   │
   │   → Worker in Session monad:                 │
   │      request L.SMethod_WorkspaceSymbol       │
   │   → lsp-test library:                        │
   │      JSON-RPC to HLS via stdio               │
   │   ← HLS response: [SymbolInformation]        │
   │   → Return to caller                         │
   └─────────────────┬────────────────────────────┘
                     │ LSP results
                     ▼
7. Scoring (within exploration)
   ┌──────────────────────────────────────────────┐
   │ rateEdge query edge                          │
   │   → runGemmaHeuristic interpreter:           │
   │      scoreEdge query edge (Heuristics.hs)    │
   │        - Pattern matching on hover info      │
   │        - Keyword detection in query          │
   │        - Risk/relevance heuristics           │
   │   → Returns Rubric                           │
   │                                              │
   │ Alternative (with GEMMA_ENDPOINT):           │
   │   → runGemmaHTTP interpreter:                │
   │      - Format prompt (2-turn minimal)        │
   │      - POST to mistralrs-server              │
   │      - Parse <start_function_call>           │
   │   → Returns Rubric from model                │
   └─────────────────┬────────────────────────────┘
                     │ Rubric
                     ▼
8. Return response
   ┌──────────────────────────────────────────────┐
   │ Build ScoutResponse:                         │
   │  { summary, pointers, nodesVisited,          │
   │    trainingExamples }                        │
   │ Serialize to JSON (toJSON)                   │
   │ Send ControlResponse::McpToolResponse        │
   └─────────────────┬────────────────────────────┘
                     │ TCP response
                     ▼
9. mantle-agent mcp returns
   ┌──────────────────────────────────────────────┐
   │ Receive McpToolResponse                      │
   │ Format as JSON-RPC result                    │
   │ Write to stdout                              │
   └─────────────────┬────────────────────────────┘
                     │ stdout (JSON-RPC)
                     ▼
10. Claude Code processes
   ┌──────────────────────────────────────────────┐
   │ Parse JSON-RPC response                      │
   │ Extract ScoutResponse                        │
   │ Claude analyzes pointers and summary         │
   │ Generates response to user                   │
   └─────────────────┬────────────────────────────┘
                     │
                     ▼
11. User sees answer
   ┌──────────────────────────────────────────────┐
   │ "Based on semantic code exploration,         │
   │  adding a variant to LLMKind will affect     │
   │  15 locations:                               │
   │                                              │
   │  High Risk Locations:                        │
   │  • Types.hs:87 - Exhaustive pattern match    │
   │  • Handler.hs:123 - Pattern match dispatch   │
   │  ..."                                        │
   └──────────────────────────────────────────────┘
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Server.hs` | Unix socket listener, LSP session management, NDJSON framing |
| `Protocol.hs` | ControlMessage/Response types (matches Rust protocol.rs) |
| `Handler.hs` | Message routing (threads LSP session through handlers) |
| `Handler/Hook.hs` | Hook event handler (currently passthrough) |
| `Handler/MCP.hs` | MCP tool handler (scout implementation) |
| `Scout/Types.hs` | ScoutQuery, ScoutResponse, Pointer, TrainingExample |
| `Scout/Explore.hs` | BFS exploration loop (pure + effectful versions) |
| `Scout/Gemma.hs` | Scoring effect + interpreters (heuristic/HTTP/mock) |
| `Scout/Heuristics.hs` | Pattern-based scoring rules and expansion decisions |
| `Scout/LSP.hs` | LSP integration helpers |
| `Scout/Templates.hs` | Jinja templates for FunctionGemma prompts |

## MCP Tools

### `scout` - Semantic Code Exploration

Explores code using LSP (hover, references, workspace symbols) and scores relevance with FunctionGemma heuristics or trained model.

**Request Schema:**
```json
{
  "query": "What breaks if I add a new variant to LLMKind?",
  "symbols": ["LLMKind"],
  "depth": "medium"
}
```

**Response Schema:**
```json
{
  "summary": "## Exploration Summary\n\nQuery: What breaks...",
  "pointers": [
    {
      "location": "Types.hs:45",
      "what": "data LLMKind = Anthropic | OpenAI | Local",
      "risk": 4,
      "relevance": 5,
      "tags": ["Exhaustive", "TypeFamily"],
      "action": "Add case for new variant"
    }
  ],
  "nodesVisited": 15,
  "trainingExamples": [...]
}
```

## Hook Handlers

All hooks are currently logged and allowed (passthrough). No blocking logic yet.

**Current behavior:**
- `PreToolUse` → allow with `permissionDecision: "allow"`
- `PostToolUse` → allow with no additional context
- Other hooks → continue with default response

**Future:** Wire hooks to Tidepool effect stack for real decision logic.

## Protocol Types

### ControlMessage (Rust → Haskell)

```haskell
data ControlMessage
  = HookEvent { input :: HookInput }
  | McpToolCall { mcpId :: Text, toolName :: Text, arguments :: Value }
```

### ControlResponse (Haskell → Rust)

```haskell
data ControlResponse
  = HookResponse { output :: HookOutput, exitCode :: Int }
  | McpToolResponse { mcpId :: Text, result :: Maybe Value, mcpError :: Maybe McpError }
```

**IMPORTANT:** Types must serialize identically to Rust `mantle_shared::protocol` types.

## LSP Session Management

The control server maintains a **long-lived LSP session** via `withLSPSession`:

```haskell
main :: IO ()
main = do
  config <- readServerConfig
  withLSPSession config.projectDir $ \lspSession -> do
    bracket (setupSocket) (closeSocket) $ \sock -> do
      forever $ do
        (conn, _) <- accept sock
        forkIO $ handleConnection lspSession conn
```

**Benefits:**
- Amortizes HLS startup cost (~5s) across many queries
- Thread-safe via Chan + worker pattern (multiple concurrent queries)
- Shared workspace index (HLS only indexes once)

## Running

```bash
# Start in project directory (creates .tidepool/control.sock)
cd /path/to/your/project
cabal run tidepool-control-server

# Or set project directory via environment
TIDEPOOL_PROJECT_DIR=/path/to/project cabal run tidepool-control-server

# With FunctionGemma model (optional)
GEMMA_ENDPOINT=http://localhost:8080 cabal run tidepool-control-server
```

**Server logs to stdout:**
```
Created .tidepool directory at ./.tidepool
Starting LSP session for project: .
[LSP] Session started, HLS initialized
LSP session initialized
Control server listening on ./.tidepool/control.sock
Connection received
[MCP] tool=scout
  query=What breaks if I add a new variant?
  symbols=LLMKind
  depth=Medium
  found 15 locations
[MCP] -> success
```

## Testing

```bash
# Check socket exists
ls -la .tidepool/control.sock

# Send test message via socat (hook)
echo '{"type":"HookEvent","input":{...}}' | \
  socat - UNIX-CONNECT:.tidepool/control.sock

# Send test message via socat (MCP)
echo '{"type":"MCPToolCall","id":"1","tool_name":"scout","arguments":{...}}' | \
  socat - UNIX-CONNECT:.tidepool/control.sock
```

## Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `TIDEPOOL_PROJECT_DIR` | Current directory | Project root (where .tidepool/ lives) |
| `GEMMA_ENDPOINT` | None | FunctionGemma inference server (optional) |
| `MANTLE_CONTROL_HOST` | (set by mantle-agent) | TCP host (unused by server, only client) |
| `MANTLE_CONTROL_PORT` | (set by mantle-agent) | TCP port (unused by server, only client) |

## Integration with Claude Code

### Configuration (.claude/settings.local.json)

```json
{
  "hooks": {
    "PreToolUse": "mantle-agent hook pre-tool-use"
  },
  "mcpServers": {
    "tidepool": {
      "command": "mantle-agent",
      "args": ["mcp"],
      "env": {
        "MANTLE_CONTROL_HOST": "127.0.0.1",
        "MANTLE_CONTROL_PORT": "7432"
      }
    }
  }
}
```

### Workflow

1. **Start control-server** in project directory (pane 2)
2. **Start Claude Code** in same directory (pane 1)
3. **User asks question** → Claude calls scout tool
4. **control-server explores** with LSP + Gemma
5. **Returns pointers** → Claude synthesizes answer

## Related Documentation

- **[rust/mantle-agent/CLAUDE.md](../../rust/mantle-agent/CLAUDE.md)** - Hook/MCP forwarding
- **[rust/mantle-shared/CLAUDE.md](../../rust/mantle-shared/CLAUDE.md)** - Protocol types (Rust side)
- **[haskell/effects/lsp-interpreter/CLAUDE.md](../effects/lsp-interpreter/CLAUDE.md)** - LSP integration
- **[haskell/tools/training-generator/CLAUDE.md](../tools/training-generator/CLAUDE.md)** - Training data format
- **[haskell/agents/semantic-scout/CLAUDE.md](../agents/semantic-scout/CLAUDE.md)** - Scout implementation details
- **[Root CLAUDE.md](../../CLAUDE.md)** - Project overview

## Next Steps

1. Wire real hook logic (currently passthrough)
2. Add more MCP tools (beads task tracking, etc.)
3. Add metrics collection to mantle-hub
4. Implement FunctionGemma HTTP interpreter (already done in Gemma.hs)
5. Generate and use training data for fine-tuning
