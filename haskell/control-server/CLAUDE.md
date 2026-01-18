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
   │ Require GEMMA_ENDPOINT env var (no fallback) │
   │ Run: runM $ runGemmaHTTP endpoint $          │
   │      runLSP session $ exploreEff query       │
   │                                              │
   │ Exploration steps:                           │
   │  a) Find entry points (LSP workspace/symbol) │
   │  b) BFS loop:                                │
   │     - Fetch context (LSP hover + file read)  │
   │     - Score edge (Gemma effect via Ollama)   │
   │     - Decide expansion (heuristics)          │
   │     - Queue children (LSP references)        │
   │  c) Collect results                          │
   │     - Top pointers (sorted by score)         │
   │     - Training examples (query+edge+rubric)  │
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
   │ rateEdge query edgeCtx                       │
   │   → runGemmaHTTP interpreter:                │
   │      - Format edge context as plain text     │
   │      - POST to Ollama /api/chat with tools   │
   │      - Parse tool_calls[0].function.arguments│
   │   → Returns Rubric from FunctionGemma 270M   │
   │                                              │
   │ Development alternatives (not in production):│
   │   runGemmaStub    - formats prompt, heuristic│
   │   runGemmaHeuristic - pattern-based rules    │
   │   runGemmaMock    - hardcoded rubric         │
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
| `Server.hs` | TCP listener (port 7432), LSP session management, NDJSON framing |
| `Protocol.hs` | ControlMessage/Response types (matches Rust protocol.rs) |
| `Handler.hs` | Message routing (HookEvent, McpToolCall, ToolsListRequest) |
| `Handler/Hook.hs` | Hook event handler (currently passthrough) |
| `Handler/MCP.hs` | MCP tool router (routes tool calls by name to handlers) |
| `Export.hs` | Automatic MCP tool discovery via `reifyMCPTools` |
| **Tier 1 Tools** | |
| `LSPTools.hs` | Find callers, show fields, show constructors (graph definitions + handlers) |
| **Tier 2 Tools** | |
| `Scout/Graph.hs` | DocGenGraph definition (teach-graph tool, Entry → Init → Process → Select → Expand → Finalize → Exit) |
| `Scout/Graph/Types.hs` | Node input/output types (ProcessInput, SelectInput, etc.) |
| `Scout/Graph/Handlers.hs` | DocGenGraph handlers (init, process, expand, finalize logic) |
| `Scout/Graph/Runner.hs` | runDocGenGraph (executes graph with teaching/production modes) |
| `Scout/Graph/Templates.hs` | SelectTpl (Jinja template for Haiku symbol selection) |
| `Scout/DocGen.hs` | Legacy BFS loop (pre-graph-DSL implementation, still used for non-graph mode) |
| `Scout/DocGen/Types.hs` | TeachQuery, TeachingDoc, LSPSymbol |
| `Scout/DocGen/Gemma.hs` | ScoutGemma effect + interpreters (HTTP via Ollama) |
| `Scout/DocGen/Teacher.hs` | FineTrainingTeacher instance for teaching mode |

## MCP Tools

All tools are **automatically discovered** from Graph DSL annotations via `reifyMCPTools`. Each graph with `MCPExport` on its entry node becomes an MCP tool.

### Tier 1: Deterministic LSP Tools

Pure LSP queries with heuristic filtering. No LLM calls.

#### `find_callers` - Find Actual Call Sites

Finds where a function is actually called, filtering out imports, type signatures, and comments.

**Request Schema:**
```json
{
  "name": "findCallersLogic",          // required
  "context_lines": 2,                  // optional, default 1
  "max_results": 50                    // optional, default 50
}
```

**Implementation:** `LSPTools.hs:156-180` (FindCallersGraph)

#### `show_fields` - Show Record Fields

Shows fields of a Haskell record type with their types and strictness.

**Request Schema:**
```json
{
  "type_name": "MyRecord"              // required
}
```

**Implementation:** `LSPTools.hs:320-331` (ShowFieldsGraph)

#### `show_constructors` - Show Sum Type Constructors

Shows constructors of a Haskell sum type or GADT.

**Request Schema:**
```json
{
  "type_name": "MyType"                // required
}
```

**Implementation:** `LSPTools.hs:452-463` (ShowConstructorsGraph)

### Tier 2: LLM-Enhanced Tools

Tools that use LLM nodes for intelligent filtering/selection.

#### `teach-graph` - Teaching Document Generation

Explores codebase concepts via BFS, using Haiku to select relevant type dependencies. Returns a teaching document with symbols ordered by prerequisites.

**Request Schema:**
```json
{
  "topic": "How the Memory effect works",     // required
  "seeds": ["getMem", "putMem"],              // required: starting symbols
  "budget": 20                                // optional, default 20
}
```

**Response Schema:**
```json
{
  "core": [...],       // depth 0 (seed symbols)
  "prereqs": [...],    // depth 1-2 (dependencies)
  "support": [...]     // depth 3+ (supporting types)
}
```

**Implementation:** `Scout/Graph.hs:125-165` (DocGenGraph)

### Tool Registration

**Automatic discovery:**
```haskell
-- Export.hs:927-943
exportMCPTools :: IO [ToolDefinition]
exportMCPTools = do
  let allTools = concat
        [ reifyMCPTools (Proxy @FindCallersGraph)
        , reifyMCPTools (Proxy @ShowFieldsGraph)
        , reifyMCPTools (Proxy @ShowConstructorsGraph)
        , reifyMCPTools (Proxy @DocGenGraph)  -- teach-graph
        ]
  pure $ map reifyToToolDef allTools
```

**How it works:**
1. `MCPExport` annotation marks entry node for discovery
2. `MCPToolDef '("tool_name", "description")` provides metadata
3. `reifyMCPTools` extracts schema from `HasJSONSchema` instance
4. `exportMCPTools` called on control-server startup
5. mantle-agent queries via `ToolsListRequest`, caches tools

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

## Ollama Setup (Required)

The scout tool requires Ollama running FunctionGemma 270M for semantic scoring.

**Why Ollama?** mistralrs doesn't support Gemma 3 architecture (`Unknown GGUF architecture 'gemma3'`). Ollama has native FunctionGemma support with automatic tool translation.

```bash
# Install Ollama (macOS)
brew install ollama

# Pull FunctionGemma 270M model (~300MB)
ollama pull functiongemma:270m

# Start Ollama server (runs on port 11434 by default)
ollama serve
```

**Verify Ollama is running:**
```bash
curl http://localhost:11434/api/tags
# Should show: {"models":[{"name":"functiongemma:270m",...}]}
```

**API format:** Ollama auto-translates OpenAI-style `tools` array → FunctionGemma's `<start_function_declaration>` format. Response is in `message.tool_calls[0].function.arguments`.

## Running

```bash
# Start in project directory (creates .tidepool/control.sock)
cd /path/to/your/project

# GEMMA_ENDPOINT is REQUIRED (no heuristic fallback)
GEMMA_ENDPOINT=http://localhost:11434 cabal run tidepool-control-server

# Or set project directory via environment
TIDEPOOL_PROJECT_DIR=/path/to/project GEMMA_ENDPOINT=http://localhost:11434 cabal run tidepool-control-server
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
  gemma=http://localhost:11434
[Gemma] HTTP call to http://localhost:11434 for: /path/to/Types.hs:42
[Gemma] -> relevance=5, risk=4
  found 15 locations
[MCP] -> success
```

## Testing

```bash
# Test tool discovery
echo '{"type":"ToolsListRequest"}' | nc localhost 7432
# Should return 4 tools: find_callers, show_fields, show_constructors, teach-graph

# Test teach-graph tool
echo '{"type":"MCPToolCall","id":"1","tool_name":"teach-graph","arguments":{"topic":"how Memory effect works","seeds":["getMem","putMem"],"budget":10}}' | nc localhost 7432

# Test find_callers tool
echo '{"type":"MCPToolCall","id":"2","tool_name":"find_callers","arguments":{"name":"runLSP"}}' | nc localhost 7432
```

## Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `TIDEPOOL_PROJECT_DIR` | Current directory | Project root (where .tidepool/ lives) |
| `GEMMA_ENDPOINT` | **Required** | Ollama endpoint (e.g., `http://localhost:11434`) |
| `MANTLE_CONTROL_HOST` | (set by mantle-agent) | TCP host (unused by server, only client) |
| `MANTLE_CONTROL_PORT` | (set by mantle-agent) | TCP port (unused by server, only client) |

**Note:** `GEMMA_ENDPOINT` must be set. The scout tool will fail with an error if not configured. No heuristic fallback.

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
3. **Claude calls MCP tool** (teach-graph, find_callers, etc.)
4. **mantle-agent forwards** via TCP to control-server
5. **control-server executes** tool logic (LSP queries, graph execution)
6. **Returns result** → Claude uses in response

## Related Documentation

- **[rust/mantle-agent/CLAUDE.md](../../rust/mantle-agent/CLAUDE.md)** - Hook/MCP forwarding
- **[rust/mantle-shared/CLAUDE.md](../../rust/mantle-shared/CLAUDE.md)** - Protocol types (Rust side)
- **[haskell/effects/lsp-interpreter/CLAUDE.md](../effects/lsp-interpreter/CLAUDE.md)** - LSP integration
- **[haskell/tools/training-generator/CLAUDE.md](../tools/training-generator/CLAUDE.md)** - Training data format
- **[haskell/agents/semantic-scout/CLAUDE.md](../agents/semantic-scout/CLAUDE.md)** - Scout implementation details
- **[Root CLAUDE.md](../../CLAUDE.md)** - Project overview

## Next Steps

1. Wire real hook logic (currently passthrough)
2. Add more MCP tools via Graph DSL (beads task tracking, git operations, etc.)
3. Add metrics collection to mantle-hub
4. Test teach-graph end-to-end with Claude inside Claude Code
5. Generate training data for FunctionGemma fine-tuning via teaching mode

## Completed

✅ **MCP Tool Infrastructure**
- Automatic tool discovery via `MCPExport` + `reifyMCPTools`
- 4 tools: find_callers, show_fields, show_constructors, teach-graph
- Type-safe schema generation from `HasJSONSchema` instances

✅ **Tier 1 Tools (Logic-only)**
- FindCallersGraph, ShowFieldsGraph, ShowConstructorsGraph
- LSP integration (workspace/symbol, hover, references)
- Heuristic filtering (imports, type sigs, comments)

✅ **Tier 2 Tools (LLM-enhanced)**
- DocGenGraph with MCPExport annotation
- BFS exploration with Haiku symbol selection
- Teaching document generation with depth-ordered symbols

✅ **Infrastructure**
- FunctionGemma HTTP interpreter via Ollama (`Scout/DocGen/Gemma.hs`)
- Long-lived LSP session management
- TCP protocol (NDJSON) between mantle-agent ↔ control-server
