# Tidepool Control Server

Unix socket server for Claude Code++ integration. Receives hook events and MCP tool calls from mantle-agent via NDJSON over Unix socket. Provides semantic code exploration via the `scout` MCP tool.

## Architecture

```
Claude Code (human-driven)
    │
    └─ MCP ──► mantle-agent ──► Unix socket ──► Control Server (Haskell)
                                 (.tidepool/control.sock)
                                                   │
                                                   ├─ MCP: "scout" tool
                                                   │    ├─ LSP (code intelligence)
                                                   │    └─ FunctionGemma (local model)
                                                   │
                                                   └─ Hook handlers (passthrough)
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

## What It Does

- Listens on Unix socket at `.tidepool/control.sock`
- Maintains long-lived LSP session for code intelligence
- Accepts NDJSON connections (JSON + newline framing)
- Parses `ControlMessage` (HookEvent | McpToolCall)
- Routes to appropriate handlers
- Returns `ControlResponse` (HookResponse | McpToolResponse)

## MCP Tools

### `scout` - Semantic Code Exploration

Explores code using LSP (hover, references, workspace symbols) and scores relevance with FunctionGemma heuristics.

**Request**:
```json
{
  "type": "MCPToolCall",
  "id": "1",
  "tool_name": "scout",
  "arguments": {
    "query": "What breaks if I add a new variant to LLMKind?",
    "symbols": ["LLMKind"],
    "depth": "medium"
  }
}
```

**Response**:
```json
{
  "type": "MCPToolResponse",
  "id": "1",
  "result": {
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
}
```

## Hook Handlers

All hooks are currently logged and allowed (passthrough). No blocking logic yet.

- PreToolUse → allow with `permissionDecision: "allow"`
- PostToolUse → allow with no additional context
- Other hooks → continue

## Running

```bash
# Start in project directory (creates .tidepool/control.sock)
cd /path/to/your/project
cabal run tidepool-control-server

# Or set project directory via environment
TIDEPOOL_PROJECT_DIR=/path/to/project cabal run tidepool-control-server
```

Server logs to stdout:
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

# Send test message via socat
echo '{"type":"MCPToolCall","id":"1","tool_name":"scout","arguments":{"query":"What breaks?","symbols":["LLMKind"],"depth":"low"}}' | \
  socat - UNIX-CONNECT:.tidepool/control.sock
```

## Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `TIDEPOOL_PROJECT_DIR` | Current directory | Project root (where .tidepool/ lives) |

## Module Structure

| File | Purpose |
|------|---------|
| `Protocol.hs` | ControlMessage/Response types (matches Rust) |
| `Server.hs` | Unix socket listener, LSP session, NDJSON framing |
| `Handler.hs` | Message routing (threads LSP session) |
| `Handler/Hook.hs` | Hook event handler (passthrough) |
| `Handler/MCP.hs` | MCP tool handler (scout implementation) |
| `Scout/*.hs` | Semantic exploration (merged from semantic-scout) |
| `Main.hs` | Entry point |

### Scout Modules

| Module | Purpose |
|--------|---------|
| `Scout/Types.hs` | ScoutQuery, ScoutResponse, Pointer |
| `Scout/EdgeTypes.hs` | EdgeType enum, EdgeContext |
| `Scout/Scoring.hs` | Composite scoring, Frontier priority queue |
| `Scout/Graph.hs` | Self-looping exploration graph |
| `Scout/Explore.hs` | BFS exploration loop (pure + effectful) |
| `Scout/Gemma.hs` | Gemma effect (heuristic fallback) |
| `Scout/Heuristics.hs` | Node/edge scoring heuristics |
| `Scout/LSP.hs` | LSP helpers (location formatting) |
| `Scout/Templates.hs` | Jinja templates for FunctionGemma prompts |

## Integration with Claude Code

1. Start control-server in project directory
2. Configure mantle-agent to connect to `.tidepool/control.sock`
3. In Claude Code: call `mcp__semantic-scout__scout` tool
4. Control server runs exploration with LSP + Gemma
5. Returns pointers and relevance scores

## Next Steps

1. Wire real FunctionGemma model (currently using heuristics)
2. Add more MCP tools (e.g., beads task tracking)
3. Wire hook handlers to effect stack
4. Add metrics collection to mantle-hub
