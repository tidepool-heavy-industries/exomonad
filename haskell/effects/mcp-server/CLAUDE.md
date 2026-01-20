# MCP Server - Expose Tidepool Agents as MCP Tools

Wraps Tidepool LLMNode definitions as MCP (Model Context Protocol) tools and serves them via stdio transport. Enables Tidepool agents to be invoked by Claude Code.

## When to Read This

Read this if you're:
- Exposing a Tidepool agent as an MCP tool
- Understanding how semantic-scout works as an MCP tool
- Implementing MCP tool servers in Haskell
- Working with the vendored mcp-server library
- Debugging MCP protocol issues

## What It Does

Converts typed Tidepool functions into MCP tools that Claude Code can call:

```haskell
-- Define a tool
executeScout :: LSPSession -> ScoutQuery -> IO ScoutResponse
executeScout session query = runM $ runLSP session $ exploreEff query

-- Wrap as MCP tool
main = runMcpServer $ McpConfig
  { mcName = "semantic-scout"
  , mcVersion = "0.1"
  , mcTools =
      [ makeMcpTool
          (Proxy @ScoutQuery)
          "scout"
          "Answer semantic questions about code"
          executeScout
      ]
  }
```

Claude Code can then call it:
```json
{
  "method": "tools/call",
  "params": {
    "name": "scout",
    "arguments": {
      "query": "What breaks if I add a variant to LLMKind?",
      "tags": "Exhaustive,PatternMatch",
      "budget": 20
    }
  }
}
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Claude Code                                                          │
│   MCP client (stdio transport)                                      │
└──────────────────────────────────┬───────────────────────────────────┘
                                   │ JSON-RPC 2.0
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│ runMcpServer (Server.hs)                                            │
│   • Stdio transport                                                 │
│   • JSON-RPC 2.0 protocol                                           │
│   • Tool routing and dispatch                                       │
└──────────────────────────────────┬───────────────────────────────────┘
                                   │
                   ┌───────────────┴───────────────┐
                   ▼                               ▼
          ┌────────────────┐            ┌────────────────┐
          │ listTools      │            │ callTool       │
          │ (metadata)     │            │ (execution)    │
          └────────┬───────┘            └────────┬───────┘
                   │                             │
                   ▼                             ▼
          ┌────────────────────────────────────────────────┐
          │ McpTool (existential wrapper)                  │
          │   • mtRunner :: i -> IO o                      │
          │   • mtInputSchema :: Value (JSON Schema)       │
          └────────────────────────────────────────────────┘
                                   │
                                   ▼
                        Your tool implementation
                        (e.g., semantic-scout)
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Server.hs` | MCP server, tool routing, JSON-RPC handling |
| `Types.hs` | McpTool, McpConfig, existential wrappers |

## Data Model

### McpConfig
```haskell
data McpConfig = McpConfig
  { mcName    :: Text       -- Server name
  , mcVersion :: Text       -- Server version
  , mcTools   :: [McpTool]  -- List of tools
  }
```

### McpTool (Existential Wrapper)
```haskell
data McpTool = forall i o. (FromJSON i, ToJSON o) => McpTool
  { mtName        :: Text
  , mtDescription :: Text
  , mtInputSchema :: Value           -- JSON Schema for input
  , mtRunner      :: i -> IO o       -- The actual computation
  }
```

**Why existential?** Allows heterogeneous collections of tools with different input/output types.

### HasJSONSchema Typeclass
```haskell
class HasJSONSchema a where
  jsonSchema :: Proxy a -> Value

-- Automatically extract JSON Schema from type
instance HasJSONSchema ScoutQuery where
  jsonSchema _ = object
    [ "type" .= "object"
    , "properties" .= object
        [ "query"  .= object ["type" .= "string"]
        , "tags"   .= object ["type" .= "string"]
        , "budget" .= object ["type" .= "integer", "default" .= 20]
        ]
    , "required" .= ["query"]
    ]
```

## Creating an MCP Tool

### Step 1: Define Input/Output Types
```haskell
data MyQuery = MyQuery
  { mqParam1 :: Text
  , mqParam2 :: Int
  } deriving (Generic, FromJSON, ToJSON)

data MyResponse = MyResponse
  { mrResult :: Text
  , mrData   :: [Value]
  } deriving (Generic, FromJSON, ToJSON)
```

### Step 2: Implement HasJSONSchema
```haskell
instance HasJSONSchema MyQuery where
  jsonSchema _ = object
    [ "type" .= "object"
    , "properties" .= object
        [ "param1" .= object ["type" .= "string"]
        , "param2" .= object ["type" .= "integer"]
        ]
    , "required" .= ["param1", "param2"]
    ]
```

### Step 3: Implement Tool Logic
```haskell
executeTool :: MyQuery -> IO MyResponse
executeTool query = do
  -- Your implementation here
  pure $ MyResponse "done" []
```

### Step 4: Wrap as MCP Tool
```haskell
main :: IO ()
main = runMcpServer $ McpConfig
  { mcName = "my-tool-server"
  , mcVersion = "1.0"
  , mcTools =
      [ makeMcpTool
          (Proxy @MyQuery)
          "my_tool"
          "Does something useful"
          executeTool
      ]
  }
```

### Step 5: Configure Claude Code
Add to `.claude/settings.local.json`:
```json
{
  "mcpServers": {
    "my-tool-server": {
      "command": "/path/to/my-tool-server",
      "args": ["--mcp"],
      "env": {}
    }
  }
}
```

## MCP Protocol

### JSON-RPC 2.0 Methods

| Method | Purpose | Request | Response |
|--------|---------|---------|----------|
| `initialize` | Handshake | `{capabilities: {...}}` | `{protocolVersion, capabilities, serverInfo}` |
| `tools/list` | List available tools | `{}` | `{tools: [{name, description, inputSchema}, ...]}` |
| `tools/call` | Execute a tool | `{name, arguments}` | `{content: [{type: "text", text: ...}]}` |

### Example: initialize
```json
// Request
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2024-11-05",
    "capabilities": {},
    "clientInfo": {
      "name": "claude-code",
      "version": "1.0"
    }
  }
}

// Response
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "tools": {}
    },
    "serverInfo": {
      "name": "semantic-scout",
      "version": "0.1"
    }
  }
}
```

### Example: tools/list
```json
// Request
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/list",
  "params": {}
}

// Response
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "tools": [
      {
        "name": "scout",
        "description": "Answer semantic questions about code",
        "inputSchema": {
          "type": "object",
          "properties": {
            "query": {"type": "string"},
            "tags": {"type": "string"},
            "budget": {"type": "integer", "default": 20}
          },
          "required": ["query"]
        }
      }
    ]
  }
}
```

### Example: tools/call
```json
// Request
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "scout",
    "arguments": {
      "query": "What breaks if I add a variant to LLMKind?",
      "tags": "Exhaustive,PatternMatch",
      "budget": 20
    }
  }
}

// Response
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "{\"summary\":\"Found 5 locations...\",\"pointers\":[...],\"nodesVisited\":15}"
      }
    ]
  }
}
```

## Tool Execution Flow

1. **Claude calls tool**: `tools/call` with name + arguments
2. **Server routes**: Finds `McpTool` by name
3. **Deserialize**: `FromJSON i` parses arguments to input type
4. **Execute**: Runs `mtRunner :: i -> IO o`
5. **Serialize**: `ToJSON o` converts output to JSON
6. **Respond**: Wraps in MCP response format

## Error Handling

```haskell
-- Tool execution errors become MCP errors
callTool :: Text -> Value -> IO McpResponse
callTool name args = do
  case findTool name of
    Nothing -> pure $ McpError (-32601) "Tool not found"
    Just tool -> do
      result <- try (runTool tool args)
      case result of
        Left (err :: SomeException) ->
          pure $ McpError (-32000) (Text.pack $ show err)
        Right output ->
          pure $ McpSuccess output
```

MCP error codes:
- `-32700`: Parse error
- `-32600`: Invalid request
- `-32601`: Method not found (tool not found)
- `-32602`: Invalid params
- `-32603`: Internal error
- `-32000` to `-32099`: Server-defined errors

## Vendored Library

The MCP protocol implementation is vendored at:
```
haskell/vendor/mcp-server/
```

**Why vendored?**
- No official Haskell MCP library exists
- Patched for Claude Code compatibility
- Allows custom protocol extensions

**Modifications:**
- Added support for stdio transport
- Fixed JSON-RPC 2.0 compliance issues
- Added tool execution helpers

## Integration with Tidepool

MCP server is **not an effect interpreter**—it's a **transport wrapper** for agents:

```
Tidepool Agent (LLMNode)
    ↓ (exposed via MCP)
MCP Server (stdio)
    ↓ (called by Claude)
Claude Code
```

This is the opposite of `session-interpreter`, which **invokes** Claude Code. MCP server **serves** Claude Code.

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| Existential wrappers | Heterogeneous tool collection without losing types |
| HasJSONSchema typeclass | Auto-generate schemas from types |
| Stdio transport | Standard MCP pattern, works with Claude Code |
| JSON-RPC 2.0 | MCP protocol requirement |
| Vendored library | No official Haskell MCP lib, custom patches needed |
| Not an effect | Tools are standalone, not part of agent execution |

## Comparison: MCP Server vs Session Interpreter

| Aspect | MCP Server | Session Interpreter |
|--------|-----------|---------------------|
| Direction | Tidepool → Claude | Haskell → Claude |
| Purpose | Serve tools to Claude | Invoke Claude as subprocess |
| Protocol | MCP (JSON-RPC 2.0) | Claude Code CLI |
| Transport | Stdio | Subprocess + stdio |
| Example | semantic-scout | V3 TDD agent |

## Related Documentation

- [agents/semantic-scout/CLAUDE.md](../../agents/semantic-scout/CLAUDE.md) - Example MCP tool
- [effects/session-interpreter/CLAUDE.md](../session-interpreter/CLAUDE.md) - Inverse: invoking Claude
- [../CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern
- [Root CLAUDE.md](../../../CLAUDE.md) - Project overview

## Built-in Tools

### bd_dispatch - Optimal Worktree Assignment

Intelligently assigns high-priority unblocked beads to new git worktrees for parallel development.

**Tool Definition:** `Tidepool.MCP.Tools.BdDispatch`

**Purpose:** Supports parallel agent workflows by identifying the N highest-priority beads that are ready to work (no blocking dependencies) and generating the shell commands to create worktrees for them.

**Input Schema:**
```haskell
newtype BdDispatchInput = BdDispatchInput
  { agents :: Maybe Int  -- Number of parallel worktrees to create (default: 1, minimum: 1)
  }
```

**Output Schema:**
```haskell
data BdDispatchOutput = BdDispatchOutput
  { commands :: [Text]  -- Shell commands: "git worktree add <path> -b <branch>"
  , beads :: [Text]     -- Bead IDs assigned
  }
```

**Usage from Claude Code:**
```json
{
  "method": "tools/call",
  "params": {
    "name": "bd_dispatch",
    "arguments": {
      "agents": 3
    }
  }
}
```

**Response Example:**
```json
{
  "commands": [
    "git worktree add /path/to/tidepool-abc -b tidepool-abc",
    "git worktree add /path/to/tidepool-def -b tidepool-def",
    "git worktree add /path/to/tidepool-ghi -b tidepool-ghi"
  ],
  "beads": ["tidepool-abc", "tidepool-def", "tidepool-ghi"]
}
```

**Algorithm:**
1. Validate inputs (must be in git repo, agents > 0)
2. Query all open beads from BD via `bd list --status=open`
3. Filter to unblocked beads (all dependencies are closed)
4. Sort by priority descending (P0=0 first, P4=4 last)
5. Take top N beads
6. Generate `git worktree add` commands for each

**Worktree Path Convention:**
```
<parent-of-current-repo>/<repo-name>-<bead-id>
```

Example: If current repo is `/Users/alice/projects/tidepool`, assigned bead `tidepool-abc` gets:
```
/Users/alice/projects/tidepool-abc
```

**Error Handling:**
- **Not in git repo:** Fails with explicit error (no silent fallbacks)
- **No open beads:** Returns empty lists
- **Invalid agents:** Clamped to minimum of 1

**Design Decisions:**
- **Uses defaultBDConfig:** Tool runs in Claude Code context, not user shell. User's `.bd/config.toml` may not be visible to MCP process.
- **Explicit failures:** Per repo guidelines, prefer failure over undocumented heuristics (no fallback to `.` for repo root).
- **Priority sort order:** Uses `Down` wrapper to sort descending (P0 first).
- **Path construction:** Uses `takeDirectory` instead of manual `".."` for clarity.

**See Also:**
- Beads workflow: [Root CLAUDE.md - Task Tracking](../../../CLAUDE.md#task-tracking-beads)
- BD interpreter: [bd-interpreter/CLAUDE.md](../bd-interpreter/CLAUDE.md)

## Future Work

- Auto-generate HasJSONSchema instances via Template Haskell
- Support HTTP transport (not just stdio)
- Tool composition (one tool calling another)
- Streaming responses (for long-running tools)
- Progress reporting during execution
- bd_dispatch: Read user's `.bd/config.toml` if available in environment
