# Mantle: Claude Code Session Orchestration

Rust workspace for spawning, managing, and visualizing Claude Code sessions. Used by Tidepool's Haskell orchestrator to control Claude Code as typed graph nodes.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          Haskell Orchestrator                                │
│                    (tidepool-session-interpreter)                            │
└─────────────────────────────┬───────────────────────────────────────────────┘
                              │ spawns subprocess
                              ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         mantle (host binary)                                 │
│  • Session lifecycle (start/continue/fork/list/cleanup)                      │
│  • Git worktree isolation per session                                        │
│  • Docker container orchestration                                            │
│  • Hook configuration generation                                             │
└─────────────────────────────┬───────────────────────────────────────────────┘
                              │ docker run
                              ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                      Docker Container                                        │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │                      Claude Code                                        │ │
│  │  • Runs prompts with --stream-json                                      │ │
│  │  • Triggers hooks on tool use, decisions, etc.                          │ │
│  └──────────────────────────┬─────────────────────────────────────────────┘ │
│                             │ hook scripts call                              │
│  ┌──────────────────────────▼─────────────────────────────────────────────┐ │
│  │                    mantle-agent                                         │ │
│  │  • Hook handler: forwards decisions to orchestrator                     │ │
│  │  • MCP server: exposes decision tools to Claude                         │ │
│  └─────────────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────────┘
                              │
                              │ HTTP/WebSocket
                              ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         mantle-hub                                           │
│  • SQLite persistence for sessions/nodes/results                             │
│  • REST API for session queries                                              │
│  • WebSocket for live streaming                                              │
│  • HTML visualization UI                                                     │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Workspace Members

| Crate | Type | Purpose |
|-------|------|---------|
| [mantle](mantle/CLAUDE.md) | Binary | Host-side CLI for session management |
| [mantle-agent](mantle-agent/CLAUDE.md) | Binary | Container-side hook handler + MCP server |
| [mantle-hub](mantle-hub/CLAUDE.md) | Binary | Session visualization daemon |
| [mantle-shared](mantle-shared/CLAUDE.md) | Library | Shared types, protocols, IPC utilities |

## Quick Reference

### Building
```bash
cargo build --release           # Build all crates
cargo build -p mantle          # Build just mantle
cargo test                     # Run all tests
```

### Running
```bash
# Start hub daemon (required for session tracking)
mantle-hub serve

# Start a new session
mantle session start --slug fix/auth-bug --prompt "Fix the auth bug"

# Continue existing session
mantle session continue <session-id> --prompt "Now add tests"

# Fork a session (create child from parent context)
mantle session fork <parent-id> --child-slug refactor --child-prompt "Refactor"

# List sessions
mantle session list --state running
```

### Environment Variables
| Variable | Used By | Purpose |
|----------|---------|---------|
| `MANTLE_HUB_URL` | mantle | Hub HTTP endpoint (default: http://localhost:7433) |
| `TIDEPOOL_CONTROL_SOCKET` | mantle-agent | Unix socket for hook decisions |
| `MANTLE_DECISION_TOOLS` | mantle-agent | JSON array of MCP decision tools |
| `MANTLE_HOOK_SOCKET` | mantle-agent | Socket path for MCP tool call reporting |
| `RUST_LOG` | all | Tracing log level (e.g., `debug`, `mantle=trace`) |

## Communication Protocols

### Stdout JSON (mantle → Haskell)
Sessions output structured JSON to stdout for Haskell consumption:
```json
{
  "exit_code": 0,
  "is_error": false,
  "session_id": "sess-abc123",
  "result": "Task completed successfully",
  "structured_output": { ... },
  "total_cost_usd": 0.15,
  "num_turns": 5,
  "interrupts": [],
  "tool_calls": [...]
}
```

### Control Socket (mantle-agent ↔ Haskell)
NDJSON over Unix domain socket:
```rust
// Agent sends
ControlMessage::HookEvent { input: HookInput }
ControlMessage::ToolCall { name, input }

// Orchestrator responds
ControlResponse::HookResponse { exit_code, hook_specific_output }
ControlResponse::Success { value }
ControlResponse::Error { message }
```

### Hub REST API
```
POST /api/sessions           Create session + root node
POST /api/sessions/empty     Create empty session (no root node)
GET  /api/sessions           List all sessions
GET  /api/sessions/{id}      Get session with nodes
DELETE /api/sessions/{id}    Delete session
POST /api/sessions/{sid}/nodes    Add node to existing session
GET  /api/sessions/{sid}/graph    Get graph visualization data
WS   /ws                     Subscribe to updates
WS   /ws/push/{sid}/{nid}    Push events for a node
```

## Graph Execution Tracking

For orchestrating recursive graph/subgraph executions (concurrent swarm), mantle supports tracking all nodes within a single hub session:

### Flow
1. **Orchestrator creates empty hub session** (via direct HTTP to hub):
   ```bash
   curl -X POST http://localhost:7433/api/sessions/empty \
     -d '{"name":"run-1"}'
   # Returns: {"session":{"id":"uuid-123",...}}
   ```

2. **Orchestrator spawns mantle for each node** with tracking args:
   ```bash
   mantle session start \
     --hub-session-id "uuid-123" \
     --execution-id "run-1" \
     --node-path "n0" \
     --node-type "hTypes" \
     --prompt "Generate types" \
     --model sonnet
   ```

3. **For nested nodes** (subgraphs), include parent reference:
   ```bash
   mantle session start \
     --hub-session-id "uuid-123" \
     --execution-id "run-1" \
     --node-path "n2.n0" \
     --node-type "hTypeAdversary" \
     --parent-hub-node-id "parent-node-uuid" \
     --prompt "Review types" \
     --model sonnet
   ```

### Branch Naming
Hierarchical branches: `{execution_id}/{node_path}/{node_type}-{6hex}`
- Root: `run-1/hTypes-a3f2c1`
- Nested: `run-1/n2/n0/hTypeAdversary-b4e5d2`

### Node Metadata
Stored in hub for querying:
```json
{
  "execution_id": "run-1",
  "node_path": "n2.n0",
  "node_type": "hTypeAdversary",
  "depth": 2
}
```

## Haskell Integration

The Rust components integrate with Tidepool's Haskell side via:

1. **Subprocess spawning**: Haskell's `tidepool-session-interpreter` spawns `mantle` as a child process
2. **Stdout capture**: Session results returned as JSON on stdout
3. **Control socket**: Real-time hook decisions over Unix socket during execution
4. **Decision tools**: MCP protocol for typed sum-type outputs (approve/reject/etc.)

See `haskell/effects/session-interpreter/` for the Haskell side.

## Data Storage

| Location | Contents |
|----------|----------|
| `.mantle/sessions.json` | Session state (per-repo, file-locked) |
| `.mantle/worktrees/` | Git worktrees for session isolation |
| `~/.tidepool/hub.db` | SQLite database (mantle-hub) |
| `~/.config/mantle/config.toml` | Docker and hub configuration |

## Docker Container

The `mantle/Dockerfile` builds the container image:
- Base: `debian:bookworm-slim`
- Claude Code via npm
- Non-root user (required by Claude Code)
- Mount points for auth credentials and sockets

Build: `docker build -t mantle-agent:latest -f rust/mantle/Dockerfile .`

## Testing

```bash
cargo test                              # All tests
cargo test -p mantle-hub                # Hub tests only
cargo test -- --ignored                 # Include E2E tests (requires Docker)
```

Integration tests in each crate's `tests/` directory cover:
- Control socket communication
- FIFO creation/cleanup
- Process supervision
- HTTP API endpoints
- Database operations

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| Typed errors (thiserror) | Structured error handling over anyhow |
| Synchronous hook socket | Hooks block anyway; simpler without async |
| RAII FIFO cleanup | FifoGuard ensures cleanup on all paths |
| Control char stripping | TTY output corrupts JSON; sanitize early |
| File locking for state | Multiple sessions may run concurrently |
| SQLite for hub | Simple, embedded, good enough for session data |
