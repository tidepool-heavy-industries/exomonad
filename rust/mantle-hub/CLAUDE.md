# mantle-hub

Session visualization hub. **Currently legacy code from headless mode** - needs repurposing for metrics.

> **Status:** The current implementation tracks sessions/nodes for headless orchestration. The planned metrics hub (tool call traces, Grafana export) is not yet implemented.

## What Exists Today

An HTTP + WebSocket server for session/node tracking:

```bash
mantle-hub serve              # Start server on port 7433
mantle-hub list               # List sessions
mantle-hub create --branch x  # Create session
mantle-hub get <session-id>   # Get session details
```

### Current Data Model

**Session** - An orchestration run (tree of nodes):
```rust
pub struct SessionInfo {
    pub id: String,
    pub name: String,
    pub state: SessionState,  // Pending, Running, Completed, Failed
    pub created_at: DateTime<Utc>,
}
```

**Node** - An individual Claude Code execution:
```rust
pub struct NodeInfo {
    pub id: String,
    pub session_id: String,
    pub parent_node_id: Option<String>,
    pub branch: String,
    pub worktree: PathBuf,
    pub prompt: String,
    pub model: String,
    pub state: NodeState,  // Pending, Running, Completed, Failed
    pub result: Option<NodeResult>,
}
```

### Current API

| Method | Path | Purpose |
|--------|------|---------|
| `POST` | `/sessions` | Create session |
| `GET` | `/sessions` | List sessions |
| `GET` | `/sessions/:id` | Get session with nodes |
| `DELETE` | `/sessions/:id` | Delete session |
| `POST` | `/sessions/:id/nodes` | Create node |
| `GET` | `/sessions/:id/nodes/:nid` | Get node |
| `PUT` | `/sessions/:id/nodes/:nid/result` | Submit node result |
| `WS` | `/ws` | Live updates |

### Module Reference

| File | Purpose |
|------|---------|
| `main.rs` | CLI and server startup |
| `routes.rs` | HTTP API routes (session/node CRUD) |
| `db.rs` | SQLite operations for sessions/nodes |
| `socket.rs` | Unix socket for container communication |
| `state.rs` | Application state (db handle, WebSocket broadcast) |
| `types.rs` | Re-exports from mantle-shared |
| `error.rs` | Error types |

## What's Planned (TODO)

Repurpose for Claude Code++ metrics:

### New Data Model
```rust
pub struct ToolCallTrace {
    pub id: String,
    pub session_id: String,
    pub timestamp: DateTime<Utc>,
    pub tool_name: String,
    pub duration_ms: u64,
    pub arguments: Value,  // Redacted
    pub result: Value,     // Redacted
    pub success: bool,
}

pub struct HookEvent {
    pub id: String,
    pub session_id: String,
    pub event_type: String,
    pub allowed: bool,
    pub duration_ms: u64,
}
```

### New API
```
POST /api/metrics/tool     Record tool call
POST /api/metrics/hook     Record hook event
GET  /api/metrics          Query metrics
GET  /metrics              Prometheus endpoint
```

### Grafana Integration
- OTLP traces to Tempo
- Prometheus metrics endpoint
- Dashboards for tool usage, costs, errors

## Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `MANTLE_HUB_PORT` | 7433 | HTTP server port |
| `MANTLE_HUB_DB` | `~/.tidepool/hub.db` | SQLite database path |
| `RUST_LOG` | info | Tracing log level |

## Testing

```bash
cargo test -p mantle-hub
```

## Migration Notes

The hub needs significant changes for Claude Code++:

1. **Remove**: Session tree visualization, node result storage
2. **Add**: Tool call traces, hook events, metrics aggregation
3. **Add**: Prometheus endpoint, OTLP export
4. **Add**: Data redaction for sensitive fields

The old session/node code can be removed once we confirm no consumers depend on it.
