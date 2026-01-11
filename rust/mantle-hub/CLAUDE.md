# mantle-hub

Session visualization and persistence daemon. Tracks Claude Code sessions across multiple runs, stores results in SQLite, and provides HTTP/WebSocket APIs for monitoring.

## Usage

```bash
# Start the hub daemon
mantle-hub serve                   # Default: localhost:7433

# Alternative port
mantle-hub serve --port 8080

# CLI commands (for debugging)
mantle-hub list                    # List all sessions
mantle-hub create --branch fix/auth --worktree /path --prompt "..." --model sonnet
mantle-hub get <session-id>
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                            mantle-hub                                        │
│                                                                              │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │                         Axum HTTP Server                               │ │
│  │                                                                        │ │
│  │  Pages:                       API:                                     │ │
│  │  GET /sessions     (HTML)     POST /api/sessions     (create)         │ │
│  │  GET /sessions/{id}(HTML)     GET  /api/sessions     (list)           │ │
│  │                               GET  /api/sessions/{id}                  │ │
│  │                               DELETE /api/sessions/{id}                │ │
│  │                               POST /api/sessions/{id}/nodes           │ │
│  │                               POST /api/sessions/{id}/nodes/{nid}/result│ │
│  │                               GET  /api/sessions/{id}/graph           │ │
│  │                                                                        │ │
│  │  WebSocket:                                                            │ │
│  │  WS /ws                       Frontend subscribes for live updates    │ │
│  │  WS /ws/push/{sid}/{nid}      mantle pushes events during execution   │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                      │                                      │
│                                      ▼                                      │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │                          SQLite (sqlx)                                 │ │
│  │                                                                        │ │
│  │  sessions: Tree containers                                            │ │
│  │  nodes: Individual Claude executions                                  │ │
│  │  results: Exit code, cost, output                                     │ │
│  │  events: StreamEvent log                                              │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Data Model

### Session
A session is a tree container for related Claude Code executions:
```rust
pub struct SessionInfo {
    pub id: String,
    pub name: String,           // Branch name
    pub state: SessionState,    // pending, running, completed, failed
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}
```

### Node
A node is a single Claude Code execution within a session:
```rust
pub struct NodeInfo {
    pub id: String,
    pub session_id: String,
    pub parent_node_id: Option<String>,  // For forked sessions
    pub branch: String,
    pub worktree: String,
    pub prompt: String,
    pub model: String,
    pub state: NodeState,        // pending, running, completed, failed
    pub result: Option<NodeResultInfo>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}
```

### Result
Attached to a node after completion:
```rust
pub struct NodeResultInfo {
    pub exit_code: i32,
    pub is_error: bool,
    pub result_text: Option<String>,
    pub structured_output: Option<Value>,
    pub total_cost_usd: f64,
    pub num_turns: i64,
    pub cc_session_id: String,
    pub duration_secs: f64,
    pub model_usage: HashMap<String, ModelUsage>,
}
```

## API Reference

### Sessions

| Method | Path | Purpose |
|--------|------|---------|
| `POST` | `/api/sessions` | Create session + root node |
| `GET` | `/api/sessions` | List all sessions |
| `GET` | `/api/sessions/{sid}` | Get session with all nodes |
| `DELETE` | `/api/sessions/{sid}` | Delete session and all nodes |
| `GET` | `/api/sessions/{sid}/graph` | Graph visualization data |

**Create session request:**
```json
{
  "branch": "fix/auth-abc123",
  "worktree": "/path/to/.mantle/worktrees/fix-auth-abc123",
  "prompt": "Fix the authentication bug",
  "model": "sonnet"
}
```

**Create session response:**
```json
{
  "session": { "id": "...", "name": "fix/auth-abc123", ... },
  "root_node": { "id": "...", "session_id": "...", "state": "pending", ... }
}
```

### Nodes

| Method | Path | Purpose |
|--------|------|---------|
| `POST` | `/api/sessions/{sid}/nodes` | Create child node (fork) |
| `GET` | `/api/sessions/{sid}/nodes/{nid}` | Get node details |
| `GET` | `/api/sessions/{sid}/nodes/{nid}/events` | Get node's event stream |
| `POST` | `/api/sessions/{sid}/nodes/{nid}/events` | Append event to node |
| `POST` | `/api/sessions/{sid}/nodes/{nid}/result` | Submit node completion |

**Submit result request:**
```json
{
  "exit_code": 0,
  "is_error": false,
  "result_text": "Successfully fixed the auth bug",
  "structured_output": null,
  "total_cost_usd": 0.15,
  "num_turns": 5,
  "cc_session_id": "abc-123",
  "duration_secs": 45.2,
  "model_usage": { ... }
}
```

### WebSocket

**Frontend subscription (`/ws`):**
Receives `HubEvent` messages:
```rust
pub enum HubEvent {
    SessionCreated { session: SessionInfo },
    SessionUpdated { session: SessionInfo },
    NodeCreated { node: NodeInfo },
    NodeUpdated { node: NodeInfo },
    NodeCompleted { node: NodeInfo },
}
```

**Event push (`/ws/push/{sid}/{nid}`):**
mantle connects during execution to stream events. Messages are `NodeEvent`:
```rust
pub struct NodeEvent {
    pub event_type: String,  // "system", "assistant", "user", "result"
    pub event_data: Value,   // Full StreamEvent JSON
    pub timestamp: DateTime<Utc>,
}
```

## Database Schema

```sql
-- Session tree containers
CREATE TABLE sessions (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL
);

-- Individual Claude executions
CREATE TABLE nodes (
    id TEXT PRIMARY KEY,
    session_id TEXT NOT NULL,
    parent_node_id TEXT,
    branch TEXT NOT NULL,
    worktree TEXT NOT NULL,
    prompt TEXT NOT NULL,
    model TEXT NOT NULL,
    state TEXT NOT NULL DEFAULT 'pending',
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE,
    FOREIGN KEY (parent_node_id) REFERENCES nodes(id) ON DELETE SET NULL
);

-- Node completion data
CREATE TABLE results (
    node_id TEXT PRIMARY KEY,
    exit_code INTEGER NOT NULL,
    is_error INTEGER NOT NULL,
    result_text TEXT,
    structured_output TEXT,
    total_cost_usd REAL NOT NULL,
    num_turns INTEGER NOT NULL,
    cc_session_id TEXT NOT NULL,
    duration_secs REAL NOT NULL,
    model_usage TEXT,
    created_at TEXT NOT NULL,
    FOREIGN KEY (node_id) REFERENCES nodes(id) ON DELETE CASCADE
);

-- StreamEvent log per node
CREATE TABLE events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    node_id TEXT NOT NULL,
    event_type TEXT NOT NULL,
    event_data TEXT NOT NULL,
    timestamp TEXT NOT NULL,
    FOREIGN KEY (node_id) REFERENCES nodes(id) ON DELETE CASCADE
);

-- Indexes
CREATE INDEX idx_nodes_session ON nodes(session_id);
CREATE INDEX idx_nodes_parent ON nodes(parent_node_id);
CREATE INDEX idx_nodes_state ON nodes(state);
CREATE INDEX idx_events_node ON events(node_id);
```

## Module Reference

| File | Purpose |
|------|---------|
| `main.rs` | CLI and server startup |
| `routes.rs` | HTTP API routes |
| `db.rs` | SQLite operations |
| `state.rs` | Application state (pool + broadcast) |
| `error.rs` | Error types |
| `types.rs` | Re-exports from mantle-shared |

## Configuration

| Variable | Default | Purpose |
|----------|---------|---------|
| `MANTLE_HUB_PORT` | 7433 | HTTP server port |
| `MANTLE_HUB_DB` | `~/.tidepool/hub.db` | SQLite database path |
| `RUST_LOG` | info | Tracing log level |

## Frontend

Static files served from `static/`:
- `index.html` - Sessions list view
- `session.html` - Session tree view
- CSS/JS assets

The frontend connects to `/ws` for live updates and renders session trees.

## Testing

```bash
cargo test -p mantle-hub

# Test files:
# tests/integration_http.rs   - HTTP API tests
# tests/integration_db.rs     - Database operations
# tests/integration_socket.rs - WebSocket tests
# tests/integration_e2e.rs    - Full E2E (requires Docker, #[ignore])
```

## Integration Flow

1. **Session start**: mantle calls `POST /api/sessions` → creates session + root node
2. **Events push**: mantle connects to `/ws/push/{sid}/{nid}` → streams events during execution
3. **Result submit**: mantle calls `POST /api/sessions/{sid}/nodes/{nid}/result` → stores completion
4. **Fork**: mantle calls `POST /api/sessions/{sid}/nodes` → creates child node
5. **Frontend**: connects to `/ws` → receives live updates → renders tree

## State Management

`AppState` holds:
- SQLite pool (sqlx)
- Broadcast channel for WebSocket events

```rust
pub struct AppState {
    pub pool: SqlitePool,
    pub broadcast_tx: broadcast::Sender<HubEvent>,
}
```

All API operations broadcast events to connected WebSocket clients.
