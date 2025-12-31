# Graph Execution Protocol (WebSocket)

WebSocket protocol for client↔worker communication during graph execution.

## Message Types

### Client → Server

#### InitMessage
Start a new graph execution.
```json
{ "type": "init", "graphId": "TestGraph", "input": <any> }
```
- `graphId`: Name of the graph to execute
- `input`: Initial input value (must match graph's Entry type)

#### ResumeMessage
Provide result for a yielded effect.
```json
{ "type": "resume", "result": <EffectResult> }
```
- `result`: Either `{ "type": "success", "value": <any> }` or `{ "type": "error", "message": "<string>" }`

#### ReconnectMessage
Resume a suspended graph after disconnection.
```json
{ "type": "reconnect", "sessionId": "<opaque-token>" }
```
- `sessionId`: Token from previous YieldMessage or ErrorMessage

#### PingMessage
Keepalive heartbeat.
```json
{ "type": "ping" }
```

### Server → Client

#### YieldMessage
Graph yielded an effect for client to handle (or observe).
```json
{ "type": "yield", "effect": <SerializableEffect>, "sessionId": "<token>" }
```
- `effect`: The effect to execute (see Effect Types below)
- `sessionId`: Opaque token for reconnection

#### ProgressMessage
Informational - effect being executed server-side.
```json
{ "type": "progress", "effect": <SerializableEffect>, "status": "executing" }
```
- `effect`: The effect being executed
- `status`: Current status (typically "executing")

#### DoneMessage
Graph completed successfully.
```json
{ "type": "done", "result": <any> }
```
- `result`: Final output value (matches graph's Exit type)

#### ErrorMessage
Graph failed or encountered an error.
```json
{ "type": "error", "message": "<string>", "recoverable": <bool>, "sessionId"?: "<token>" }
```
- `message`: Human-readable error description
- `recoverable`: Whether the session can be resumed
- `sessionId`: Present if session is recoverable

#### PongMessage
Keepalive response.
```json
{ "type": "pong" }
```

## Effect Types

Effects that may appear in YieldMessage or ProgressMessage:

| Type | Description |
|------|-------------|
| `LlmComplete` | LLM completion request (system prompt, user content, schema) |
| `HttpFetch` | HTTP request (url, method) |
| `LogInfo` | Info-level log message |
| `LogError` | Error-level log message |
| `Habitica` | Habitica API operation |

## Lifecycle

1. Client connects via WebSocket to `/session/<sessionId>`
2. Client sends `InitMessage` with graph ID and input
3. Server runs graph, sends `ProgressMessage` for each effect executed server-side
4. If effect needs client input: server sends `YieldMessage`, waits for `ResumeMessage`
5. When graph completes: server sends `DoneMessage`
6. Connection closes (or client can start new graph)

```
Client                         Server
  │                              │
  │──── WebSocket Connect ──────►│
  │                              │
  │──── InitMessage ────────────►│
  │                              │
  │◄─── ProgressMessage ─────────│  (effect executing)
  │◄─── ProgressMessage ─────────│  (another effect)
  │                              │
  │◄─── YieldMessage ────────────│  (needs client input)
  │──── ResumeMessage ──────────►│
  │                              │
  │◄─── DoneMessage ─────────────│
  │                              │
  │──── Close ──────────────────►│
```

## Reconnection

If the connection drops while a graph is suspended (waiting for client input):

1. `YieldMessage` and recoverable `ErrorMessage` include a `sessionId`
2. Client reconnects to the **same URL path** (`/session/:sessionId`) - this routes to the same Durable Object instance
3. Client sends `ReconnectMessage` with the `sessionId`
4. Server validates the sessionId matches, retrieves session state from storage
5. Server re-sends the pending effect as a new `YieldMessage`
6. Sessions expire after **5 minutes** of inactivity

**Important**: The `sessionId` in the URL path determines which Durable Object handles the request. Reconnecting to a different path will route to a different DO that doesn't have the session data.

```
Client                         Server (DO for session "abc123")
  │                              │
  │◄─── YieldMessage ────────────│  (sessionId: "abc123")
  │                              │
  ╳     (connection drops)       │
  │                              │
  │──── WS Connect /session/abc123 ─►│
  │──── ReconnectMessage ───────►│  (sessionId: "abc123")
  │                              │
  │◄─── YieldMessage ────────────│  (same effect, new chance to respond)
  │──── ResumeMessage ──────────►│
  │                              │
```

## Keepalive

To prevent connection timeouts, clients should send `PingMessage` periodically:

- Recommended interval: 30 seconds
- Server responds with `PongMessage`
- Connections idle for >60 seconds may be closed by infrastructure

## Error Handling

| Scenario | Server Response |
|----------|-----------------|
| Invalid JSON | `ErrorMessage` with `recoverable: false` |
| Unknown message type | `ErrorMessage` with `recoverable: false` |
| Invalid/empty graph ID | `ErrorMessage` with `recoverable: false` |
| Effect execution failed | `YieldMessage` with the failed effect (client can retry) |
| Session expired | `ErrorMessage` with `recoverable: false` |
| Session state inconsistent | `ErrorMessage` with `recoverable: false` |
| Internal error | `ErrorMessage` with `recoverable: true` and `sessionId` (if session exists) |

## Session Storage

Sessions are stored in Durable Object storage with the following structure:

```typescript
interface SessionState {
  graphId: string;
  machineState: unknown;      // Serialized WASM state
  pendingEffect: SerializableEffect | null;
  lastActivity: number;       // Unix timestamp (ms)
}
```

Storage key: `session:<sessionId>`

Sessions are cleaned up:
- Via DO alarm that checks for and removes sessions inactive for 5+ minutes
- The alarm reschedules itself while active sessions remain
- When graph completes successfully
- When session state becomes inconsistent

**Note**: WASM machine state is kept in-memory within the DO instance. If the DO instance is evicted, the in-memory state is lost. Session storage preserves the pending effect for reconnection, but full graph state restoration requires the DO to remain active.
