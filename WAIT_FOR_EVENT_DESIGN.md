# wait_for_event Tool Design

## Problem
TL spawns async workers but has no efficient way to wait for completion. Current options:
1. Poll `get_messages` in a loop → wastes tokens, looks ugly
2. Manually check periodically → TL forgets about workers

Need: **one tool call that blocks until something interesting happens**, then returns with the event.

## Solution: `wait_for_event` Tool

### What it does
Blocks until any of these events occur:
- Worker completes (stop hook fires)
- Message arrives in inbox
- Question needs answering
- Timeout expires

Returns immediately with the event details.

### Usage
```typescript
// TL spawns workers
spawn_worker({ name: "fix-bug", prompt: "..." })
spawn_worker({ name: "add-tests", prompt: "..." })

// Wait for any completion
const event = wait_for_event({ timeout_secs: 300 })

// Returns: { type: "worker_complete", worker_id: "fix-bug-gemini", status: "success", changes: [...] }
```

### Event Types

```typescript
type Event =
  | { type: "worker_complete", worker_id: string, status: "success" | "error", changes: string[], message: string }
  | { type: "message", from: string, body: string }
  | { type: "question", from: string, question_id: string, body: string }
  | { type: "timeout" }
```

## Implementation

### Architecture (In-Memory, Direct Signaling)

```
┌─────────────────────────────────────────┐
│ TL calls wait_for_event(types=[...])   │
│ Blocks on server's in-memory queue     │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│  Server: HashMap<SessionId, Queue>     │
│  - FIFO queue per TL session            │
│  - wait_for_event blocks on queue       │
│  - Wakes when matching event arrives    │
└─────────────────────────────────────────┘
                    │
                    ▼  (worker completes)
┌─────────────────────────────────────────┐
│  Worker stop hook calls MCP tool:       │
│  notify_completion(session_id, event)   │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│  Server appends event to TL's queue     │
│  Wakes blocked wait_for_event           │
│  Returns event to TL                    │
└─────────────────────────────────────────┘
```

### Design Decisions

**In-Memory Queue (No Filesystem)**
- Events live in server RAM: `HashMap<SessionId, VecDeque<Event>>`
- FIFO ordering: TL sees events in order they occurred
- Lost on server restart (acceptable for phase 1)
- No filesystem polling needed

**Direct MCP Signaling**
- Worker stop hook calls `notify_completion` MCP tool
- Server immediately appends to TL's queue
- No temp files, no watching, no polling

**Session ID Inheritance**
- Spawn sets `EXOMONAD_SESSION_ID` env var in worker
- Worker hook reads it, passes to `notify_completion`
- Server routes event to correct TL session

**Type Filtering**
- `wait_for_event(types=["worker_complete"])` ignores other events
- Server only wakes for matching event types
- Unmatched events remain queued

### Files to Create/Modify

#### 1. EventQueue Service (new)
**File:** `rust/exomonad-core/src/services/event_queue.rs`

**Data structure:**
```rust
pub struct EventQueue {
    // Per-session FIFO queues
    queues: Arc<Mutex<HashMap<SessionId, VecDeque<Event>>>>,
    // Wakers for blocked wait_for_event calls
    wakers: Arc<Mutex<HashMap<SessionId, Vec<Waker>>>>,
}
```

**API:**
```rust
impl EventQueue {
    /// Block until matching event or timeout
    pub async fn wait_for_event(
        &self,
        session_id: &SessionId,
        types: &[EventType],
        timeout: Duration
    ) -> Result<Event>;

    /// Append event, wake waiting calls
    pub fn notify_event(&self, session_id: &SessionId, event: Event);
}
```

#### 2. notify_completion MCP Tool (new)
**File:** `haskell/wasm-guest/src/ExoMonad/Guest/Tools/NotifyCompletion.hs`

**Tool schema:**
```typescript
{
  name: "notify_completion",
  description: "Signal that a worker has completed",
  inputSchema: {
    session_id: { type: "string" },
    worker_id: { type: "string" },
    status: { type: "string", enum: ["success", "error"] },
    changes: { type: "array", items: { type: "string" } },
    message: { type: "string" }
  }
}
```

**Handler:**
- Receives completion data from worker hook
- Calls `NotifyEvent` effect
- Rust appends to session's queue, wakes wait_for_event

#### 3. Worker Stop Hook (modify)
**File:** `haskell/wasm-guest/src/ExoMonad/Guest/Tool/Runtime.hs`

**Current:** `handleWorkerExit` calls `sendNote`

**New:** `handleWorkerExit` calls `notify_completion`:
1. Get worker ID from `EXOMONAD_AGENT_ID`
2. Get session ID from `EXOMONAD_SESSION_ID`
3. Run `git diff --stat` to capture changes
4. Call `notify_completion` MCP tool with event data
5. Return success

#### 4. spawn_worker Environment Setup (modify)
**File:** `rust/exomonad-core/src/services/agent_control.rs`

**In `spawn_worker` method:**
- Add `EXOMONAD_SESSION_ID` to worker's env vars
- Value: current session ID (from MCP context or TL's agent ID)

#### 5. wait_for_event MCP Tool (new)
**File:** `haskell/wasm-guest/src/ExoMonad/Guest/Tools/WaitForEvent.hs`

**Tool schema:**
```typescript
{
  name: "wait_for_event",
  description: "Block until a matching event occurs or timeout expires",
  inputSchema: {
    types: { type: "array", items: { type: "string" }, default: ["worker_complete"] },
    timeout_secs: { type: "number", default: 300 }
  }
}
```

**Handler:**
- Call `WaitForEvent` effect with type filter
- Rust handler blocks on EventQueue
- Returns first matching event or timeout

#### 6. Proto (new)
**File:** `proto/effects/event_queue.proto`

```protobuf
message WaitForEventRequest {
  repeated string types = 1;  // Filter by event type
  int32 timeout_secs = 2;
}

message WaitForEventResponse {
  Event event = 1;
}

message NotifyEventRequest {
  string session_id = 1;
  Event event = 2;
}

message NotifyEventResponse {
  bool success = 1;
}

message Event {
  oneof event_type {
    WorkerComplete worker_complete = 1;
    Message message = 2;
    Question question = 3;
    Timeout timeout = 4;
  }
}

message WorkerComplete {
  string worker_id = 1;
  string status = 2;  // "success" or "error"
  repeated string changes = 3;
  string message = 4;
}

// ... other event types
```

## Minimal MVP

Start with just worker completions:

1. Add `EventQueue` service (in-memory HashMap + wakers)
2. Add `notify_completion` MCP tool (Haskell + Rust handler)
3. Add `wait_for_event` MCP tool (Haskell + Rust handler)
4. Modify `handleWorkerExit` to call `notify_completion`
5. Add `EXOMONAD_SESSION_ID` env var to worker spawn

Later expand to:
- Message events (unified with current messaging)
- Question events
- Persist queue on server shutdown (optional)
- Multiple TL sessions with isolated queues

## Test Pattern

```bash
# TL session
spawn_worker name="test-1" prompt="echo hello > /tmp/test1.txt"
spawn_worker name="test-2" prompt="echo world > /tmp/test2.txt"

# Wait for first completion
wait_for_event timeout_secs=60
# → Returns: { type: "worker_complete", worker_id: "test-1-gemini", ... }

# Wait for second completion
wait_for_event timeout_secs=60
# → Returns: { type: "worker_complete", worker_id: "test-2-gemini", ... }

# No more workers
wait_for_event timeout_secs=5
# → Returns: { type: "timeout" }
```

## Resolved Design Questions

1. **Event persistence:** In-memory only. Lost on server restart. (Acceptable for phase 1)

2. **Event queuing:** FIFO queue per session. All events preserved in order.

3. **Queue location:** Server process maintains `HashMap<SessionId, VecDeque<Event>>`.

4. **Worker notification:** Stop hook calls `notify_completion` MCP tool → direct server signaling.

5. **Session routing:** Worker inherits `EXOMONAD_SESSION_ID` via env var from parent.

6. **Type filtering:** `wait_for_event(types=["worker_complete"])` only wakes on matching events.

7. **Event consumption:** Auto-consumed on read. Each call pops one event from queue.
