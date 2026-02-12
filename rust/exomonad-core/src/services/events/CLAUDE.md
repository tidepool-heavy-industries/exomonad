# EventQueue Service

The `EventQueue` service provides zero-polling coordination for parallel workers in the ExoMonad system.

## Purpose

To allow agents (workers) to signal completion or other events to the orchestration layer (TL) without requiring the TL to constantly poll for status. This enables efficient "wait until X happens" workflows.

## Architecture

The service is built around a concurrent `HashMap` structure that pairs event queues with waiting channels.

- **State**:
  - `queues`: `HashMap<SessionId, VecDeque<Event>>` - Stores events that arrived but haven't been consumed.
  - `wakers`: `HashMap<SessionId, Vec<oneshot::Sender<Event>>>` - Stores active waiters (oneshot channels).

- **Concurrency**: Uses `tokio::sync::Mutex` for thread safety and `tokio::sync::oneshot` for blocking notification.

## Key Methods

### `wait_for_event`
```rust
pub async fn wait_for_event(
    &self,
    session_id: &str,
    types: &[String],
    timeout: Duration,
) -> Result<Event>
```
Blocks until a matching event occurs or the timeout is reached.
1. Checks the existing queue for matching events.
2. If none found, registers a `oneshot` channel in `wakers`.
3. Awaits the channel or the timeout.

### `notify_event`
```rust
pub async fn notify_event(&self, session_id: &str, event: Event)
```
Publishes an event to a session.
1. Appends the event to the queue.
2. Checks for any active wakers for this session.
3. If a waker exists, sends the event to it (waking up the waiter).

## Usage Examples

From `rust/exomonad-core/src/services/event_queue.rs` tests:

### Notify then Wait
If an event is notified before the waiter starts, it is stored in the queue and returned immediately when `wait_for_event` is called.

```rust
let queue = EventQueue::new();
queue.notify_event("session1", event.clone()).await;
let result = queue.wait_for_event("session1", &["worker_complete".to_string()], Duration::from_secs(1)).await;
assert!(result.is_ok());
```

### Wait then Notify
If `wait_for_event` is called first, it blocks until `notify_event` is called.

```rust
let queue = Arc::new(EventQueue::new());
// Spawn waiter
tokio::spawn(async move {
    queue.wait_for_event("session2", &["worker_complete".to_string()], Duration::from_secs(5)).await
});
// Notify later
queue.notify_event("session2", event).await;
```

## Session ID Flow

The `SessionId` (string) partitions the event space.
- **TL Role**: Listens on its own session ID (or a specific worker's ID if using granular sessions).
- **Worker Role**: Sends `notify_completion` events tagged with the target `session_id`.
- **Handler**: The `EventHandler` resolves the session ID (defaulting to "default" if not set) and delegates to `EventQueue`.
