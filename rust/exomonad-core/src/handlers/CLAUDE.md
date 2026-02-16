# Effect Handlers

Documentation for the various effect handlers in `exomonad-core`.

## EventEffects Handler (`handlers/events.rs`)

Handles effects in the `events.*` namespace, enabling synchronization between agents and the orchestration layer.

### Capabilities

- **`wait_for_event`**: Internal effect for blocking wait on event types. Not exposed as an MCP tool — parent notification uses Zellij STDIN injection instead.
- **`notify_event`**: Publishes an event to a session queue.
- **`notify_parent`**: Signals completion to parent agent. Routes to parent's EventQueue AND injects natural-language notification into parent's Zellij pane via `inject_input`.

### Remote Forwarding

The handler can be configured with a `remote_url` (derived from `remote_port`).
- If `remote_url` is set: `notify_event` calls are forwarded via HTTP POST to the specified URL (e.g., `http://127.0.0.1:{port}/events`).
- If `remote_url` is NOT set: `notify_event` calls are handled locally by the `EventQueue` service.

This allows worker agents running in isolated processes (or even machines) to notify the main orchestration server.

### Session ID Resolution

- The handler is initialized with an optional `session_id`.
- If not provided, it defaults to `"default"`.
- This ID is used to scope event queues, ensuring events are delivered to the correct listener.

## SessionHandler (`handlers/session.rs`)

Handles effects in the `session.*` namespace.

### Capabilities

- **`register_claude_id`**: Registers a mapping between a session's unique ID and its Claude Code session UUID.
  - This is called by the `SessionStart` hook handler in WASM.
  - The ID is stored in the `ClaudeSessionRegistry`.
  - Used by `spawn_subtree` to resolve the parent's Claude session ID when spawning children with `--resume --fork-session`.