# WASM Guest Documentation

Documentation for the Haskell WASM guest plugin, which defines MCP tools and effects.

## Effects

The WASM guest utilizes a variety of effects to interact with the host system.
These are defined in `ExoMonad.Effects.*` and interpreted by the Rust host.

- `Git`: Git operations (branch, status, log, etc.)
- `GitHub`: GitHub API interactions (issues, PRs).
- **`Events`**: Inter-agent synchronization (wait/notify).
- `Log`: Logging to the host console.
- `FS`: File system access.
- `Agent`: Agent lifecycle management.

## MCP Tools

The guest exports MCP tools that agents can call. These are defined in `ExoMonad.Guest.Tools.*`.

### Events Tools (`ExoMonad.Guest.Tools.Events`)

- **`wait_for_event`**: Blocks until a specific event type occurs (e.g., `worker_complete`) or a timeout is reached. Useful for orchestration agents waiting for sub-tasks.
- **`notify_completion`**: Used by worker agents to signal completion to their parent (TL) agent. Sends a `worker_complete` event with status and message.

### Other Tools

- `spawn_subtree`, `spawn_workers`
- `file_pr`
- `popup`
- `note`, `question` (dev role)
- `get_agent_messages`, `answer_question` (TL role)
