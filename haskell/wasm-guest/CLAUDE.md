# WASM Guest Documentation

Documentation for the Haskell WASM guest plugin, which defines MCP tools and effects.

## Effects

The WASM guest utilizes a variety of effects to interact with the host system.
These are defined in `ExoMonad.Effects.*` and interpreted by the Rust host.

- `Git`: Git operations (branch, status, log, etc.)
- `GitHub`: GitHub API interactions (issues, PRs).
- **`Events`**: Inter-agent synchronization (wait/notify).
- **`Session`**: Session lifecycle management (register Claude session ID).
- `Log`: Logging to the host console.
- `FS`: File system access.
- `Agent`: Agent lifecycle management.

## MCP Tools

The guest exports MCP tools that agents can call. These are defined in `ExoMonad.Guest.Tools.*`.

### Events Tools (`ExoMonad.Guest.Tools.Events`)

- **`notify_parent`**: Used by worker/subtree agents to signal completion to their parent. Routes to parent via server — delivers as a native `<teammate-message>` through Claude Code's Teams inbox when a team is active, falls back to Zellij STDIN injection otherwise. Available as a bare field in both TL and dev roles.

### Spawn Tools (`ExoMonad.Guest.Tools.Spawn`)

- **`spawn_subtree`**: Fork a Claude agent in a new git worktree + Zellij tab (TL role, can further decompose).
- **`spawn_leaf_subtree`**: Fork a Gemini agent in a new git worktree + Zellij tab (dev role, isolated, files PR).
- **`spawn_workers`**: Spawn ephemeral Gemini agents as panes in the parent directory (no branch, no worktree).

### Prompt Builder (`ExoMonad.Guest.Prompt`)

Pure Haskell prompt assembly for worker/leaf agents. Replaces the former template effect round-trip (Haskell → proto → Rust disk I/O → proto → Haskell) with direct string composition.

- Builder monoid: `task`, `boundary`, `steps`, `context`, `verify`, `doneCriteria`, `readFirst`, `raw`
- Inline profiles: `leafProfile`, `workerProfile`, `generalProfile`, `rustProfile`, `haskellProfile`

### Other Tools

- `file_pr` (tl, dev roles)
- `popup` (tl role)
- `merge_pr` (tl role)
- `notify_parent` (all roles)

### Roles

| Role | Tools | Spawned by |
|------|-------|------------|
| **tl** | spawn (3), merge_pr, file_pr, popup, notify_parent | `spawn_subtree` |
| **dev** | file_pr, notify_parent | `spawn_leaf_subtree` |
| **worker** | notify_parent | `spawn_workers` |

## Hooks

The guest handles hooks invoked by Claude Code:
- **`onSessionStart`**: Captures Claude session ID and yields `SessionRegister` effect.
- **`onPreToolUse`**: Validates tool calls (stops restricted tools).
- **`onPostToolUse`**: Logs tool usage.
- **`onSubagentStop`**: Validates child agent exit status.
- **`onSessionEnd`**: Cleans up resources.
