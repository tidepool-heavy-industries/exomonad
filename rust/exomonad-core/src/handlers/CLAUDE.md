# Effect Handlers

Documentation for the various effect handlers in `exomonad-core`.

## EventEffects Handler (`handlers/events.rs`)

Handles effects in the `events.*` namespace, enabling synchronization between agents and the orchestration layer.

### Capabilities

- **`wait_for_event`**: Internal effect for blocking wait on event types. Not exposed as an MCP tool — parent notification uses Zellij STDIN injection instead.
- **`notify_event`**: Publishes an event to a session queue.
- **`notify_parent`**: Signals completion to parent agent. Routes to parent's EventQueue AND injects natural-language notification into parent's Zellij pane via `inject_input`.

### Type Safety

- `notified_agents` uses `HashSet<AgentName>` (not raw strings) for dedup
- `resolve_parent_tab_name()` returns `TabName` (constructed via `BirthBranch::tab_name()` or `TabName::tl()`)
- Agent identity resolved as `AgentName` from `EffectContext` (structural, baked into PluginManager)

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
  - The ID is stored in the `ClaudeSessionRegistry` (keyed by `AgentName`, not raw strings).
  - Used by `spawn_subtree` to resolve the parent's Claude session ID when spawning children with `--resume --fork-session`.

## AgentHandler (`handlers/agent.rs`)

Handles effects in the `agent.*` namespace.

### Capabilities

- **`spawn_gemini_teammate`**: Spawns a Gemini worker pane in the parent directory.
- **`spawn_subtree`**: Creates a Claude subtree in a new git worktree + Zellij tab.
- **`spawn_leaf_subtree`**: Creates a Gemini leaf in a new git worktree + Zellij tab.
- **`cleanup_merged`**: Removes worktrees for merged branches.

### Type Safety

- `SpawnResult.agent_dir` is `PathBuf` (not String)
- `SpawnResult.agent_type` uses the `AgentType` enum (not string "claude"/"gemini")
- `SpawnOptions.base_branch` and `SpawnGeminiTeammateOptions.base_branch` are `Option<BirthBranch>`
- `SpawnGeminiTeammateOptions.name` is `AgentName`
- `AgentInfo.agent_dir` is `Option<PathBuf>`, `AgentInfo.slug` is `Option<AgentName>`
- Proto conversion uses `AgentType::suffix()` and `AgentType::emoji()` methods
- Claude session registry lookups use `AgentName`

## CoordinationHandler (`handlers/coordination.rs`)

Handles effects in the `coordination.*` namespace.

### Type Safety

- `Task.owner` is `AgentName` (not String)
- `Message.from` is `AgentName` (not String)
- Proto strings are wrapped to `AgentName` at the handler boundary

## Domain Types Used Across Handlers

| Type | Purpose | Used In |
|------|---------|---------|
| `AgentName` | Agent identity (e.g., "root", "feature-a") | events, session, agent |
| `TabName` | Zellij tab display name | events, messaging |
| `BirthBranch` | Branch-based agent identity | events, messaging, github_poller |
| `AgentType` | Claude vs Gemini enum | agent |
| `CiStatus` | CI check status enum | github_poller |
| `GithubOwner` / `GithubRepo` | GitHub repo coordinates | github_poller |