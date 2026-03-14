# Effect Handlers

Documentation for the various effect handlers in `exomonad-core`.

## CoordinationHandler (`handlers/coordination.rs`)

Handles effects in the `coordination.*` namespace, providing in-memory mutual exclusion for parallel agents accessing shared resources.

### Capabilities

- **`acquire_mutex`**: Acquire a named mutex. Idempotent (same agent re-acquiring returns existing lock_id). FIFO wait queue for contention. TTL auto-expiry via background task. Defaults `estimated_time_secs` and `timeout_secs` to 300 if 0.
- **`release_mutex`**: Release a mutex by resource name and lock_id. Validates lock_id to prevent cross-agent release. Grants next waiter in FIFO order.

### Design

- `MutexRegistry` service (`services/mutex_registry.rs`) holds all state
- `LockState`: lock_id (UUID), holder_agent, intent, acquired_at, expires_at
- `Waiter`: agent, intent, estimated_time, oneshot::Sender for wake-up
- Background expiry task checks every 1s for expired locks
- Effect-only — no MCP tool exposed. Haskell tool handlers use it internally.

## ProcessHandler (`handlers/process.rs`)

Handles effects in the `process.*` namespace, providing ad-hoc process execution.

### Capabilities

- **`run`**: Execute an arbitrary command with args, env vars, working dir, and optional timeout. Stdout/stderr captured as strings. Working dir is required (empty = error), resolved relative to agent worktree.

### Design

- No service layer — `tokio::process::Command` is simple enough to inline
- Working dir resolution reuses `resolve_agent_working_dir()` from `services::agent_control`
- Timeout via `tokio::time::timeout` wrapping `.output()`
- No command allowlist — trusts WASM author

## EventEffects Handler (`handlers/events.rs`)

Handles effects in the `events.*` namespace, enabling synchronization between agents and the orchestration layer.

### Capabilities

- **`wait_for_event`**: Internal effect for blocking wait on event types. Not exposed as an MCP tool — parent notification uses tmux STDIN injection instead.
- **`notify_event`**: Publishes an event to a session queue.
- **`notify_parent`**: Sends a message to the parent agent. Delegates to `delivery::notify_parent_delivery()` — the single codepath for all notify_parent operations (event log, EventQueue, `[from:]`/`[FAILED:]` formatting, multi-channel delivery).
- **`send_message`**: Resolves recipients and delivers arbitrary messages between agents. Routes via Teams inbox, ACP, UDS, or tmux fallback based on the recipient's type and capabilities.

### Type Safety

- `notified_agents` uses `HashSet<String>` for dedup
- Parent window name resolved via shared `resolve_parent_window_name()` from `services::agent_control`
- Window display names computed via `AgentType::window_display_name(slug)` — single source of truth
- Agent identity resolved as `AgentName` from `EffectContext` (structural, baked into PluginManager)

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
- **`spawn_subtree`**: Creates a Claude subtree in a new git worktree + tmux window.
- **`spawn_leaf_subtree`**: Creates a Gemini leaf in a new git worktree + tmux window.
- **`cleanup_merged`**: Removes worktrees for merged branches.

### Type Safety

- `SpawnResult.agent_dir` is `PathBuf` (not String)
- `SpawnResult.agent_type` uses the `AgentType` enum (not string "claude"/"gemini")
- `SpawnSubtreeOptions.agent_type` and `SpawnLeafOptions.agent_type` are `AgentType` (required, no default — `AGENT_TYPE_UNSPECIFIED` is rejected)
- `SpawnOptions.base_branch` and `SpawnGeminiTeammateOptions.base_branch` are `Option<BirthBranch>`
- `SpawnGeminiTeammateOptions.name` is `AgentName`
- `AgentInfo.agent_dir` is `Option<PathBuf>`, `AgentInfo.slug` is `Option<AgentName>`
- Proto conversion uses `AgentType::suffix()` and `AgentType::emoji()` methods
- Claude session registry lookups use `AgentName`

## FilePRHandler (`handlers/file_pr.rs`)

Handles effects in the `file_pr.*` namespace.

### Working Directory Resolution

The handler derives the agent's working directory from `EffectContext` via `resolve_agent_working_dir()`:
- Root agents (birth_branch without dots): server CWD (project root)
- Spawned agents (birth_branch with dots, e.g. `main.feature.scaffold`): `.exo/worktrees/{slug}/`

All git/gh CLI commands in the file_pr service run with `.dir(working_dir)` to ensure they operate in the correct worktree.

## MergePRHandler (`handlers/merge_pr.rs`)

Handles effects in the `merge_pr.*` namespace.

### Branch Preservation

`gh pr merge` runs WITHOUT `--delete-branch` because worktree branches cannot be deleted while checked out. Branch cleanup happens via `cleanup_merged` instead.

Post-merge, the Haskell tool handler runs `git pull` via `process.run` to fast-forward the local branch. If pull fails, the tool response includes guidance to run `git pull --rebase` manually.

## Delivery Pipeline (`services/delivery.rs`)

Two abstraction levels — choose the right one:

| Function | When to use |
|----------|-------------|
| `notify_parent_delivery()` | Notifying a parent agent (logs, EventQueue, `[from: id]`/`[FAILED: id]` prefix) |
| `deliver_to_agent()` | Peer messaging, sibling notifications, injecting into agent panes |

Both the `notify_parent` effect handler and the poller's `NotifyParentAction` use `notify_parent_delivery()`. All messages get `[from: id]` prefix (or `[FAILED: id]` for failures). Event handler messages include structural tags inside the body (e.g. `[from: leaf-id] [PR READY] PR #5...`). Never use raw `deliver_to_agent()` for parent notifications.

`deliver_to_agent()` is correct for peer-to-peer messaging (send_message, event handler InjectMessage).

### Routing Resolution

`deliver_to_agent()` resolves the agent's routing.json by trying multiple path candidates: first the direct `agent_key` directory, then slug (last dot-segment) and full `agent_key` with `-gemini`, `-claude`, and `-shoal` suffixes. This handles both peer messaging (where `agent_key` is already the directory name) and parent notification (where `agent_key` is a dotted branch name). Reads `pane_id` (workers), `window_id` (subtrees/leaves), or `parent_tab` (fallback).

### Session-Qualified Targets

`TmuxIpc::inject_input` session-qualifies all targets (`{session}:{target}`) to ensure `paste-buffer` and `send-keys` resolve to the same pane deterministically. Without qualification, tmux resolves display-name targets against the "most recently used" session, which is nondeterministic for subprocess calls.

### Injection Locking

`TmuxIpc::inject_input` serializes calls to the same target via a per-target `std::sync::Mutex` stored as `Weak` references in a static map. Entries are pruned on each lookup, preventing unbounded growth from ephemeral worker panes. Different targets are independent — locking `@1` does not block injection into `@2`.

A 150ms debounce between `paste-buffer` and `send-keys Enter` gives complex TUIs (Claude Code's Ink renderer, Gemini CLI's readline) time to process pasted text before submission. The Enter keystroke is retried up to 3 times with 200ms between attempts to handle dropped keystrokes. Before injection, copy/scroll mode is detected via `#{pane_in_mode}` and cancelled to prevent silent paste failures.

## Shared Helpers (`handlers/mod.rs`)

Proto3 uses empty strings/zero values as defaults. These helpers eliminate repetitive empty-check boilerplate at handler boundaries:

| Helper | Signature | Purpose |
|--------|-----------|---------|
| `non_empty(s)` | `String → Option<String>` | Converts empty string to `None` |
| `working_dir_or_default(s)` | `String → String` | Returns `"."` for empty strings |
| `working_dir_path_or_default(s)` | `&str → PathBuf` | Returns `PathBuf::from(".")` for empty |

**Error conversion**: All handlers use `ResultExt::effect_err(namespace)` from `effects/error.rs` instead of manual `.map_err(|e| EffectError::custom(...))` closures. For async spawn_blocking patterns, use `spawn_blocking_effect(namespace, closure)`.

## Domain Types Used Across Handlers

| Type | Purpose | Used In |
|------|---------|---------|
| `AgentName` | Agent identity (e.g., "root", "feature-a") | events, session, agent |
| `BirthBranch` | Branch-based agent identity | events, github_poller |
| `BranchName` | Git branch name | file_pr, agent |
| `PRNumber` | GitHub PR number (wraps u64) | file_pr, merge_pr, copilot, github_poller |
| `AgentType` | Claude vs Gemini enum, provides `window_display_name()` and `from_dir_name()` | agent, github_poller |
| `GithubOwner` / `GithubRepo` | GitHub repo coordinates | github_poller |

### Boundary Conversion Pattern

All handlers convert proto strings/integers to domain newtypes at the handler boundary:
- Proto `string` → `BranchName::from(req.field.as_str())`
- Proto `int64` → `PRNumber::new(req.pr_number as u64)`
- Domain → Proto: `.to_string()` for string types, `.as_u64() as i64` for PRNumber