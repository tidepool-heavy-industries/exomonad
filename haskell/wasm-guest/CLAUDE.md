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

- **`notify_parent`**: Used by worker/subtree agents to send messages to their parent. Routes to parent via server — delivers as a native `<teammate-message>` through Claude Code's Teams inbox when a team is active, falls back to tmux STDIN injection otherwise. Agent messages get `[from: id]` prefix; failure messages get `[FAILED: id]` prefix. Available as a bare field in both TL and dev roles.
- **`send_message`**: Tool for sending arbitrary messages between exomonad-spawned agents.

### Spawn Tools (`ExoMonad.Guest.Tools.Spawn`)

- **`spawn_subtree`**: Fork a Claude agent in a new git worktree + tmux window (TL role, can further decompose). Supports `permissions` and `standalone_repo: true`.
- **`spawn_leaf_subtree`**: Fork a Gemini agent in a new git worktree + tmux window (dev role, isolated, files PR). Supports `standalone_repo: true`.
- **`spawn_workers`**: Spawn ephemeral Gemini agents as panes in the parent directory (no branch, no worktree).

**Standalone repo mode**: Both `spawn_subtree` and `spawn_leaf_subtree` accept `standalone_repo: true`. This creates a fresh `git init` repo instead of a worktree, providing stronger filesystem isolation for the subagent.

### Defining MCP Tools (`ExoMonad.Guest.Tool.Class`)

MCP tools are defined by implementing the `MCPTool` typeclass for a specific type.

```haskell
class (FromJSON (Args t), ToJSON (Result t)) => MCPTool t where
  type Args t :: Type
  type Result t :: Type
  toolName :: Text
  toolDescription :: Text
  toolSchema :: Aeson.Object  -- JSON Schema as an Object (Aeson.KeyMap Value)
  handleCall :: Args t -> Eff es (Result t)
```

Tool schemas are typically derived using `genericToolSchema` from `ExoMonad.Guest.Tool.Schema`.

### Permissions (`ExoMonad.Guest.Types.Permissions`)

Claude-only permissions system using the `ClaudePermissions` DSL.

- **`ToolPattern`**: DSL for defining tool allow/deny patterns (e.g., `bash`, `gh`, `read_file`).
- **`ClaudePermissions`**: Record of allowed/denied tools and paths.
- **`renderPermissions`**: Renders to Claude Code's native `settings.local.json` format.

### Prompt Builder (`ExoMonad.Guest.Prompt`)

Pure Haskell prompt assembly for worker/leaf agents. Replaces the former template effect round-trip (Haskell → proto → Rust disk I/O → proto → Haskell) with direct string composition.

- Builder monoid: `task`, `boundary`, `steps`, `context`, `verify`, `doneCriteria`, `readFirst`, `raw`
- Inline profiles: `leafProfile`, `workerProfile`, `generalProfile`, `rustProfile`, `haskellProfile`

### Other Tools

- `file_pr` (tl, dev roles)
- `merge_pr` (tl role)
- `notify_parent` (all roles)

### Roles

| Role | Tools | Spawned by |
|------|-------|------------|
| **tl** | spawn (3), merge_pr, file_pr, notify_parent, send_message | `spawn_subtree` |
| **dev** | file_pr, notify_parent, send_message, shutdown | `spawn_leaf_subtree` |
| **worker** | notify_parent, send_message, shutdown | `spawn_workers` |

## Hooks

The guest handles hooks invoked by Claude Code:
- **`onSessionStart`**: Captures Claude session ID and yields `SessionRegister` effect.
- **`onPreToolUse`**: Validates tool calls (stops restricted tools).
- **`onPostToolUse`**: Logs tool usage.
- **`onSubagentStop`**: Validates child agent exit status.
- **`onStop`**: Stop hook — gates agent exit. Uses `StopCheckResult` (MustBlock/ShouldNudge/Clean).

### Stop Hook State Machine (`ExoMonad.Guest.Effects.StopHook`)

| PR State | Decision | Agent can exit? |
|----------|----------|----------------|
| `changes_requested` | **MustBlock** | No — must address review comments |
| Has comments (not changes_requested) | ShouldNudge | Yes, with nudge |
| No reviews yet | ShouldNudge | Yes — "system will auto-notify your parent" |
| Approved | Clean | Yes |
| No PR, uncommitted work | ShouldNudge | Yes, with nudge |
| No PR, clean | Clean | Yes |
| On main/master | Allow | Yes |

## Event Handlers

Third dispatch category alongside tools and hooks. Reactive to world events (GitHub poller, timers).

### Architecture

```
GitHub poller (Rust, 60s interval)
  → detects state change (new comments, approval, timeout, merge)
  → calls WASM handle_event({ role, event_type, payload })
  → Haskell dispatchEvent routes to EventHandlerConfig handler
  → handler returns EventAction
  → Rust acts on action (InjectMessage → deliver to agent, NotifyParent → notify_parent_delivery)
```

### Types (`ExoMonad.Guest.Events`)

| Type | Purpose |
|------|---------|
| `EventHandlerConfig` | Per-role handler config: `onPRReview`, `onCIStatus`, `onTimeout`, `onSiblingMerged` |
| `EventAction` | Handler return: `InjectMessage Text`, `NotifyParentAction Text Int`, `NoAction` |
| `PRReviewEvent` | `ReviewReceived` (comments), `ReviewApproved`, `ReviewTimeout`, `FixesPushed` (CI status) |
| `SiblingMergedEvent` | `mergedBranch`, `parentBranch`, `siblingPRNumber` |
| `EventInput` | Top-level wrapper with `event_type` discriminator for dispatch |

### PR Review Handler (`.exo/lib/PRReviewHandler.hs`)

| Event | Action | Effect |
|-------|--------|--------|
| `ReviewReceived` | `InjectMessage` | Copilot comments injected into agent pane |
| `ReviewApproved` | `NotifyParentAction` | Sends `[from: id] [PR READY] PR #N...` to parent via `notify_parent_delivery` |
| `ReviewTimeout` (15 min initial, 5 min after fixes) | `NotifyParentAction` | Sends `[from: id] [REVIEW TIMEOUT] PR #N...` to parent via `notify_parent_delivery` |
| `FixesPushed` | `NotifyParentAction` | Sends `[from: id] [FIXES PUSHED] PR #N...` to parent — Copilot does NOT re-review, so this is the actionable signal |
| `SiblingMerged` | `InjectMessage` | Injects rebase instructions when a sibling branch is merged |

### Wiring

- **Dispatch**: `handle_event` FFI export in `Main.hs`, routes `{ role, event_type, payload }` JSON to the role's `EventHandlerConfig`
- **Config**: Dev and TL roles use `prReviewEventHandlers`, Worker uses `defaultEventHandlers` (all NoAction)
- **Extensibility**: Add new event types to `EventInput` + new handlers to `EventHandlerConfig`. The poller fires events, WASM decides actions.
