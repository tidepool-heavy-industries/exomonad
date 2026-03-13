# exomonad-core — Unified Library

ExoMonad core is the unified library providing the effect system framework, WASM hosting via Extism, and built-in effect handlers and services for git, GitHub, agent orchestration, and more. It defines the FFI boundary using protobuf and provides a lightweight UI protocol for frontend plugins.

## Module Structure

| Directory | Purpose |
|-----------|---------|
| `effects/` | EffectHandler trait, EffectRegistry, dispatch, error helpers |
| `handlers/` | Effect handler implementations (git, github, log, agent, fs, etc.) |
| `services/` | Business logic services (git, github, agent_control, event_queue, etc.) |
| `services/external/` | External API clients (anthropic, github/octocrab, ollama, otel) |
| `mcp/` | MCP types (ToolDefinition) and tools module |
| `protocol/` | Wire format types (hook, mcp, service) |

## Feature Flags

| Feature | Default | Description |
|---------|---------|-------------|
| `runtime` | Yes | Full runtime: WASM hosting, effect handlers, services |

Without `runtime`: only `ui_protocol` module available. Used by `exomonad-plugin` which targets wasm32-wasi.

## Key Types

| Type | Purpose |
|------|---------|
| `EffectHandler` | Trait for implementing namespace-based effect handlers |
| `EffectRegistry` | Registry for dispatching effects by namespace |
| `EffectContext` | Identity context (agent name, birth branch) passed to all handlers |
| `EffectError` | Common error type for all effects with protobuf mapping |
| `PluginManager` | Manages WASM guest calls and host function dispatch via Extism |
| `RuntimeBuilder` | Fluent API for assembling handlers and loading WASM |
| `AcpRegistry` | Registry of active ACP connections to Gemini agents |
| `AcpConnection` | Wraps `ClientSideConnection` + agent_id + session_id |
| `SpawnSubtreeOptions` | Options for spawning a Claude agent (permissions, etc.) |
| `SpawnLeafOptions` | Options for spawning a Gemini agent |

## ACP Integration

Agent Client Protocol (ACP) provides structured JSON-RPC messaging to Gemini agents, replacing fragile tmux STDIN injection.

**Key files:**
- `services/acp_registry.rs` — `AcpRegistry` (connection store) + `connect_and_prompt()` (spawn + handshake + first prompt)
- `services/acp_client.rs` — `ExoMonadAcpClient` (implements ACP `Client` trait: auto-approve permissions, log notifications)

**Delivery priority** (in `services/delivery.rs`): Teams inbox → ACP prompt → HTTP-over-UDS (`.exo/agents/{name}/notify.sock`) → tmux STDIN injection.

**Vendor patches:** `vendor/acp-rust-sdk/` has Send patches (Rc→Arc, LocalBoxFuture→BoxFuture, async_trait(?Send)→async_trait) to work with tokio's multi-threaded runtime.

## Delivery Pipeline (`services/delivery.rs`)

Two levels of abstraction for sending messages:

| Function | Purpose | Used by |
|----------|---------|---------|
| `deliver_to_agent()` | Low-level multi-channel delivery (Teams → ACP → UDS → tmux) | Peer messaging (`send_message`), event handler `InjectMessage` |
| `notify_parent_delivery()` | High-level parent notification: event log + EventQueue + `[from: id]`/`[FAILED: id]` prefix + `deliver_to_agent()` | `EventHandler::notify_parent` (agent-initiated), poller `NotifyParent` action (system-initiated) |

**Worker pane delivery** (tmux fallback for workers): `routing.json` stores `slug_key` (e.g. `"main/test-worker-gemini"`) rather than `pane_id`. `inject_input` passes `slug_key` as the `target` argument.

All messages are prefixed with `[from: id]` (or `[FAILED: id]` for failures). Event handler messages include structural tags inside the body (e.g. `[from: leaf-id] [PR READY] PR #5 approved...`).

**Rule**: Any code path that notifies a parent MUST use `notify_parent_delivery()`, never raw `deliver_to_agent()`. This ensures event log entries, EventQueue publication, and consistent `[from:]`/`[FAILED:]` formatting.

`deliver_to_agent()` is correct for peer-to-peer messaging (send_message, event handler InjectMessage).

## GitHub Poller State Machine (`services/github_poller.rs`)

Background tokio task polling GitHub every 60s. Tracks per-PR state in `HashMap<PRNumber, PRState>`.

### PR Lifecycle States

```
ReviewState::None ──(Copilot approves)──→ ReviewState::Approved
       │                                         │
       │                                    sends [PR READY] to parent
       │
       ├──(Copilot requests changes)──→ ReviewState::ChangesRequested
       │                                         │
       │                                    stop hook blocks exit
       │                                         │
       │                              (agent pushes, SHA changes)
       │                                         │
       │                              fires [FIXES PUSHED] to parent
       │                              sets addressed_changes = true
       │                                    reset → None
       │
       └──(timeout, no review)──→ timeout
              │                      sends [REVIEW TIMEOUT] to parent
              │
              15 min (initial) / 5 min (after addressing changes)
```

**Copilot review lifecycle:** Copilot's first review is automatic (triggered on PR creation). Subsequent reviews after pushing fixes are NOT — Copilot does not re-review. The `FixesPushed` event fills this gap: when the poller detects a SHA change on a PR that was `ChangesRequested`, it fires `fixes_pushed` immediately and uses a shorter 5-minute fallback timeout.

### Event Dispatch Flow

1. Poller detects state change (new comments, approval, timeout, merge)
2. Calls `call_handle_event()` → WASM `handle_event` FFI
3. Haskell `dispatchEvent` routes to role's `EventHandlerConfig` handler
4. Handler returns `EventAction` (InjectMessage, NotifyParentAction, NoAction)
5. Poller acts on the action via `handle_event_action()`

### Merge Detection

When a tracked PR's branch disappears from the open PR list, it was merged/closed. The poller:
- Fires `sibling_merged` WASM event on sibling agents (same parent branch, open PRs) via `call_handle_event`
- Logs `agent.sibling_merged` event
- Removes the PRState from tracking

## Related Documentation

- [Root CLAUDE.md](../../CLAUDE.md)
- [Handlers CLAUDE.md](src/handlers/CLAUDE.md)
- [Haskell WASM guest](../../haskell/wasm-guest/CLAUDE.md)
