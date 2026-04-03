# exomonad-core — Unified Library

ExoMonad core is the unified library providing the effect system framework, WASM hosting via Extism, and built-in effect handlers and services for git, GitHub, agent orchestration, and more. It defines the FFI boundary using protobuf.

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

Without `runtime`: only `ui_protocol` module available (agent event types, telemetry).

## Key Types

| Type | Purpose |
|------|---------|
| `EffectHandler` | Trait for implementing namespace-based effect handlers |
| `EffectRegistry` | Registry for dispatching effects by namespace |
| `EffectContext` | Identity context (agent name, birth branch, working dir) passed to all handlers |
| `EffectError` | Common error type for all effects with protobuf mapping |
| `PluginManager` | Manages WASM guest calls and host function dispatch via Extism |
| `RuntimeBuilder` | Fluent API for assembling handlers and loading WASM |
| `AcpRegistry` | Registry of active ACP connections to Gemini agents |
| `AcpConnection` | Wraps `ClientSideConnection` + agent_id + session_id |
| `SpawnSubtreeOptions` | Options for spawning a Claude agent (permissions, etc.) |
| `SpawnLeafOptions` | Options for spawning a Gemini agent |

## Capability Traits (`Has*` Pattern)

Handlers and delivery functions are generic over a context `C` bounded by capability traits. Each consumer declares only the traits it needs — the bounds ARE the dependency graph.

**Traits** (defined in `services/mod.rs`, implemented on `Services`):

| Trait | Provides |
|-------|----------|
| `HasTeamRegistry` | `&TeamRegistry` |
| `HasAcpRegistry` | `&AcpRegistry` |
| `HasAgentResolver` | `&AgentResolver` |
| `HasEventQueue` | `&EventQueue` |
| `HasEventLog` | `Option<&EventLog>` |
| `HasProjectDir` | `&Path` |
| `HasSupervisorRegistry` | `&SupervisorRegistry` |
| `HasClaudeSessionRegistry` | `&ClaudeSessionRegistry` |
| `HasMutexRegistry` | `&MutexRegistry` |
| `HasGitHubClient` | `Option<&Arc<GitHubClient>>` |
| `HasGitWorktreeService` | `&Arc<GitWorktreeService>` |

**Handler pattern** — each handler is `Handler<C>` with `Arc<C>`:
```rust
pub struct SessionHandler<C> { ctx: Arc<C> }
impl<C: HasClaudeSessionRegistry + HasTeamRegistry + HasSupervisorRegistry + 'static>
    EffectHandler for SessionHandler<C> { ... }
```

**Delivery functions** — `impl Trait` bounds:
```rust
pub async fn route_message(
    ctx: &(impl HasTeamRegistry + HasAcpRegistry + HasAgentResolver + HasProjectDir),
    address: &Address, from: &AgentName, content: &str, summary: &str,
) -> DeliveryOutcome
```

**Concrete wiring** — only `groups.rs` and `serve.rs` name `Services`:
```rust
// groups.rs — the bridge between generic handlers and concrete Services
pub fn orchestration_handlers(
    agent_control: Arc<AgentControlService<Services>>,
    services: Arc<Services>,
    ...
) -> Vec<Box<dyn EffectHandler>>
```

**Handlers unchanged** (no `Services`/`ctx` dependency): `GitHandler`, `FsHandler`, `ProcessHandler`, `CopilotHandler`, `KvHandler`, `GitHubHandler`.

## ACP Integration

Agent Client Protocol (ACP) provides structured JSON-RPC messaging to Gemini agents, replacing fragile tmux STDIN injection.

**Key files:**
- `services/acp_registry.rs` — `AcpRegistry` (connection store) + `connect_and_prompt()` (spawn + handshake + first prompt)
- `services/acp_client.rs` — `ExoMonadAcpClient` (implements ACP `Client` trait: auto-approve permissions, log notifications)

**Delivery priority** (in `services/delivery.rs`): Teams inbox → ACP prompt → HTTP-over-UDS (`.exo/agents/{name}/notify.sock`) → tmux STDIN injection.

**Vendor patches:** `vendor/acp-rust-sdk/` has Send patches (Rc→Arc, LocalBoxFuture→BoxFuture, async_trait(?Send)→async_trait) to work with tokio's multi-threaded runtime.

## Delivery Pipeline (`services/delivery.rs`)

Delivery functions are generic over `C` via `impl Has*` bounds (no concrete `Services` type):

| Function | Bounds | Used by |
|----------|--------|---------|
| `route_message()` | `HasTeamRegistry + HasAcpRegistry + HasAgentResolver + HasProjectDir` | `send_message` effect |
| `deliver_to_agent()` | `HasTeamRegistry + HasAcpRegistry + HasProjectDir` | Peer messaging, event handler `InjectMessage` |
| `notify_parent_delivery()` | `HasTeamRegistry + HasAcpRegistry + HasEventLog + HasEventQueue + HasProjectDir` | `notify_parent` effect, poller `NotifyParent` action |

**Worker pane delivery** (tmux fallback for workers): `routing.json` stores `pane_id` (e.g. `%42`) for direct tmux targeting. `inject_input` passes `pane_id` as the `target` argument.

All messages are prefixed with `[from: id]` (or `[FAILED: id]` for failures). Event handler messages include structural tags inside the body (e.g. `[from: leaf-id] [PR READY] PR #5 approved...`).

**Rule**: Any code path that notifies a parent MUST use `notify_parent_delivery()`, never raw `deliver_to_agent()`. This ensures OTel span events, EventQueue publication, and consistent `[from:]`/`[FAILED:]` formatting.

`deliver_to_agent()` is correct for peer-to-peer messaging (send_message, event handler InjectMessage).

## GitHub Poller State Machine (`services/github_poller.rs`)

`GitHubPoller<C>` is generic over capability traits. Single-phase init: `GitHubPoller::new(ctx)` — no `with_services()`. Background tokio task polling GitHub every 60s. Tracks per-PR state in `HashMap<PRNumber, PRState>`.

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

### Stale Notification Guard

Once the parent has been notified (via `[PR READY]` approval or `[REVIEW TIMEOUT]`), `compute_pr_actions` suppresses all further events for that PR. Late Copilot reviews, CI status changes, and new commits are silently dropped — the TL has already been told to merge, so any further notifications are stale and confusing.

### Merge Detection

When a tracked PR's branch disappears from the open PR list, it was merged/closed. The poller:
- Fires `sibling_merged` WASM event on sibling agents (same parent branch, open PRs) via `call_handle_event`
- Emits `agent.sibling_merged` OTel span event
- Removes the PRState from tracking

## Related Documentation

- [Root CLAUDE.md](../../CLAUDE.md)
- [Handlers CLAUDE.md](src/handlers/CLAUDE.md)
- [Haskell WASM guest](../../haskell/wasm-guest/CLAUDE.md)
