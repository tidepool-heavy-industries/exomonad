# exomonad-core — Unified Library

ExoMonad core is the unified library providing the effect system framework, WASM hosting via Extism, an MCP server implementation, and built-in effect handlers and services for git, GitHub, agent orchestration, and more. It defines the FFI boundary using protobuf and provides a lightweight UI protocol for frontend plugins.

## Module Structure

| Directory | Purpose |
|-----------|---------|
| `effects/` | EffectHandler trait, EffectRegistry, dispatch, error helpers |
| `handlers/` | Effect handler implementations (git, github, log, agent, fs, etc.) |
| `services/` | Business logic services (git, github, agent_control, event_queue, etc.) |
| `services/external/` | External API clients (anthropic, github/octocrab, ollama, otel) |
| `mcp/` | MCP server implementation (tools, server, state) |
| `protocol/` | Wire format types (hook, mcp, service) |

## Feature Flags

| Feature | Default | Description |
|---------|---------|-------------|
| `runtime` | Yes | Full runtime: WASM hosting, effect handlers, MCP server, services |

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

## ACP Integration

Agent Client Protocol (ACP) provides structured JSON-RPC messaging to Gemini agents, replacing fragile Zellij STDIN injection.

**Key files:**
- `services/acp_registry.rs` — `AcpRegistry` (connection store) + `connect_and_prompt()` (spawn + handshake + first prompt)
- `services/acp_client.rs` — `ExoMonadAcpClient` (implements ACP `Client` trait: auto-approve permissions, log notifications)

**Delivery priority** (in `services/delivery.rs`): Teams inbox → ACP prompt → Zellij STDIN injection.

**Vendor patches:** `vendor/acp-rust-sdk/` has Send patches (Rc→Arc, LocalBoxFuture→BoxFuture, async_trait(?Send)→async_trait) to work with tokio's multi-threaded runtime.

## Related Documentation

- [Root CLAUDE.md](../../CLAUDE.md)
- [Handlers CLAUDE.md](src/handlers/CLAUDE.md)
- [Haskell effects](../../haskell/effects/CLAUDE.md)
