# Socket Client — Shared Unix Domain Socket Protocol

Shared library for NDJSON-based communication over Unix Domain Sockets. This is the primary transport layer for interpreters that communicate with the Rust backend services (e.g., LLM, GitHub, and Observability).

## When to Read This

Read this if you're:
- Modifying the wire protocol between Haskell and Rust
- Adding new service request or response variants to the protocol
- Debugging low-level socket connection or JSON decoding issues

## Architecture

```
┌──────────────────────────┐         ┌──────────────────────────┐
│ Haskell Interpreter      │         │ Rust Backend Service     │
│ (LLM, GitHub, etc.)      │         │ (exomonad-core)          │
└────────────┬─────────────┘         └────────────┬─────────────┘
             │                                    │
             │  ServiceRequest (JSON)             │
             ├───────────────────────────────────▶│
             │  Unix Socket (NDJSON)              │
             │                                    │
             │  ServiceResponse (JSON)            │
             │◀───────────────────────────────────┤
             │                                    │
```

## Usage

The library is typically used by other interpreters via `sendRequest`:

```haskell
import ExoMonad.Effects.SocketClient (sendRequest, SocketConfig(..), ServiceRequest(..))

config = SocketConfig ".exo/sockets/service.sock" 5000
req = GitHubCheckAuth

main = do
  result <- sendRequest config req
  -- result is Either ServiceError ServiceResponse
```

## Protocol Types

### ServiceRequest
Mirror of Rust protocol types including `AnthropicChat`, `GitHubGetIssue`, `OllamaGenerate`, `OtelSpan`, etc.

### ServiceResponse
Mirror of Rust protocol types including `AnthropicChatResponse`, `GitHubIssueResponse`, `OtelAckResponse`, `ErrorResponse`, etc.

## Key Modules

| Module | Purpose |
|--------|---------|
| `ExoMonad.Effects.SocketClient` | Shared protocol types and socket I/O logic |

## Related Documentation

- [effects/llm-interpreter/CLAUDE.md](../llm-interpreter/CLAUDE.md) - LLM interpreter
- [effects/github-interpreter/CLAUDE.md](../github-interpreter/CLAUDE.md) - GitHub interpreter
- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter listing
