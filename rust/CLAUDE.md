# Claude Code++: Human-Augmented Sessions

Rust workspace for augmenting human-driven Claude Code sessions with Tidepool integrations.

**This is NOT a headless orchestration system.** Humans interact with Claude Code directly via TTY; this infrastructure adds superpowers.

> **Archived:** The previous headless Docker orchestration is preserved in git tag `headless-mantle-archive`. To resurrect: `git show headless-mantle-archive:rust/mantle/`

## Documentation Tree

```
rust/CLAUDE.md  ← YOU ARE HERE (router)
├── mantle-agent/CLAUDE.md  ← Hook handler (IMPLEMENTED)
│   • hook subcommand: forwards CC hooks to HTTP control server (Unix socket)
│   • health subcommand: check control server connectivity
│   • Fail-closed: errors if control server unavailable (catches config issues)
│
├── mantle-hub/CLAUDE.md  ← Session hub (LEGACY, needs repurposing)
│   • Currently: session/node tracking for headless mode
│   • Planned: metrics collection, Grafana export
│
├── mantle-shared/CLAUDE.md  ← Shared types and utilities
│   • protocol.rs: HookInput, HookOutput, ControlMessage, ControlResponse
│   • socket.rs: TCP client (ControlSocket), NDJSON protocol
│   • commands/hook.rs: handle_hook() implementation
│   • Fail-open behavior, builder helpers
│
└── tui-sidebar/CLAUDE.md  ← TUI sidebar (IMPLEMENTED)
    • TCP server (port 7433) for rendering interactive UIs
    • Receives UISpec from Haskell tui-interpreter
    • Renders with ratatui (Text, Button, Input, Progress)
    • Sends Interaction events (ButtonClicked, InputSubmitted)
```

## Architecture Overview

```
Human TTY ──▶ Claude Code (in nix shell)
               │
               ├─ MCP tools ──────▶ HTTP MCP (http+unix://) ──▶ Control Server (HTTP)
               │                                                    │
               └─ Hooks ──────────▶ mantle-agent hook ──────────────┘
                                          │
                                          ▼
                                   Haskell control-server
                                   (effect stack + handlers)
```

**Current state:**
- `mantle-agent hook` - Forwards Claude Code hooks to HTTP control server (Unix socket) ✓
- `haskell/control-server` - Haskell HTTP server (passthrough hook handlers, MCP tools) ✓
- `flake.nix` - Nix shell with zellij layout for Claude Code++ ✓
- **MCP:** Claude Code now uses HTTP MCP transport directly (`http+unix://`) to talk to control-server ✓
- **Missing:** Daemon mode, metrics hub, real hook logic in Haskell

## Workspace Members

| Crate | Type | Purpose |
|-------|------|---------|
| [mantle-agent](mantle-agent/CLAUDE.md) | Binary | Hook handler (HTTP over Unix socket) |
| [mantle-hub](mantle-hub/CLAUDE.md) | Binary | Metrics/telemetry hub (WIP) |
| [mantle-shared](mantle-shared/CLAUDE.md) | Library | Shared types, protocols, HTTP socket client (curl-based) |
| [tui-sidebar](tui-sidebar/CLAUDE.md) | Binary + Lib | TUI sidebar: Unix socket server for interactive UIs |

## Quick Reference

### Building
```bash
cargo build --release           # Build all crates
cargo build -p mantle-agent     # Build just agent
cargo test                      # Run all tests
```

### Current Subcommands
```bash
# Handle Claude Code hook (reads JSON stdin, outputs JSON stdout)
mantle-agent hook pre-tool-use

# Check control server health
mantle-agent health
```

### Environment Variables
| Variable | Used By | Purpose |
|----------|---------|---------|
| `TIDEPOOL_CONTROL_SOCKET` | mantle-agent | Unix socket path for control server (required; e.g., `.tidepool/sockets/control.sock`) |
| `RUST_LOG` | all | Tracing log level |

## What Works Today

### 1. Hook Handler
```bash
mantle-agent hook pre-tool-use  # Reads JSON from stdin
```
- Called by `.claude/settings.local.json` hooks
- Parses Claude Code's hook JSON from stdin
- Forwards to control server via HTTP over Unix socket (using `curl` subprocess)
- Returns response JSON to stdout
- **Fail-closed:** Errors immediately if control server unavailable (catches config issues during dev)

### 2. MCP Tools (Direct HTTP)
Claude Code now connects directly to control-server's HTTP MCP endpoint:
- Transport: `http+unix://.tidepool/sockets/control.sock`
- Protocol: MCP over HTTP (no proxy needed)
- Configuration: `.mcp.json` in project root
- Tools: Discovered automatically from control-server's `/mcp/tools` endpoint

**The `mantle-agent mcp` subcommand has been removed.** Claude Code's built-in HTTP MCP support eliminates the need for a stdio proxy.

## Control Server Protocol

**Hooks** use HTTP over Unix Domain Socket:

```
mantle-agent hook               Control Server (Haskell)
     │                                    │
     │  POST /hook                        │
     │  Body: [HookInput, Runtime]        │
     │───────────────────────────────────▶│
     │                                    │
     │  200 OK                            │
     │  Body: HookResponse                │
     │◀───────────────────────────────────│
```

**MCP tools** use HTTP MCP transport (direct from Claude Code):

```
Claude Code                     Control Server (Haskell)
     │                                    │
     │  GET /mcp/tools                    │
     │───────────────────────────────────▶│
     │  200 OK (tool definitions)         │
     │◀───────────────────────────────────│
     │                                    │
     │  POST /mcp/call                    │
     │  Body: {tool_name, arguments}      │
     │───────────────────────────────────▶│
     │  200 OK (result)                   │
     │◀───────────────────────────────────│
```

**Implementation:** `mantle_shared::socket` uses `curl` subprocess for HTTP-over-Unix-socket requests.

## What's Missing (TODO)

### Daemon Mode
Goal: Long-lived process that collects metrics + forwards to Haskell

```bash
# Planned:
mantle-agent daemon start  # Connection pooling, metrics collection
```

Status: Not implemented. Current architecture uses per-hook process spawning with `curl` subprocess.

### Metrics Hub
Goal: Store tool call traces, export to Grafana

Status: mantle-hub needs repurposing from session tracking to metrics collection.

### Real Hook Logic in Haskell
Goal: Wire control-server handlers to Tidepool effect stack

Status: Current implementation is passthrough (logs and allows all hooks).

### MCP Tool Implementation
Goal: Expose more Tidepool agents as MCP tools via control-server

Status: Basic tools implemented (find_callers, teach-graph, etc.). More agents coming.

### Fail-Open Mode for Production
Goal: Add configurable fail-open mode so Claude Code works even if control server is down

Status: Not implemented. Currently always fails closed (errors if server unavailable). For production deployments, need:
- `MANTLE_FAIL_MODE` environment variable
- Monitoring/alerting when falling back to fail-open
- Graceful degradation logic

See TODO comment in `rust/mantle-shared/src/commands/hook.rs`

## Testing

```bash
cargo test                              # All tests
cargo test -p mantle-agent              # Agent tests only
cargo test -p mantle-shared             # Shared library tests
```

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| TCP (not Unix socket) | Works across Docker boundaries (legacy), simpler |
| NDJSON protocol | Human-readable, easy to debug |
| Fail-closed hooks | Errors immediately if server missing; catches config issues during development |
| Sync TCP client | Hooks block anyway; async adds complexity |

## Migration from Headless Mode

The previous headless Docker orchestration is archived. Key differences:

| Aspect | Old (headless) | New (Claude Code++) |
|--------|----------------|---------------------|
| Execution | Docker container | Nix shell (`nix develop .#claude-code-plus`) |
| Control | Haskell subprocess | Human TTY |
| Hub role | Session tracking | Metrics/telemetry (TODO) |
| Focus | Automation | Augmentation |

To resurrect old code:
```bash
git show headless-mantle-archive:rust/mantle/src/session/start.rs
git checkout headless-mantle-archive -- rust/mantle/
```
