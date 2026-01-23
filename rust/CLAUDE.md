# Claude Code++: Human-Augmented Sessions

Rust workspace for augmenting human-driven Claude Code sessions with Tidepool integrations.

**This is NOT a headless orchestration system.** Humans interact with Claude Code directly via TTY; this infrastructure adds superpowers.

> **Archived:** The previous headless Docker orchestration is preserved in git tag `headless-mantle-archive`. To resurrect: `git show headless-mantle-archive:rust/mantle/`

## Documentation Tree

```
rust/CLAUDE.md  ← YOU ARE HERE (router)
├── mantle-agent/CLAUDE.md  ← Hook handler + MCP server (IMPLEMENTED)
│   • hook subcommand: forwards CC hooks to TCP control server
│   • mcp subcommand: JSON-RPC 2.0 stdio server, forwards tool calls
│   • Fail-open: works without control server
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
               ├─ MCP tools ──────▶ mantle-agent mcp ──▶ Control Server (TCP)
               │                                              │
               └─ Hooks ──────────▶ mantle-agent hook ────────┘
                                          │
                                          ▼
                                   Haskell native-server
                                   (effect stack + handlers)
```

**Current state:**
- `mantle-agent hook` - Forwards Claude Code hooks to TCP control socket ✓
- `mantle-agent mcp` - MCP stdio server that forwards tool calls to TCP control socket ✓
- `haskell/control-server` - Haskell TCP endpoint (passthrough hook handlers, stub MCP) ✓
- `flake.nix` - Nix shell with zellij layout for Claude Code++ ✓
- **Missing:** Daemon mode, metrics hub, real hook/MCP logic in Haskell

## Workspace Members

| Crate | Type | Purpose |
|-------|------|---------|
| [mantle-agent](mantle-agent/CLAUDE.md) | Binary | Hook handler + MCP server |
| [mantle-hub](mantle-hub/CLAUDE.md) | Binary | Metrics/telemetry hub (WIP) |
| [mantle-shared](mantle-shared/CLAUDE.md) | Library | Shared types, protocols, TCP socket client |
| [tui-sidebar](tui-sidebar/CLAUDE.md) | Binary + Lib | TUI sidebar: TCP server (7433) for interactive UIs |

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

# Run MCP stdio server for MCP tools
mantle-agent mcp
```

### Environment Variables
| Variable | Used By | Purpose |
|----------|---------|---------|
| `MANTLE_CONTROL_HOST` | mantle-agent | TCP host for control socket (required) |
| `MANTLE_CONTROL_PORT` | mantle-agent | TCP port for control socket (required) |
| `MANTLE_DECISION_TOOLS_FILE` | mantle-agent mcp | Path to JSON file with MCP tool definitions |
| `RUST_LOG` | all | Tracing log level |

## What Works Today

### 1. Hook Handler
```bash
mantle-agent hook pre-tool-use  # Reads JSON from stdin
```
- Called by `.claude/settings.local.json` hooks
- Parses Claude Code's hook JSON from stdin
- Forwards to control server via TCP (NDJSON protocol)
- Returns response JSON to stdout
- **Fail-closed:** Errors immediately if control server unavailable (catches config issues during dev)

### 2. MCP Server
```bash
mantle-agent mcp [--tools <TOOL1>,<TOOL2>,...]
```
- JSON-RPC 2.0 over stdio (MCP protocol)
- Queries tool definitions from control server at startup
- **Role-based filtering:** `--tools` flag restricts which tools are exposed
  - PM role: `--tools pm_propose,pm_approve_expansion,pm_prioritize,pm_status,pm_review_dag,exo_status`
  - TL role: Omit `--tools` for full tool access
- Forwards `tools/call` requests to control server via Unix socket
- Rejects calls to non-allowlisted tools with clear error message
- Returns tool results to Claude Code

## Control Socket Protocol

Both hooks and MCP use the same TCP protocol (NDJSON):

```
mantle-agent                    Control Server (Haskell)
     │                                    │
     │  ControlMessage (JSON + newline)   │
     │───────────────────────────────────▶│
     │                                    │
     │  ControlResponse (JSON + newline)  │
     │◀───────────────────────────────────│
```

**ControlMessage variants:**
- `HookEvent { input: HookInput }` - Claude Code hook event
- `MCPToolCall { id, tool_name, arguments }` - MCP tool call

**ControlResponse variants:**
- `HookResponse { output: HookOutput, exit_code }` - Hook decision
- `MCPToolResponse { id, result, error }` - Tool result

## What's Missing (TODO)

### Daemon Mode
Goal: Long-lived process that collects metrics + forwards to Haskell

```bash
# Planned:
mantle-agent daemon start  # Listen on TCP, forward to Haskell
```

Status: Not implemented. Current architecture uses per-hook process spawning.

### Metrics Hub
Goal: Store tool call traces, export to Grafana

Status: mantle-hub needs repurposing from session tracking to metrics collection.

### Real Hook Logic in Haskell
Goal: Wire control-server handlers to Tidepool effect stack

Status: Current implementation is passthrough (logs and allows all hooks).

### MCP Tool Implementation
Goal: Expose Tidepool agents (e.g., semantic-scout) as MCP tools via control-server

Status: Current implementation returns "no tools available".

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
