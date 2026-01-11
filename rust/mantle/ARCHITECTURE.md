# Mantle Architecture

Mantle is Tidepool's harness for spawning and controlling multiple Claude Code sessions concurrently. It provides process supervision, IPC, and hook interception for the Haskell orchestration layer.

## Overview

```
                                   TIDEPOOL (Haskell)
                                          |
                                          | spawns
                                          v
                              +------------------------+
                              |     mantle run         |
                              |    (orchestrator)      |
                              +------------------------+
                                          |
                                          | creates FIFO, spawns Zellij pane
                                          v
                              +------------------------+
                              |     Zellij Pane        |
                              +------------------------+
                                          |
                                          | runs
                                          v
                              +------------------------+
                              |     mantle wrap        |
                              |     (supervisor)       |
                              +------------------------+
                                          |
                                          | spawns & supervises
                                          v
                              +------------------------+
                              |     Claude Code        |
                              |    (subprocess)        |
                              +------------------------+
                                     |          |
                        hooks call   |          | in-band signals
                                     v          v
                              +-----------+  +-----------+
                              |mantle hook|  |mantle     |
                              |(subcommand)|  |signal     |
                              +-----------+  +-----------+
```

## Architecture Layers

### 1. `mantle run` - Orchestrator

The entry point called by Tidepool's Haskell code. Responsibilities:

- Creates a result FIFO for receiving the final `RunResult`
- Builds the `wrap` command with all necessary arguments
- Spawns a new Zellij pane running `mantle wrap`
- Blocks reading from the result FIFO until session completes
- Returns structured JSON to Tidepool

### 2. `mantle wrap` - Supervisor

Runs inside the Zellij pane, managing the Claude Code subprocess. Responsibilities:

- Installs signal handlers (SIGINT/SIGTERM forwarding)
- Creates a signal FIFO for interrupt communication
- Generates hook configuration (`.claude/settings.local.json`)
- Spawns Claude Code with appropriate arguments
- Parses streaming JSON output for TUI display
- Enforces timeout (kills child if exceeded)
- Collects events and writes final `RunResult` to result FIFO

### 3. `mantle signal` - In-Band Signaling

Called by Claude Code (via Bash tool) to send signals to the supervisor:

```bash
mantle signal transition --state need_review --reason "Blocked on type"
```

Writes JSON to the signal FIFO, which `wrap` polls and includes in the final result.

### 4. `mantle hook` - Hook Interception

Called by generated hook scripts when Claude Code triggers a hook event:

```
Claude Code hook trigger
    -> .claude/settings.local.json defines command
    -> mantle hook pre-tool-use
    -> reads hook payload from stdin
    -> forwards to control socket (Haskell)
    -> outputs response to stdout
    -> exit code determines allow (0) or deny (2)
```

## Why Zellij?

Zellij provides several benefits for concurrent Claude sessions:

1. **Isolated panes**: Each Claude session gets its own TUI with scroll history
2. **Session lifecycle**: Clean startup/shutdown semantics
3. **Visibility**: Human operators can observe all running sessions
4. **Multiplexing**: Multiple sessions share a single terminal

## IPC Mechanisms

Mantle uses two distinct IPC channels with different purposes:

### Signal FIFO (Unidirectional: Claude -> Wrap)

**Path**: `/tmp/mantle-<pid>.signal`

**Purpose**: In-band signals for state transitions and escalations. Claude Code calls `mantle signal` which writes JSON to this FIFO.

**Flow**:
```
Claude Code (Bash tool)
    -> mantle signal transition --state X
    -> writes to FIFO
    -> wrap's reader thread receives
    -> included in RunResult.interrupts
```

**Message format** (`InterruptSignal`):
```json
{
  "signal_type": "transition",
  "state": "need_more_types",
  "reason": "Missing Foo type definition"
}
```

### Control Socket (Bidirectional: Hook <-> Haskell)

**Path**: Provided via `--control-socket` / `TIDEPOOL_CONTROL_SOCKET`

**Purpose**: Hook interception where Haskell can inspect, modify, or reject tool calls before they execute.

**Flow**:
```
Claude Code hook
    -> mantle hook pre-tool-use
    -> connect to Unix socket
    -> send HookInput
    -> receive HookOutput
    -> output to stdout, exit with code
```

**Protocol**: Newline-delimited JSON (NDJSON) over Unix domain socket.

**Design choice**: Synchronous I/O because hooks block Claude Code anyway.

## Key Abstractions

### `Supervisor` (supervisor.rs)

Process lifecycle management with:
- Spawn with environment setup
- Timeout enforcement (actually kills child)
- Signal forwarding (SIGINT/SIGTERM)
- Cleanup on drop (no orphan processes)

**Limitation**: Global signal state means only one Supervisor per process.

### `HookConfig` (hooks.rs)

RAII guard for hook configuration:
- Generates `.claude/settings.local.json` with hooks pointing to `mantle hook <event>`
- Merges with existing settings (preserves user hooks)
- Cleans up on drop (restores original or deletes)

### `ResultFifo` / `SignalFifo` (fifo.rs)

RAII wrappers for named pipes:
- `ResultFifo`: Write-once result communication (wrap -> run)
- `SignalFifo`: Streaming interrupt signals with background reader thread

### `ControlSocket` (socket.rs)

Unix socket client for hook communication:
- Synchronous connect/send/receive
- Newline-delimited JSON framing
- 30-second default timeout

## Session Tag

The `--session-tag` parameter is a correlation ID that:
- Passes through from Tidepool to `run` to `wrap` to `RunResult`
- Identifies which orchestrator context (e.g., git worktree) a session belongs to
- Enables Tidepool to route results back to the correct graph node

## Module Structure

```
src/
  main.rs        CLI entry point, subcommand dispatch
  lib.rs         Public API exports
  supervisor.rs  Process supervision
  hooks.rs       Hook configuration generation
  fifo.rs        Named pipe abstractions
  socket.rs      Unix socket client
  protocol.rs    Control envelope message types
  events.rs      Claude Code stream event parsing
  humanize.rs    Human-readable output formatting
  error.rs       Error types
  tui/           Ratatui TUI implementation
```

## Data Flow Summary

```
Tidepool (Haskell)
    |
    | mantle run --session X --prompt "..."
    v
mantle run
    |
    | creates /tmp/mantle-<pid>.fifo
    | spawns zellij pane
    v
zellij -> mantle wrap --result-fifo ... -- claude args
    |
    | creates signal fifo
    | generates hooks config
    | spawns claude subprocess
    v
Claude Code
    |
    +---> stdout (stream-json) ---> wrap parses events
    |
    +---> hook triggers ---> mantle hook ---> control socket ---> Haskell
    |                                                               |
    |                            <--- allow/deny/modify <-----------+
    |
    +---> mantle signal ---> signal fifo ---> wrap collects interrupts
    |
    v
claude exits
    |
    v
wrap writes RunResult to result fifo
    |
    v
mantle run reads result, prints JSON to stdout
    |
    v
Tidepool receives structured result
```

## Auth Configuration

Dockerized Claude Code shares the host's OAuth credentials via volume mount:

- Mantle auto-mounts `~/.claude:/home/user/.claude` into containers
- Authenticate once on host: `claude` (normal CLI login)
- All containers share that auth automatically

For production with named volumes:
```toml
# ~/.config/mantle/config.toml
[docker]
auth_volume = "tidepool-claude-auth"  # Shared named volume
```
