# Mantle Architecture

Mantle is Tidepool's harness for spawning and controlling Claude Code sessions via Docker containers. It provides process supervision, hook interception, and real-time streaming to mantle-hub for visualization.

## Overview

```
                                   TIDEPOOL (Haskell)
                                          |
                                          | spawns
                                          v
                              +------------------------+
                              |   mantle session start |
                              |    (orchestrator)      |
                              +------------------------+
                                          |
                                          | docker run
                                          v
                              +------------------------+
                              |   Docker Container     |
                              |   +-----------------+  |
                              |   |   Claude Code   |  |
                              |   +-----------------+  |
                              |   |  mantle-agent   |  |
                              |   +-----------------+  |
                              +------------------------+
                                     |          |
                        hooks via    |          | stream events
                        unix socket  |          | via WebSocket
                                     v          v
                              +-----------+  +-----------+
                              |  control  |  | mantle-hub|
                              |  socket   |  | (daemon)  |
                              +-----------+  +-----------+
```

## Architecture Layers

### 1. `mantle session start` - Orchestrator

The entry point called by Tidepool's Haskell code. Responsibilities:

- Creates git worktree for session isolation
- Registers session with mantle-hub (or existing hub session for graph tracking)
- Spawns Docker container running Claude Code
- Streams events to hub via WebSocket in real-time
- Parses Claude Code's `--stream-json` output
- Returns structured JSON to Tidepool on stdout

### 2. Docker Container

Contains Claude Code and mantle-agent. Benefits:

- **Isolation**: Each session runs in its own container
- **Reproducibility**: Consistent environment across runs
- **Security**: Claude Code can't escape the container
- **Auth sharing**: OAuth credentials mounted from host

### 3. `mantle-agent` - Container-Side Handler

Runs inside the container, handling hooks and MCP tools:

- **Hook handler**: Receives Claude Code hooks, forwards to orchestrator via control socket
- **MCP server**: Exposes decision tools (approve/reject/etc.) to Claude

### 4. `mantle-hub` - Visualization Daemon

Background service for session tracking and visualization:

- SQLite persistence for session/node state
- REST API for querying sessions
- WebSocket for live event streaming
- HTML UI for visualizing session graphs

## IPC Mechanisms

Mantle uses two distinct IPC channels:

### Control Socket (Bidirectional: Container <-> Host)

**Path**: Unix socket mounted into container

**Purpose**: Hook interception where Haskell can inspect, modify, or reject tool calls.

**Flow**:
```
Claude Code hook trigger
    -> mantle-agent hook <event>
    -> connect to Unix socket
    -> send ControlMessage::HookEvent
    -> receive ControlResponse::HookResponse
    -> exit code determines allow (0) or deny (2)
```

**Protocol**: Newline-delimited JSON (NDJSON) over Unix domain socket.

### Hub WebSocket (Unidirectional: Host -> Hub)

**URL**: `ws://localhost:7433/ws/push/{session_id}/{node_id}`

**Purpose**: Real-time streaming of Claude Code events for visualization.

**Flow**:
```
Claude Code stdout (--stream-json)
    -> mantle parses events
    -> forwards to hub via WebSocket
    -> hub stores and broadcasts to viewers
```

## Key Abstractions

### `ContainerConfig` (docker/container.rs)

Docker container configuration:
- Image selection and volume mounts
- Environment variable setup
- Timeout enforcement
- Control socket path generation

### `ControlListener` (docker/control_listener.rs)

Synchronous socket server for MCP tool calls:
- Runs in background thread
- Accumulates tool calls during execution
- Returns collected decisions when container exits

### `HubClient` (mantle-shared/hub/client.rs)

HTTP + WebSocket client for hub communication:
- Session and node registration
- Result submission
- Event streaming

### `SyncEventStream` (mantle-shared/hub/client.rs)

Synchronous WebSocket for event streaming:
- Blocking sends (simple integration with sync code)
- Success/failure counters for summary logging
- Graceful close with cleanup

## Session Lifecycle

### Start Session

1. Generate session ID and branch name
2. Create git worktree from base branch
3. Register with hub (get session_id, node_id)
4. Connect WebSocket for event streaming
5. Spawn Docker container with Claude Code
6. Parse stream-json output, forward events to hub
7. Wait for container exit
8. Submit final result to hub
9. Return SessionOutput on stdout

### Continue Session

1. Load existing session metadata
2. Validate session state (completed/failed/cancelled)
3. Get cc_session_id for `--resume` flag
4. Register continuation as new hub session
5. Execute with same flow as start

### Fork Session

1. Load parent session
2. Create child worktree from parent branch
3. Use `--resume --fork-session` flags
4. Execute with same flow as start

## Module Structure

```
mantle/src/
  main.rs           CLI entry point, subcommand dispatch
  config.rs         Load ~/.config/mantle/config.toml
  stream_parser.rs  Parse Claude Code --stream-json output
  session/
    start.rs        start_session() - main entry point
    continue_.rs    continue_session() - resume existing
    fork.rs         fork_session() - branch from parent
    state.rs        File-locked session store
    types.rs        SessionMetadata, SessionOutput
    worktree.rs     Git worktree management
  docker/
    container.rs    Docker run with TTY, event streaming
    control_listener.rs  Socket server for MCP tool calls
```

## Data Flow Summary

```
Tidepool (Haskell)
    |
    | mantle session start --slug "..." --prompt "..."
    v
mantle session start
    |
    | creates worktree
    | registers with hub
    | connects WebSocket
    | docker run
    v
Docker Container
    |
    +---> Claude Code stdout ---> mantle parses ---> hub WebSocket
    |                                                    |
    +---> hook triggers ---> mantle-agent ---> control socket ---> Host
    |                                                               |
    |                            <--- allow/deny/modify <-----------+
    |
    v
container exits
    |
    v
mantle submits result to hub, prints JSON to stdout
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
image = "mantle-agent:latest"
```
