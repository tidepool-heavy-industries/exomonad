# Dockerification Plan

## Goal

Move from per-worktree process-compose stacks to fully containerized architecture. Control-server runs in Docker (Linux), enabling Unix socket communication between containers. Host only runs Zellij + tui-sidebar.

## Architecture

**Simplified: Everything in one orchestrator container**

```
macOS HOST
│
└── docker run -it --detach-keys="ctrl-e,e" \
      -v /var/run/docker.sock:/var/run/docker.sock \
      -v $(pwd)/worktrees:/worktrees \
      exomonad/orchestrator
    │
    └── tini (PID 1) → zellij attach --create orchestrator
        │
        ├── Pane: control-server (Haskell, Servant HTTP)
        │   ├── Unix socket: /sockets/control.sock
        │   └── WebSocket for TUI communication
        │
        ├── Pane: agent-1 (docker run -it via DooD)
        │   ├── tini → claude --permission-mode bypassPermissions
        │   ├── exomonad → /sockets/control.sock
        │   └── /workspace (bind-mount to /worktrees/agent-1)
        │
        ├── Pane: agent-2 (same, sibling container)
        │
        └── Pane: TUI popup (spawned by Zellij on demand)

Shared volumes (Docker volumes, not host mounts):
├── exomonad-sockets  → /sockets/
├── git-cache         → /git-cache
├── cargo-cache       → /cargo-cache
└── cabal-cache       → /cabal-cache
```

**Why this architecture?**
- macOS cannot bind-mount Unix sockets into containers (VM boundary)
- Docker socket IS specially handled by Docker Desktop (works)
- Running Zellij inside Docker eliminates all host-side components
- Agent containers are siblings via Docker-outside-of-Docker pattern
- Single `docker run -it` gives you the whole environment

**Key details:**
- `--detach-keys="ctrl-e,e"` avoids Ctrl+P conflict with Zellij/Claude
- tini as PID 1 for signal handling and zombie reaping
- Wrapper script with trap handler for sibling container cleanup (see below)
- `zellij attach --create` for resilient reattachment
- `session_serialization true` + cache volume for session persistence
- control-server talks to Zellij directly for TUI popups (no TCP needed)
- Agents connect via shared socket volume

**Critical: Sibling Container Cleanup**

Agent containers are siblings (spawned via DooD), not children of tini. tini cannot propagate signals to them. We need an explicit cleanup wrapper:

```bash
#!/bin/bash
# /usr/local/bin/orchestrator-entrypoint.sh

cleanup() {
    echo "Stopping sibling agents..."
    docker ps -q --filter "label=exomonad.orchestrator=$HOSTNAME" | xargs -r docker kill
}
trap cleanup EXIT

exec zellij attach --create orchestrator
```

**Signal flow:**
1. `docker stop orchestrator` → SIGTERM to tini
2. tini forwards SIGTERM to wrapper script
3. Wrapper's EXIT trap fires, kills labeled siblings
4. Zellij exits, wrapper exits, tini exits cleanly

## Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Architecture | Single orchestrator container | Eliminates host-side components, everything in Docker |
| Orchestrator PID 1 | tini → wrapper → Zellij | tini handles signals/zombies, wrapper handles sibling cleanup |
| Sibling cleanup | Wrapper script with EXIT trap | Agents are siblings (DooD), tini can't signal them |
| Agent labeling | `--label exomonad.orchestrator=$HOSTNAME` | Enables targeted cleanup of sibling containers |
| Agent spawning | Docker-outside-of-Docker (DooD) | Mount docker.sock, spawn siblings via Zellij panes |
| Detach keys | `--detach-keys="ctrl-e,e"` | **Critical**: Avoid Ctrl+P conflict with Zellij/Claude |
| Zellij startup | `zellij attach --create orchestrator` | Resilient reattachment if accidentally detached |
| Zellij persistence | `session_serialization true` + volume | Survives container restart, mount `/root/.cache/zellij` |
| Agent init | tini (not Horust) | 10KB, handles signals + zombies |
| Docker CLI | Static binary from `docker:cli` | Minimal size, API backward compatible |
| Git sharing | alternates mechanism + `preciousObjects` | Safe for concurrent access, prevents gc corruption |
| MCP transport | HTTP over Unix socket (`http+unix://`) | Claude Code supports this natively |
| Hooks | exomonad → Unix socket | Ephemeral process, 5m timeout |
| Protocol | Servant HTTP over Unix socket | Agents connect via shared volume |
| TUI popups | Zellij pane spawned by control-server | Direct IPC, no TCP needed |
| TUI communication | WebSocket with correlation IDs | Bidirectional, supports concurrent popups |
| TUI disconnect handling | `TVar ConnectionStatus` + STM `orElse` | Prevents blocked threads on pane close |
| Container exit | tini + `--rm` flag | Container dies, worktree persists |
| Claude auth | Bind-mount ~/.claude.json | OAuth session from host's `claude login` |
| SSH auth | Docker Desktop magic socket | `/run/host-services/ssh-auth.sock` |
| Docker socket perms | GID matching or --group-add | Container user needs docker group access |

## Phases

### Phase 1: Protocol Migration

**Goal:** Replace NDJSON-over-raw-socket with Servant HTTP-over-Unix-socket.

1. **Add Servant HTTP to control-server**
   - Add warp, servant-server, warp-unix dependencies
   - Define API types in new module `ExoMonad.Control.API`
   - Implement endpoints:
     - `POST /hook` - receives hook event, returns allow/deny
     - `POST /mcp/call` - receives tool call, returns result
     - `POST /tui/spawn` - spawns popup, blocks until user responds, returns result
   - Listen on Unix socket via warp-unix

2. **Migrate exomonad to HTTP client**
   - Add hyper-unix or reqwest with unix socket support
   - Hook subcommand: POST to `/hook`
   - Remove MCP proxy code (Claude talks HTTP MCP directly)

3. **Remove LSP from control-server**
   - Comment out LSP-dependent MCP tools (find_callers, show_fields, show_constructors, teach-graph)
   - Remove lsp-interpreter dependency
   - Remove LSP session management code

4. **Test with existing setup**
   - Verify hooks work via HTTP
   - Verify MCP tools work via HTTP MCP transport
   - Verify TUI popups work

### Phase 2: Orchestrator Container

**Goal:** Create single orchestrator container with Zellij + control-server.

1. **Create orchestrator Dockerfile** (`docker/orchestrator/Dockerfile`)
   ```dockerfile
   FROM docker:cli AS docker-cli

   FROM debian:bookworm-slim

   RUN apt-get update && apt-get install -y \
       tini curl ca-certificates bash

   # Static Docker CLI (minimal, ~50MB vs ~500MB for docker.io)
   COPY --from=docker-cli /usr/local/bin/docker /usr/local/bin/

   # Install Zellij
   RUN curl -L https://github.com/zellij-org/zellij/releases/latest/download/zellij-x86_64-unknown-linux-musl.tar.gz \
       | tar -xz -C /usr/local/bin

   # Copy pre-built binaries
   COPY bin/control-server /usr/local/bin/
   COPY bin/tui-popup /usr/local/bin/

   # Wrapper script for sibling cleanup
   COPY scripts/orchestrator-entrypoint.sh /usr/local/bin/
   RUN chmod +x /usr/local/bin/orchestrator-entrypoint.sh

   # Zellij layout and config
   COPY config/orchestrator.kdl /root/.config/zellij/layouts/default.kdl
   COPY config/zellij.kdl /root/.config/zellij/config.kdl

   ENV TERM=xterm-256color
   ENV DOCKER_HOST=unix:///var/run/docker.sock

   # tini handles zombies/signals, wrapper handles sibling cleanup
   ENTRYPOINT ["/usr/bin/tini", "--"]
   CMD ["/usr/local/bin/orchestrator-entrypoint.sh"]
   ```

2. **Create Zellij layout** (`docker/orchestrator/config/orchestrator.kdl`)
   - Pane 1: control-server (starts automatically)
   - Pane 2: Empty (for agent spawning)
   - Pane 3: Logs/monitoring

3. **Update agent Dockerfile** (from PR #313)
   - Already has tini, Claude Code, exomonad
   - Ensure socket path matches orchestrator's shared volume

4. **Test orchestrator manually**
   ```bash
   docker run -it --rm \
     --detach-keys="ctrl-e,e" \
     -v /var/run/docker.sock:/var/run/docker.sock \
     -v $(pwd)/worktrees:/worktrees \
     -v ~/.claude.json:/root/.claude.json:ro \
     exomonad/orchestrator
   ```

### Phase 3: Agent Spawning via Zellij

**Goal:** control-server spawns agent containers as Zellij panes.

1. **Implement spawn_agents in control-server**
   - Receives: number of agents, task description
   - Creates worktrees (git worktree add)
   - Calls `zellij action new-pane -- docker run -it ...`
   - Agent containers are siblings (DooD pattern)
   - Returns agent IDs

2. **Agent container launch command**
   ```bash
   docker run -it --rm \
     --detach-keys="ctrl-e,e" \
     -v exomonad-sockets:/sockets \
     -v /worktrees/agent-1:/workspace \
     -v exomonad-git-cache:/git-cache:ro \
     -v exomonad-cargo-cache:/cargo-cache \
     -e GIT_ALTERNATES_OBJECT_DIR=/git-cache/objects \
     exomonad/claude-agent
   ```

3. **Agent lifecycle**
   - Zellij pane shows live Claude session
   - Human switches panes to interact
   - When Claude exits, container dies (`--rm`), pane closes
   - Worktree persists for review

4. **Test multi-agent**
   - Spawn 3 agents from control-server
   - Switch between Zellij panes
   - Verify all connect to control-server socket

### Phase 4: TUI Popups via Zellij

**Goal:** TUI popups as Zellij panes with WebSocket communication back to control-server.

#### WebSocket Architecture (from oracle consultation)

**Integration:** Use `servant-websockets` within existing Servant API (same Unix socket, protocol upgrade).

**Connection Management:**
```haskell
-- Per-popup state
data PopupConnection = PopupConnection
  { pcAgentId    :: AgentID
  , pcRequestId  :: RequestID
  , pcResponseVar :: TMVar Response
  , pcStatus     :: TVar ConnectionStatus
  }

-- Server state
data TUIState = TUIState
  { tsConnections :: TVar (Map RequestID PopupConnection)
  , tsBroadcast   :: TChan GlobalMessage  -- shutdown, etc.
  }
```

**Request-Response with Correlation IDs:**
1. Server generates unique `RequestID`, creates empty `TMVar`
2. Server sends `{ "id": 123, "type": "AskUser", "spec": ... }` to popup
3. Server blocks on `takeTMVar responseVar`
4. Popup sends back `{ "id": 123, "response": "OK" }`
5. WebSocket reader does `putTMVar`, unblocking server thread

**Disconnect Handling (prevents deadlocks):**
```haskell
-- Waiter uses orElse to race response vs disconnect
waitForResponse :: PopupConnection -> STM (Either ClientGone Response)
waitForResponse pc =
  (Right <$> takeTMVar (pcResponseVar pc))
  `orElse`
  (checkDisconnected (pcStatus pc) >> return (Left ClientGone))

-- Handler sets status on socket close
handlePopup :: Connection -> TUIState -> IO ()
handlePopup conn state = do
  statusVar <- newTVarIO Connected
  finally
    (popupLoop conn state statusVar)
    (atomically $ writeTVar statusVar Disconnected)
```

#### Implementation Steps

1. **Create tui-popup binary**
   - Minimal Rust binary (reuse tui-sidebar rendering code)
   - Connects to control-server WebSocket: `ws+unix:///sockets/control.sock/tui/ws`
   - Receives UISpec, renders popup, captures input
   - Sends response with correlation ID
   - Exits when done

2. **Add WebSocket endpoint to control-server**
   - `"tui" :> "ws" :> WebSocket` in Servant API
   - Manages `TVar (Map RequestID (TMVar Response))`
   - Handles connection lifecycle with `bracket`/`finally`

3. **Implement popup spawning**
   - Spawns Zellij pane: `zellij action new-pane -- tui-popup`
   - Popup connects via WebSocket, receives spec
   - Server blocks on TMVar until response or disconnect

4. **Wire up MCP tools**
   - confirm_action → spawns confirmation popup pane
   - select_option → spawns selection popup pane
   - request_guidance → spawns text input popup pane

5. **Test end-to-end**
   - Agent calls confirm_action MCP tool
   - Popup appears as new Zellij pane
   - Human responds
   - Result flows back to agent via WebSocket → TMVar
   - MCP tool returns result to Claude

## What Gets Removed

- LSP code from control-server
- LSP-dependent MCP tools (commented out, not deleted)
- NDJSON protocol code (replaced by Servant HTTP)
- MCP proxy in exomonad (Claude uses HTTP MCP directly)
- process-compose per-worktree setup
- Host-side tui-sidebar (replaced by Zellij panes)
- Host-side Zellij session (now inside orchestrator container)

## What Gets Added

- Orchestrator Dockerfile (tini + wrapper + Zellij + control-server + docker CLI)
- Orchestrator entrypoint script (sibling cleanup via EXIT trap)
- Zellij config with `session_serialization true`
- Agent Dockerfile (tini + Claude + exomonad) [PR #313]
- Zellij layout for orchestrator
- tui-popup binary (WebSocket client, ratatui renderer)
- Servant API definitions [PR #312]
- WebSocket endpoint for TUI (`/tui/ws`)
- TMVar-based request-response with correlation IDs
- spawn_agents via Zellij panes with agent labeling
- Docker-outside-of-Docker pattern for sibling containers

## Resolved Questions

1. **How does TUI popup result get back?** ✅ RESOLVED
   - **Answer:** WebSocket with correlation IDs
   - Popup connects to `ws+unix:///sockets/control.sock/tui/ws`
   - Server blocks on `TMVar`, popup sends response with request ID
   - Disconnect handling via `TVar ConnectionStatus` + STM `orElse`

2. **How to handle sibling container cleanup?** ✅ RESOLVED
   - **Answer:** Wrapper script with EXIT trap
   - tini can't signal siblings (they're spawned by host daemon)
   - Label agents with `--label exomonad.orchestrator=$HOSTNAME`
   - Trap kills labeled containers on orchestrator exit

3. **Docker CLI in orchestrator?** ✅ RESOLVED
   - **Answer:** Static binary from `docker:cli` image (multi-stage)
   - ~50MB vs ~500MB for full docker.io
   - API is backward compatible

## Open Questions

1. **Where does spawn_agents live?** Options:
   - Haskell (in control-server, exposed as CLI or MCP tool)
   - Rust (new binary, or extend exomonad)
   - Shell script (simplest, might be enough)

2. **Agent identification:** How does control-server know which container is calling?
   - Socket path encodes agent ID (agent-1.sock → agent "1")
   - Or explicit header/field in requests
   - Or agent passes ID in request body/headers

## Success Criteria

- [ ] Single containerized Claude can complete a coding task
- [ ] Hooks work (exomonad → HTTP → control-server)
- [ ] MCP tools work (Claude → HTTP MCP → control-server)
- [ ] TUI popups work (popup appears on host, result flows back)
- [ ] 3 concurrent agents can work without interference
- [ ] Build caches are shared (second build is fast)
- [ ] Git operations work (push/pull from inside container)
