# HTTP Singleton MCP Server + Claude Code Teams Integration

## Status: Phase 1-3 Implemented, Phase 4+ Redesigned (Feb 2026)

Phases 1-3 (HTTP transport, hot WASM reload, recompile command) are implemented.
The architecture has been simplified: shared directory (no worktrees), unix socket (no TCP), Claude Code Teams as the orchestration layer.

## Architecture

```
exomonad init
    |
    +-- Starts server in dedicated Zellij tab
    |
    +-- Unix socket server (.exomonad/server.sock)
    |   +-- MCP protocol (JSON-RPC over unix socket)
    |   +-- Per-session role assignment
    |
    +-- PluginManager (Arc<RwLock<Plugin>>)
    |   +-- Unified WASM loaded from .exomonad/wasm/wasm-guest-unified.wasm
    |   +-- Checked for mtime change on each tool call
    |   +-- Reloaded transparently if changed
    |
    +-- EffectRegistry (shared across all connections)
    |   +-- All built-in handlers (git, github, agent, messaging, etc.)
    |
    +-- .exomonad/server.pid (JSON: {pid, socket})
        +-- Clients discover socket path from this file
        +-- Process liveness verified via kill(pid, 0)
```

### Connection Model

```
TL Session (role=tl)
    ├── Claude TL ─────────────┐
    ├── Spawned Claude Agent 1 ─┤── share one MCP connection (unix socket)
    └── Spawned Claude Agent 2 ─┘

Gemini Agent 1 (role=dev) ──── own MCP connection (same unix socket)
Gemini Agent 2 (role=dev) ──── own MCP connection (same unix socket)
```

Claude Code Teams handles agent lifecycle. Spawned Claude agents inherit the TL's MCP connection. Gemini agents connect independently with role=dev.

### Hot Reload Loop (xmonad pattern)

```
1. Edit Haskell DSL (tools, effects, routing logic)
2. exomonad recompile [--role unified]
   -> nix develop .#wasm -c wasm32-wasi-cabal build ...
   -> copies .wasm to .exomonad/wasm/wasm-guest-unified.wasm
3. Next tool call -> server detects mtime change -> swaps WASM
   -> zero downtime, no agent restart
```

### Two Loading Modes

| Mode | When | WASM Source | Hot Reload |
|------|------|------------|------------|
| Embedded (`include_bytes!`) | `exomonad mcp-stdio` | Compiled into binary | No |
| File-based (`from_file`) | `exomonad serve` | `.exomonad/wasm/` | Yes |

Both modes share the same `PluginManager`, `EffectRegistry`, and handler stack.
Stdio mode is preserved for single-session debugging. Unix socket mode is for multi-agent.

## Claude Code Teams Integration Model

### Key Insight

Claude Code Teams is the orchestration layer. ExoMonad is the typed policy and effect layer.

| Primitive | Claude Code Teams | ExoMonad |
|-----------|------------------|----------|
| Shared task list | Yes | N/A (Teams owns this) |
| Inter-agent messaging | Yes (DM + broadcast) | Yes (inbox/outbox for Gemini agents) |
| Idle/shutdown negotiation | Yes | Yes (stop hooks) |
| Plan approval flow | Yes | Yes (pre-tool hooks) |
| Directory model | Shared | Shared |
| Heterogeneous agents | No (Claude-only) | Yes (Claude + Gemini via unix socket) |
| Hot-reloadable coordination | No | Yes (Haskell WASM) |
| Centralized policy bus | No (peer-to-peer) | Yes (unix socket singleton) |

### How It Works Together

- **Claude Code Teams gives UX**: idle notifications, plan approval UI, message rendering, task list
- **ExoMonad gives infrastructure**: typed permission cascade, heterogeneous agent support, hot-reloadable policy, centralized effect execution

The TL uses `spawn_agents` to create Gemini agents (each gets a Zellij tab + own MCP connection). Claude agents are spawned via Claude Code Teams (share the TL's connection). Both types coordinate through the Teams task list and messaging.

### Three-Tier Permission Cascade

```
Agent requests action (e.g. git push)
    |
    v
ExoMonad server (WASM policy layer)
    |-- Auto-approve: within agent's typed permission set
    |   (Gemini dev: commit, push to feature branch, read files)
    |
    |-- TL auto-approves: within TL's broader scope
    |   (TL: push to any non-main branch, create PRs, close issues)
    |   Permission sets defined in Haskell DSL, compiled to WASM
    |
    +-- Escalate to human:
        |-- Outside TL scope (push to main, delete branches, deploy)
        |-- TL uncertain (novel situation)
        +-- Rendered as Zellij popup with structured context
```

The permission sets are:
- **Typed**: Haskell ADTs (`DevPermission`, `TLPermission`), not string matching
- **Hot-reloadable**: edit, `exomonad recompile`, next request uses new policy
- **Hierarchical**: agent < TL < human, with typed scope boundaries

### Gemini Integration via Shared Bus

Gemini agents slot into the model via:
1. **Zellij tabs/panes**: shared directory, side-by-side with status plugin
2. **ExoMonad pretool hooks**: Gemini's pretool hook fires before each tool call
3. **Yolo mode + policy layer**: Gemini runs fast, ExoMonad mediates permissions
4. **Teams messaging bridge**: Gemini's notes/questions flow through inbox system
5. **HTTP MCP config**: all agents share the singleton server

### Zellij UI Model

```
Zellij Session
    |
    +-- Tab: Server (exomonad serve — logs, health)
    |
    +-- Tab: TL (Claude Code, role=tl)
    |   +-- Pane: Claude TL (main)
    |   +-- Pane: ExoMonad status (heartbeat, pending approvals)
    |
    +-- Tab: gh-42 (Gemini agent, role=dev)
    |   +-- Pane: Gemini agent (main)
    |   +-- Pane: ExoMonad status
    |   +-- Popup: "Agent wants to push. Approve? [Y/N]"
    |
    +-- Tab: gh-56 (another Gemini agent, role=dev)
        +-- Pane: Gemini agent (main)
        +-- Pane: ExoMonad status
```

- **Tab** = agent workspace (all in same directory)
- **Pane within tab** = coordination view
- **Popup overlay** = approval/permission UI (human bypass)

## Implementation Status

- [x] `exomonad serve` starts HTTP server
- [x] `POST /mcp` handles JSON-RPC (reuses stdio dispatch)
- [x] `GET /health` returns version info
- [x] WASM loaded from file path at startup
- [x] WASM auto-reloaded when file changes (mtime check per tool call)
- [x] `exomonad recompile` builds WASM and copies to `.exomonad/wasm/`
- [x] Running server picks up new WASM on next tool call after recompile
- [x] Stdio mode still works unchanged
- [x] PID file written for client discovery
- [ ] Unix socket transport (replacing TCP port)
- [ ] `exomonad init` creates server Zellij tab
- [ ] Unified WASM build (`AllRoles.hs`)
- [ ] Per-session role assignment at connection init
- [ ] Permission cascade (three-tier approval)
- [ ] Gemini pretool hook → ExoMonad policy layer
- [ ] Zellij popup approval UI wiring
