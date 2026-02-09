# HTTP Singleton MCP Server + Claude Code Teams Integration

## Status: Phase 1-3 Implemented (Feb 2026)

Phases 1-3 (HTTP transport, hot WASM reload, recompile command) are implemented.
Phase 4 (spawn_agents HTTP integration) is wired. Phase 5+ is future work.

## Architecture

```
exomonad serve --port 7432
    |
    +-- axum HTTP server (JSON-RPC over HTTP POST)
    |   +-- POST /mcp  -> same dispatch as stdio mode
    |   +-- GET /health -> liveness check
    |
    +-- PluginManager (Arc<RwLock<Plugin>>)
    |   +-- WASM loaded from file path (.exomonad/wasm/{role}.wasm)
    |   +-- Checked for mtime change on each tool call
    |   +-- Reloaded transparently if changed
    |
    +-- EffectRegistry (shared across all connections)
    |   +-- All built-in handlers (git, github, agent, messaging, etc.)
    |
    +-- .exomonad/server.pid (JSON: {pid, port, role})
        +-- Clients discover port from this file
        +-- Process liveness verified via kill(pid, 0)
```

### Hot Reload Loop (xmonad pattern)

```
1. Edit Haskell DSL (tools, effects, routing logic)
2. exomonad recompile [--role tl]
   -> nix develop .#wasm -c wasm32-wasi-cabal build ...
   -> copies .wasm to .exomonad/wasm/wasm-guest-{role}.wasm
3. Next tool call -> server detects mtime change -> swaps WASM
   -> zero downtime, no agent restart
```

### Two Loading Modes

| Mode | When | WASM Source | Hot Reload |
|------|------|------------|------------|
| Embedded (`include_bytes!`) | `exomonad mcp-stdio` | Compiled into binary | No |
| File-based (`from_file`) | `exomonad serve` | `.exomonad/wasm/` | Yes |

Both modes share the same `PluginManager`, `EffectRegistry`, and handler stack.
Stdio mode is preserved for single-session debugging. HTTP mode is for multi-agent.

### spawn_agents Transport Selection

When `spawn_agents` creates a worktree, it checks for `.exomonad/server.pid`:
- If server is running: writes HTTP `.mcp.json` (`"type": "sse", "url": "..."`)
- If no server: writes stdio `.mcp.json` (`"command": "exomonad", "args": ["mcp-stdio"]`)

This means the same codebase works for both single-session and multi-agent scenarios.

## Claude Code Teams Integration Model

### Key Insight

Claude Code Teams is ExoMonad without worktrees. Same primitives:

| Primitive | Claude Code Teams | ExoMonad |
|-----------|------------------|----------|
| Shared task list | Yes | Building (via Teams filesystem) |
| Inter-agent messaging | Yes (DM + broadcast) | Yes (inbox/outbox + Teams bridge) |
| Idle/shutdown negotiation | Yes | Yes (stop hooks) |
| Plan approval flow | Yes | Yes (pre-tool hooks) |
| Git worktree isolation | No | Yes |
| Heterogeneous agents | No (Claude-only) | Yes (Claude + Gemini) |
| Hot-reloadable coordination | No | Yes (Haskell WASM) |
| Centralized coordination bus | No (peer-to-peer) | Yes (HTTP singleton) |

### Merge Path: Both at Once

PR #561 already wires this: spawned agents register as synthetic teammates in
Claude Code Teams, getting messaging + task list access alongside worktree isolation.

The HTTP server becomes the coordination bus that Teams wishes it had:
- **Teams gives UX**: idle notifications, plan approval UI, message rendering
- **ExoMonad gives infrastructure**: worktrees, heterogeneous agents, typed effects

### Three-Tier Permission Cascade

```
Agent requests action (e.g. git push)
    |
    v
ExoMonad HTTP server (WASM policy layer)
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
- **Typed**: Haskell ADTs, not string matching
- **Hot-reloadable**: edit, `exomonad recompile`, next request uses new policy
- **Hierarchical**: agent < TL < human, with typed scope boundaries

### Gemini Integration via Shared Bus

Gemini agents slot into the model via:
1. **Zellij tabs/panes**: same worktree, side-by-side with status plugin
2. **ExoMonad pretool hooks**: Gemini's pretool hook fires before each tool call
3. **Yolo mode + policy layer**: Gemini runs fast, ExoMonad mediates permissions
4. **Teams messaging bridge**: Gemini's notes/questions flow through inbox system
5. **HTTP .mcp.json**: all agents share the singleton server

### Zellij UI Model

```
Zellij Session
    |
    +-- Tab: TL (Claude Code + exomonad serve)
    |
    +-- Tab: gh-42 (worktree isolation)
    |   +-- Pane: Gemini agent (main)
    |   +-- Pane: ExoMonad status (heartbeat, pending approvals)
    |   +-- Popup: "Agent wants to push. Approve? [Y/N]"
    |
    +-- Tab: gh-56 (another worktree)
        +-- Pane: Claude agent (main)
        +-- Pane: ExoMonad status
```

- **Tab** = separate worktree (code isolation)
- **Pane within tab** = same worktree (coordination view)
- **Popup overlay** = approval/permission UI (human bypass)

## Implementation Status

- [x] `exomonad serve` starts HTTP server on configurable port
- [x] `POST /mcp` handles JSON-RPC (reuses stdio dispatch)
- [x] `GET /health` returns version info
- [x] WASM loaded from file path at startup (not include_bytes!)
- [x] WASM auto-reloaded when file changes (mtime check per tool call)
- [x] `exomonad recompile` builds WASM and copies to `.exomonad/wasm/`
- [x] Running server picks up new WASM on next tool call after recompile
- [x] Stdio mode still works unchanged
- [x] Port written to `.exomonad/server.pid` for client discovery
- [x] spawn_agents detects HTTP server and writes appropriate .mcp.json
- [ ] SSE streaming for server->client push (future)
- [ ] Permission cascade (three-tier approval) (future)
- [ ] Gemini pretool hook -> ExoMonad policy layer (future)
- [ ] Zellij popup approval UI wiring (future)
