# HTTP-Native Roles Design

## Status: Active (Feb 2026)

## Context

The migration from N stdio MCP processes to a singleton MCP server (`exomonad serve`) changes the agent model fundamentally. Previously, each agent was a fully isolated unit: own worktree, own MCP process, own WASM. Now all agents share a centralized server, a shared working directory, and a unified WASM module. Orchestration is handled by Claude Code Teams.

## Key Decisions

### 1. Shared Directory: No Worktrees

**Decision: All agents work in the same directory. No git worktrees.**

This follows the Claude Code Teams model: heterogeneous agents (Claude + Gemini) collaborate in a single repository checkout. File-level coordination is the responsibility of the orchestration layer (Claude Code Teams' task list and messaging), not git-level isolation.

What this means:
- **Before**: worktree per agent + dedicated stdio MCP process + dedicated WASM
- **After**: shared directory + shared unix socket MCP server + shared WASM (with per-session role filtering)

Agents avoid conflicts through task assignment and communication, not filesystem isolation. The TL assigns non-overlapping files/directories to different agents via the task system.

### 2. Unix Socket Server

**Decision: Server listens on `.exomonad/server.sock`. No TCP ports.**

```
.exomonad/
├── server.sock           # Unix domain socket (MCP server listens here)
├── server.pid            # JSON: {"pid": 12345, "socket": "/abs/path/.exomonad/server.sock"}
├── wasm/                 # Compiled WASM modules
└── config.toml           # Project config (default_role, etc.)
```

Client `.mcp.json` configuration:
```json
{
  "mcpServers": {
    "exomonad": {
      "type": "sse",
      "url": "unix:///abs/path/to/project/.exomonad/server.sock"
    }
  }
}
```

**Liveness check**: Clients read `.exomonad/server.pid`, verify PID is alive via `kill(pid, 0)`, then connect to the socket. If PID is stale, the human runs `exomonad init` to start a fresh server.

### 3. Server Lifecycle

**Decision: `exomonad init` starts the server in its own Zellij tab. Manual start.**

```bash
exomonad init              # Creates Zellij tab with server process
                           # Server writes server.sock + server.pid
                           # Tab persists for the session lifetime
```

The server is not auto-started. The human explicitly initializes it at the beginning of a session. This keeps the lifecycle visible and debuggable — the server tab shows logs, and the human can restart it if needed.

### 4. Per-Session Role Assignment

**Problem**: With a shared server, all roles share one WASM module. But a dev should not see `spawn_agents` and the TL should not see `file_pr`.

**Solution: Role established at session init, not per-request.**

Two connection patterns:
- **Claude Code Teams**: Spawned Claude agents share the TL's MCP connection (role=tl). Tool filtering is unnecessary — the TL session already has the right tool set.
- **Gemini agents**: Each gets its own connection to the same unix socket (role=dev). The server establishes role at connection init.

```
TL Session (role=tl)
    ├── Claude TL ─────────────┐
    ├── Spawned Claude Agent 1 ─┤── share one MCP connection (unix socket)
    └── Spawned Claude Agent 2 ─┘

Gemini Agent 1 (role=dev) ──── own MCP connection (same unix socket)
Gemini Agent 2 (role=dev) ──── own MCP connection (same unix socket)
```

**Implementation**: The WASM module contains ALL tools (union of all roles). The Rust server maintains a role→tool-set mapping derived from the role configs at WASM load time. Filtering happens in the Rust layer:

```
Session init with role=dev
    → Server resolves SomeRoleConfig for "dev"
    → tools/list returns only dev's tool schemas
    → Tool calls outside dev's set are rejected: "tool 'spawn_agents' not available for role 'dev'"
```

**Why a single WASM instead of per-role WASM**: Hot reload. When the TL recompiles, one WASM swap updates all roles simultaneously. No need to coordinate multiple WASM files.

WASM build pipeline:
- **Before**: `just wasm tl` and `just wasm dev` produce separate WASM modules
- **After**: `just wasm` produces a single unified WASM containing all tool records; `Role.hs` files become tool-set declarations that the Rust server reads at startup for filtering

### 5. Role Definitions

**TL Role** (unchanged):
```haskell
data Tools mode = Tools
  { agents :: AgentTools mode,
    popups :: PopupTools mode
  } deriving Generic

config :: RoleConfig (Tools AsHandler)
config = RoleConfig { roleName = "tl", tools = Tools { ... }, hooks = defaultHooks }
```

**Dev Role** (hooks updated for HTTP model):
```haskell
data Tools mode = Tools
  { pr :: FilePRTools mode,
    messaging :: MessagingTools mode
  } deriving Generic

config :: RoleConfig (Tools AsHandler)
config = RoleConfig { roleName = "dev", tools = Tools { ... }, hooks = httpDevHooks }
```

### 6. Hook Model

**Decision: Two-layer hook model.**

| Hook Type | Where | Why |
|-----------|-------|-----|
| **Permission checks** (preToolUse) | Server-side WASM | Policy is centralized; agents can't bypass it |
| **Stop validation** (onStop, onSubagentStop) | Server-side WASM | Server knows agent state |
| **UI prompts** (approval popups) | Client-side (Zellij plugin) | UI must be local to the human's terminal |

#### Stop Hooks (Simplified)

In the shared-directory model, stop hooks check:
1. Has the agent's assigned task been completed?
2. Has work been committed to the shared repo?
3. Is there a PR filed for the work?

No worktree-specific checks (uncommitted changes in worktree, etc.) — those don't apply when everyone shares the same directory.

```haskell
httpDevHooks :: HookConfig
httpDevHooks =
  HookConfig
    { preToolUse = permissionCascade,   -- Three-tier permission check
      onStop = \_ -> embed runStopHookChecks,
      onSubagentStop = \_ -> embed runStopHookChecks
    }
```

Hook triggering:
- **Claude agents**: Claude Code's `SessionEnd` hook calls the server's hook endpoint. The server runs WASM stop hook logic and returns allow/block.
- **Gemini agents**: Gemini's `AfterAgent` hook calls the same endpoint.

#### Permission Cascade (preToolUse)

Three-tier typed permission checking:

```haskell
permissionCascade :: HookInput -> Sem HookEffects HookOutput
permissionCascade input = do
  let tool = hookInputToolName input
      args = hookInputToolArgs input
  case checkAgentPermissions (hookInputRole input) tool args of
    Allowed -> pure (allowResponse Nothing)
    Escalate -> do
      tlDecision <- escalateToTL tool args
      case tlDecision of
        TLApproved -> pure (allowResponse (Just "TL-approved"))
        TLEscalate -> pure (escalateResponse "Requires human approval")
        TLDenied reason -> pure (denyResponse reason)
```

Permission sets are typed ADTs:

```haskell
data DevPermission
  = DevCommitAllowed
  | DevPushFeatureBranch BranchName
  | DevReadFile FilePath
  | DevRunTests
  | DevFilePR
  | DevSendMessage

data TLPermission
  = TLPushAnyBranch BranchName
  | TLClosePR
  | TLCloseIssue
  | TLDeleteMergedBranch BranchName
  | TLSpawnAgents
  | TLCleanupAgents
  | TLShowPopup
```

### 7. Unified WASM Architecture

Single WASM module contains all roles:

```haskell
module AllRoles (allConfigs, lookupRole) where

import qualified TLRole
import qualified DevRole

allConfigs :: Map Text SomeRoleConfig
allConfigs = Map.fromList
  [ ("tl", mkSomeRoleConfig TLRole.config)
  , ("dev", mkSomeRoleConfig DevRole.config)
  ]
```

`SomeRoleConfig` captures dispatch, listing, and hook capabilities via a `RoleCapabilities` record (no existential types needed). The Rust server resolves the role once at session init and holds the capabilities for the session lifetime.

### 8. Hot Reload

**Decision: Hot reload is a feature. No versioning needed yet.**

When the TL edits `Role.hs` and recompiles:
1. The unified WASM is rebuilt
2. The server detects mtime change on next tool call
3. New WASM is loaded; tool filtering updated

**Why this is safe:**
- Tool schemas change atomically (single WASM swap)
- In-flight requests complete with the old WASM; next request gets the new one
- Claude/Gemini agents re-fetch `tools/list` naturally when their context suggests tools changed
- The TL is the only one who can recompile — devs have no access to `exomonad recompile`

## Migration Path

### Phase 1: Unix Socket Server + Unified WASM + Per-Session Roles (Current Sprint)
- `exomonad init` starts server on `.exomonad/server.sock`
- Unified WASM with `AllRoles.hs` containing all role configs
- Role established per-session (TL via Claude Code Teams, dev via Gemini connection)
- `just wasm` builds one artifact
- Hot reload updates all roles atomically
- Stop hooks check task completion + committed work + PR status
- `.mcp.json` points to unix socket

### Phase 2: Permission Cascade
- Implement `permissionCascade` in preToolUse
- Typed permission sets per role (DevPermission, TLPermission ADTs)
- TL auto-approval scope
- Human escalation via Zellij popup

### Phase 3: Gemini Pretool Hook Integration
- Gemini's pretool hook fires before each tool call
- ExoMonad mediates permissions via the same cascade
- Yolo mode + policy layer: Gemini runs fast, server gates dangerous actions

## Summary

| Aspect | Current (stdio) | HTTP-native |
|--------|-----------------|-------------|
| WASM per agent | Yes (embedded) | No (shared server) |
| Directory model | Worktree per agent | Shared directory |
| Transport | Stdio per agent | Unix socket (shared) |
| Server lifecycle | Per-agent process | `exomonad init` (manual, persistent) |
| Stop hooks | Worktree-specific checks | Task completion + commit + PR checks |
| PreToolUse | Passthrough | Permission cascade (phased) |
| Tool filtering | Compile-time (separate WASM) | Runtime (per-session role) |
| Hot reload | N/A (embedded) | Yes (TL recompiles, all roles update) |
| Role identification | config.local.toml in worktree | Per-session at connection init |
| Orchestration | ExoMonad spawn_agents | Claude Code Teams + ExoMonad server |
