# Exo Tools - External Orchestration

MCP tools for external orchestration: beads (BD), git, subprocess operations, and parallel agent spawning.

## When to Read This

Read this if you're:
- Using spawn_agents to dispatch parallel agents in Zellij
- Working with beads task tracking integration
- Understanding the exo_* MCP tools
- Debugging worktree creation or agent orchestration

## Tools Overview

| Tool | Purpose | Tier |
|------|---------|------|
| `spawn_agents` | Create worktrees and launch parallel agents in Zellij tabs | Tier 3 |
| `exo_status` | Get current bead context, git status, and PR info | Tier 3 |
| `exo_complete` | Mark bead as complete and clean up worktree | Tier 3 |
| `exo_reconstitute` | Sync beads from main and refresh context | Tier 3 |
| `file_pr` | File GitHub PR with bead context | Tier 3 |

---

# spawn_agents - Parallel Agent Orchestration

Create git worktrees for multiple beads and launch isolated agent sessions in Zellij tabs.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│ User (in Zellij session)                                        │
│  Calls: spawn_agents(["4hj", "kg6", "hp4"])                     │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│ spawn_agents Handler                                            │
│  1. Validate Zellij environment                                 │
│  2. Discover Hangar root                                        │
│  3. For each bead ID:                                           │
│     a. Fetch bead from BD                                       │
│     b. Create git worktree                                      │
│     c. Bootstrap .tidepool/ directory                           │
│     d. Generate process-compose.yaml                            │
│     e. Write .env with merged environment                       │
│     f. Write backend config (.claude/ or .gemini/)              │
│     g. Write bead context to .claude/context/bead.md            │
│     h. Launch Zellij tab with layout                            │
└─────────────────┬───────────────────────────────────────────────┘
                  │
                  ▼
┌──────────────────────────────────────────────────────────────────┐
│ Hangar Worktrees Structure                                       │
│                                                                  │
│  ~/hangars/tidepool/                                             │
│    ├── Hangar.toml                                               │
│    ├── runtime/bin/                                              │
│    │   ├── tidepool-control-server                              │
│    │   ├── mantle-agent                                          │
│    │   └── tui-sidebar                                           │
│    └── worktrees/                                                │
│        ├── bd-4hj-fix-tui-socket-deadlock/                       │
│        │   ├── .tidepool/logs/                                   │
│        │   ├── .claude/                                          │
│        │   │   ├── settings.local.json (hooks)                   │
│        │   │   └── context/bead.md                               │
│        │   ├── .env (merged environment)                         │
│        │   └── process-compose.yaml (generated)                  │
│        ├── bd-kg6-update-spawn-agents-paths/                     │
│        └── bd-hp4-update-process-compose/                        │
│                                                                  │
│  /tmp/tidepool-4hj/                                              │
│    ├── control.sock                                              │
│    └── tui.sock                                                  │
└──────────────────────────────────────────────────────────────────┘
```

## Request Schema

```json
{
  "bead_ids": ["4hj", "kg6", "hp4"],
  "backend": "claude"  // Optional: "claude" or "gemini", defaults to "claude"
}
```

**Field Details:**

- `bead_ids`: Array of short-form bead IDs (e.g., "wzi", "1b2")
  - Automatically prefixed with "tidepool-" if missing
  - Used for branch naming: `bd-<id>/<slug>`
  - Used for worktree directory: `bd-<id>-<slug>`

- `backend`: Backend CLI to use
  - `"claude"`: Launches `claude --debug --verbose`
  - `"gemini"`: Launches `gemini --debug`
  - Default: `"claude"`

## Response Schema

```json
{
  "worktrees": [
    ["4hj", "/path/to/worktrees/bd-4hj-fix-bug"],
    ["kg6", "/path/to/worktrees/bd-kg6-feature"]
  ],
  "tabs": [
    ["4hj", "4hj"],
    ["kg6", "kg6"]
  ],
  "failed": [
    ["hp4", "Bead is blocked: tidepool-hp4"]
  ]
}
```

**Field Details:**

- `worktrees`: Successfully created worktrees (shortId, path)
- `tabs`: Successfully launched Zellij tabs (shortId, tabId)
- `failed`: Failed operations (shortId, reason)

## How It Works

### 1. Environment Validation

Checks that we're running inside a Zellij session:

```haskell
mZellijSession <- checkZellijEnv
case mZellijSession of
  Nothing -> fail "Not running in Zellij session"
  Just _ -> proceed
```

**Error if not in Zellij:**
```json
{
  "worktrees": [],
  "tabs": [],
  "failed": [["*", "Not running in Zellij session"]]
}
```

### 2. Hangar Root Discovery

Finds the hangar root directory for shared binaries:

1. Check `HANGAR_ROOT` environment variable
2. If not set, walk up from repo root looking for `Hangar.toml`
3. Falls back to repo root if no hangar found

**Worktree Target Directory:**
- If hangar found: `<hangar>/worktrees/`
- If no hangar: `<repo>/.worktrees/tidepool/`

### 3. Per-Bead Processing

For each bead ID:

#### a. Fetch Bead Info
```haskell
mBead <- getBead fullId  -- "tidepool-4hj"
```

**Validation checks:**
- Bead ID contains no path separators (prevent traversal)
- Binary exists at `<hangar>/runtime/bin/tidepool-control-server`
- Bead exists in `.beads/beads.jsonl`
- Bead is not blocked (status ≠ blocked)

#### b. Create Git Worktree

```haskell
let branchName = "bd-4hj/fix-bug"
    targetPath = "/hangars/tidepool/worktrees/bd-4hj-fix-bug"
    spec = WorktreeSpec
      { wsBaseName = "bd-4hj"
      , wsFromBranch = Just "origin/main"
      , wsBranchName = Just branchName
      , wsPath = Just targetPath
      }
createWorktree spec
```

**Idempotency:** If worktree directory already exists, returns error instead of duplicating.

#### c. Bootstrap .tidepool/ Directory

Creates runtime directory structure:

**Socket Directory** (in `/tmp` to avoid path length limits):
```
/tmp/tidepool-4hj/
  ├── control.sock
  └── tui.sock
```

**Logs Directory** (in worktree):
```
<worktree>/.tidepool/logs/
  └── process-compose.log
```

**Why split?** Unix sockets have ~104 byte path limit on macOS. Worktree paths can exceed this, so we use short `/tmp` paths for sockets.

#### d. Generate process-compose.yaml

**Type-safe generation** from Haskell types (no templates):

```haskell
let binPath = "<hangar>/runtime/bin/tidepool-control-server"
    socketDir = "/tmp/tidepool-4hj"
    controlSocket = "/tmp/tidepool-4hj/control.sock"
    tuiSocket = "/tmp/tidepool-4hj/tui.sock"
    pcConfig = PC.generateSubagentConfig binPath socketDir controlSocket tuiSocket
```

**Generated YAML:**
```yaml
version: "0.5"
processes:
  control-server:
    command: |
      mkdir -p "/tmp/tidepool-4hj"
      rm -f "/tmp/tidepool-4hj/control.sock" "/tmp/tidepool-4hj/tui.sock"
      "<hangar>/runtime/bin/tidepool-control-server" --no-tui
    working_dir: "."
    environment:
      - TIDEPOOL_BIN_DIR=<hangar>/runtime/bin
      - TIDEPOOL_SOCKET_DIR=/tmp/tidepool-4hj
      - TIDEPOOL_CONTROL_SOCKET=/tmp/tidepool-4hj/control.sock
      - TIDEPOOL_TUI_SOCKET=/tmp/tidepool-4hj/tui.sock
    readiness_probe:
      exec:
        command: test -S "/tmp/tidepool-4hj/control.sock"
      initial_delay_seconds: 2
      period_seconds: 3
      failure_threshold: 10
    availability:
      restart: on_failure
      max_restarts: 5
    shutdown:
      command: rm -f "/tmp/tidepool-4hj/control.sock" "/tmp/tidepool-4hj/tui.sock"
      timeout_seconds: 5
```

**Key features:**
- `--no-tui` flag disables TUI sidebar (subagents don't need it)
- Socket health check for readiness
- Auto-restart on failure (max 5 times)
- Clean socket removal on shutdown

#### e. Write .env File

Merges root environment with subagent-specific overrides:

```haskell
-- 1. Capture current environment (Haskell source of truth)
rootEnvVars <- getEnvironment

-- 2. Define subagent overrides
let subagentVars =
      [ ("SUBAGENT_CMD", backendCmd)
      , ("HANGAR_ROOT", hangarRoot)
      , ("TIDEPOOL_BIN_DIR", binDir)
      , ("TIDEPOOL_SOCKET_DIR", socketDir)
      , ("TIDEPOOL_CONTROL_SOCKET", controlSocket)
      , ("TIDEPOOL_TUI_SOCKET", tuiSocket)
      ]

-- 3. Filter root vars (remove keys that subagent overrides)
let filteredRootVars = filter (\(k, _) -> k `notElem` subagentKeys) rootEnvVars

-- 4. Merge: subagent vars take precedence
let mergedVars = filteredRootVars ++ subagentVars

-- 5. Write to <worktree>/.env
```

**Why merge?** Ensures all secrets (ANTHROPIC_API_KEY, etc.) propagate to subagents while allowing subagent-specific socket paths.

**Backend command:**
- Claude: `claude --debug --verbose`
- Gemini: `gemini --debug`

#### f. Write Backend Configuration

**For Claude** (`.claude/settings.local.json`):
```json
{
  "hooks": {
    "SessionStart": [
      {
        "matcher": "startup",
        "hooks": [
          {
            "type": "command",
            "command": "<hangar>/runtime/bin/mantle-agent hook session-start"
          }
        ]
      },
      {
        "matcher": "resume",
        "hooks": [
          {
            "type": "command",
            "command": "<hangar>/runtime/bin/mantle-agent hook session-start"
          }
        ]
      }
    ]
  }
}
```

**For Gemini** (`.gemini/settings.json`):
```json
{
  "hooksConfig": {
    "enabled": true
  },
  "hooks": {
    "SessionStart": [
      {
        "matcher": "startup",
        "hooks": [
          {
            "name": "init-agent",
            "type": "command",
            "command": "<hangar>/runtime/bin/mantle-agent hook session-start"
          }
        ]
      },
      {
        "matcher": "resume",
        "hooks": [
          {
            "name": "resume-agent",
            "type": "command",
            "command": "<hangar>/runtime/bin/mantle-agent hook session-start"
          }
        ]
      }
    ]
  },
  "mcpServers": {
    "tidepool": {
      "command": "<hangar>/runtime/bin/mantle-agent",
      "args": ["mcp"],
      "env": {
        "TIDEPOOL_CONTROL_SOCKET": "/tmp/tidepool-4hj/control.sock"
      }
    }
  }
}
```

**Key differences:**
- Gemini combines hooks + MCP in one file
- Gemini uses absolute paths (no env var expansion)
- Gemini requires `"hooksConfig": {"enabled": true}`

Both configs are written to `.gitignore` automatically.

#### g. Write Bead Context

Creates `.claude/context/bead.md`:

```markdown
# Task: Fix TUI bidirectional socket deadlock on CloseUI

**ID:** tidepool-4hj
**Status:** in_progress
**Priority:** 2
**Branch:** bd-4hj/fix-tui-bidirectional-socket-deadlock

## Description

The TUI sidebar closes prematurely when CloseUI is called...

## Dependencies

- tidepool-abc: Container infrastructure (closed)
- tidepool-def: Control-server HTTP migration (closed)

## Workflow

1. Implement changes
2. Commit: [tidepool-4hj] <description>
3. Push: git push -u origin bd-4hj/fix-tui-bidirectional-socket-deadlock
4. File PR: Call the 'file_pr' tool (do NOT use gh cli manually)
```

This context is loaded automatically by Claude Code on session start.

#### h. Launch Zellij Tab

```haskell
let tabConfig = TabConfig
      { tcName = "4hj"
      , tcLayout = "<repo>/.zellij/worktree.kdl"
      , tcCwd = "<worktree>"
      , tcEnv = subagentVars
      }
newTab tabConfig
```

**Zellij Layout** (`.zellij/worktree.kdl`):

```kdl
tab name="subagent" {
    pane split_direction="vertical" {
        // LEFT: Main interaction pane (60%)
        pane size="60%" focus=true {
            command "bash"
            args "-c" "source .env && ./scripts/wait-for-socket.sh \"$TIDEPOOL_CONTROL_SOCKET\" 60 ControlServer && exec $SUBAGENT_CMD"
        }

        // RIGHT: Stacked infrastructure panes (40%)
        pane split_direction="horizontal" stacked=true {
            pane name="process-compose" {
                command "bash"
                args "-c" "source .env && mkdir -p .tidepool/logs && export PC_NO_SERVER=true PC_USE_UDS=true PC_SOCKET_PATH=.tidepool/sockets/process-compose.sock && exec process-compose up -L .tidepool/logs/process-compose.log"
            }
            pane name="logs" {
                command "tail"
                args "-F" ".tidepool/logs/process-compose.log"
            }
        }
    }
}
```

**Pane breakdown:**
- **Left pane (60%)**: Claude Code or Gemini CLI (main interaction)
- **Top-right pane**: process-compose dashboard
- **Bottom-right pane**: control-server logs

**Startup sequence:**
1. process-compose starts control-server
2. control-server creates sockets
3. wait-for-socket.sh polls for socket existence
4. Once socket ready, launches $SUBAGENT_CMD (claude/gemini)
5. Backend connects to control-server via MCP

### 4. Result Collection

After processing all beads:

```haskell
let (failed, succeeded) = partitionEithers results
    worktrees = [(sid, path) | (sid, path, _) <- succeeded]
    tabs = [(sid, tabId) | (sid, _, TabId tabId) <- succeeded]
```

Returns structured result with all successes and failures.

## Usage Examples

### Basic Usage (Claude)

```javascript
// Spawn 3 agents with Claude backend
spawn_agents({
  bead_ids: ["4hj", "kg6", "hp4"]
})

// Result:
{
  "worktrees": [
    ["4hj", "/Users/you/hangars/tidepool/worktrees/bd-4hj-fix-bug"],
    ["kg6", "/Users/you/hangars/tidepool/worktrees/bd-kg6-feature"],
    ["hp4", "/Users/you/hangars/tidepool/worktrees/bd-hp4-refactor"]
  ],
  "tabs": [
    ["4hj", "4hj"],
    ["kg6", "kg6"],
    ["hp4", "hp4"]
  ],
  "failed": []
}
```

### With Gemini Backend

```javascript
spawn_agents({
  bead_ids: ["xyz"],
  backend: "gemini"
})

// Creates worktree with .gemini/settings.json instead of .claude/
```

### Handling Failures

```javascript
spawn_agents({
  bead_ids: ["abc", "blocked-bead", "xyz"]
})

// Result:
{
  "worktrees": [
    ["abc", "/path/to/bd-abc-task"],
    ["xyz", "/path/to/bd-xyz-task"]
  ],
  "tabs": [
    ["abc", "abc"],
    ["xyz", "xyz"]
  ],
  "failed": [
    ["blocked-bead", "Bead is blocked: tidepool-blocked-bead"]
  ]
}
```

## Error Codes

| Code | Name | Example |
|------|------|---------|
| -32001 | NotFound | Bead not found in `.beads/beads.jsonl` |
| -32002 | InvalidInput | Invalid bead ID (contains path separators) |
| -32003 | ExternalFailure | Git worktree creation failed |
| -32004 | StateError | Bead is blocked, worktree already exists |
| -32005 | EnvironmentError | Not in Zellij session, binary missing |

## Troubleshooting

### "Not running in Zellij session"

**Symptom:**
```json
{"failed": [["*", "Not running in Zellij session"]]}
```

**Cause:** The `spawn_agents` tool requires running inside Zellij.

**Fix:**
```bash
# Start Zellij first
./start-augmented.sh

# Or manually
zellij

# Then call spawn_agents from Claude Code
```

### "Binary missing: /path/to/tidepool-control-server"

**Symptom:**
```json
{"failed": [["4hj", "Binary missing: /hangars/tidepool/runtime/bin/tidepool-control-server"]]}
```

**Cause:** Pre-built binaries don't exist in hangar.

**Fix:**
```bash
# Build binaries
cd ~/hangars/tidepool/runtime/tidepool
cabal build tidepool-control-server
cd ~/hangars/tidepool/runtime/tidepool/rust
cargo build --release -p mantle-agent -p tui-sidebar

# Copy to runtime/bin/
mkdir -p ~/hangars/tidepool/runtime/bin
cp dist-newstyle/build/.../tidepool-control-server ~/hangars/tidepool/runtime/bin/
cp rust/target/release/mantle-agent ~/hangars/tidepool/runtime/bin/
cp rust/target/release/tui-sidebar ~/hangars/tidepool/runtime/bin/
```

### "Worktree already exists"

**Symptom:**
```json
{"failed": [["4hj", "Worktree already exists at /path/to/bd-4hj-fix-bug. Use Zellij tabs or delete worktree first."]]}
```

**Cause:** Directory already exists from previous spawn.

**Fix:**
```bash
# Option 1: Switch to existing tab
zellij action go-to-tab-name 4hj

# Option 2: Delete worktree
git worktree remove worktrees/bd-4hj-fix-bug
git branch -d bd-4hj/fix-bug
```

### "Bead is blocked"

**Symptom:**
```json
{"failed": [["xyz", "Bead is blocked: tidepool-xyz"]]}
```

**Cause:** Bead has dependencies that aren't complete.

**Fix:**
```bash
# Check blockers
bd show tidepool-xyz

# Unblock by completing dependencies
bd close tidepool-abc  # Complete blocking bead
```

### Subagent Shows "MCP failed" on Startup

**Symptom:** In subagent Claude Code, `/mcp` shows "failed" status.

**Cause:** control-server not yet ready when Claude Code starts.

**Fix:**
```
# In Claude Code
/mcp
# Select "Reconnect" for tidepool server
```

The `wait-for-socket.sh` script should prevent this, but if Claude Code starts too quickly, manual reconnect works.

### Socket Path Too Long Error

**Symptom:**
```
Error: Unix socket path exceeds 104 bytes
```

**Cause:** Worktree path is too deep for socket.

**Fix:** This should not happen - sockets are in `/tmp/tidepool-<id>/` specifically to avoid this. If you see this error, check that `TIDEPOOL_CONTROL_SOCKET` is set correctly.

## Implementation Details

### File Locations

| Module | Path | Purpose |
|--------|------|---------|
| Graph definition | `ExoTools/SpawnAgents.hs:168-179` | MCPExport annotated graph |
| Core logic | `ExoTools/SpawnAgents.hs:194-244` | Main handler |
| Bead processing | `ExoTools/SpawnAgents.hs:247-362` | Per-bead worktree creation |
| Bootstrap | `ExoTools/SpawnAgents.hs:365-446` | .tidepool/ setup |
| Config generation | `Runtime/ProcessCompose.hs:87-121` | Type-safe YAML |
| Path utilities | `Runtime/Paths.hs` | Socket path construction |
| Zellij effect | `../../effects/zellij-interpreter/` | CLI wrapper |
| MCP registration | `Export.hs:54` | Tool discovery |
| MCP handler | `Handler/MCP.hs:193, 276-308` | Request dispatch |

### Effect Stack

```haskell
handleSpawnAgentsTool :: Logger -> Text -> Value -> IO ControlResponse
handleSpawnAgentsTool logger reqId args = do
  resultOrErr <- try $ runM
    $ runLog Debug
    $ runGeminiIO           -- Gemini CLI (for backend="gemini")
    $ runBDIO               -- Beads task tracking
    $ runGitIO              -- Git worktree operations
    $ runWorktreeIO         -- Worktree abstraction
    $ runFileSystemIO       -- File I/O
    $ runEnvIO              -- Environment variables
    $ runZellijIO           -- Zellij CLI commands
    $ spawnAgentsLogic args
```

### Type Safety Guarantees

1. **Process Compose Config:** Generated from Haskell types (`ProcessComposeConfig`, `ProcessDef`), serialized to YAML. No template files, no string interpolation.

2. **Socket Paths:** Computed via `Paths.hs` functions, not hardcoded strings. OS-specific path limits handled automatically.

3. **Environment Merging:** Root environment captured via `getEnvironment`, subagent vars take precedence. No variable shadowing.

4. **Backend Selection:** Validated at parse time (`"claude"` | `"gemini"`), enforced in handler dispatch.

## Related Documentation

- **[control-server/CLAUDE.md](../CLAUDE.md)** - MCP tool overview
- **[Runtime/ProcessCompose.hs](../Runtime/ProcessCompose.hs)** - Config generation
- **[Runtime/Paths.hs](../Runtime/Paths.hs)** - Path utilities
- **[../../effects/zellij-interpreter/CLAUDE.md](../../effects/zellij-interpreter/CLAUDE.md)** - Zellij effect
- **[Root CLAUDE.md](../../../../CLAUDE.md)** - Hangar structure

## Future Enhancements

- [ ] Support for remote worktrees (SSH)
- [ ] Automatic cleanup of stale worktrees on session end
- [ ] Parallel worktree creation (currently sequential)
- [ ] Health monitoring dashboard for all spawned agents
- [ ] Automatic bead priority-based ordering
