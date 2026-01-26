# Tidepool - Type-Safe LLM Agent Framework

A Haskell library for building LLM agents as typed state machines. Agents are IO-blind - they yield typed effects that runners interpret.

## Two Audiences

This doc serves two audiences:

1. **Using Tidepool** - Building agents in consuming repos (anemone, urchin)
2. **Developing Tidepool** - Working on the framework itself

### HUMAN STYLE OVERRIDES

ALWAYS update CLAUDE.md files when you make changes. Adding new documentation is critical, as is removing stale documentation.

Comments should always focus on what is or will be. Never leave comments about why you deleted something, its in the git history which is enough.

The repository should be kept clean of dead code, placeholders, and half-done heuristics.

Always prefer failure to an undocumented heuristic or fallback.

### DOCKER ENVIRONMENT

**CRITICAL:** Docker runs on a **remote NixOS host**, not locally on macOS. This means:
- Host bind mounts using macOS paths (e.g., `/Users/...`) will NOT work - those paths don't exist on NixOS
- The Docker socket is forwarded via SSH, but the filesystem is not shared
- Containers must use named volumes or clone repos internally
- Don't assume local filesystem access from containers

### AGGRESSIVE LOGGING

Silent failures are unacceptable. When code shells out to subprocesses, calls external services, or crosses process/container boundaries, **log aggressively**:

1. **Before the call**: Log what you're about to do (command, key parameters)
2. **After the call**: Log exit code, status, response size
3. **On error**: Log stderr, error messages, enough context to debug without reproducing
4. **On success**: Log the result summary (e.g., `button=submit`, `items=5`)

**Pattern for subprocess calls (Haskell):**
```haskell
logInfo logger $ "[Component] Starting operation: " <> summary
logDebug logger $ "[Component] Full params: " <> T.pack (show params)

(exitCode, stdout, stderr) <- readProcessWithExitCode cmd args ""

logInfo logger $ "[Component] Exit code: " <> T.pack (show exitCode)
unless (null stderr) $
  logDebug logger $ "[Component] stderr: " <> T.pack (take 500 stderr)

case exitCode of
  ExitFailure code -> do
    logError logger $ "[Component] FAILED: " <> T.pack stderr
    -- handle error
  ExitSuccess -> do
    logInfo logger $ "[Component] Success: " <> resultSummary
    -- handle success
```

**Pattern for subprocess calls (Rust):**
```rust
tracing::info!("Executing: {} {}", cmd, args.join(" "));
let status = Command::new(cmd).args(&args).status()?;
tracing::info!("{} returned: {:?}", cmd, status);
if !status.success() {
    tracing::error!("{} failed with status: {}", cmd, status);
}
```

The goal: when something fails, the logs should tell you exactly where and why without needing to add more logging and reproduce.


## Documentation Tree

Navigate to the right docs for your task:

```
CLAUDE.md  ← YOU ARE HERE (project overview)
├── haskell/CLAUDE.md  ← Haskell package organization
│   ├── control-server/CLAUDE.md ⭐ Claude Code++ hub (hooks/MCP/scout)
│   ├── dsl/core/CLAUDE.md      ← Graph DSL reference (START HERE for handlers)
│   ├── dsl/teaching/CLAUDE.md  ← LLM-level teaching for FunctionGemma training
│   ├── agents/                 ← Production agents
│   │   └── semantic-scout/CLAUDE.md ← MERGED into control-server (redirect)
│   ├── effects/CLAUDE.md       ← Effect interpreters
│   │   ├── llm-interpreter/     ← Anthropic/OpenAI API
│   │   ├── lsp-interpreter/     ← Language Server Protocol
│   │   └── ...
│   ├── runtime/CLAUDE.md       ← Execution backends
│   │   └── actor/CLAUDE.md     ← Actor model details
│   ├── protocol/CLAUDE.md      ← Wire formats
│   └── tools/CLAUDE.md         ← Dev tools (ghci-oracle, sleeptime, training-generator)
├── rust/CLAUDE.md             ← Claude Code++ (hook handler + MCP forwarding + TUI)
├── mantle-agent/CLAUDE.md  ← Hook handler (HTTP over Unix socket) (IMPLEMENTED)
├── docker-ctl/CLAUDE.md    ← Container lifecycle + remote exec (IMPLEMENTED)
├── mantle-hub/CLAUDE.md    ← Metrics hub (LEGACY, needs repurposing)
├── mantle-shared/CLAUDE.md ← Protocol types, Unix socket client
├── ssh-proxy/CLAUDE.md     ← DEPRECATED (replaced by docker-ctl)
│   ├── tui-sidebar/CLAUDE.md   ← TUI sidebar: ratatui rendering for graph UIs (IMPLEMENTED)
│   ├── tui-popup/CLAUDE.md     ← TUI popup: floating pane UI for user interaction
│   └── tui-spawner/CLAUDE.md   ← FIFO-based popup spawning for cross-container TUI
├── types-first-dev/CLAUDE.md   ← V3 TDD protocol project
├── deploy/CLAUDE.md            ← Cloudflare deployment
├── anemone/CLAUDE.md           ← Debug/diagnostic Solid.js UI (in-repo, not ~/tidepool-labs)
├── tools/CLAUDE.md             ← Root-level tools (micro-gastown, blast-radius)
└── typescript/
    ├── native-gui/CLAUDE.md    ← Solid.js frontend (alternative UI)
    └── telegram-bot/CLAUDE.md  ← Telegram integration
```

## When to Read Which CLAUDE.md

| I want to... | Read this |
|--------------|-----------|
| Work on Claude Code++ (hooks/MCP/scout) ⭐ | `haskell/control-server/CLAUDE.md` |
| Understand MCP tool architecture/tiers | `docs/architecture/ADR-003-MCP-Tool-Design-Patterns.md` |
| Understand hook/MCP forwarding (Rust side) | `rust/mantle-agent/CLAUDE.md` |
| Define a graph, handlers, annotations | `haskell/dsl/core/CLAUDE.md` |
| Work on LLM-level teaching infrastructure | `haskell/dsl/teaching/CLAUDE.md` |
| Add or modify an effect interpreter | `haskell/effects/CLAUDE.md` |
| Understand actor execution model | `haskell/runtime/actor/CLAUDE.md` |
| Work on semantic-scout code exploration | `haskell/control-server/CLAUDE.md` (merged from agents/semantic-scout) |
| Work with LSP (Language Server Protocol) | `haskell/effects/lsp-interpreter/CLAUDE.md` |
| Generate training data for FunctionGemma | `haskell/tools/training-generator/CLAUDE.md` |
| Work on types-first-dev V3 protocol | `types-first-dev/CLAUDE.md` |
| Deploy to Cloudflare Workers | `deploy/CLAUDE.md` |
| Work on the native server | `haskell/native-server/CLAUDE.md` |
| Work on debug UI frontend | `anemone/CLAUDE.md` or `typescript/native-gui/CLAUDE.md` |
| Work on TUI sidebar (terminal UI rendering) | `rust/tui-sidebar/CLAUDE.md` |
| Work on container spawning/exec | `rust/docker-ctl/CLAUDE.md` |
| Understand control protocol types | `rust/mantle-shared/CLAUDE.md` |

---

# Part 1: Using Tidepool

## Core Concepts

### Graphs
Agents are defined as typed state machine graphs:
- **Nodes**: LLM calls or pure logic
- **Edges**: Derived from type annotations (data flow) or explicit `Goto` (control flow)
- **Validation**: Compile-time via type families

```haskell
data MyAgent mode = MyAgent
  { entry    :: mode :- Entry Message
  , classify :: mode :- LLMNode :@ Input Message :@ Schema Intent
  , route    :: mode :- LogicNode :@ Input Intent :@ UsesEffects [Goto "handle" Message, Goto Exit Response]
  , handle   :: mode :- LLMNode :@ Input Message :@ Schema Response
  , exit     :: mode :- Exit Response
  }
```

See `haskell/dsl/core/CLAUDE.md` for the full Graph DSL reference.

### Effects
Agents yield effects; runners interpret them:
- `LLM` - Call language model with template + schema
- `State s` - Read/write agent state
- `Emit evt` - Emit events for logging/UI
- `RequestInput` - Get user input (choices, text)
- `Log` - Structured logging
- `Time` - Current time (IO-blind)
- `Memory s` - Persistent node-private state
- `Goto target` - Transition to another node

### Templates
Jinja templates with compile-time validation:
```haskell
myTemplate :: TypedTemplate MyContext SourcePos
myTemplate = $(typedTemplateFile ''MyContext "templates/my_prompt.jinja")
```

### Tools
LLM-invocable actions via `ToolDef` typeclass:
```haskell
instance ToolDef MyTool where
  toolName = "my_tool"
  toolDescription = "Does something useful"
  toolSchema = ... -- JSON Schema
  toolExecute = ... -- Handler
```

## Running Agents

### WASM/Cloudflare (Production - Frozen)
Compile to WASM, deploy to Cloudflare Durable Objects. TypeScript harness interprets effects.

```bash
cd deploy && pnpm dev  # Local CF worker
```

## Consuming Repos

Tidepool is a library. Agents live in separate repos:

### anemone (`~/tidepool-labs/anemone`)
Example consuming repo with working agents:
- Native and Cloudflare WIPs (both work e2e)
- Agent definitions using Graph DSL
- Templates and schemas

### urchin (`~/tidepool-labs/urchin`)
Context generation tooling for coding agents:
- `urchin prime` - Generate context from git/GitHub/LSP for agent bootstrap
- `urchin lsp` - LSP impact analysis for Haskell code

## Sleeptime

**Sleeptime** is the evolution pattern for agents:

1. Agents run and produce logs/traces
2. **Cron jobs in consuming repos** observe these runs
3. Cron jobs file issues and PRs to improve the agent
4. Changes: state fields, output schemas, templates, tools

The cron jobs live in the consuming repo (anemone, urchin), not in tidepool itself. Tidepool provides the infrastructure; consuming repos implement the evolution loop.

## Claude Code++ Integration

Human-driven Claude Code sessions augmented with Tidepool. **Not headless automation** - humans interact via TTY; we add superpowers.

### Architecture

```
┌────────────────────────────────────────────────────────────────┐
│ User TTY (Zellij 3-pane)                                       │
│  Pane 1: Claude Code  │  Pane 2: control-server  │ Pane 3: TUI │
└───────────┬──────────────────────────────────┬─────────────────┘
            │ Hooks/MCP                        │
            ▼                                  │
┌─────────────────────────────────────────┐    │
│ mantle-agent (Rust)                     │    │
│  • hook: CC hooks → Unix Socket         │    │
└───────────┬─────────────────────────────┘    │
            │ Unix Socket NDJSON               │
            │ .tidepool/sockets/control.sock   │
            ▼                                  │
┌─────────────────────────────────────────┐    │
│ control-server (Haskell)                │    │
│  • Long-lived LSP session (HLS)         │    │
│  • Hook Handler: Passthrough            │    │
│  • MCP Handler: 7 tools (auto-discovery)│    │
│  • TUI Handler: Listens for Sidebar     │◄───┘
└─────────────────────────────────────────┘    │
                                               │ Unix Socket NDJSON
                                               │ .tidepool/sockets/tui.sock
                                               │
┌──────────────────────────────────────────────┘
│
▼
┌─────────────────────────────────────────┐
│ tui-sidebar (Rust)                      │
│  • Connects to control-server           │
│  • Renders UISpec with ratatui          │
│  • Captures keyboard (Tab, Enter)       │
│  • Sends Interaction events             │
└─────────────────────────────────────────┘
```

### Key Components

| Component | Location | Purpose |
|-----------|----------|---------|
| **mantle-agent** | `rust/mantle-agent/` | Hook forwarding to control server (HTTP over Unix socket) |
| **control-server** | `haskell/control-server/` | Haskell server with LSP + MCP tools + TUI handler |
| **tui-sidebar** | `rust/tui-sidebar/` | Rust TUI: renders UISpec, captures Interaction |
| **Protocol types** | `rust/mantle-shared/protocol.rs` + `haskell/control-server/Protocol.hs` | Bidirectional message types (must match exactly) |

### Data Flow

**Hook Flow (PreToolUse):**
```
1. Claude Code wants to call Write tool
2. Generates hook JSON on stdin
3. mantle-agent hook pre-tool-use reads stdin
4. Forwards ControlMessage::HookEvent via Unix Socket
5. control-server receives, routes to handleHook
6. Returns HookResponse (allow/deny)
7. mantle-agent prints to stdout
8. Claude Code proceeds or blocks
```

**MCP Tool Flow:**
```
Tools: find_callers, show_fields, show_constructors, teach-graph, popup, spawn_agents, exo_status, file_pr, ...

1. User asks question requiring code intelligence or human decision
2. Claude plans to call MCP tool (e.g., teach-graph, popup)
3. Claude Code sends HTTP request to control-server (TCP port 7432)
4. control-server routes to appropriate handler
   - Tier 1 (LSP-only): find_callers, show_fields, show_constructors
   - Tier 2 (LLM-enhanced): teach-graph (LSP + Haiku selection)
   - Tier 3 (External): spawn_agents, exo_status, file_pr
   - Tier 4 (TUI-interactive): popup
5. Returns tool result (JSON) via HTTP response
6. Claude analyzes and responds to user
```

### Configuration

**Need help with Claude Code settings?** We have a Claude Code configuration specialist (preloaded with official documentation) available as an oracle. When you have questions about:
- Hook configuration syntax (SessionStart, PreToolUse, etc.)
- Settings file structure and scope (project vs local vs user)
- MCP server setup
- Environment variables and integration
- Debugging hook execution

Ask the specialist directly instead of guessing. They have authoritative knowledge about Claude Code internals, hook lifecycle, and best practices.

In `.claude/settings.local.json`:
```json
{
  "hooks": {
    "PreToolUse": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "mantle-agent hook pre-tool-use"
          }
        ]
      }
    ]
  }
}
```

**Note:** MCP server configuration uses `.mcp.json`. Claude Code connects directly to control-server via HTTP (TCP port 7432):
```json
{"mcpServers": {"tidepool": {"type": "http", "url": "http://localhost:7432/role/tl/mcp"}}}
```

### Running

**Docker Compose - Container Separation Architecture (Recommended)**

The Docker Compose setup uses a separated container architecture:

```
┌─────────────────────────────────────────────────────────────────┐
│ zellij container (human attaches here)                          │
│  • Minimal: Zellij + Docker CLI + curl                          │
│  • Panes: docker attach tl, docker attach pm, ...               │
└─────────────────────────────────────────────────────────────────┘
              │ docker attach
              ▼
┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐
│ tl (claude-agent)│  │ pm (claude-agent)│  │ subagents...     │
│ ROLE=tl          │  │ ROLE=pm          │  │ (dynamic)        │
└────────┬─────────┘  └────────┬─────────┘  └────────┬─────────┘
         │ TCP :7432           │                     │
         ▼                     ▼                     ▼
┌─────────────────────────────────────────────────────────────────┐
│ control-server container                                        │
│  • MCP server (TCP 7432)                                        │
│  • Calls docker-ctl (Rust binary) for spawn + exec              │
│  • Creates Zellij tabs (shared socket)                          │
└─────────────────────────────────────────────────────────────────┘
```

**Quick Start:**
```bash
./ide              # Start containers + connect to Zellij (idempotent)
./refresh-ide      # Rebuild + force-recreate + connect (after Dockerfile changes)

# Detach with Ctrl+p, Ctrl+q
```

**Manual (if scripts don't work):**
```bash
docker compose up -d
docker attach tidepool-zellij
```

**Services:**
| Container | Purpose |
|-----------|---------|
| `tidepool-zellij` | Minimal Zellij multiplexer (human attaches here) |
| `tidepool-control-server` | Haskell MCP server (TCP 7432) |
| `tidepool-tl` | Tech Lead agent (coding) |
| `tidepool-pm` | Project Manager agent (planning) |
| `docker-ctl` | Container lifecycle CLI (inside control-server) |

**Features:**
- ✅ Named agent containers (TL + PM always running)
- ✅ TCP MCP transport (no Unix socket complexity)
- ✅ `docker-ctl exec` for remote command execution
- ✅ Zellij panes connect via `docker attach`
- ✅ Clean separation of concerns
- ✅ Dynamic subagent spawning via docker-ctl

**Testing MCP connection:**
```
# In TL or PM pane
/mcp
Expected: Shows "tidepool" server connected

/tools
Expected: Lists MCP tools
```

**Testing docker-ctl exec:**
```bash
docker exec tidepool-control-server docker-ctl exec tidepool-tl -- echo hello
```

**Rollback to Legacy Orchestrator:**
```bash
# Use the legacy profile
docker compose --profile legacy up orchestrator
docker attach tidepool-orchestrator
```

**Troubleshooting:**
- **MCP shows "failed" on startup**: control-server still initializing. Use `/mcp` → `Reconnect` after 10 seconds.
- **Agent not responding**: Check `docker logs tidepool-tl` or `docker logs tidepool-pm`.
- **Authentication errors**: Verify credentials in `tidepool-claude-auth` volume.

**Hybrid Tidepool Architecture (process-compose + Zellij - Local Development)**

**Recommended: start-augmented.sh**
```bash
./start-augmented.sh
```

This launches the Hybrid Tidepool setup as a **TUI IDE** wrapping Claude Code:
- **process-compose**: Orchestrates services (control-server, tui-sidebar)
- **Zellij**: 3-pane TUI IDE layout
  - Pane 1 (left): **Claude Code** (auto-launches, main interface)
  - Pane 2 (top-right): process-compose dashboard (infrastructure monitoring)
  - Pane 3 (bottom-right): control-server logs (backend telemetry)

Claude Code starts automatically in the tidepool directory. MCP tools connect directly to control-server via HTTP MCP transport.

**Note:** If MCP shows "failed" on startup (control-server not yet ready), use `/mcp` → `Reconnect` once services are healthy.

**Orchestration features (process-compose):**
- Socket existence checks (test -S) for zero-overhead health probes
- Dependency DAG: tui-sidebar waits for control-server health
- Automatic restart on failure with exponential backoff
- Centralized logging to `.tidepool/logs/`
- Service readiness probes

**Manual process-compose (without Zellij):**
```bash
# Create runtime directories
mkdir -p .tidepool/{sockets,logs}

# Start all services
~/.local/bin/process-compose up --tui

# In another terminal: Start Claude Code
cd /path/to/project
claude-code
```

**Manual startup (3 terminals - NOT recommended):**
```bash
# Terminal 1: Start control server
GEMMA_ENDPOINT=http://localhost:11434 cabal run tidepool-control-server

# Terminal 2: Wait for TUI socket, then start tui-sidebar
./scripts/wait-for-socket.sh .tidepool/sockets/tui.sock 60 TUIServer
cargo run -p tui-sidebar -- --socket .tidepool/sockets/tui.sock

# Terminal 3: Start Claude Code
cd /path/to/project
claude-code
```

**Note:** Use `start-augmented.sh` for reliable startup. Manual methods require careful sequencing.

### Orchestration Internals

Understanding the runtime stack for debugging and extension.

#### Troubleshooting

**`start-augmented.sh` Hangs on Startup**
- **Symptom**: The script hangs indefinitely at "Starting Hybrid Tidepool...".
- **Cause**: A stale `process-compose` session (often headless) is holding the socket and refusing to terminate.
- **Fix**: The script now includes robust cleanup logic (timeout + force kill). If it still hangs, manually run:
  ```bash
  pkill -9 process-compose
  rm -f .tidepool/sockets/process-compose.sock
  ```

**`Killed: 9` on macOS (Apple Silicon)**
- **Symptom**: `control-server` or `mantle-agent` exits immediately with `Killed: 9` (SIGKILL).
- **Cause**: The binaries in `../runtime/bin` are unsigned or have invalid signatures. macOS arm64 requires all native executables to be signed.
- **Fix**: Ad-hoc sign the binaries:
  ```bash
  codesign -s - --force ../runtime/bin/tidepool-control-server
  codesign -s - --force ../runtime/bin/mantle-agent
  codesign -s - --force ../runtime/bin/tui-sidebar
  ```

**Subagent "Log File Not Found"**
- **Symptom**: Subagent Zellij tabs show a blank log pane or "No such file" error.
- **Cause**: Mismatch between Zellij layout expecting `pc.log` and process-compose writing `process-compose.log`.
- **Fix**: The hangar-root `.zellij/worktree.kdl` must:
  1. Create directories: `mkdir -p .tidepool/logs .tidepool/sockets`
  2. Force log filename: `process-compose up -L .tidepool/logs/process-compose.log`
  3. Use robust tail: `tail -F .tidepool/logs/process-compose.log` (capital F waits for file creation)
- **Note**: Fixed in hangar root as of 2026-01-21. New spawned subagents should work correctly.

#### Socket Lifecycle

Sockets are managed to ensure clean transitions between sessions and prevent stale connections:

1. **Bootstrap (`start-augmented.sh`)**: Canonical cleanup occurs here. It detects stale `process-compose` sessions via UDS and shuts them down, then deletes all remaining `.sock` files in `.tidepool/sockets/`.
2. **Runtime**: Services like `control-server` create their own sockets upon startup.
3. **Shutdown (`process-compose.yaml`)**: The `shutdown` command for `control-server` removes its specific sockets. This is a best-effort cleanup for graceful shutdown; the bootstrap cleanup is the source of truth for clearing stale state.

#### Port Allocation

| Port | Service | Protocol | Purpose |
|------|---------|----------|---------|
| (none) | process-compose | UDS | API (stale session detection) |

**Sockets (Environment Driven):**
- `$TIDEPOOL_CONTROL_SOCKET` (default: `.tidepool/sockets/control.sock`): Main protocol (mantle-agent connects)
- `$TIDEPOOL_TUI_SOCKET` (default: `.tidepool/sockets/tui.sock`): TUI sidebar (control-server listens, tui-sidebar connects)
- `.tidepool/sockets/process-compose.sock`: process-compose API (eliminates port 8080 conflicts)

**Docker Volumes (Cross-Container):**
- `tidepool-sockets`: Shared volume for control.sock and tui.sock
- `tidepool-zellij`: Shared XDG_RUNTIME_DIR (`/run/user/1000`) for cross-container Zellij access. Enables control-server to create Zellij tabs in orchestrator's session via `zellij --session orchestrator action new-tab`.

Canonical values are defined in `start-augmented.sh` and can be overridden in `.env`.

#### Config Files

| File | Format | Purpose |
|------|--------|---------|
| `process-compose.yaml` | YAML | Service definitions, health probes, dependencies |
| `.zellij/tidepool.kdl` | KDL | 3-pane TUI layout |
| `.zellij/config.kdl` | KDL | Zellij behavior (force_close, mouse) |
| `scripts/tidepool-runner.sh` | Bash | Trap handlers for cleanup |
| `.env` | Shell | Environment (ANTHROPIC_API_KEY required) |

#### Lifecycle Scripts

**`start-augmented.sh`** - Entry point:
1. Validates `.env` contains `ANTHROPIC_API_KEY`
2. Creates `.tidepool/{sockets,logs}` directories
3. Checks process-compose installed
4. Detects/cleans stale sessions via Unix socket
5. Launches Zellij with layout

**`scripts/tidepool-runner.sh`** - Cleanup wrapper:
```bash
trap cleanup EXIT SIGINT SIGTERM SIGHUP

cleanup() {
    # Primary: API shutdown (respects dependency order)
    process-compose down --ordered-shutdown
    # Fallback: SIGTERM if API unreachable
    kill -TERM "$PC_PID"
}
```

#### Critical: `on_force_close "quit"`

In `.zellij/config.kdl`:
```kdl
on_force_close "quit"
```

**Why this matters:** Without this, Zellij detaches instead of exiting when closed. Detaching means:
- Trap handlers in `tidepool-runner.sh` never fire
- Services continue running in background (orphaned)
- Next `start-augmented.sh` detects stale session

With `"quit"`, Zellij actually exits → triggers EXIT trap → orderly shutdown.

#### Shutdown Flow

```
Zellij quit (Ctrl+P → q)
    ↓
on_force_close "quit" (doesn't detach)
    ↓
tidepool-runner.sh EXIT trap fires
    ↓
process-compose down --ordered-shutdown
    ├─ tui-sidebar stops (no dependents)
    └─ control-server stops (tui-sidebar done)
    ↓
All services terminated
```

#### Health Probes

**control-server** (Unix Socket):
```yaml
readiness_probe:
  exec:
    command: "test -S $TIDEPOOL_CONTROL_SOCKET"
  initial_delay_seconds: 2
  period_seconds: 3
  failure_threshold: 10
```

**tui-sidebar** (Unix Socket):
```yaml
readiness_probe:
  exec:
    command: "test -S $TIDEPOOL_TUI_SOCKET"
```

#### Dependency DAG

```
tui-sidebar ──depends_on──→ control-server (condition: process_healthy)
```

tui-sidebar blocks until control-server's HTTP health probe succeeds.

#### Binary Dependencies

The orchestration stack relies on pre-built binaries for Haskell and Rust components. `start-augmented.sh` attempts to build these if missing or stale, but manual builds may be required for troubleshooting.

**Required Binaries & Paths:**
- `tidepool-control-server`: `dist-newstyle/build/.../tidepool-control-server`
- `mantle-agent`: `rust/target/debug/mantle-agent`
- `tui-sidebar`: `rust/target/debug/tui-sidebar`

**When are they built?**
`start-augmented.sh` runs `cabal build tidepool-control-server` and `cargo build -p mantle-agent -p tui-sidebar` on startup.

**Recovery:**
If binaries are missing or the "command not found" error occurs within `process-compose`:
1. Run `cabal build tidepool-control-server` manually.
2. Run `cargo build -p mantle-agent -p tui-sidebar` manually.
3. Restart `./start-augmented.sh`.

#### process-compose Setup

`process-compose` is the primary orchestrator for the Tidepool development environment.

**Installation:**
- **Nix (Recommended):** Included in `shell.nix` / `flake.nix`.
- **Homebrew (macOS):** `brew install F1bonacc1/tap/process-compose`
- **Manual:** Download from [GitHub Releases](https://github.com/F1bonacc1/process-compose/releases).

**PATH Requirement:**
Ensure `process-compose` is in your `PATH`. The scripts do not use hardcoded paths to the orchestrator to ensure compatibility across different installation methods.

**Troubleshooting:**
- **"command not found: process-compose"**: Verify installation and that `$(go env GOPATH)/bin` or your brew/nix bin directory is in your `PATH`.
- **Stale session**: `process-compose` uses a Unix socket at `.tidepool/sockets/process-compose.sock` for its API, eliminating port 8080 conflicts. `start-augmented.sh` will attempt to detect and kill stale sessions via this socket.

### Status

- ✅ Hook forwarding (passthrough)
- ✅ MCP server + 20+ tools via auto-discovery (popup, spawn_agents, exo_status, file_pr, pm_status, mailbox tools, GitHub tools, etc.)
- ✅ LSP integration (HLS via lsp-test)
- ✅ FunctionGemma scoring (HTTP interpreter via Ollama)
- ✅ Automatic tool registration via MCPExport annotation + reifyMCPTools
- ✅ Hybrid orchestration (process-compose + Zellij)
- ✅ Unix socket health checks for robust readiness checks
- ✅ Declarative service dependencies and restart policies
- ✅ MCP direct execution via .mcp.json (mantle-agent spawned per-call by Claude)
- ✅ Unix socket communication for all local components
- 🔄 Training data generation (types ready, CLI pending)
- ❌ Daemon mode (not implemented, uses per-call connection)
- ❌ Metrics hub (mantle-hub needs repurposing)
- ❌ Real hook logic (currently allow-all passthrough)

### Stopping

Zellij session exit automatically triggers cleanup via trap handler.

**Normal exit:**
1. Press `Ctrl+P` → `q` to quit Zellij
2. Trap handler calls `process-compose down --ordered-shutdown`
3. All services stop gracefully in reverse dependency order
4. Verify: `pgrep -f control-server` returns nothing

**Emergency cleanup (if session crashes):**
```bash
~/.local/bin/process-compose down --ordered-shutdown
# Verify all stopped
pgrep -f "control-server|tui-sidebar|process-compose"
```

**Stale session prevention:**
Running `./start-augmented.sh` automatically detects and cleans up any existing sessions before launching.

### See Also

- **[haskell/control-server/CLAUDE.md](haskell/control-server/CLAUDE.md)** - Complete data flow + implementation
- **[rust/CLAUDE.md](rust/CLAUDE.md)** - Rust workspace overview
- **[rust/mantle-agent/CLAUDE.md](rust/mantle-agent/CLAUDE.md)** - Hook/MCP implementation
- **[haskell/agents/semantic-scout/CLAUDE.md](haskell/agents/semantic-scout/CLAUDE.md)** - Scout exploration algorithm

---

# Part 2: Developing Tidepool

## Package Inventory

All Haskell packages now live under `haskell/`. See `haskell/CLAUDE.md` for full details.

### Core (`haskell/dsl/`, `haskell/runtime/`)
| Package | Purpose |
|---------|---------|
| `haskell/dsl/core` | Graph DSL, effects, templates, validation |
| `haskell/dsl/teaching` | LLM-level teaching infrastructure for FunctionGemma training |
| `haskell/runtime/actor` | Actor runtime with graph-to-actor execution |
| `haskell/runtime/parallel` | Parallel fan-out/fan-in execution with ki |
| `haskell/runtime/wasm` | WASM deployment scaffolding |
| `haskell/platform` | Platform abstraction layer (deprecated) |

### Agents (`haskell/agents/`)
| Package | Purpose |
|---------|---------|
| `haskell/agents/semantic-scout` | Code exploration MCP tool using LSP |

### Effect Interpreters (`haskell/effects/`)
| Package | Purpose |
|---------|---------|
| `haskell/native-server` | Servant + WebSocket server (facade) |
| `haskell/effects/llm-interpreter` | Anthropic/OpenAI API calls |
| `haskell/effects/habitica-interpreter` | Habitica API |
| `haskell/effects/ui-interpreter` | WebSocket ↔ UI bridging |
| `haskell/effects/observability-interpreter` | OpenTelemetry traces to Grafana |
| `haskell/effects/lsp-interpreter` | LSP via lsp-client |
| `haskell/effects/ghci-interpreter` | GHCi Oracle thin client |
| `haskell/effects/github-interpreter` | GitHub API integration |
| `haskell/effects/worktree-interpreter` | Git worktree management |
| `haskell/effects/cabal-interpreter` | Cabal build operations |
| `haskell/effects/devlog-interpreter` | Devlog effect interpreter |

### Integrations (`haskell/effects/`, `haskell/protocol/`)
| Package | Purpose |
|---------|---------|
| `haskell/effects/habitica` | Habitica effect types (standalone) |
| `haskell/effects/telegram` | Telegram effect types (disabled) |
| `haskell/protocol/wire-types` | Native protocol types |
| `haskell/protocol/generated-ts` | TypeScript type generation |

### Tools (`haskell/tools/`)
| Package | Purpose |
|---------|---------|
| `haskell/tools/ghci-oracle` | Persistent GHCi session server |
| `haskell/tools/sleeptime` | Log analysis for agent evolution |
| `haskell/tools/training-generator` | Training data types for FunctionGemma |
| `tools/micro-gastown` | Experimental tooling (non-Haskell) |

### TypeScript Packages (`typescript/`)
| Package | Purpose |
|---------|---------|
| `typescript/telegram-bot` | Telegram bot implementation |
| `typescript/native-gui` | Solid.js frontend for native server |

### Deployment
| Directory | Purpose |
|-----------|---------|
| `deploy/` | Cloudflare Worker Durable Object harness |

## Where Things Go

| Thing | Location |
|-------|----------|
| New effect type | `haskell/dsl/core/src/Tidepool/Effect/Types.hs` |
| New integration | `haskell/dsl/core/src/Tidepool/Effects/` (plural) |
| New graph annotation | `haskell/dsl/core/src/Tidepool/Graph/Types.hs` |
| New interpreter | `haskell/effects/<name>-interpreter/` |
| New MCP tool/agent | `haskell/agents/<name>/` |
| TypeScript frontend/bot | `typescript/<name>/` |
| Agents (consuming repos) | Separate repo (anemone, urchin, etc.) |

### Naming Conventions
- **Effect** (singular) = core infrastructure (`Tidepool.Effect.*`)
- **Effects** (plural) = integrations/contrib (`Tidepool.Effects.*`)
- **Interpreter** = effect implementation (replaces "executor" terminology)

## Task Tracking (GitHub)

Task tracking via GitHub Issues.

### Workflow

1.  **Branching**: Use the `gh-{number}/{description}` naming convention for all task-related branches.
2.  **Development**: Implement changes incrementally.
3.  **Commits**: Reference issue number in commit messages (e.g. `[#123] ...`).
4.  **Closing**: Issues are closed via PR merges ("Closes #123").

## Building & Testing

```bash
cabal build all            # Build everything
just pre-commit            # Run all checks
cabal test all             # Run tests
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Agent Turn Loop                         │
│                                                              │
│  1. Build context (State → TemplateContext)                  │
│  2. Render template (Jinja → prompt)                         │
│  3. Call LLM (prompt + schema + tools → result)              │
│  4. Apply structured output (result → State')                │
│  5. Handle transitions (Goto → next node)                    │
└─────────────────────────────────────────────────────────────┘
```

### Key Design Decisions

1. **freer-simple for effects** - Reified continuations for WASM yield/resume
2. **Typed Jinja templates** - Compile-time validation via ginger
3. **OneOf sum type** - Fully typed dispatch without Dynamic
4. **IO-blind agents** - All IO in runners, enables WASM + deterministic testing


### Code Smells: Data Flow Dead-Ends

**The `_` prefix is a huge signal.** When you see `_someField` in a pattern match, it means data is being captured but ignored. This is almost always a data flow dead-end that needs fixing.

```haskell
-- BAD: Data captured but ignored
ImplRequestRetry diagnosis _strategyFrom _strategyTo _failingTests -> do
  let retryInput = originalInput { iiAttemptCount = count + 1 }
  pure $ gotoChoice @"v3Impl" retryInput

-- GOOD: Data flows to next node
ImplRequestRetry diagnosis strategyFrom strategyTo failingTests -> do
  let critiques = buildCritiquesFrom diagnosis strategyFrom strategyTo failingTests
  let retryInput = originalInput
        { iiAttemptCount = count + 1
        , iiCritiqueList = Just critiques  -- Data flows forward
        }
  pure $ gotoChoice @"v3Impl" retryInput
```

**When reviewing handlers, grep for `_` prefixes in pattern matches.** Each one is a potential bug where:
- Exit types capture useful info that never reaches the next node
- Template context is built but never rendered
- Memory fields are written but never read

The fix is usually: thread the data forward (via input fields, memory, or context) so downstream nodes/templates can use it.


## LSP Integration

The `tidepool-lsp-interpreter` provides LSP for code intelligence:

```haskell
import Tidepool.Effects.LSP
import Tidepool.LSP.Interpreter (withLSPSession, runLSP)

withLSPSession "/path/to/project" $ \session -> do
  info <- runLSP session $ hover doc pos
  ...
```

## References

- [haskell/dsl/core/CLAUDE.md](haskell/dsl/core/CLAUDE.md) - Graph DSL reference
- [haskell/native-server/CLAUDE.md](haskell/native-server/CLAUDE.md) - Native server docs
- [deploy/CLAUDE.md](deploy/CLAUDE.md) - Cloudflare deployment
- [freer-simple](https://hackage.haskell.org/package/freer-simple) - Effect system
- [Anthropic tool use](https://docs.anthropic.com/en/docs/tool-use)