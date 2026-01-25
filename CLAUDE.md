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
│   ├── mantle-agent/CLAUDE.md  ← Hook handler (HTTP over Unix socket) (IMPLEMENTED)
│   ├── mantle-hub/CLAUDE.md    ← Metrics hub (LEGACY, needs repurposing)
│   ├── mantle-shared/CLAUDE.md ← Protocol types, Unix socket client
│   └── tui-sidebar/CLAUDE.md   ← TUI sidebar: ratatui rendering for graph UIs (IMPLEMENTED)
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
  , route    :: mode :- LogicNode :@ Input Intent :@ UsesEffects '[Goto "handle" Message, Goto Exit Response]
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
- `urchin prime` - Generate context from git/bd/LSP for agent bootstrap
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

**MCP Tool Flow (7 available tools):**
```
Tools: find_callers, show_fields, show_constructors, teach-graph, confirm_action, select_option, request_guidance

1. User asks question requiring code intelligence or human decision
2. Claude plans to call MCP tool (e.g., teach-graph, confirm_action)
3. Claude Code sends HTTP request to control-server (TCP port 7432)
4. control-server routes to appropriate handler
   - Tier 1 (LSP-only): find_callers, show_fields, show_constructors
   - Tier 2 (LLM-enhanced): teach-graph (LSP + Haiku selection)
   - Tier 4 (TUI-interactive): confirm_action, select_option, request_guidance
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

**Docker Compose Orchestration (Recommended for Linux)**

The Docker Compose setup provides a containerized environment with browser-based Zellij access:

```bash
# Start the orchestrator container
docker compose up -d orchestrator

# Generate a login token
docker exec tidepool-orchestrator gosu user zellij web --create-token

# Open browser to: https://localhost:8080/orchestrator
# (or https://<hostname>:8080/orchestrator for remote servers)
```

**Features:**
- ✅ Browser-based Zellij TUI (no terminal required)
- ✅ 3-pane layout: control-server logs | Claude Code CLI | tail logs
- ✅ Claude Code with MCP tools + hooks (full integration)
- ✅ control-server with MCP tools via Unix socket (`/sockets/control.sock`)
- ✅ Cross-container Zellij tab creation (spawn_agents creates tabs in orchestrator)
- ✅ Clean shutdown: `docker compose down` (no hangs)
- ✅ Persistent sessions via session serialization

**Remote access:**
```bash
# Control remote Docker via SSH
export DOCKER_HOST=ssh://user@hostname
docker compose up -d
docker exec tidepool-orchestrator gosu user zellij web --create-token

# Open: https://hostname:8080/orchestrator
```

#### Browser-based Claude Code Session

The orchestrator's middle pane runs Claude Code CLI with full MCP and hook integration:

**What you get:**
- Claude Code session accessible via web browser (no local terminal needed)
- MCP tools from control-server (`find_callers`, `show_fields`, `show_constructors`, `teach-graph`, `confirm_action`, `select_option`, `request_guidance`)
- PreToolUse hooks via mantle-agent (tool call interception)
- Working directory: `/worktrees` (project root, bind-mounted from host)
- **SSH Access**: Agent containers accept SSH connections from the orchestrator for remote command execution.

**Agent SSH Setup:**
- **Pre-build Requirement**: Before building Docker images, run `./scripts/docker-prebuild.sh` (or `just docker-prebuild`) to generate the necessary SSH keypair.
- **Security**: SSH is restricted to the internal Docker network. Root login is enabled via Ed25519 public key authentication only (no passwords).
- **Testing**: Use `just test-ssh` to verify connectivity between the orchestrator and an agent container.

**Basic workflow:**
1. Run pre-build: `./scripts/docker-prebuild.sh`
2. Start orchestrator: `docker compose up -d orchestrator`
3. Generate token: `docker exec tidepool-orchestrator zellij web --create-token`
4. Open browser: `https://nixos:8080/orchestrator` (or `https://localhost:8080/orchestrator`)
5. Middle pane shows Claude Code prompt

**Testing MCP connection:**
```
Type in Claude Code pane: /mcp
Expected: Shows "tidepool" server status
If connected: /tools
Expected: Lists 7 MCP tools
```

**Testing hooks:**
```
Ask Claude: "Read the CLAUDE.md file"
Expected: PreToolUse hook fires (visible in control-server logs, left pane)
```

**Configuration (auto-generated at startup):**
- `/root/.claude.json` - Skip onboarding
- `/root/.mcp.json` - MCP server pointing to `/sockets/control.sock`
- `/root/.claude/settings.json` - Hook configuration for mantle-agent

**Troubleshooting:**
- **MCP shows "failed" on startup**: control-server still initializing. Use `/mcp` → `Reconnect` after 10 seconds.
- **"Command not found: claude-code"**: Check build logs for Claude CLI installation errors.
- **Authentication errors**: Verify `~/.claude/.credentials.json` exists on host and is mounted.
- **Working directory wrong**: Claude Code should start in `/worktrees`. Check Zellij layout args.
- **Container restart loop**: Fixed by using `zellij web --daemonize` with `tail -f /dev/null` to keep PID 1 alive. Requires both `tty: true` and `stdin_open: true` in docker-compose.yml.

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
- ✅ MCP server + 7 tools via auto-discovery (find_callers, show_fields, show_constructors, teach-graph, confirm_action, select_option, request_guidance)
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
| `haskell/effects/bd-interpreter` | Beads integration + urchin CLI |
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

## Task Tracking (Beads)

Git-native task tracking via BD. Database at `.beads/` (gitignored, shared across worktrees).

### Workflow

1.  **Branching**: Use the `bd-{id}/{description}` naming convention for all task-related branches.
2.  **Bootstrap**: Run `./scripts/bead-context` after checkout to load task details into the agent/IDE context.
3.  **In Progress**: Mark task `in_progress` when starting work.
4.  **Commits**: Use `[tidepool-{id}]` prefix in commit messages.
5.  **Closing**: Mark `closed` after PR is merged.
6.  **Sync**: All worktrees share the same database; use `bd sync` if needed.

### Basic Commands

```bash
bd list --all              # List tasks
bd create -t task "..."    # Create task
bd update ID -s in_progress # Update status
bd show ID                 # View details
```

### Templates and Validation
Task creation uses templates located in `.bd/templates/`.
- **task**: Requires `## Acceptance Criteria`
- **bug**: Requires `## Steps to Reproduce` and `## Acceptance Criteria`
- **feature**: Requires `## Acceptance Criteria`
- **epic**: Requires `## Success Criteria`

When creating a new issue, use the `--validate` flag to ensure required sections are present:
```bash
bd create -t task "Implement X" --description "$(cat .bd/templates/task.md)" --validate
```
Or ensure your description manually includes the required headers.

### Landing the Plane

When ending a session:

1. File issues for remaining work (`bd create`)
2. Run quality gates (`just pre-commit`)
3. Update issue status (`bd close <id>`)
4. Push: `git pull --rebase && bd sync && git push`
5. Verify: `git status` shows "up to date"

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

### Using bv as an AI sidecar

bv is a graph-aware triage engine for Beads projects (.beads/beads.jsonl). Instead of parsing JSONL or hallucinating graph traversal, use robot flags for deterministic, dependency-aware outputs with precomputed metrics (PageRank, betweenness, critical path, cycles, HITS, eigenvector, k-core).

**Scope boundary:** bv handles *what to work on* (triage, priority, planning). For agent-to-agent coordination (messaging, work claiming, file reservations), use [MCP Agent Mail](https://github.com/Dicklesworthstone/mcp_agent_mail).

**⚠️ CRITICAL: Use ONLY `--robot-*` flags. Bare `bv` launches an interactive TUI that blocks your session.**

#### The Workflow: Start With Triage

**`bv --robot-triage` is your single entry point.** It returns everything you need in one call:
- `quick_ref`: at-a-glance counts + top 3 picks
- `recommendations`: ranked actionable items with scores, reasons, unblock info
- `quick_wins`: low-effort high-impact items
- `blockers_to_clear`: items that unblock the most downstream work
- `project_health`: status/type/priority distributions, graph metrics
- `commands`: copy-paste shell commands for next steps

bv --robot-triage        # THE MEGA-COMMAND: start here
bv --robot-next          # Minimal: just the single top pick + claim command

#### Other Commands

**Planning:**
| Command | Returns |
|---------|---------|
| `--robot-plan` | Parallel execution tracks with `unblocks` lists |
| `--robot-priority` | Priority misalignment detection with confidence |

**Graph Analysis:**
| Command | Returns |
|---------|---------|
| `--robot-insights` | Full metrics: PageRank, betweenness, HITS (hubs/authorities), eigenvector, critical path, cycles, k-core, articulation points, slack |
| `--robot-label-health` | Per-label health: `health_level` (healthy\|warning\|critical), `velocity_score`, `staleness`, `blocked_count` |
| `--robot-label-flow` | Cross-label dependency: `flow_matrix`, `dependencies`, `bottleneck_labels` |
| `--robot-label-attention [--attention-limit=N]` | Attention-ranked labels by: (pagerank × staleness × block_impact) / velocity |

**History & Change Tracking:**
| Command | Returns |
|---------|---------|
| `--robot-history` | Bead-to-commit correlations: `stats`, `histories` (per-bead events/commits/milestones), `commit_index` |
| `--robot-diff --diff-since <ref>` | Changes since ref: new/closed/modified issues, cycles introduced/resolved |

**Other Commands:**
| Command | Returns |
|---------|---------|
| `--robot-burndown <sprint>` | Sprint burndown, scope changes, at-risk items |
| `--robot-forecast <id\|all>` | ETA predictions with dependency-aware scheduling |
| `--robot-alerts` | Stale issues, blocking cascades, priority mismatches |
| `--robot-suggest` | Hygiene: duplicates, missing deps, label suggestions, cycle breaks |
| `--robot-graph [--graph-format=json\|dot\|mermaid]` | Dependency graph export |
| `--export-graph <file.html>` | Self-contained interactive HTML visualization |

#### Scoping & Filtering

bv --robot-plan --label backend              # Scope to label's subgraph
bv --robot-insights --as-of HEAD~30          # Historical point-in-time
bv --recipe actionable --robot-plan          # Pre-filter: ready to work (no blockers)
bv --recipe high-impact --robot-triage       # Pre-filter: top PageRank scores
bv --robot-triage --robot-triage-by-track    # Group by parallel work streams
bv --robot-triage --robot-triage-by-label    # Group by domain

#### Understanding Robot Output

**All robot JSON includes:**
- `data_hash` — Fingerprint of source beads.jsonl (verify consistency across calls)
- `status` — Per-metric state: `computed|approx|timeout|skipped` + elapsed ms
- `as_of` / `as_of_commit` — Present when using `--as-of`; contains ref and resolved SHA

**Two-phase analysis:**
- **Phase 1 (instant):** degree, topo sort, density — always available immediately
- **Phase 2 (async, 500ms timeout):** PageRank, betweenness, HITS, eigenvector, cycles — check `status` flags

**For large graphs (>500 nodes):** Some metrics may be approximated or skipped. Always check `status`.

#### jq Quick Reference

bv --robot-triage | jq '.quick_ref'                        # At-a-glance summary
bv --robot-triage | jq '.recommendations[0]'               # Top recommendation
bv --robot-plan | jq '.plan.summary.highest_impact'        # Best unblock target
bv --robot-insights | jq '.status'                         # Check metric readiness
bv --robot-insights | jq '.Cycles'                         # Circular deps (must fix!)
bv --robot-label-health | jq '.results.labels[] | select(.health_level == "critical")'

**Performance:** Phase 1 instant, Phase 2 async (500ms timeout). Prefer `--robot-plan` over `--robot-insights` when speed matters. Results cached by data hash.

Use bv instead of parsing beads.jsonl—it computes PageRank, critical paths, cycles, and parallel tracks deterministically.


## References

- [haskell/dsl/core/CLAUDE.md](haskell/dsl/core/CLAUDE.md) - Graph DSL reference
- [haskell/native-server/CLAUDE.md](haskell/native-server/CLAUDE.md) - Native server docs
- [deploy/CLAUDE.md](deploy/CLAUDE.md) - Cloudflare deployment
- [freer-simple](https://hackage.haskell.org/package/freer-simple) - Effect system
- [Anthropic tool use](https://docs.anthropic.com/en/docs/tool-use)
