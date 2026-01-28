# ExoMonad - Type-Safe LLM Agent Framework

A Haskell library for building LLM agents as typed state machines. Agents are IO-blind - they yield typed effects that runners interpret.

## Two Audiences

This doc serves two audiences:

1. **Using ExoMonad** - Building agents in consuming repos (urchin)
2. **Developing ExoMonad** - Working on the framework itself

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

### DOCKER BUILD SYSTEM

Hermetic build system using `docker buildx bake` with multi-stage dependency caching.

**Commands:**

| Command | Purpose |
|---------|---------|
| `./build` | Build all images (auto-injects GIT_SHA) |
| `./build --load` | Build and load into local Docker daemon |
| `./build control-server` | Build specific target |
| `./ide` | Start/attach to IDE (idempotent) |
| `./refresh-ide` | Hard refresh (rebuild all + recreate containers) |
| `just check-freshness` | Verify running containers match local git |
| `just update-rust` | Update Cargo.lock via Docker |
| `just freeze-haskell` | Update cabal.project.freeze via Docker |

**Architecture:**

```
docker-bake.hcl orchestrates the build DAG:

Rust:     rust-planner → rust-cacher → rust-builder
Haskell:  haskell-freeze → haskell-cacher → haskell-builder
                                ↓
Final:    control-server, claude-agent, zellij
          (COPY --from=rust-builder, COPY --from=haskell-builder)
```

**Key files:**
- `./build` - Wrapper that auto-injects GIT_SHA from git
- `docker/docker-bake.hcl` - BuildKit bake orchestration
- `docker/base/Dockerfile.rust-deps` - Rust multi-stage (cargo-chef)
- `docker/base/Dockerfile.haskell-deps` - Haskell multi-stage (cabal freeze)
- `justfile` - Operations recipes (build, check-freshness, update deps)

**OCI Labels:** All images include `org.opencontainers.image.revision` with git SHA for freshness tracking.

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
│   ├── effects/CLAUDE.md       ← Effect interpreters
│   │   ├── llm-interpreter/     ← Anthropic/OpenAI API
│   │   ├── lsp-interpreter/     ← Language Server Protocol
│   │   └── ...
│   ├── runtime/CLAUDE.md       ← Execution backends
│   │   └── actor/CLAUDE.md     ← Actor model details
│   ├── protocol/CLAUDE.md      ← Wire formats
│   └── tools/CLAUDE.md         ← Dev tools (ghci-oracle, sleeptime, training-generator)
├── rust/CLAUDE.md             ← Claude Code++ (hook handler + MCP forwarding + TUI)
├── exomonad/CLAUDE.md  ← Hook handler (HTTP over Unix socket) (IMPLEMENTED)
├── docker-ctl/CLAUDE.md    ← Container lifecycle + remote exec (IMPLEMENTED)
├── effector/CLAUDE.md      ← Stateless IO executor (Cabal, Git, GH)
├── exomonad-shared/CLAUDE.md ← Protocol types, Unix socket client
├── exomonad-services/      ← External service clients (Anthropic, GitHub, Ollama, OTLP)
├── agent-status/           ← TUI Dashboard (Status, Logs, Controls)
│   ├── tui-popup/CLAUDE.md     ← TUI popup: floating pane UI for user interaction
│   └── tui-spawner/CLAUDE.md   ← FIFO-based popup spawning for cross-container TUI
├── deploy/CLAUDE.md            ← Cloudflare deployment
├── tools/CLAUDE.md             ← Root-level tools (micro-gastown, blast-radius)
└── typescript/
    └── telegram-bot/CLAUDE.md  ← Telegram integration
```

## When to Read Which CLAUDE.md

| I want to... | Read this |
|--------------|-----------|
| Work on Claude Code++ (hooks/MCP/scout) ⭐ | `haskell/control-server/CLAUDE.md` |
| Understand MCP tool architecture/tiers | `docs/architecture/ADR-003-MCP-Tool-Design-Patterns.md` |
| Understand hook/MCP forwarding (Rust side) | `rust/exomonad/CLAUDE.md` |
| Define a graph, handlers, annotations | `haskell/dsl/core/CLAUDE.md` |
| Work on LLM-level teaching infrastructure | `haskell/dsl/teaching/CLAUDE.md` |
| Add or modify an effect interpreter | `haskell/effects/CLAUDE.md` |
| Understand actor execution model | `haskell/runtime/actor/CLAUDE.md` |
| Work on semantic-scout code exploration | `haskell/control-server/CLAUDE.md` (merged from agents/semantic-scout) |
| Work with LSP (Language Server Protocol) | `haskell/effects/lsp-interpreter/CLAUDE.md` |
| Generate training data for FunctionGemma | `haskell/tools/training-generator/CLAUDE.md` |
| Deploy to Cloudflare Workers | `deploy/CLAUDE.md` |
| Work on stateless IO execution | `rust/effector/CLAUDE.md` |
| Work on container spawning/exec | `rust/docker-ctl/CLAUDE.md` |
| Understand control protocol types | `rust/exomonad-shared/CLAUDE.md` |

---

# Part 1: Using ExoMonad

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

ExoMonad is a library. Agents live in separate repos:

Example consuming repo with working agents:
- Native and Cloudflare WIPs (both work e2e)
- Agent definitions using Graph DSL
- Templates and schemas

### urchin (`~/exomonad-labs/urchin`)
Context generation tooling for coding agents:
- `urchin prime` - Generate context from git/GitHub/LSP for agent bootstrap
- `urchin lsp` - LSP impact analysis for Haskell code

## Sleeptime

**Sleeptime** is the evolution pattern for agents:

1. Agents run and produce logs/traces
2. **Cron jobs in consuming repos** observe these runs
3. Cron jobs file issues and PRs to improve the agent
4. Changes: state fields, output schemas, templates, tools

The cron jobs live in the consuming repo (urchin), not in exomonad itself. ExoMonad provides the infrastructure; consuming repos implement the evolution loop.

## Claude Code++ Integration

Human-driven Claude Code sessions augmented with ExoMonad. **Not headless automation** - humans interact via TTY; we add superpowers.

### Architecture

```
┌────────────────────────────────────────────────────────────────┐
│ User TTY (Zellij 3-pane)                                       │
│  Pane 1: Claude Code  │  Pane 2: control-server  │ Pane 3: TUI │
└───────────┬──────────────────────────────────┬─────────────────┘
            │ Hooks/MCP                        │
            ▼                                  │
┌─────────────────────────────────────────┐    │
│ exomonad (Rust)                     │    │
│  • hook: CC hooks → Unix Socket         │    │
└───────────┬─────────────────────────────┘    │
            │ Unix Socket NDJSON               │
            │ .exomonad/sockets/control.sock   │
            ▼                                  │
┌─────────────────────────────────────────┐    │
│ control-server (Haskell)                │    │
│  • Long-lived LSP session (HLS)         │    │
│  • Hook Handler: Passthrough            │    │
│  • MCP Handler: 7 tools (auto-discovery)│    │
│  • TUI Handler: Listens for Dashboard   │◄───┘
└─────────────────────────────────────────┘    │
                                               │ Unix Socket NDJSON
                                               │ .exomonad/sockets/tui.sock
                                               │
┌──────────────────────────────────────────────┘
│
▼
┌─────────────────────────────────────────┐
│ agent-status (Rust)                     │
│  • Connects to control-server           │
│  • Renders Status/Logs/Controls         │
│  • Captures keyboard (Tab, Enter)       │
│  • Sends Interaction events             │
└─────────────────────────────────────────┘
```

### Key Components

| Component | Location | Purpose |
|-----------|----------|---------|
| **exomonad** | `rust/exomonad/` | Hook forwarding to control server (HTTP over Unix socket) |
| **control-server** | `haskell/control-server/` | Haskell server with LSP + MCP tools + TUI handler |
| **agent-status** | `rust/agent-status/` | Rust TUI: renders dashboard, captures Interaction |
| **Protocol types** | `rust/exomonad-shared/protocol.rs` + `haskell/control-server/Protocol.hs` | Bidirectional message types (must match exactly) |

### Data Flow

**Hook Flow (PreToolUse):**
```
1. Claude Code wants to call Write tool
2. Generates hook JSON on stdin
3. exomonad hook pre-tool-use reads stdin
4. Forwards ControlMessage::HookEvent via Unix Socket
5. control-server receives, routes to handleHook
6. Returns HookResponse (allow/deny)
7. exomonad prints to stdout
8. Claude Code proceeds or blocks
```

**Transcript Shipping (SessionEnd/SubagentStop):**
```
1. Claude session ends or subagent finishes
2. exomonad hook [session-end|subagent-stop] reads transcript path from stdin
3. control-server reads JSONL transcript, enriches with metadata (session_id, role, etc.)
4. POSTs to OpenObserve (claude_sessions stream) in background
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
            "command": "exomonad hook pre-tool-use"
          }
        ]
      }
    ],
    "SessionEnd": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "exomonad hook session-end"
          }
        ]
      }
    ],
    "SubagentStop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "exomonad hook subagent-stop"
          }
        ]
      }
    ]
  }
}
```

**Note:** MCP server configuration uses `.mcp.json`. Claude Code connects directly to control-server via HTTP (TCP port 7432):
```json
{"mcpServers": {"exomonad": {"type": "http", "url": "http://localhost:7432/role/tl/mcp"}}}
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
./ide              # Connect to Zellij (starts containers if needed, idempotent)

# Detach with Ctrl+p, Ctrl+q
```

**After Dockerfile/layout changes:**
```bash
./refresh-ide      # Rebuild images + recreate containers (no TTY required)
./ide              # Then connect
```

Note: `./refresh-ide` is agent-safe (no TTY required). Agents can run it to rebuild/restart
the environment, then humans connect with `./ide`.

**Manual (if scripts don't work):**
```bash
docker compose up -d
docker exec -it exomonad-zellij gosu user zellij attach --create main
```

**Services:**
| Container | Purpose |
|-----------|---------|
| `exomonad-zellij` | Minimal Zellij multiplexer (human attaches here) |
| `exomonad-control-server` | Haskell MCP server (TCP 7432) |
| `exomonad-tl` | Tech Lead agent (coding) |
| `exomonad-pm` | Project Manager agent (planning) |
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
Expected: Shows "exomonad" server connected

/tools
Expected: Lists MCP tools
```

**Testing docker-ctl exec:**
```bash
docker exec exomonad-control-server docker-ctl exec exomonad-tl -- echo hello
```

**Rollback to Legacy Orchestrator:**
```bash
# Use the legacy profile
docker compose --profile legacy up orchestrator
docker attach exomonad-orchestrator
```

**Troubleshooting:**
- **MCP shows "failed" on startup**: control-server still initializing. Use `/mcp` → `Reconnect` after 10 seconds.
- **Agent not responding**: Check `docker logs exomonad-tl` or `docker logs exomonad-pm`.
- **Authentication errors**: Verify credentials in `exomonad-claude-auth` volume.

**Local development (without Docker):**

Not recommended. Use Docker Compose for consistency. If you must run locally:
```bash
# Build binaries
cabal build exomonad-control-server
cargo build -p exomonad -p tui-sidebar

# Run control-server (Terminal 1)
cabal run exomonad-control-server

# Run Claude Code with hooks configured (Terminal 2)
claude
```

### Orchestration Internals

Understanding the Docker-based runtime stack for debugging and extension.

#### Scripts

| Script | TTY | Purpose |
|--------|-----|---------|
| `./ide` | Yes | Connect to Zellij (human use) |
| `./refresh-ide` | No | Build + recreate containers (agent-safe) |
| `./build` | No | Build images via docker buildx bake |

#### Docker Volumes

| Volume | Purpose |
|--------|---------|
| `exomonad-sockets` | Shared `/sockets` for control.sock |
| `exomonad-zellij` | Shared XDG_RUNTIME_DIR for cross-container Zellij |
| `exomonad-claude-auth` | Claude authentication persistence |

#### Config Files

| File | Format | Purpose |
|------|--------|---------|
| `docker/zellij/layout.kdl` | KDL | Zellij tab layout |
| `docker/zellij/config.kdl` | KDL | Zellij behavior |
| `docker-compose.yml` | YAML | Service orchestration |
| `docker/docker-bake.hcl` | HCL | Multi-stage build DAG |

#### Health Checks

All containers use Docker health checks. `./refresh-ide` waits for all services to report healthy before completing.

```bash
# Check health status
docker inspect --format='{{.State.Health.Status}}' exomonad-control-server
```

#### Troubleshooting

**Containers won't start:**
```bash
docker compose logs control-server  # Check for errors
docker compose down && ./refresh-ide  # Clean restart
```

**Stale Zellij session:**
```bash
# refresh-ide cleans volatile state automatically, but if needed:
docker run --rm -v exomonad-zellij:/run/user/1000 debian:bookworm-slim rm -rf /run/user/1000/*
```

**Build cache issues:**
```bash
./build --no-cache  # Force full rebuild
```

### Status

- ✅ Hook forwarding (passthrough)
- ✅ MCP server + 20+ tools via auto-discovery (popup, spawn_agents, exo_status, file_pr, pm_status, mailbox tools, GitHub tools, etc.)
- ✅ LSP integration (HLS via lsp-test)
- ✅ FunctionGemma scoring (HTTP interpreter via Ollama)
- ✅ Automatic tool registration via MCPExport annotation + reifyMCPTools
- ✅ Hybrid orchestration (Zellij)
- ✅ Unix socket health checks for robust readiness checks
- ✅ Declarative service dependencies and restart policies
- ✅ MCP direct execution via .mcp.json (exomonad spawned per-call by Claude)
- ✅ Unix socket communication for all local components
- 🔄 Training data generation (types ready, CLI pending)
- ❌ Daemon mode (not implemented, uses per-call connection)
- ❌ Metrics hub (exomonad-hub needs repurposing)
- ❌ Real hook logic (currently allow-all passthrough)

### Stopping

**From inside Zellij:**
- Detach: `Ctrl+p, Ctrl+q` (containers keep running)
- Quit: `Ctrl+P` → `q` (exits Zellij, containers keep running)

**Stop all containers:**
```bash
docker compose down
```

**Full cleanup (including volumes):**
```bash
docker compose down -v
```

### See Also

- **[haskell/control-server/CLAUDE.md](haskell/control-server/CLAUDE.md)** - Complete data flow + implementation
- **[rust/CLAUDE.md](rust/CLAUDE.md)** - Rust workspace overview
- **[rust/exomonad/CLAUDE.md](rust/exomonad/CLAUDE.md)** - Hook/MCP implementation
- **[haskell/agents/semantic-scout/CLAUDE.md](haskell/agents/semantic-scout/CLAUDE.md)** - Scout exploration algorithm

---

# Part 2: Developing ExoMonad

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

### Effect Interpreters (`haskell/effects/`)
| Package | Purpose |
|---------|---------|
| `haskell/effects/llm-interpreter` | Anthropic API calls (Socket preferred) |
| `haskell/effects/habitica-interpreter` | Habitica API |
| `haskell/effects/observability-interpreter` | OpenTelemetry traces to Grafana |
| `haskell/effects/lsp-interpreter` | LSP via lsp-client |
| `haskell/effects/ghci-interpreter` | GHCi Oracle thin client |
| `haskell/effects/github-interpreter` | GitHub API integration |
| `haskell/effects/worktree-interpreter` | Git worktree management |
| `haskell/effects/cabal-interpreter` | Cabal build operations |

### Integrations (`haskell/effects/`, `haskell/protocol/`)
| Package | Purpose |
|---------|---------|
| `haskell/effects/habitica` | Habitica effect types (standalone) |
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
| New effect type | `haskell/dsl/core/src/ExoMonad/Effect/Types.hs` |
| New integration | `haskell/dsl/core/src/ExoMonad/Effects/` (plural) |
| New graph annotation | `haskell/dsl/core/src/ExoMonad/Graph/Types.hs` |
| New interpreter | `haskell/effects/<name>-interpreter/` |
| New MCP tool/agent | `haskell/agents/<name>/` |
| TypeScript bot | `typescript/<name>/` |
| Agents (consuming repos) | Separate repo (urchin, etc.) |

### Naming Conventions
- **Effect** (singular) = core infrastructure (`ExoMonad.Effect.*`)
- **Effects** (plural) = integrations/contrib (`ExoMonad.Effects.*`)
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

The `exomonad-lsp-interpreter` provides LSP for code intelligence:

```haskell
import ExoMonad.Effects.LSP
import ExoMonad.LSP.Interpreter (withLSPSession, runLSP)

withLSPSession "/path/to/project" $ \session -> do
  info <- runLSP session $ hover doc pos
  ...
```

## References

- [haskell/dsl/core/CLAUDE.md](haskell/dsl/core/CLAUDE.md) - Graph DSL reference
- [deploy/CLAUDE.md](deploy/CLAUDE.md) - Cloudflare deployment
- [freer-simple](https://hackage.haskell.org/package/freer-simple) - Effect system
- [Anthropic tool use](https://docs.anthropic.com/en/docs/tool-use)