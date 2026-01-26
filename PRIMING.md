# Tidepool System Priming

Current state of the deployed Docker-based development environment.

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           ENTRY POINTS                                       │
│  ./ide              Idempotent session entry (creates/attaches/resurrects)  │
│  ./refresh-ide      Hard refresh (rebuild + fresh session)                  │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                        RUNNING CONTAINERS                                    │
│                                                                             │
│  tidepool-zellij ─────────────── Terminal multiplexer (user attaches here) │
│       │                          Runs: zellij attach --create               │
│       │ docker attach                                                       │
│       ▼                                                                     │
│  tidepool-tl ────────────────── Claude Code agent (Tech Lead role)         │
│  tidepool-pm ────────────────── Claude Code agent (PM role)                │
│       │                                                                     │
│       │ MCP (TCP 7432)                                                      │
│       ▼                                                                     │
│  tidepool-control-server ────── Haskell MCP server (23+ tools)             │
│       │                         - BD tools (bd_list, bd_create, ...)        │
│       │                         - PM tools (pm_status, pm_review_dag, ...)  │
│       │                         - Exo tools (spawn_agents, exo_status)      │
│       │                         - LSP tools (find_callers, teach-graph)     │
│       │                                                                     │
│       │ HTTP (7435)                                                         │
│       ▼                                                                     │
│  tidepool-docker-spawner ────── Container lifecycle API                    │
│                                 POST /spawn, /stop/{id}, /exec/{id}         │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Docker Images

Each container is built from a Dockerfile in `docker/`:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          DOCKER IMAGES                                       │
│                                                                             │
│  repo-zellij ◄──────────────── docker/zellij/Dockerfile                    │
│     │  Contains: zellij, docker CLI                                         │
│     │  Purpose: Visual multiplexer, panes attach to agent containers        │
│     │                                                                        │
│  repo-claude-agent ◄────────── docker/claude-agent/Dockerfile              │
│     │  Contains: claude-code, mantle-agent, git, common tools               │
│     │  Purpose: Agent runtime (TL, PM, and spawned subagents)               │
│     │                                                                        │
│  repo-control-server ◄──────── docker/control-server/Dockerfile            │
│     │  Contains: tidepool-control-server (Haskell), bd CLI, zellij          │
│     │  Purpose: MCP server, tool execution, cross-container orchestration   │
│     │                                                                        │
│  repo-docker-spawner ◄──────── docker/docker-spawner/Dockerfile            │
│        Contains: docker-spawner (Rust)                                      │
│        Purpose: Container spawn/stop/exec API                               │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Binary Dependencies

Binaries that must be built and available:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         RUST BINARIES                                        │
│  Built with: cargo build -p <name>                                          │
│                                                                             │
│  DEPLOYED:                                                                  │
│  docker-spawner ◄──────────── rust/docker-spawner/                         │
│     │  HTTP API for container lifecycle                                     │
│     │  Depends: bollard (Docker API), axum (HTTP)                           │
│     │                                                                        │
│  mantle-agent ◄────────────── rust/mantle-agent/                           │
│     │  Hook forwarding (spawned per-hook by Claude Code)                    │
│     │  Depends: mantle-shared (protocol types)                              │
│     │                                                                        │
│  mantle-shared ◄───────────── rust/mantle-shared/                          │
│     │  Protocol types (ControlMessage, HookInput, etc.)                     │
│     │  Must match: haskell/control-server/Protocol.hs                       │
│     │                                                                        │
│  effector ◄──────────────────  rust/effector/                              │
│     │  Stateless IO executor (cabal, git, gh commands)                      │
│     │  Runs in agent containers, returns structured JSON                    │
│     │  Used by control-server via SshExec effect (POST /exec/{id})          │
│     │                                                                        │
│  tui-sidebar ◄───────────────  rust/tui-sidebar/                           │
│        Ratatui-based sidebar rendering (connects to control.sock)           │
│                                                                             │
│  IN WORKSPACE (not deployed):                                               │
│  tui-popup ◄─────────────────  rust/tui-popup/                             │
│     │  Generic TUI popup (depends on tui-sidebar)                           │
│     │                                                                        │
│  mantle-hub ◄────────────────  rust/mantle-hub/                            │
│     │  Telemetry/metrics hub (WIP, needs repurposing)                       │
│     │                                                                        │
│  ssh-proxy ◄─────────────────  rust/ssh-proxy/                             │
│        HTTP-to-SSH bridge (DEPRECATED, replaced by docker-spawner /exec)    │
└─────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────┐
│                        HASKELL BINARY                                        │
│  Built with: cabal build tidepool-control-server                            │
│                                                                             │
│  tidepool-control-server ◄─── haskell/control-server/                      │
│        MCP server, tool dispatch, effect interpretation                     │
│        TCP listener on port 7432                                            │
└─────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────┐
│                        EXTERNAL BINARIES                                     │
│  Installed in control-server container                                      │
│                                                                             │
│  bd ◄──────────────────────── cargo install beads-rs                       │
│     │  Issue tracking CLI (git-native)                                      │
│     │  Built from source in Dockerfile (glibc compatibility)                │
│     │                                                                        │
│  zellij ◄──────────────────── cargo-binstall zellij                        │
│        Terminal multiplexer                                                 │
│        Used for cross-container tab creation                                │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Haskell Package Graph

The control-server binary depends on these packages:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    CONTROL SERVER (haskell/control-server/)                  │
│                                                                             │
│  Server.hs ─────────── TCP listener, request routing                        │
│  Protocol.hs ───────── Wire types (must match Rust)                         │
│  Handler/MCP.hs ────── Tool dispatch (23+ tools)                            │
│  Handler/Hook.hs ───── Hook processing (passthrough)                        │
│  Export.hs ─────────── MCP tool auto-discovery                              │
│  RoleConfig.hs ─────── Role-based access control                            │
│                                                                             │
│  Tool Definitions (by tier):                                                │
│                                                                             │
│  TIER 1 - LSP-only:                                                         │
│    LSPTools.hs ─────── find_callers, show_fields, show_constructors         │
│                                                                             │
│  TIER 2 - LLM-enhanced:                                                     │
│    Scout/Graph.hs ──── teach-graph (LSP + Haiku selection)                  │
│    Scout/DocGen.hs ─── Teaching document generation (BFS exploration)       │
│    Scout/DocGen/Gemma.hs ── FunctionGemma scoring via Ollama                │
│                                                                             │
│  TIER 3 - External orchestration:                                           │
│    BDTools.hs ──────── bd_list, bd_create, bd_update, bd_close, ...        │
│    SpawnAgents.hs ──── spawn_agents (parallel agent dispatch)               │
│    ExoTools.hs ─────── exo_status, exo_complete                             │
│    FilePR.hs ───────── file_pr (GitHub PR filing)                           │
│                                                                             │
│  TIER 4 - TUI-interactive + PM dashboard:                                   │
│    TUITools.hs ─────── confirm_action, select_option, request_guidance      │
│    PMStatus.hs ─────── pm_status (sprint health dashboard)                  │
│    PMReviewDAG.hs ──── pm_review_dag (dependency analysis)                  │
│    PMPrioritize.hs ─── pm_prioritize, pm_propose, pm_approve_expansion      │
│                                                                             │
│  TIER 5 - Mailbox communication:                                            │
│    MailboxTools.hs ─── send_message, read_message, check_inbox, mark_read   │
│    FeedbackTools.hs ── register_feedback                                    │
│                                                                             │
│  Workflows:                                                                 │
│    StopHook/ ───────── Stop hook enforcement (PR filing, pre-commit checks) │
│      Graph.hs ──────── StopHookGraph with circuit breaker                   │
│      Handlers.hs ───── Templated guidance for blocking stages               │
└───────────────────────────────────┬─────────────────────────────────────────┘
                                    │ imports
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                    EFFECT INTERPRETERS (haskell/effects/)                    │
│                                                                             │
│  CORE (always used):                                                        │
│  bd-interpreter ◄────── Shells out to bd CLI                               │
│     │  runBDIO :: BDConfig -> Eff (BD ': es) a -> Eff es a                  │
│     │                                                                        │
│  docker-spawner-interpreter ◄── HTTP calls to docker-spawner               │
│     │  runDockerSpawner :: Config -> Eff (DockerSpawner ': es) a -> ...     │
│     │                                                                        │
│  zellij-interpreter ◄── Shells out to zellij CLI                           │
│     │  runZellijIO :: Eff (Zellij ': es) a -> Eff es a                      │
│     │                                                                        │
│  worktree-interpreter ◄── Git worktree operations                          │
│  filesystem-interpreter ◄── File read/write                                │
│  env-interpreter ◄──── Environment variable access                         │
│  github-interpreter ◄── GitHub API (PR filing)                             │
│  lsp-interpreter ◄──── LSP client (HLS communication)                      │
│                                                                             │
│  REMOTE EXECUTION (via docker-spawner /exec/{id}):                          │
│  SshExec ◄───────────── Low-level HTTP exec to docker-spawner              │
│     │  control-server/Effects/SshExec.hs                                    │
│     │  Calls POST /exec/{container} with command JSON                       │
│     │                                                                        │
│  Higher-level effects (use SshExec under the hood):                         │
│  Effector ◄──────────── Structured cabal/git/gh via effector binary        │
│  Cabal ◄─────────────── Cabal build/test operations                        │
│  Git ◄───────────────── Git status/diff operations                         │
│  Justfile ◄──────────── Just recipe execution                              │
│                                                                             │
│  OBSERVABILITY:                                                             │
│  observability-interpreter ◄── OpenTelemetry traces (Grafana Cloud)        │
│  gemini-interpreter ◄── Gemini API calls (runGeminiIO)                     │
└───────────────────────────────────┬─────────────────────────────────────────┘
                                    │ imports
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                       DSL CORE (haskell/dsl/)                                │
│                                                                             │
│  tidepool-core (dsl/core/) ◄── Main DSL package                            │
│     Graph DSL ─────── Type-safe state machine definitions                   │
│        EntryNode, LogicNode, ExitNode                                       │
│        MCPExport annotation → auto-discovery                                │
│        MCPToolDef annotation → tool name + description                      │
│                                                                             │
│     Effects ───────── Algebraic effect types                                │
│        BD, LSP, DockerSpawner, Zellij, FileSystem, Env, Log, ...           │
│        Defined as GADTs, interpreted by effect interpreters                 │
│                                                                             │
│     Templates ─────── Typed Jinja (ginger)                                  │
│        TypedTemplate with compile-time context validation                   │
│                                                                             │
│     Schema ────────── JSON Schema generation                                │
│        HasJSONSchema typeclass → tool input schemas                         │
│                                                                             │
│  tidepool-teaching (dsl/teaching/) ◄── LLM teaching infrastructure         │
│     Teaching mode for FunctionGemma training data capture                   │
│     FineTrainingTeacher typeclass for structured examples                   │
└───────────────────────────────────┬─────────────────────────────────────────┘
                                    │ depends on
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                      HASKELL LIBRARIES (Hackage)                             │
│                                                                             │
│  freer-simple ─────── Algebraic effects (free monad)                        │
│  ginger ───────────── Jinja2 template engine                                │
│  aeson ────────────── JSON serialization                                    │
│  servant ──────────── HTTP API types                                        │
│  lsp-test ─────────── LSP client library                                    │
│  warp ─────────────── HTTP server                                           │
│  network ──────────── TCP sockets                                           │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Data Flow

### Agent → MCP Tool → Effect → External System

```
Claude Code (in tidepool-tl)
    │
    │ MCP request: {"tool": "bd_create", "arguments": {"title": "..."}}
    ▼
HTTP POST to tidepool-control-server:7432/role/tl/mcp
    │
    │ Handler/MCP.hs dispatches to BDTools.bdCreateLogic
    ▼
bdCreateLogic runs in Eff monad with BD effect
    │
    │ BD effect interpreted by runBDIO
    ▼
bd-interpreter shells out: bd create "..." --json --repo /beads
    │
    │ bd CLI writes to /beads volume (git-native storage)
    ▼
Response: {"bead_id": "beads-xyz"}
    │
    │ Flows back through MCP response
    ▼
Claude Code receives tool result
```

### Subagent Spawning

```
TL calls spawn_agents with bead IDs
    │
    ▼
SpawnAgents.hs:
    1. Fetch bead info (BD effect → bd CLI)
    2. Create worktree (Worktree effect → git)
    3. Bootstrap .tidepool/ config
    4. Spawn container (DockerSpawner effect → docker-spawner:7435)
    5. Create Zellij tab with "docker attach <container>"
    │
    ▼
New agent container running, attached to Zellij
```

## Volume Mounts

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           DOCKER VOLUMES                                     │
│                                                                             │
│  tidepool-beads ────── /beads in control-server                            │
│     │  Git-native issue tracking database                                   │
│     │  Initialized with: git init + bd init                                 │
│     │                                                                        │
│  tidepool-worktrees ── /worktrees in tl, pm, docker-spawner                │
│     │  Shared worktree storage for subagent spawning                        │
│     │  Persists across container restarts                                   │
│     │                                                                        │
│  tidepool-sockets ──── /sockets in control-server                          │
│     │  Unix sockets for IPC (control.sock, tui.sock)                        │
│     │                                                                        │
│  tidepool-zellij ───── /run/user/1000 shared across containers             │
│     │  Zellij session socket for cross-container tab creation               │
│     │                                                                        │
│  /var/run/docker.sock ── Docker socket (mounted in zellij, spawner)        │
│        Enables container management                                         │
└─────────────────────────────────────────────────────────────────────────────┘
```

## MCP Tools by Role

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              MCP TOOLS                                       │
│                                                                             │
│  TL (Tech Lead) ─────── /role/tl/mcp                                       │
│     spawn_agents        Dispatch parallel agents to beads                   │
│     exo_status          Development context (bead + git + PR)               │
│     bd_*                Full BD access (list, create, update, close, ...)   │
│     file_pr             File pull requests                                  │
│     teach-graph         LSP + LLM code exploration                          │
│     send_message        Send async message to other agents                  │
│     check_inbox         Check for pending messages                          │
│                                                                             │
│  PM (Product Manager) ── /role/pm/mcp                                      │
│     pm_status           Sprint health dashboard                             │
│     pm_review_dag       Dependency graph analysis                           │
│     pm_prioritize       Batch prioritization                                │
│     pm_propose          Propose new beads                                   │
│     pm_approve_expansion Approve/reject expansion plans                     │
│     bd_list, bd_show    Read-only BD access                                 │
│     send_message        Send async message to TL/Dev                        │
│                                                                             │
│  Dev (Developer) ─────── /role/dev/mcp                                     │
│     file_pr             File pull requests                                  │
│     exo_status          Development context                                 │
│     find_callers        LSP: find call sites                                │
│     confirm_action      TUI: confirmation dialog                            │
│     select_option       TUI: option selection                               │
│     request_guidance    TUI: ask human for help                             │
│     read_message        Read messages from inbox                            │
│     mark_read           Mark message as read                                │
│     register_feedback   Submit feedback on tool/workflow                    │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Environment Variables

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        REQUIRED ENVIRONMENT                                  │
│                                                                             │
│  ANTHROPIC_API_KEY ──── Claude API key (agent containers)                  │
│  BEADS_DIR ─────────── /beads (control-server)                             │
│  DOCKER_SPAWNER_URL ── http://docker-spawner:7435 (control-server)         │
│                                                                             │
│  OPTIONAL:                                                                  │
│  GEMMA_ENDPOINT ────── Ollama URL for teach-graph scoring                  │
│  ZELLIJ_SESSION_NAME ── Target session for cross-container tabs            │
└─────────────────────────────────────────────────────────────────────────────┘
```

## What's NOT in the Deployed System

These exist in the repo but are not part of the current Docker deployment:

**Deployment Backends (frozen/alternative):**
- `deploy/` - Cloudflare Workers (WASM deployment, frozen)
- `haskell/runtime/wasm/` - WASM execution backend

**Alternative Servers/UIs:**
- `haskell/native-server/` - WebSocket server (replaced by control-server)
- `typescript/native-gui/` - Solid.js frontend
- `anemone/` - Debug UI (Solid.js)

**Integrations (not wired up):**
- `typescript/telegram-bot/` - Telegram integration
- `haskell/effects/habitica-interpreter/` - Habitica integration

**Tools (cron/offline):**
- `haskell/tools/sleeptime/` - Log analysis (cron job infrastructure)
- `haskell/tools/training-generator/` - FunctionGemma training data generation
- `haskell/tools/ghci-oracle/` - Persistent GHCi session server

**Experimental/Prototypes:**
- `tools/micro-gastown/` - Code intelligence research sandbox

**Documentation/Planning (not runtime):**
- `plans/` - Architectural plans (dockerized-claude-code)
- `work/` - Active design docs (docgen-graph epic, LSP tools spec)
- `deps/` - Dependency documentation

**Optional Docker Profiles (not default):**
- `orchestrator` (profile: legacy) - Fallback container for legacy setup
- `openobserve` (profile: monitoring) - Observability backend (optional)

## Quick Reference

```bash
# Enter the IDE (idempotent)
./ide

# Hard refresh (rebuild everything)
./refresh-ide

# Detach from Zellij (session keeps running)
Ctrl+o then d

# Quit Zellij (closes session)
Ctrl+q

# Check container status
docker compose ps

# View control-server logs
docker logs -f tidepool-control-server

# Rebuild and restart a service
docker compose build control-server && docker compose up -d control-server
```
