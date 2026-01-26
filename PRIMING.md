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
│  tidepool-control-server ────── Haskell MCP server (20+ tools)             │
│       │                         - GH tools (gh_issue_list, gh_issue_create) │
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
│     │  Contains: claude-code, mantle-agent, effector, git, gh CLI          │
│     │  Purpose: Agent runtime (TL, PM, and spawned subagents)               │
│     │                                                                        │
│  repo-control-server ◄──────── docker/control-server/Dockerfile            │
│     │  Contains: tidepool-control-server (Haskell), gh CLI, zellij         │
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
│  tui-spawner ◄──────────────  rust/tui-spawner/                            │
│     │  FIFO-based popup spawning for cross-container TUI                    │
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
│  gh ◄────────────────────────  GitHub CLI                                  │
│     │  Issue tracking, PR management                                        │
│     │  Installed via official apt repo                                      │
│     │  Requires: GH_TOKEN env var or `gh auth login`                        │
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
│  Handler/MCP.hs ────── Tool dispatch (20+ tools)                            │
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
│    GHTools.hs ──────── gh_issue_list, gh_issue_show, gh_issue_create,      │
│                        gh_issue_update, gh_issue_close, gh_issue_reopen     │
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
│  github-interpreter ◄─── Shells out to gh CLI                              │
│     │  runGitHubIO :: GitHubConfig -> Eff (GitHub ': es) a -> Eff es a     │
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
│        GitHub, LSP, DockerSpawner, Zellij, FileSystem, Env, Log, ...       │
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
    │ MCP request: {"tool": "gh_issue_create", "arguments": {"title": "..."}}
    ▼
HTTP POST to tidepool-control-server:7432/role/tl/mcp
    │
    │ Handler/MCP.hs dispatches to GHTools.ghIssueCreateLogic
    ▼
ghIssueCreateLogic runs in Eff monad with GitHub effect
    │
    │ GitHub effect interpreted by runGitHubIO
    ▼
github-interpreter shells out: gh issue create --title "..." --repo <repo>
    │
    │ gh CLI calls GitHub API
    ▼
Response: {"issue_number": 123, "url": "https://..."}
    │
    │ Flows back through MCP response
    ▼
Claude Code receives tool result
```

### Subagent Spawning

```
TL calls spawn_agents with issue numbers
    │
    ▼
SpawnAgents.hs:
    1. Fetch issue info (GitHub effect → gh CLI)
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
│  tidepool-worktrees ── /worktrees in tl, pm, docker-spawner                │
│     │  Shared worktree storage for subagent spawning                        │
│     │  Persists across container restarts                                   │
│     │                                                                        │
│  tidepool-sockets ──── /sockets in control-server, tl, pm, zellij          │
│     │  Unix sockets for IPC (control.sock, tui.sock)                        │
│     │                                                                        │
│  tidepool-zellij ───── /run/user/1000 shared across containers             │
│     │  Zellij session socket for cross-container tab creation               │
│     │                                                                        │
│  tidepool-gh-auth ──── /home/user/.config/gh in control-server             │
│     │                  /home/agent/.config/gh in tl, pm                     │
│     │  GitHub CLI authentication (persists across restarts)                 │
│     │                                                                        │
│  tidepool-claude-tl ── /home/agent/.claude in tl                           │
│  tidepool-claude-pm ── /home/agent/.claude in pm                           │
│     │  Claude Code session state (separate per agent)                       │
│     │                                                                        │
│  /var/run/docker.sock ── Docker socket (mounted in zellij, spawner,        │
│        control-server, tl, pm)                                              │
│        Enables container management                                         │
└─────────────────────────────────────────────────────────────────────────────┘
```

## MCP Tools by Role

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              MCP TOOLS                                       │
│                                                                             │
│  TL (Tech Lead) ─────── /role/tl/mcp                                       │
│     spawn_agents        Dispatch parallel agents to issues                  │
│     exo_status          Development context (issue + git + PR)              │
│     gh_issue_*          Full GitHub access (list, show, create, update,    │
│                         close, reopen)                                      │
│     file_pr             File pull requests                                  │
│     teach-graph         LSP + LLM code exploration                          │
│     send_message        Send async message to other agents                  │
│     check_inbox         Check for pending messages                          │
│                                                                             │
│  PM (Product Manager) ── /role/pm/mcp                                      │
│     pm_status           Sprint health dashboard                             │
│     pm_review_dag       Dependency graph analysis                           │
│     pm_prioritize       Batch prioritization                                │
│     pm_propose          Propose new issues                                  │
│     pm_approve_expansion Approve/reject expansion plans                     │
│     gh_issue_list       Read GitHub issues                                  │
│     gh_issue_show       View issue details                                  │
│     send_message        Send async message to TL/Dev                        │
│                                                                             │
│  Dev (Developer) ─────── /role/dev/mcp                                     │
│     file_pr             File pull requests                                  │
│     exo_status          Development context                                 │
│     find_callers        LSP: find call sites                                │
│     gh_issue_list       List issues                                         │
│     gh_issue_show       View issue details                                  │
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
│  DOCKER_SPAWNER_URL ── http://docker-spawner:7435 (control-server)         │
│                                                                             │
│  GITHUB (one of):                                                           │
│  GH_TOKEN ───────────── GitHub personal access token (recommended)         │
│    OR                                                                       │
│  Run `docker exec -u user -it tidepool-control-server gh auth login`       │
│                                                                             │
│  OPTIONAL:                                                                  │
│  GITHUB_REPO ────────── Default repo (default: tidepool-heavy-industries/  │
│                         tidepool)                                           │
│  GEMMA_ENDPOINT ────── Ollama URL for teach-graph scoring                  │
│  ZELLIJ_SESSION_NAME ── Target session for cross-container tabs            │
└─────────────────────────────────────────────────────────────────────────────┘
```

## First-Time Setup

### GitHub Authentication

The GitHub MCP tools require authentication. Two options:

**Option 1: Environment Variable (recommended for automation)**
```bash
# In your .env file
GH_TOKEN=ghp_your_personal_access_token
```

**Option 2: Interactive Login (recommended for humans)**
```bash
# After containers are running
docker exec -u user -it tidepool-control-server gh auth login

# Follow prompts, select:
# - GitHub.com
# - HTTPS
# - Authenticate with browser (copy code, paste in browser)
```

**Note:** Must use `-u user` flag. Running as root writes auth to wrong location.

## Troubleshooting

### Build Failures

**"fatal: Unable to create ... index.lock: File exists"**

BuildKit cache mount is corrupted. Clear all caches:
```bash
docker buildx prune --all -f
./refresh-ide
```

**"Submodule path ... checked out" then fails**

The hs-opentelemetry dependency has a git submodule that can corrupt in cache:
```bash
docker buildx prune --all -f
./refresh-ide
```

**Parallel build race conditions**

Multiple Dockerfiles build the same Haskell code. If builds fail randomly, the cache mounts may be racing. The Dockerfiles now use unique cache IDs (`id=control-server-dist`, `id=claude-agent-dist`) to prevent this.

### GitHub Tool Failures

**"Not authenticated. Run: gh auth login"**

Either:
1. `GH_TOKEN` not set in environment, OR
2. Auth was done as root instead of user

Fix:
```bash
# Fix permissions if needed
docker exec tidepool-control-server chown -R 1000:1000 /home/user/.config/gh

# Re-authenticate as user
docker exec -u user -it tidepool-control-server gh auth login
```

**"Issue not found" from spawn_agents**

Check that `GITHUB_REPO` is set correctly. Default is `tidepool-heavy-industries/tidepool`.

### Container Issues

**MCP shows "failed" on startup**

Control-server takes ~10 seconds to initialize. Use `/mcp` → `Reconnect` in Claude Code.

**"permission denied" errors**

Volume ownership mismatch. The entrypoint fixes common cases, but for manual fixes:
```bash
docker exec tidepool-control-server chown -R 1000:1000 /home/user/.config/gh
docker exec tidepool-control-server chown -R 1000:1000 /sockets
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

**Legacy Code (exists but not deployed):**
- `haskell/effects/bd-interpreter/` - Beads CLI integration (volume removed)
- `haskell/control-server/src/Tidepool/Control/BDTools.hs` - BD MCP tools (not exposed)

**Tools (cron/offline):**
- `haskell/tools/training-generator/` - FunctionGemma training data generation
- `haskell/tools/ghci-oracle/` - Persistent GHCi session server

**Experimental/Prototypes:**
- `tools/micro-gastown/` - Code intelligence research sandbox

**Documentation/Planning (not runtime):**
- `work/` - Active design docs (docgen-graph epic, LSP tools spec)
- `deps/` - Dependency documentation

**Optional Docker Profiles (not default):**
- `orchestrator` (profile: legacy) - Fallback container for legacy setup
- `openobserve` (profile: monitoring) - Observability backend (optional)

## Quick Reference

```bash
# Enter the IDE (idempotent)
./ide

# Hard refresh (rebuild everything, clears caches)
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

# Clear BuildKit caches (fixes corrupted builds)
docker buildx prune --all -f

# GitHub auth (must use -u user)
docker exec -u user -it tidepool-control-server gh auth login

# Check GitHub auth status
docker exec tidepool-control-server gh auth status
```
