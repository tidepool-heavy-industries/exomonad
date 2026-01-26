# ExoMonad System Priming

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
│  exomonad-zellij ─────────────── Terminal multiplexer (user attaches here) │
│       │                          Runs: zellij attach --create               │
│       │ docker attach                                                       │
│       ▼                                                                     │
│  exomonad-tl ────────────────── Claude Code agent (Tech Lead role)         │
│  exomonad-pm ────────────────── Claude Code agent (PM role)                │
│       │                                                                     │
│       │ MCP (TCP 7432)                                                      │
│       ▼                                                                     │
│  exomonad-control-server ────── Haskell MCP server (20+ tools)             │
│       │                         - GH tools (gh_issue_list, gh_issue_create) │
│       │                         - PM tools (pm_status, pm_prioritize, ...)  │
│       │                         - Exo tools (spawn_agents, exo_status)      │
│       │                         - LSP tools (find_callers, teach-graph)     │
│       │                                                                     │
│       │ subprocess                                                          │
│       ▼                                                                     │
│  docker-ctl (Rust binary) ───── Container lifecycle CLI                     │
│                                 spawn, stop, exec, status                   │
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
│     │  Contains: claude-code, exomonad, effector, git, gh CLI          │
│     │  Purpose: Agent runtime (TL, PM, and spawned subagents)               │
│     │                                                                        │
│  repo-control-server ◄──────── docker/control-server/Dockerfile            │
│     │  Contains: exomonad-control-server (Haskell), gh CLI, zellij,       │
│     │            docker-ctl (Rust)                                        │
│     │  Purpose: MCP server, tool execution, cross-container orchestration   │
│     │                                                                        │
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
│  docker-ctl ◄────────────────  rust/docker-ctl/                            │
│     │  CLI for container lifecycle (spawn, stop, exec)                      │
│     │  Depends: bollard (Docker API)                                        │
│     │                                                                        │
│  exomonad ◄────────────── rust/exomonad/                           │
│     │  Hook forwarding (spawned per-hook by Claude Code)                    │
│     │  Depends: exomonad-shared (protocol types)                              │
│     │                                                                        │
│  exomonad-shared ◄───────────── rust/exomonad-shared/                          │
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
│  exomonad-hub ◄────────────────  rust/exomonad-hub/                            │
│     │  Telemetry/metrics hub (WIP, needs repurposing)                       │
│     │                                                                        │
│  ssh-proxy ◄─────────────────  rust/ssh-proxy/                             │
│        HTTP-to-SSH bridge (DEPRECATED, replaced by docker-ctl exec)         │
└─────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────┐
│                        HASKELL BINARY                                        │
│  Built with: cabal build exomonad-control-server                            │
│                                                                             │
│  exomonad-control-server ◄─── haskell/control-server/                      │
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
│    LSPTools.hs ─────── find_callers, show_type                              │
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
│    TUITools.hs ─────── popup (general-purpose UI dialog)                    │
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
│  DockerCtl (in control-server) ◄── Subprocess calls to docker-ctl          │
│     │  ExoMonad.Control.Effects.DockerCtl                                  │
│     │                                                                        │
│  zellij-interpreter ◄── Shells out to zellij CLI                           │
│     │  runZellijIO :: Eff (Zellij ': es) a -> Eff es a                      │
│     │                                                                        │
│  worktree-interpreter ◄── Git worktree operations                          │
│  filesystem-interpreter ◄── File read/write                                │
│  env-interpreter ◄──── Environment variable access                         │
│  lsp-interpreter ◄──── LSP client (HLS communication)                      │
│                                                                             │
│  REMOTE EXECUTION (via docker-ctl exec):                                   │
│  SshExec ◄───────────── Subprocess calls to docker-ctl exec                │
│     │  control-server/Effects/SshExec.hs                                    │
│     │  Calls docker-ctl exec {container} with arguments                     │
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
│  exomonad-core (dsl/core/) ◄── Main DSL package                            │
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
│  exomonad-teaching (dsl/teaching/) ◄── LLM teaching infrastructure         │
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
Claude Code (in exomonad-tl)
    │
    │ MCP request: {"tool": "gh_issue_create", "arguments": {"title": "..."}}
    ▼
HTTP POST to exomonad-control-server:7432/role/tl/mcp
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
    3. Bootstrap .exomonad/ config
    4. Spawn container (DockerSpawner effect → docker-ctl spawn)
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
│  exomonad-worktrees ── /worktrees in tl, pm, control-server                │
│     │  Shared worktree storage for subagent spawning                        │
│     │  Persists across container restarts                                   │
│     │                                                                        │
│  exomonad-sockets ──── /sockets in control-server, tl, pm, zellij          │
│     │  Unix sockets for IPC (control.sock, tui.sock)                        │
│     │                                                                        │
│  exomonad-zellij ───── /run/user/1000 shared across containers             │
│     │  Zellij session socket for cross-container tab creation               │
│     │                                                                        │
│  exomonad-gh-auth ──── /home/user/.config/gh in control-server             │
│     │                  /home/agent/.config/gh in tl, pm                     │
│     │  GitHub CLI authentication (persists across restarts)                 │
│     │                                                                        │
│  exomonad-claude-tl ── /home/agent/.claude in tl                           │
│  exomonad-claude-pm ── /home/agent/.claude in pm                           │
│     │  Claude Code session state (separate per agent)                       │
│     │                                                                        │
│  /var/run/docker.sock ── Docker socket (mounted in zellij,                 │
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
│                                                                             │
│  GITHUB (one of):                                                           │
│  GH_TOKEN ───────────── GitHub personal access token (recommended)         │
│    OR                                                                       │
│  Run `docker exec -u user -it exomonad-control-server gh auth login`       │
│                                                                             │
│  OPTIONAL:                                                                  │
│  GITHUB_REPO ────────── Default repo (default: exomonad-heavy-industries/  │
│                         exomonad)                                           │
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
docker exec -u user -it exomonad-control-server gh auth login

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
docker exec exomonad-control-server chown -R 1000:1000 /home/user/.config/gh

# Re-authenticate as user
docker exec -u user -it exomonad-control-server gh auth login
```

**"Issue not found" from spawn_agents**

Check that `GITHUB_REPO` is set correctly. Default is `exomonad-ai/exomonad`.

### Container Issues

**MCP shows "failed" on startup**

Control-server takes ~10 seconds to initialize. Use `/mcp` → `Reconnect` in Claude Code.

**"permission denied" errors**

Volume ownership mismatch. The entrypoint fixes common cases, but for manual fixes:
```bash
docker exec exomonad-control-server chown -R 1000:1000 /home/user/.config/gh
docker exec exomonad-control-server chown -R 1000:1000 /sockets
```

## Docker Orchestration Layer (Review Focus)

This section details the custom Docker orchestration we built. **This is the area where we may be reinventing wheels.**

### The Problem We're Solving

LLM agents (Claude Code sessions) need to:
1. **Spawn parallel workers** - TL dispatches tasks to Dev agents, each in isolated worktrees
2. **Execute commands remotely** - Control-server runs `cabal build`, `gh issue create` in agent containers
3. **Coordinate via TUI** - Agents need to show popups/confirmations to the human in the Zellij session
4. **Share authentication** - GitHub tokens, Claude auth must be available across containers

### What We Built

#### docker-ctl (Rust, ~150 LOC)

A thin CLI wrapper around Docker's API via bollard crate.

**Commands:**
```
docker-ctl spawn          Create container with mounts + labels
docker-ctl exec {id}      Run command in container, return JSON stdout/stderr/exit_code
docker-ctl status {id}    Container status
docker-ctl stop {id}      Stop + remove container
```

**Spawn creates containers with:**
```rust
Config {
    image: "exomonad-agent:latest",
    labels: { "com.exomonad.issue_number": "...", "com.exomonad.role": "agent" },
    mounts: [
        bind: worktree_path → /worktrees/{id},
        volume: exomonad-gh-auth → /home/agent/.config/gh
    ],
    network: "exomonad-internal",
    user: "1000:1000",
    env: ["EXOMONAD_ISSUE_NUMBER=...", "EXOMONAD_BACKEND=claude|gemini"]
}
```

**Exec returns JSON on stdout:**
```json
{"exit_code": 0, "stdout": "...", "stderr": "..."}
```

#### Why CLI Tool?

1. **No network hop** - Eliminates HTTP overhead and failure modes.
2. **Simplified architecture** - No need for a persistent spawner service.
3. **Better error handling** - Exit codes and stderr are more natural for subprocesses.
4. **Shared docker.sock** - The control-server already has the socket; calling a CLI tool is cleaner.

#### What docker-ctl Actually Does

1. **Wraps bollard** - Rust Docker client, handles socket communication
2. **Hardcodes our patterns** - Mounts, labels, network always the same
3. **Provides CLI interface** - So Haskell control-server can call it via subprocess
4. **Captures exec output** - Buffers stdout/stderr, returns JSON structured exit info

#### Could We Replace It?

**Yes, with:**
- **Portainer API** - Has exec endpoint, but heavy (UI we don't need)
- **Custom shell script + `docker` CLI** - Parse JSON output of `docker inspect`, `docker exec`
- **Direct bollard from Haskell** - But we'd need Haskell Docker bindings

**The 300 LOC buys:**
- Typed request/response (SpawnRequest, ExecResponse)
- Error handling for 404 (container not found)
- Graceful stop (10s timeout, then remove)
- Logging via tracing

### Container Topology

```
┌─────────────────────────────────────────────────────────────────────────────┐
│ HOST MACHINE                                                                 │
│                                                                             │
│  ┌─────────────────┐                                                        │
│  │ docker.sock     │◄───────────────────────────────────────────────────┐  │
│  └────────┬────────┘                                                    │  │
│           │                                                             │  │
│           ▼                                                             │  │
│  ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐     │  │
│  │ control-server  │    │ zellij          │    │ docker.sock     │     │  │
│  │ :7432           │    │ (human here)    │◄───┤                 │     │  │
│  │ (docker-ctl)    │    │                 │    │                 │     │  │
│  └────────┬────────┘    └────────┬────────┘    └─────────────────┘     │  │
│           │                      │                                     │  │
│           │ creates/calls        │ attaches                            │  │
│           ▼                      ▼                                     │  │
│  ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐     │  │
│  │ exomonad-tl     │    │ exomonad-pm     │    │ subagent-1      │     │  │
│  │ Claude Code     │    │ Claude Code     │    │ Claude Code     │◄────┘  │
│  │ (Tech Lead)     │    │ (PM)            │    │ (spawned)       │        │
│  └─────────────────┘    └─────────────────┘    └─────────────────┘        │
│                                                                             │
│  SHARED VOLUMES:                                                           │
│  ├── exomonad-worktrees    (git worktrees, /worktrees in containers)       │
│  ├── exomonad-sockets      (Unix sockets, /sockets in containers)          │
│  ├── exomonad-gh-auth      (GitHub CLI auth, ~/.config/gh)                 │
│  ├── exomonad-zellij       (Zellij session socket, /run/user/1000)         │
│  └── exomonad-claude-{tl,pm} (Claude Code state, separate per agent)       │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Haskell Effect Integration

Control-server calls docker-ctl via an effect interpreter:

```haskell
-- Effect type (dsl/core)
data DockerSpawner m a where
  SpawnContainer :: SpawnConfig -> DockerSpawner m ContainerId
  ExecContainer  :: ContainerId -> [Text] -> DockerSpawner m ExecResult
  StopContainer  :: ContainerId -> DockerSpawner m ()

-- Interpreter (control-server/Effects/DockerCtl.hs)
runDockerCtl :: FilePath -> Eff (DockerSpawner ': es) a -> Eff es a
runDockerCtl binPath = interpret $ \case
  SpawnContainer cfg -> liftIO $ readProcess binPath ["spawn", ...] ""
  ExecContainer id cmd -> liftIO $ readProcess binPath ["exec", id, "--", cmd] ""
  StopContainer id -> liftIO $ readProcess binPath ["stop", id] ""
```

### TUI Cross-Container Communication

For popups (confirm_action, select_option), we use:

```
control-server                    tui-spawner                  tui-popup
     │                                 │                            │
     │ subprocess spawn                │                            │
     │────────────────────────────────▶│                            │
     │                                 │ mkfifo /tmp/popup-xyz      │
     │                                 │ zellij new-pane --floating │
     │                                 │────────────────────────────▶│
     │                                 │                            │ render to /dev/tty
     │                                 │                            │ user clicks button
     │                                 │◀────────────────────────────│ write result to FIFO
     │◀────────────────────────────────│ read FIFO, return          │
     │                                 │                            │
```

This is complex because Zellij doesn't have a native popup API. We spawn a floating pane that writes to a FIFO.

### Questions for Researcher

1. **CLI vs Service**: We replaced the older HTTP-based spawner with a CLI `docker-ctl`. Does this subprocess approach scale well for higher concurrency?

2. **Zellij limitations**: We work around Zellij's lack of popup API with FIFOs. Is there a better terminal multiplexer for this use case?

3. **Volume sharing patterns**: We share 5+ volumes across containers. Is there a standard pattern for multi-container dev environments?

4. **Bollard deprecation warnings**: The bollard crate has deprecated APIs we're using in `docker-ctl`. Should we switch to another Docker client?

5. **Single-binary alternative**: Could we collapse `docker-ctl` into `control-server`? The separation exists because Haskell Docker bindings are immature.

## What's NOT in the Deployed System

These exist in the repo but are not part of the current Docker deployment:

**Deployment Backends (frozen/alternative):**
- `deploy/` - Cloudflare Workers (WASM deployment, frozen)
- `haskell/runtime/wasm/` - WASM execution backend

**Alternative Servers/UIs:**
- `haskell/native-server/` - WebSocket server (replaced by control-server)
- `typescript/native-gui/` - Solid.js frontend

**Integrations (not wired up):**
- `typescript/telegram-bot/` - Telegram integration
- `haskell/effects/habitica-interpreter/` - Habitica integration

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
docker logs -f exomonad-control-server

# Rebuild and restart a service
docker compose build control-server && docker compose up -d control-server

# Clear BuildKit caches (fixes corrupted builds)
docker buildx prune --all -f

# GitHub auth (must use -u user)
docker exec -u user -it exomonad-control-server gh auth login

# Check GitHub auth status
docker exec exomonad-control-server gh auth status
```