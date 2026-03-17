# ExoMonad

Type-safe LLM agent orchestration. Haskell WASM defines all logic (tool schemas, handlers, decision trees). Rust executes I/O effects. tmux provides isolation and multiplexing. Agents are IO-blind state machines that yield typed effects.

---

## Rules

### Style

ALWAYS update CLAUDE.md files when you make changes. Adding new documentation is critical, as is removing stale documentation.

Comments should always focus on what is or will be. Never leave comments about why you deleted something, its in the git history which is enough.

The repository should be kept clean of dead code, placeholders, and half-done heuristics.

Always prefer failure to an undocumented heuristic or fallback.

### Single Code Path

Never maintain two code paths that do the same thing. Redundant paths cause bug risk — fixes applied to one path get missed on the other. If there's a "debug mode" or "legacy mode" that duplicates a primary path, cut it.

### All Tools and Hooks in Haskell WASM

**Never add direct Rust MCP tools.** All MCP tools and hooks are defined in Haskell WASM — tool schemas, argument parsing, dispatch logic, everything. Rust is the I/O runtime: it executes effects that the Haskell DSL yields. If a new tool needs new I/O capabilities, add a new effect handler in Rust and a corresponding effect type in Haskell. The tool itself lives in `haskell/wasm-guest/src/ExoMonad/Guest/Tools/`.

This is the entire architectural premise. Haskell WASM is the single source of truth for tool definitions. Rust never defines tool schemas, never parses tool arguments, never contains tool logic.

### Crosscutting Rules

When you learn something that applies to a crosscutting context (a programming language, a tool like git worktrees, a pattern that spans directories), **create or update a `.claude/rules/*.md` file** rather than documenting it in a directory-specific CLAUDE.md.

Examples: language idioms (`.claude/rules/haskell.md`, `.claude/rules/rust.md`), tool usage patterns (git, cabal, cargo, tmux), architectural patterns that span the codebase.

Rules files use YAML frontmatter to scope when they load:
```yaml
---
paths:
  - "**/*.hs"
---
```

### Logging

Silent failures are unacceptable. When code shells out to subprocesses, calls external services, or crosses process/container boundaries, **log aggressively**:

1. **Before the call**: Log what you're about to do (command, key parameters)
2. **After the call**: Log exit code, status, response size
3. **On error**: Log stderr, error messages, enough context to debug without reproducing
4. **On success**: Log the result summary (e.g., `button=submit`, `items=5`)

**Haskell pattern:**
```haskell
logInfo logger $ "[Component] Starting operation: " <> summary
(exitCode, stdout, stderr) <- readProcessWithExitCode cmd args ""
logInfo logger $ "[Component] Exit code: " <> T.pack (show exitCode)
case exitCode of
  ExitFailure code -> logError logger $ "[Component] FAILED: " <> T.pack stderr
  ExitSuccess -> logInfo logger $ "[Component] Success: " <> resultSummary
```

**Rust pattern:**
```rust
tracing::info!("Executing: {} {}", cmd, args.join(" "));
let status = Command::new(cmd).args(&args).status()?;
tracing::info!("{} returned: {:?}", cmd, status);
if !status.success() {
    tracing::error!("{} failed with status: {}", cmd, status);
}
```

---

## Getting Started

### Session Entry Point

**`exomonad init` is THE entry point for development sessions.** It creates a tmux session with:
- **Server window**: Runs `exomonad serve` (the MCP server, binds to `.exo/server.sock`)
- **TL window**: Runs `nix develop` (where you launch `claude` or work directly)

The server must be running before Claude Code or Gemini can use MCP tools. Without it, every tool call fails. Init also writes `.mcp.json` (MCP server config) and `.claude/settings.local.json` (hooks and session settings).

```bash
cd exomonad/                  # Run from the project root
exomonad init                 # Creates tmux session, starts server
# Then in the TL window:
claude                        # MCP tools available immediately
```

Use `--recreate` to tear down and rebuild the session (e.g., after binary updates).

**Server management:**
```bash
exomonad reload                  # Clear WASM plugin cache (next call loads fresh from disk)
exomonad shutdown                # Gracefully shut down the running server
```

### MCP Registration

`exomonad init` automatically registers the Claude MCP server. For Gemini, register manually via stdio:
```bash
gemini mcp add exomonad --command "exomonad mcp-stdio"
```

### Zero-Config for Consuming Repos

After running `just install-all` (which installs WASM to `~/.exo/wasm/`), any project works out of the box:

```bash
cd ~/new-project && git init
exomonad init
# → Bootstraps .exo/config.toml (empty, all defaults)
# → Copies WASM from ~/.exo/wasm/ (global install)
# → Starts server, registers Claude MCP via .mcp.json
# → Creates tmux session
```

For custom roles, copy `.exo/roles/` and `.exo/lib/` from exomonad and `exomonad init` will build WASM from source instead.

### Building

```bash
# One-command install (recommended - uses debug build for fast iteration)
just install-all-dev

# Or install release build (optimized, slower compile)
just install-all

# WASM builds (two equivalent options)
just wasm-all                     # Build all WASM via nix
exomonad recompile --role devswarm # Build specific role's WASM via nix
# Both are standalone CLI commands — neither requires the server to be running.
# Output: .exo/wasm/wasm-guest-devswarm.wasm

# Rust sidecar only
cargo build -p exomonad

# Hot reload: server checks WASM mtime per tool call, so after recompile
# the next MCP call picks up the new WASM automatically.
# For immediate reload: `exomonad reload` clears the plugin cache explicitly.
```

**What `just install-all-dev` does:**
1. Builds devswarm WASM plugin via nix
2. Builds exomonad Rust binary (debug mode)
3. Copies binary to `~/.cargo/bin/exomonad`

**WASM build pipeline:**
1. Role configs in `.exo/roles/devswarm/` define tool composition per role (`TLRole.hs`, `DevRole.hs`)
2. `AllRoles.hs` registers all roles; `Main.hs` provides FFI exports
3. `cabal.project.wasm` lists the devswarm package alongside `wasm-guest` SDK
4. `just wasm-all` builds via `nix develop .#wasm -c wasm32-wasi-cabal build ...`
5. Compiled WASM copied to `.exo/wasm/wasm-guest-devswarm.wasm`
6. `exomonad serve` loads devswarm WASM from `.exo/wasm/` at runtime (hot reload via mtime check)

### Configuration

**Bootstrap:** `exomonad init` auto-creates `.exo/config.toml` (empty, all defaults) and `.gitignore` entries if missing. Works in any project directory. All fields are optional — auto-detection handles the common case. **Claude rules:** `exomonad init` copies `.exo/rules/exomonad.md` → `.claude/rules/exomonad.md` (if the template exists and the destination doesn't). Template resolution: project-local `.exo/rules/` → global `~/.exo/rules/`. This gives fresh Claude instances automatic knowledge of exomonad MCP tools.

```toml
# All fields below are optional — shown with their auto-detected defaults
default_role = "tl"          # auto-detected from .exo/roles/ if exactly one role exists
project_dir = "."
shell_command = "nix develop" # environment wrapper for TL tab + server
wasm_dir = ".exo/wasm"       # project-local (default), override for shared installs
wasm_name = "devswarm"       # auto-detected from .exo/roles/ if exactly one role exists
```

**Config hierarchy:**
- `config.toml` uses `default_role` (project-wide default)
- `config.local.toml` uses `role` (worktree-specific override)
- Resolution: `local.role > global.default_role`
- WASM: `wasm_dir` in config > `.exo/wasm/` (project-local)

**Hook configuration** is auto-generated in two places:
- **`exomonad init`**: Writes `.claude/settings.local.json` with all hooks (SessionStart, PreToolUse, etc.) for the root TL session
- **`fork_wave`**: Writes `.claude/settings.local.json` into each spawned Claude worktree

The `SessionStart` hook is critical — it registers the Claude session UUID in `ClaudeSessionRegistry`, which `fork_wave` reads to pass `--resume <uuid> --fork-session` for context inheritance. Without it, spawned subtrees start with no context.

Gemini agents get settings via `GEMINI_CLI_SYSTEM_SETTINGS_PATH` env var (NOT `.gemini/settings.json`).

**Claude Code settings help:** We have a Claude Code configuration specialist (preloaded with official documentation) available as an oracle for hook syntax, settings structure, MCP setup, and debugging.

---

## Capabilities

What you can do with exomonad right now, end-to-end.

### Orchestration

Spawn heterogeneous agent teams as a recursive tree:

- **`fork_wave`** — Fork N parallel Claude agents from your current conversation context, each in its own worktree. Children inherit the full context window and only need a slug + task. Requires clean git state (committed and pushed).
- **`spawn_leaf_subtree`** — Fork a Gemini agent into its own git worktree + tmux window. Gets dev role, files PR when done.
- **`spawn_workers`** — Spawn multiple Gemini agents as tmux panes in the parent's directory. Ephemeral (no branch, no worktree). Config in `.exo/agents/{name}/`.

**Agent Types:** `Claude` (🤖), `Gemini` (💎), `Shoal` (🌊). Shoal is for custom binary agents that connect via rmcp MCP client and receive notifications via HTTP-over-Unix-domain-socket at `.exo/agents/{name}/notify.sock`.

**Multi-WASM:** The server loads multiple WASM modules from `.exo/wasm/`. Convention: if `wasm-guest-{role}.wasm` exists, it's used for that role; otherwise falls back to `wasm-guest-{wasm_name}.wasm` (default). Drop a WASM file, it's available.

**Standalone repo mode:** `spawn_leaf_subtree` accepts `standalone_repo: true`. Instead of a git worktree (which shares `.git` with the parent), this creates a fresh `git init` repo. Claude's native project discovery treats the local `.git` as the boundary — the agent cannot traverse into the parent repository. Use this for information segmentation (e.g., enterprise customers with proprietary root-level IP).

**Branch naming:** `{parent_branch}.{slug}` (dot separator). PRs target parent branch, not main — merged via recursive fold up the tree.

**Identity:** Birth-branch as session ID (immutable, deterministic). Root TL = "root". Filesystem IS the registry — scan `.exo/worktrees/` and `.exo/agents/` to discover agents.

### Coordination

Push-based parallel worker coordination via **Claude Code Teams inbox**:

1. TL spawns workers and **returns** (no blocking wait)
2. Each worker gets `EXOMONAD_SESSION_ID` env var (parent's birth-branch)
3. When worker completes, it calls `notify_parent`
4. Server resolves parent agent from caller identity, writes to the parent's Teams inbox (`~/.claude/teams/{name}/inboxes/{inbox}.json`)
5. Claude Code's InboxPoller detects the new message and delivers it as a native `<teammate-message>` in the parent's conversation
6. TL sees the message and wakes up — no polling, no hacks

This is **native Claude Code Teams integration**. Messages from child agents arrive exactly like messages from Claude Code teammates — structured, attributed, and delivered through the official inbox mechanism. The TL doesn't poll, doesn't block, and doesn't parse raw text. It gets a proper teammate notification.

**Pipeline:** `notify_parent` → server resolves parent via `TeamRegistry` → `teams_mailbox::write_to_inbox()` → CC InboxPoller → `<teammate-message>` delivered to parent conversation.

**Bidirectional Messaging:** The `send_message` tool enables arbitrary bidirectional messaging between any exomonad-spawned agents, routing via Teams inbox, ACP, UDS, or tmux fallback depending on the target agent's type and connection status.

**Fallback:** If Teams inbox delivery fails (no team registered, inbox write error), falls back to tmux STDIN injection via buffer pattern (`load-buffer` + `paste-buffer`).

### PR Workflow

- **`file_pr`** — Create or update a PR for the current branch. Auto-detects base branch from dot-separated naming convention.
- **`merge_pr`** — Merge a child's PR (`gh pr merge` + `git fetch` for auto-rebase). TL role only.

### Built Infrastructure

| Feature | Status |
|---------|--------|
| **Teams inbox delivery** | **Live.** `notify_parent` → Teams inbox → native `<teammate-message>` in parent conversation. Full E2E verified. |
| **ACP messaging** (Gemini agents) | **Built.** Structured JSON-RPC messaging via Agent Client Protocol. `AcpRegistry` manages connections, `connect_and_prompt()` establishes ACP sessions. Delivery priority: Teams inbox → ACP prompt → HTTP-over-UDS → tmux STDIN. Vendor SDK patched for Send safety. |
| **HTTP-over-UDS delivery** (Shoal/custom agents) | **Built.** `notify_parent` → POST to `.exo/agents/{name}/notify.sock`. Fire-and-forget with 5s timeout. For custom binary agents that run their own HTTP server on a Unix socket. |
| **Event router** (tmux STDIN fallback) | Built. Fallback path: `notify_parent` → `inject_input` into parent pane via tmux buffer pattern. |
| **Event handlers** (WASM dispatch for world events) | **Built.** Third dispatch category alongside tools and hooks. GitHub poller calls `handle_event` on agent's PluginManager for PR review events (reviews, approvals, timeouts) and **sibling merge events**. Handlers return `EventAction` (InjectMessage, NotifyParent, NoAction). |
| **GitHub poller** (PR status → events) | Built. Background service polls PR/CI status, fires WASM event handlers, and injects notifications into agent panes. Tracks `first_seen`, `last_review_state`, and `notified_parent_timeout` per PR. |
| **OTel observability** | **Built.** Axum middleware auto-attributes every agent request span with `agent_id`, `agent.role`, `agent.parent`, `swarm.run_id`. `swarm.run_id` persisted to `.exo/run_id`, set as OTel resource attribute, propagated to children via env. Query all spans in a run: `resource.swarm.run_id = '{id}'`. Reconstruct spawn tree: `groupBy agent.parent, agent_id`. |
| **Coordination mutexes** | Built. In-memory `MutexRegistry` with FIFO wait queues, TTL auto-expiry, idempotent acquire. Effect-only (`coordination.acquire_mutex`, `coordination.release_mutex`) — no MCP tool exposed. |
| **Tempo observability** | **Built.** Grafana Tempo for lightweight trace storage (~100-200MB RAM). Agents query traces via `curl` + TraceQL against Tempo's HTTP API (port 3200). Optional Grafana UI at `http://localhost:3000`. |

### Tempo Observability

Grafana Tempo provides lightweight trace storage with TraceQL query support. Agents query traces directly via `curl` against Tempo's HTTP API — no MCP tools needed.

```bash
# Start Tempo
docker compose -f .exo/otel/docker-compose.yml up -d

# Start Tempo + Grafana UI
docker compose -f .exo/otel/docker-compose.yml --profile grafana up -d

# Set otlp_endpoint in .exo/config.toml:
# otlp_endpoint = "http://localhost:4317"

# Endpoints:
#   OTLP:       localhost:4317 (gRPC), localhost:4318 (HTTP)
#   Tempo API:  http://localhost:3200 (TraceQL queries)
#   Grafana UI: http://localhost:3000 (optional, with --profile grafana)
```

**Querying traces (TraceQL via curl):**
```bash
# All spans in a run
curl -s 'http://localhost:3200/api/search?q=%7B+resource.swarm.run_id+%3D+%22abc%22+%7D&limit=50&spss=100'

# Find error spans for an agent
curl -s 'http://localhost:3200/api/search?q=%7B+span.agent_id+%3D+%22my-agent%22+%26%26+span%3Astatus+%3D+error+%7D'

# Parent-child structural query
curl -s 'http://localhost:3200/api/search?q=%7B+span.agent_id+%3D+%22tl%22+%7D+%3E%3E+%7B+span.agent_id+%3D+%22worker-1%22+%7D'

# Full trace by ID
curl -s 'http://localhost:3200/api/traces/{traceID}'
```

Without Tempo running, spans still appear in stderr via the tracing fmt layer.

---

## Architecture

### Components

```
Human in tmux session
    └── Claude Code + exomonad (Rust + Haskell WASM)
            ├── MCP tools via WASM (fork_wave, spawn_leaf_subtree, spawn_workers, etc.)
            └── Agent tree:
                ├── worktree: main.feature-a (TL role, can spawn children)
                │   ├── worker: rust-impl (Gemini, in-place pane)
                │   └── worker: haskell-impl (Gemini, in-place pane)
                └── worktree: main.feature-b (TL role)
                    └── ...
```

**Haskell WASM = Embedded DSL**
- Defines tool schemas, handlers, decision logic
- Yields typed effects (no I/O)
- Compiled to WASM32-WASI, loaded via Extism
- Single source of truth for MCP tools
- Hot reload: serve mode checks mtime per tool call

**Rust = Runtime**
- Hosts WASM plugin, executes all effects (git, GitHub API, filesystem, tmux)
- Owns the process lifecycle
- REST server on UDS (started by `exomonad init`), `mcp-stdio` translates MCP JSON-RPC to REST

**Worktrees + tmux = Isolation/Multiplexing**
- Git worktrees for code isolation (no Docker containers)
- tmux windows for Claude subtrees, panes for Gemini workers
- Each agent = worktree + window (or pane), managed by Rust runtime

### Data Flows

**MCP Tool Call:**
```
Claude Code → stdio (JSON-RPC) → exomonad mcp-stdio (translates JSON-RPC → REST)
→ UDS GET /agents/{role}/{name}/tools (list) or POST /agents/{role}/{name}/tools/call (call)
→ exomonad serve REST handler → WASM handle_list_tools / handle_mcp_call
→ Haskell dispatches to tool handler → yields effects
→ Rust executes effects via host functions → result returned
→ mcp-stdio translates REST response → JSON-RPC → stdout → Claude Code
```

**Hook Call:**
```
Claude Code → exomonad hook pre-tool-use (reads stdin JSON)
→ UDS request to server → WASM handle_pre_tool_use
→ Haskell decides allow/deny → HookEnvelope { stdout, exit_code }
→ Claude Code proceeds or blocks
```

**Session Start:**
```
Claude Code starts → exomonad hook session-start
→ WASM yields SessionRegister effect with claude_session_id
→ Server stores in ClaudeSessionRegistry
→ fork_wave uses this ID for --fork-session
```

**Event Handler Call:**
```
GitHub poller detects world event (Copilot review, CI status, timeout)
→ Poller resolves agent's PluginManager from plugins map
→ Calls WASM handle_event with { role, event_type, payload }
→ Haskell dispatches to EventHandlerConfig handler → returns EventAction
→ Rust acts on EventAction: InjectMessage (deliver to agent pane) or NotifyParent (deliver to parent)
```

**Fail-open:** If the server is unreachable, `exomonad hook` prints `{"continue":true}` and exits 0.

### MCP Tools Reference

All tools implemented in Haskell WASM (`haskell/wasm-guest/src/ExoMonad/Guest/Tools/`):

| Tool | Role | Description |
|------|------|-------------|
| `fork_wave` | tl | Fork N parallel Claude agents from current conversation context, each in its own worktree. Requires clean git state. |
| `spawn_leaf_subtree` | tl | Fork Gemini agent into worktree or standalone repo + tmux window (dev role, files PR). Supports `standalone_repo`. |
| `spawn_workers` | tl | Spawn Gemini agents as tmux panes (ephemeral, no worktree) |
| `file_pr` | tl, dev | Create/update PR (auto-detects base branch from naming) |
| `merge_pr` | tl | Merge child PR (gh merge + git fetch) |
| `notify_parent` | all | Send message to parent agent. Auto-routed via Teams inbox (primary) or tmux STDIN (fallback) |
| `send_message` | all | Send message to another exomonad-spawned agent (routes via Teams inbox, ACP, UDS, or tmux) |
| `shutdown` | dev, worker | Gracefully exit: notify parent, close own pane |

**Note**: Git operations (`git status`, `git log`, etc.) and GitHub operations (`gh pr list`, etc.) use the Bash tool with `git` and `gh` commands, not MCP tools.

---

## Documentation Tree

```
CLAUDE.md  ← YOU ARE HERE (project overview)
├── proto/CLAUDE.md    ← Protocol buffers (FFI boundary types)
├── haskell/CLAUDE.md  ← Haskell package organization
│   ├── wasm-guest/CLAUDE.md    ← MCP tool definitions (WASM guest logic)
│   └── proto/CLAUDE.md         ← Generated Haskell types for proto
├── rust/CLAUDE.md             ← Rust workspace overview (3 crates)
│   ├── exomonad/CLAUDE.md  ← MCP server + hook handler (binary)
│   ├── exomonad-core/CLAUDE.md ← Unified library: framework, handlers, services, protocol, UI types
│   └── exomonad-proto/     ← Proto-generated types (prost) for FFI + effects
└── docs/decisions/            ← Architecture decision records (living docs)
```

| I want to... | Read this |
|--------------|-----------|
| Add FFI boundary types | `proto/CLAUDE.md` |
| Understand MCP tool architecture | `rust/exomonad/CLAUDE.md` |
| Work on exomonad-core framework | `rust/exomonad-core/CLAUDE.md` |
| Work on effect handlers or services | `rust/exomonad-core/` (handlers/, services/) |
| Extend the effect framework | `rust/exomonad-core/` (effects/) |
| Understand shared protocol types | `rust/exomonad-core/` (protocol/) |
| Work with external service clients | `rust/exomonad-core/` (services/external/) |
| Work on WASM guest (MCP tools) | `haskell/wasm-guest/CLAUDE.md` |
| Understand architectural decisions | `docs/decisions/` |

---

## Developing ExoMonad

### Package Inventory

All Haskell packages live under `haskell/`. See `haskell/CLAUDE.md` for full details.

| Package | Purpose |
|---------|---------|
| `haskell/wasm-guest` | WASM guest with MCP tool definitions (freer-simple) |
| `haskell/proto` | Generated Haskell proto types |
| `haskell/vendor/ginger` | Typed Jinja templates (vendored) |
| `haskell/vendor/freer-simple` | Effect system (vendored, GHC 9.12 patches) |
| `haskell/vendor/exomonad-pdk` | Extism PDK (vendored) |
| `haskell/vendor/proto3-runtime` | Protobuf runtime (vendored) |

### Where Things Go

| Thing | Location |
|-------|----------|
| New MCP tool | `haskell/wasm-guest/src/ExoMonad/Guest/Tools/` |
| New WASM effect | `haskell/wasm-guest/src/ExoMonad/Guest/Effects/` |
| New Rust effect handler | `rust/exomonad-core/src/handlers/` |
| New proto type | `proto/` + `rust/exomonad-proto/proto/` |
| New event handler | `haskell/wasm-guest/src/ExoMonad/Guest/Events.hs` (types), `.exo/lib/` (handlers) |

### Building & Testing

```bash
cabal build all            # Build Haskell
cargo test --workspace     # Rust tests (from repo root)
just pre-commit            # Run all checks
cabal test all             # Haskell tests
```

### Task Tracking

GitHub Issues. Branch naming: `gh-{number}/{description}`. Reference issue in commits (`[#123] ...`). Issues closed via PR merges (`Closes #123`).

### Key Design Decisions

1. **freer-simple for effects** — Standardized on freer-simple for reified continuations (WASM yield/resume)
2. **IO-blind agents** — All IO in Rust runners, enables WASM + deterministic testing
3. **Haskell WASM = embedded DSL** — All logic in Haskell, Rust handles I/O only

---

## Tech Lead Praxis

How to coordinate heterogeneous agent teams. The TL is a compiler: it transforms high-level intent into leaf-executable specs, then gets out of the way.

### Intelligence Gradient

Claude (Opus) decomposes and dispatches. Gemini implements. Copilot reviews. The TL never implements directly and never manually reviews intermediate output.

**Cost model:** Opus tokens are 10-30x Gemini tokens. Every line of code the TL writes is expensive code. Every review cycle the TL performs is an expensive review cycle. The TL's job is producing specs sharp enough that the leaf + Copilot convergence loop handles quality without TL involvement.

### Fire-and-Forget Execution

The TL's workflow is: **decompose → spec → spawn → move on**. The TL does not wait, poll, review intermediate output, or re-spec. It spawns all leaves it can, then idles until messages arrive.

**Convergence is leaf + Copilot + event handlers, not TL:**
1. TL writes spec, spawns leaf (Gemini), returns immediately
2. Leaf works → commits → files PR
3. GitHub poller detects Copilot review comments → fires `handle_event(PRReview::ReviewReceived)` → handler injects comments into leaf's pane
4. Leaf reads Copilot feedback, fixes, pushes
5. Poller detects SHA change after `ChangesRequested` → fires `handle_event(PRReview::FixesPushed)` → handler sends `[FIXES PUSHED]` to TL
6. TL sees `[from: leaf-id] [FIXES PUSHED] PR #N — CI passing. Ready to merge.` → merges the PR

**Note:** Copilot's first review is automatic (triggered on PR creation). Subsequent reviews after pushing fixes are NOT — Copilot does not re-review. The `FixesPushed` event is the system's signal that the iteration loop completed.

**Alternative paths:**
- **Copilot approves first time** → poller fires `handle_event(PRReview::Approved)` → handler sends `[PR READY]` to TL → TL merges
- **No Copilot review after timeout** → poller fires `handle_event(PRReview::ReviewTimeout)` → handler sends `[REVIEW TIMEOUT]` to TL → TL merges if CI passes (15 min initial, 5 min after addressing changes)
- **Leaf sends status updates** → `notify_parent` delivers `[from: leaf-id] message` to TL → informational, TL reads but does not auto-merge
- **Leaf fails** → `notify_parent` with `failure` status → delivers `[FAILED: leaf-id] message` to TL → TL re-decomposes

**Escalation, not iteration.** If a leaf fails after 3+ Copilot rounds, it calls `notify_parent` with `failure` status. The TL then decides: re-decompose, try a different approach, or flag for human intervention. The TL never manually fixes a leaf's code.

### Spec Quality (You Only Get One Shot)

Since the TL doesn't iterate on specs, the v1 spec must be production-quality. Every spec follows this structure:

```
1. ANTI-PATTERNS      — Known Gemini failure modes as explicit "DO NOT" rules (FIRST)
2. READ FIRST         — Exact files to read (CLAUDE.md, source files, proto files)
3. STEPS              — Numbered, each step = one concrete action with code snippets
4. VERIFY             — Exact build/test commands with env vars (PROTOC path, etc.)
5. DONE CRITERIA      — What "done" looks like (tests pass, PR filed)
```

**Anti-patterns section is mandatory and comes first.** These are known Gemini failure modes — front-load them so the agent reads them before touching code:

| Known Failure Mode | Anti-Pattern Rule |
|---|---|
| Adds unnecessary dependencies | "ZERO external dependencies. Do NOT add serde/tokio/etc." |
| Invents escape hatches | "No `todo!()`, `Raw(String)`, `Other(Box<dyn Any>)` variants" |
| Writes thinking-out-loud comments | "No stream-of-consciousness comments. Doc comments only." |
| Renames types/variants to "simpler" names | "Use EXACT type signatures below. Do not rename." |
| Makes architectural decisions | "Do not change the module structure. Files listed below are exhaustive." |
| Overengineers | "This is N lines in M files, not a new module/framework." |

**Key rules:**
- **One agent = one focused change.** If it touches >3 files or requires architectural decisions, split it.
- **Include complete code.** Don't describe what to write — show the exact code. Gemini executes better from examples than descriptions.
- **Include exact commands.** Not "run the tests" but `PROTOC=/nix/store/... cargo test --workspace`.
- **Name every file.** Not "update the proto" but "edit `proto/effects/agent.proto` AND `rust/exomonad-proto/proto/effects/agent.proto`".
- **Specs are self-contained.** The leaf has no context from previous attempts. Every spec must stand alone with complete code snippets and full file paths.

### Parallelization

Spawn multiple leaves when tasks are independent (no file conflicts, no ordering dependency). The TL spawns a wave, returns, and gets poked as each leaf completes. Examples:
- Proto plumbing + nix shell wrapping — independent, parallel.
- Haskell tool changes + Rust handler changes — often dependent (proto-gen first), sequential.

### When TL Gets Notified

The TL is idle between spawning and receiving notifications. It wakes up for:
- **`[FIXES PUSHED]`** (event handler) — leaf addressed Copilot review comments and pushed fixes. Copilot does NOT re-review, so this is the actionable signal. TL merges if CI passes.
- **`[PR READY]`** (event handler) — Copilot approved a leaf's PR on first review. TL merges and verifies the result builds cleanly. Multiple leaves landing in parallel may interact.
- **`[REVIEW TIMEOUT]`** (event handler) — no Copilot review after timeout (15 min initial, 5 min after addressing changes). TL merges if CI passes.
- **`[from: agent-id]`** (agent message) — informational update from a leaf. Do not auto-merge; read the message.
- **`[FAILED: agent-id]`** — leaf exhausted retries. TL re-decomposes or escalates.
- GitHub poller notifications (CI status, PR merge conflicts).

The TL does NOT wake up for intermediate progress, Copilot comments, or partial results. The convergence loop (leaf + Copilot) runs without TL involvement.

---

## References

- [rust/exomonad/CLAUDE.md](rust/exomonad/CLAUDE.md) — MCP server + WASM host
- [haskell/wasm-guest/CLAUDE.md](haskell/wasm-guest/CLAUDE.md) — MCP tool definitions
- [freer-simple](https://hackage.haskell.org/package/freer-simple) — Effect system
- [Anthropic tool use](https://docs.anthropic.com/en/docs/tool-use)
