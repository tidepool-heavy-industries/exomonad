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

### SINGLE CODE PATH

Never maintain two code paths that do the same thing. Redundant paths cause bug risk — fixes applied to one path get missed on the other. If there's a "debug mode" or "legacy mode" that duplicates a primary path, cut it.

Concrete example: if you find two code paths doing the same dispatch (e.g., "direct Rust tools" alongside WASM routing), delete the redundant one.

### SESSION ENTRY POINT

**`exomonad init` is THE entry point for development sessions.** It creates a Zellij session with:
- **Server tab**: Runs `exomonad serve --port 7432` (the HTTP MCP server, required for all tool calls)
- **TL tab**: Runs `nix develop` (where you launch `claude` or work directly)

The server must be running before Claude Code or Gemini can use MCP tools. Without it, every tool call fails. `exomonad init` ensures this by making the server tab part of the session layout.

```bash
cd exomonad/                  # Run from the project root
exomonad init                 # Creates Zellij session, starts server
# Then in the TL tab:
claude                        # MCP tools available immediately
```

Use `--recreate` to tear down and rebuild the session (e.g., after binary updates).
Use `--port` to override the default port (7432).

MCP registration is separate — use `claude mcp add` / `gemini mcp add` once per project:
```bash
claude mcp add --transport http exomonad http://localhost:7432/tl/mcp
gemini mcp add --transport http exomonad http://localhost:7432/tl/mcp
```

### ALL MCP TOOLS AND HOOKS MUST BE DEFINED IN HASKELL DSL

**Never add direct Rust MCP tools.** All MCP tools and hooks are defined in Haskell WASM — tool schemas, argument parsing, dispatch logic, everything. Rust is the I/O runtime: it executes effects that the Haskell DSL yields. If a new tool needs new I/O capabilities, add a new effect handler in Rust and a corresponding effect type in Haskell. The tool itself lives in `haskell/wasm-guest/src/ExoMonad/Guest/Tools/`.

This is the entire architectural premise. Haskell WASM is the single source of truth for tool definitions. Rust never defines tool schemas, never parses tool arguments, never contains tool logic.

### CROSSCUTTING RULES

When you learn something that applies to a crosscutting context (a programming language, a tool like git worktrees, a pattern that spans directories), **create or update a `.claude/rules/*.md` file** rather than documenting it in a directory-specific CLAUDE.md.

Examples of crosscutting concerns:
- Language idioms (`.claude/rules/haskell.md`, `.claude/rules/rust.md`)
- Tool usage patterns (git, cabal, cargo, zellij)
- Architectural patterns that span the codebase

Rules files use YAML frontmatter to scope when they load:
```yaml
---
paths:
  - "**/*.hs"
---
```

Keep rules files focused and concise. If a rule only applies to one directory, put it in that directory's CLAUDE.md instead.

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
├── proto/CLAUDE.md    ← Protocol buffers (FFI boundary types)
├── haskell/CLAUDE.md  ← Haskell package organization
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
├── rust/CLAUDE.md             ← Rust workspace overview (3 crates)
│   ├── exomonad/CLAUDE.md  ← MCP server + hook handler (binary)
│   ├── exomonad-core/      ← Unified library: framework, handlers, services, protocol, UI types
│   ├── exomonad-proto/     ← Proto-generated types (prost) for FFI + effects
│   └── exomonad-plugin/CLAUDE.md   ← Zellij WASM plugin (status + popups)
└── tools/CLAUDE.md             ← Root-level tools (micro-gastown, blast-radius)
```

## When to Read Which CLAUDE.md

| I want to... | Read this |
|--------------|-----------|
| Add FFI boundary types | `proto/CLAUDE.md` |
| Understand MCP tool architecture | `rust/exomonad/CLAUDE.md` |
| Work on effect handlers or services | `rust/exomonad-core/` (handlers/, services/) |
| Extend the effect framework | `rust/exomonad-core/` (effects/) |
| Understand shared protocol types | `rust/exomonad-core/` (protocol/) |
| Work with external service clients | `rust/exomonad-core/` (services/external/) |
| Modify popup UI protocol | `rust/exomonad-core/` (ui_protocol.rs) |
| Work on Zellij plugin | `rust/exomonad-plugin/CLAUDE.md` |
| Define a graph, handlers, annotations | `haskell/dsl/core/CLAUDE.md` |
| Work on LLM-level teaching infrastructure | `haskell/dsl/teaching/CLAUDE.md` |
| Add or modify an effect interpreter | `haskell/effects/CLAUDE.md` |
| Understand actor execution model | `haskell/runtime/actor/CLAUDE.md` |
| Work with LSP (Language Server Protocol) | `haskell/effects/lsp-interpreter/CLAUDE.md` |
| Generate training data for FunctionGemma | `haskell/tools/training-generator/CLAUDE.md` |

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

### WASM (Production - Frozen)
Compile to WASM.

```bash
just wasm tl
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

**Haskell WASM = Embedded DSL**
- Defines tool schemas, handlers, decision logic
- Yields typed effects (no I/O)
- Compiled to WASM32-WASI
- Single source of truth for MCP tools

**Rust = Runtime**
- Hosts WASM plugin via Extism
- Executes all effects (git, GitHub API, filesystem, Zellij)
- Owns the process lifecycle
- MCP server (HTTP, started by `exomonad init`)

**Worktrees + Zellij = Isolation/Multiplexing**
- Git worktrees for code isolation (no Docker containers)
- Zellij tabs within the enclosing session
- Each agent = worktree + tab, managed by Rust runtime

```
Human in Zellij session
    └── Claude Code + exomonad (Rust + Haskell WASM)
            ├── MCP tools via WASM (git_branch, spawn_agents, etc.)
            └── Spawn agents:
                ├── worktree: ./agents/issue-123
                │   └── Zellij tab: "123-fix-bug"
                └── worktree: ./agents/issue-456
                    └── Zellij tab: "456-add-feature"
```

### Data Flow

**MCP Tool Call:**
```
1. User asks question in Claude Code
2. Claude plans to call MCP tool (e.g., spawn_agents, git_branch)
3. Claude Code sends request to exomonad (stdio)
4. Rust calls WASM handle_mcp_call
5. Haskell dispatches to tool handler
6. Handler yields effects (GitGetBranch, SpawnAgent, etc.)
7. Rust executes effects via host functions
8. Result returned to Claude Code
```

**Hook Call:**
```
1. Claude Code wants to call Write tool
2. Generates hook JSON on stdin
3. exomonad hook pre-tool-use reads stdin
4. Calls WASM handle_pre_tool_use
5. Haskell logic decides allow/deny
6. Returns HookOutput to stdout
7. Claude Code proceeds or blocks
```

### Configuration

**Need help with Claude Code settings?** We have a Claude Code configuration specialist (preloaded with official documentation) available as an oracle. When you have questions about:
- Hook configuration syntax (SessionStart, PreToolUse, etc.)
- Settings file structure and scope (project vs local vs user)
- MCP server setup
- Environment variables and integration
- Debugging hook execution

Ask the specialist directly instead of guessing. They have authoritative knowledge about Claude Code internals, hook lifecycle, and best practices.

Hook configuration is **auto-generated per worktree** by `write_context_files()` in `agent_control.rs` during `spawn_agents`. Each spawned Claude agent gets `.claude/settings.local.json` with PreToolUse, SubagentStop, and SessionEnd hooks. Gemini agents get `.gemini/settings.json` with AfterAgent hooks. Do not manually create hook settings — they are generated at spawn time.

**MCP server configuration:** Use CLI-native config commands (one-time setup):
```bash
claude mcp add --transport http exomonad http://localhost:7432/tl/mcp
gemini mcp add --transport http exomonad http://localhost:7432/tl/mcp
```

**Two WASM loading modes:**
- **Embedded** (`include_bytes!`): WASM compiled into binary. Used by `exomonad mcp-stdio` and `exomonad hook`. Role selected via config.
- **File-based** (`from_file`): WASM loaded from `.exomonad/wasm/`. Used by `exomonad serve`. Enables hot reload — mtime checked per tool call, new WASM swapped in transparently.

```toml
default_role = "tl"  # or "dev"
project_dir = "."
```

**Config hierarchy:**
- `config.toml` uses `default_role` (project-wide default)
- `config.local.toml` uses `role` (worktree-specific override)
- Resolution: `local.role > global.default_role`

### Building

```bash
# One-command install (recommended - uses debug build for fast iteration)
just install-all-dev

# Or install release build (optimized, slower compile)
just install-all

# WASM builds (hermetic via nix)
just wasm tl
just wasm dev

# Rust sidecar only
cargo build -p exomonad

# Hot reload workflow (HTTP serve mode, Unix socket)
exomonad serve                # Start server on .exomonad/server.sock
# ... edit .exomonad/roles/tl/Role.hs ...
exomonad recompile --role tl  # Rebuild WASM via nix, copy to .exomonad/wasm/
# Next tool call picks up new WASM automatically
```

**What `just install-all-dev` does:**
1. Builds both WASM plugins (tl and dev) via nix
2. Builds exomonad Rust binary (debug mode, embeds WASM via `build.rs` + `include_bytes!`)
3. Copies binary to `~/.cargo/bin/exomonad`
4. Builds and installs Zellij plugins

**WASM build pipeline:**
1. User-authored `Role.hs` in `.exomonad/roles/<role>/` defines tool composition
2. Generated `Main.hs` and `<role>.cabal` provide FFI scaffolding (gitignored)
3. `cabal.project.wasm` lists role packages alongside `wasm-guest` SDK
4. `just wasm <role>` builds via `nix develop .#wasm -c wasm32-wasi-cabal build ...`
5. Compiled WASM copied to `.exomonad/wasm/wasm-guest-<role>.wasm`
6. For embedded mode: `build.rs` copies WASM into Rust binary via `include_bytes!`
7. For serve mode: HTTP server loads directly from `.exomonad/wasm/` with hot reload

### MCP Tools

All tools are implemented in Haskell WASM (`haskell/wasm-guest/src/ExoMonad/Guest/Tools.hs`):

| Tool | Description |
|------|-------------|
| `git_branch` | Get current git branch |
| `git_status` | Get dirty files |
| `git_log` | Get recent commits |
| `read_file` | Read file contents |
| `github_list_issues` | List GitHub issues |
| `github_get_issue` | Get single issue details |
| `github_list_prs` | List GitHub pull requests |
| `spawn_agents` | Spawn agents (Claude/Gemini) in Zellij tabs with isolated git worktrees |
| `spawn_gemini_teammate` | Spawn a named Gemini teammate with a direct prompt (no GitHub issue required) |
| `cleanup_agents` | Clean up agent worktrees and close Zellij tabs |
| `list_agents` | List active agent worktrees |
| `claim_task` | Claim a task from the shared task list (Teams) |
| `complete_task` | Mark a task as completed (Teams) |
| `list_tasks` | List tasks from the shared task list (Teams) |
| `get_task` | Get task details by ID (Teams) |
| `report_status` | Report agent status to the team (Teams) |
| `ask_question` | Ask a question to the team lead (Teams) |
| `get_agent_messages` | Read notes and questions from agent outboxes (TL messaging) |
| `answer_question` | Answer a pending question from an agent (TL messaging) |

**How spawn_agents works:**
1. Creates git worktree: `.exomonad/worktrees/gh-{issue}-{title}-{agent}/`
2. Creates branch: `gh-{issue}/{title}-{agent}`
3. Writes `.exomonad/config.toml` (default_role="dev") and CLI-native MCP config (`.gemini/settings.json` for Gemini agents)
4. Builds initial prompt with full issue context
5. Creates Zellij tab using KDL layout with agent-specific command:
   - `claude --prompt '...'` for Claude agents
   - `gemini --prompt-interactive '...'` for Gemini agents
6. Tab auto-closes when agent exits (`close_on_exit true`)

### Status

- ✅ 100% WASM routing (all logic in Haskell, Rust handles I/O only)
- ✅ MCP stdio server for Claude Code
- ✅ Hook forwarding via WASM
- ✅ Embedded WASM (compile-time `include_bytes!`, no file paths)
- ✅ spawn_agents: Git worktrees + Zellij KDL layouts
- ✅ Proper Zellij tab creation with full UI (tab-bar, status-bar)
- ✅ Shell-wrapped commands for environment inheritance
- ✅ GitHub API integration with token from ~/.exomonad/secrets
- ✅ Auto-cleanup tabs on agent exit
- ✅ Zellij plugin (exomonad-plugin) for status display and popup UI
- ✅ KDL layout generation (zellij-gen) for proper environment inheritance
- ✅ Stop hook logic (SubagentStop, SessionEnd) - validates uncommitted changes, unpushed commits, PR status, Copilot review
- ✅ Claude Code Teams integration (synthetic teammate registration via `spawn_gemini_teammate`)
- ✅ Gemini MCP wiring (`.gemini/settings.json` with server URL on spawn)

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
| `haskell/effects/llm-interpreter` | Anthropic/OpenAI API calls |
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

### Tools (`haskell/tools/`)
| Package | Purpose |
|---------|---------|
| `haskell/tools/ghci-oracle` | Persistent GHCi session server |
| `haskell/tools/sleeptime` | Log analysis for agent evolution |
| `haskell/tools/training-generator` | Training data types for FunctionGemma |
| `tools/micro-gastown` | Experimental tooling (non-Haskell) |

## Where Things Go

| Thing | Location |
|-------|----------|
| New effect type | `haskell/dsl/core/src/ExoMonad/Effect/Types.hs` |
| New integration | `haskell/dsl/core/src/ExoMonad/Effects/` (plural) |
| New graph annotation | `haskell/dsl/core/src/ExoMonad/Graph/Types.hs` |
| New interpreter | `haskell/effects/<name>-interpreter/` |
| New MCP tool | `haskell/wasm-guest/src/ExoMonad/Guest/Tools.hs` |
| Agents (consuming repos) | Separate repo (urchin, etc.) |

### Naming Conventions
- **Effect** (singular) = core infrastructure (`ExoMonad.Effect.*`)
- **Effects** (plural) = integrations/contrib (`ExoMonad.Effects.*`)
- **Interpreter** = effect implementation (replaces "executor" terminology)

## Tech Lead Praxis

How to coordinate heterogeneous agent teams effectively. These are proven patterns from real sessions, not theory.

### Intelligence Gradient

Claude (Opus) reasons and coordinates. Gemini implements with clear instructions. The TL never implements directly — it decomposes, specs, spawns, reviews.

**Cost model:** Opus tokens are 10-30x Gemini tokens. Every line of code the TL writes directly is expensive code. The TL's job is producing *specs* that make Gemini's output correct on the first try.

### Spawn Prompt Structure

Gemini agents are cheap junior devs. They execute pre-decomposed work, they don't decompose it. Every spawn prompt follows this structure:

```
1. READ FIRST        — List exact files to read (CLAUDE.md, source files, proto files)
2. STEPS             — Numbered, each step = one concrete action with code snippets
3. VERIFY            — Exact build/test commands with env vars (PROTOC path, etc.)
4. DONE CRITERIA     — Acceptance tests: what "done" looks like
5. BOUNDARY          — "Do NOT commit" / "Do NOT push" — TL controls merge
```

**Key rules:**
- **One agent = one focused change.** If it touches >3 files or requires architectural decisions, split it.
- **Include code snippets.** Don't describe what to write — show it. Gemini executes better from examples than descriptions.
- **Include exact commands.** Not "run the tests" but `PROTOC=/nix/store/... cargo test --workspace`. Env vars, flags, paths — all explicit.
- **Name the files.** Not "update the proto" but "edit `proto/effects/agent.proto` AND `rust/exomonad-proto/proto/effects/agent.proto`".

### Parallelization

Spawn multiple agents when tasks are independent (no file conflicts, no ordering dependency). Examples:
- Proto plumbing (touches proto/, haskell/proto/, rust/) + nix shell wrapping (touches rust/services/) — independent, parallel.
- Haskell tool changes + Rust handler changes — often dependent (proto-gen must run first), sequential.

### Review Protocol

When an agent reports done:
1. `git diff --stat HEAD` — see what changed
2. `diff` vendor copies (proto files must be byte-identical)
3. Read the actual changes, not just the summary
4. Run `cargo check` / `cargo test` yourself — don't trust "tests pass" claims
5. Check for prost struct literal completeness (new fields must appear in ALL construction sites)

### Anti-Patterns

| Don't | Do Instead |
|-------|------------|
| Vague task: "implement the identity system" | Specific: "add `topology` field to 4 proto messages, regenerate, plumb defaults" |
| Let agent make architectural decisions | Make the decision in the spec, agent executes |
| Trust "it compiles" without verification | Run checks yourself from TL session |
| Spawn in plan mode | Plan mode gates every write. Spawn in default mode with plan as context |
| Give short directives ("go") | Full context in one shot: files, format, examples, commands |
| Let agent claim unsourced constraints | Demand: "WHERE did you learn this? Show the code/docs" |

### Agent Supervision

Agents hallucinate confidently about infrastructure constraints. "axum nest_service doesn't support dynamic segments" — stated as fact, nearly steered a design decision, turned out to be wrong. **Always demand sources for infrastructure claims.**

Agents overengineer when unsupervised. Given "add a route with a path extractor" an agent built a full tower middleware stack. **Specify the complexity budget:** "this is 10 lines in an existing function, not a new module."

### Teams Strategy: Mirror Signatures, Own the Backend

Claude Code Teams provides coordination primitives (TaskCreate, TaskUpdate, SendMessage) that Claude models are trained on. ExoMonad's strategy:

- **Mirror the tool signatures** — same tool names, same arg shapes, same response shapes. The model's training transfers.
- **Own the backend** — ExoMonad's singleton MCP server is the coordination bus. State lives there, not in `~/.claude/teams/` filesystem.
- **Route around jank** — Teams' UX (idle spam, shutdown ceremony, plan mode death spiral) is tolerated for bus access. Our WASM layer is where compensating logic lives — typed, hot-reloadable, not buried in prompts.

The bet: Anthropic iterates on Teams primitives, we get upgrades for free. When they don't fix something, the crash cage handles it.

## Task Tracking (GitHub)

Task tracking via GitHub Issues.

### Workflow

1.  **Branching**: Use the `gh-{number}/{description}` naming convention for all task-related branches.
2.  **Development**: Implement changes incrementally.
3.  **Commits**: Reference issue number in commit messages (e.g. `[#123] ...`).
4.  **Closing**: Issues are closed via PR merges ("Closes #123").

## Building & Testing

```bash
cabal build all            # Build Haskell
cargo test --workspace     # Rust tests (from repo root)
just pre-commit            # Run all checks
cabal test all             # Haskell tests
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
5. **Haskell WASM = embedded DSL** - All logic in Haskell, Rust handles I/O only


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
- [rust/exomonad/CLAUDE.md](rust/exomonad/CLAUDE.md) - MCP server + WASM host
- [freer-simple](https://hackage.haskell.org/package/freer-simple) - Effect system
- [Anthropic tool use](https://docs.anthropic.com/en/docs/tool-use)
