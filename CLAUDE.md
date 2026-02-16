# ExoMonad

Type-safe LLM agent orchestration. Haskell WASM defines all logic (tool schemas, handlers, decision trees). Rust executes I/O effects. Zellij provides isolation and multiplexing. Agents are IO-blind state machines that yield typed effects.

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

Examples: language idioms (`.claude/rules/haskell.md`, `.claude/rules/rust.md`), tool usage patterns (git, cabal, cargo, zellij), architectural patterns that span the codebase.

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

**`exomonad init` is THE entry point for development sessions.** It creates a Zellij session with:
- **Server tab**: Runs `exomonad serve --port 7432` (the HTTP MCP server, required for all tool calls)
- **TL tab**: Runs `nix develop` (where you launch `claude` or work directly)

The server must be running before Claude Code or Gemini can use MCP tools. Without it, every tool call fails.

```bash
cd exomonad/                  # Run from the project root
exomonad init                 # Creates Zellij session, starts server
# Then in the TL tab:
claude                        # MCP tools available immediately
```

Use `--recreate` to tear down and rebuild the session (e.g., after binary updates).
Use `--port` to override the default port (7432).

### MCP Registration

One-time setup per project:
```bash
claude mcp add --transport http exomonad http://localhost:7432/tl/mcp
gemini mcp add --transport http exomonad http://localhost:7432/tl/mcp
```

### Building

```bash
# One-command install (recommended - uses debug build for fast iteration)
just install-all-dev

# Or install release build (optimized, slower compile)
just install-all

# WASM builds (hermetic via nix)
just wasm-all

# Rust sidecar only
cargo build -p exomonad

# Hot reload workflow (HTTP serve mode)
exomonad serve                # Start server
# ... edit .exo/roles/tl/Role.hs ...
exomonad recompile --role tl  # Rebuild WASM via nix, copy to .exo/wasm/
# Next tool call picks up new WASM automatically
```

**What `just install-all-dev` does:**
1. Builds unified WASM plugin via nix
2. Builds exomonad Rust binary (debug mode)
3. Copies binary to `~/.cargo/bin/exomonad`
4. Builds and installs Zellij plugins

**WASM build pipeline:**
1. User-authored `Role.hs` in `.exo/roles/<role>/` defines tool composition
2. Generated `Main.hs` and `<role>.cabal` provide FFI scaffolding (gitignored)
3. `cabal.project.wasm` lists role packages alongside `wasm-guest` SDK
4. `just wasm <role>` builds via `nix develop .#wasm -c wasm32-wasi-cabal build ...`
5. Compiled WASM copied to `.exo/wasm/wasm-guest-<role>.wasm`
6. Both `exomonad hook` and `exomonad serve` load WASM from `.exo/wasm/` at runtime

### Configuration

**Bootstrap:** `exomonad init` auto-creates `.exo/config.toml` and `.gitignore` entries if missing. Works in any project directory.

```toml
default_role = "tl"  # or "dev"
project_dir = "."
shell_command = "nix develop"  # optional: environment wrapper for TL tab + server
wasm_dir = ".exo/wasm"    # optional: override WASM location (default: ~/.exo/wasm/)
```

**Config hierarchy:**
- `config.toml` uses `default_role` (project-wide default)
- `config.local.toml` uses `role` (worktree-specific override)
- Resolution: `local.role > global.default_role`
- WASM: `wasm_dir` in config > `~/.exo/wasm/`

**Hook configuration** is auto-generated per worktree by `write_context_files()` in `agent_control.rs` during agent spawning. Each spawned Claude agent gets `.claude/settings.local.json` with PreToolUse, SubagentStop, and SessionEnd hooks. Gemini agents get settings via `GEMINI_CLI_SYSTEM_SETTINGS_PATH` env var (NOT `.gemini/settings.json`). Do not manually create hook settings.

**Claude Code settings help:** We have a Claude Code configuration specialist (preloaded with official documentation) available as an oracle for hook syntax, settings structure, MCP setup, and debugging.

---

## Capabilities

What you can do with exomonad right now, end-to-end.

### Orchestration

Spawn heterogeneous agent teams as a recursive tree:

- **`spawn_subtree`** — Fork a Claude agent into its own git worktree + Zellij tab. Gets TL role (can spawn its own children). Depth-capped at 2.
- **`spawn_leaf_subtree`** — Fork a Gemini agent into its own git worktree + Zellij tab. Gets dev role, files PR when done.
- **`spawn_workers`** — Spawn multiple Gemini agents as Zellij panes in the parent's directory. Ephemeral (no branch, no worktree). Config in `.exo/agents/{name}/`.

**Branch naming:** `{parent_branch}.{slug}` (dot separator). PRs target parent branch, not main — merged via recursive fold up the tree.

**Identity:** Birth-branch as session ID (immutable, deterministic). Root TL = "root". Filesystem IS the registry — scan `.exo/worktrees/` and `.exo/agents/` to discover agents.

### Coordination

Push-based parallel worker coordination via Zellij STDIN injection:

1. TL spawns workers and **returns** (no blocking wait)
2. Each worker gets `EXOMONAD_SESSION_ID` env var (parent's birth-branch)
3. When worker exits, session-end hook calls `notify_parent`
4. Server resolves parent tab from caller identity, injects `[CHILD COMPLETE: agent-id] message` into parent's Zellij pane
5. TL sees the injected text as a new user message and wakes up

The TL does not poll or block. It finishes its turn after spawning, and gets poked by the system when children complete. This is true push notification via `inject_input` through the Zellij plugin pipe.

### Messaging

Inter-agent communication between TL and dev agents:

- **`note`** — Dev sends async note to TL (fire-and-forget)
- **`question`** — Dev asks TL a question and blocks until answer arrives (trampoline architecture via `suspend`/`resume`)
- **`get_agent_messages`** — TL reads notes and questions from agent outboxes (supports long-poll)
- **`answer_question`** — TL answers a pending question, unblocking the dev agent

### PR Workflow

- **`file_pr`** — Create or update a PR for the current branch. Auto-detects base branch from dot-separated naming convention.
- **`merge_pr`** — Merge a child's PR (`gh pr merge` + `jj git fetch` for auto-rebase). TL role only.

### Interactive UI

- **`popup`** — Display interactive popup in Zellij (choices, text input, checkboxes, sliders, multiselect). Rendered by the exomonad-plugin.

### Built Infrastructure

| Feature | Status |
|---------|--------|
| **Event router** (Zellij STDIN injection) | Built. `notify_parent` → `inject_input` into parent pane via Zellij plugin pipe. |
| **GitHub poller** (PR status → events) | Built. Background service polls PR/CI status and injects notifications into agent panes. |

---

## Architecture

### Components

```
Human in Zellij session
    └── Claude Code + exomonad (Rust + Haskell WASM)
            ├── MCP tools via WASM (spawn_subtree, spawn_leaf_subtree, spawn_workers, file_pr, etc.)
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
- Hosts WASM plugin, executes all effects (git, GitHub API, filesystem, Zellij)
- Owns the process lifecycle
- MCP server (HTTP, started by `exomonad init`)

**Worktrees + Zellij = Isolation/Multiplexing**
- Git worktrees for code isolation (no Docker containers)
- Zellij tabs for Claude subtrees, panes for Gemini workers
- Each agent = worktree + tab (or pane), managed by Rust runtime

### Data Flows

**MCP Tool Call:**
```
Claude Code → HTTP request → exomonad serve → WASM handle_mcp_call
→ Haskell dispatches to tool handler → yields effects
→ Rust executes effects via host functions → result returned
```

**Hook Call:**
```
Claude Code → exomonad hook pre-tool-use (reads stdin JSON)
→ HTTP POST to server → WASM handle_pre_tool_use
→ Haskell decides allow/deny → HookEnvelope { stdout, exit_code }
→ Claude Code proceeds or blocks
```

**Session Start:**
```
Claude Code starts → exomonad hook session-start
→ WASM yields SessionRegister effect with claude_session_id
→ Server stores in ClaudeSessionRegistry
→ spawn_subtree uses this ID for --fork-session
```

**Fail-open:** If the server is unreachable, `exomonad hook` prints `{"continue":true}` and exits 0.

### MCP Tools Reference

All tools implemented in Haskell WASM (`haskell/wasm-guest/src/ExoMonad/Guest/Tools/`):

| Tool | Description |
|------|-------------|
| `spawn_subtree` | Fork Claude agent into worktree + Zellij tab (TL role, can spawn children) |
| `spawn_leaf_subtree` | Fork Gemini agent into worktree + Zellij tab (dev role, files PR) |
| `spawn_workers` | Spawn Gemini agents as Zellij panes (ephemeral, no worktree) |
| `file_pr` | Create/update PR (auto-detects base branch from naming) |
| `merge_pr` | Merge child PR (gh merge + jj fetch). TL role only |
| `popup` | Interactive UI in Zellij (choices, text, sliders) |
| `note` | Dev → TL async message |
| `question` | Dev → TL blocking question (trampoline) |
| `get_agent_messages` | TL reads agent outboxes (long-poll) |
| `answer_question` | TL answers pending question |
| `notify_parent` | Signal completion to parent. Auto-routed, injects into parent pane |

**Note**: Git operations (`git status`, `git log`, etc.) and GitHub operations (`gh pr list`, etc.) use the Bash tool with `git` and `gh` commands, not MCP tools.

---

## Documentation Tree

```
CLAUDE.md  ← YOU ARE HERE (project overview)
├── proto/CLAUDE.md    ← Protocol buffers (FFI boundary types)
├── haskell/CLAUDE.md  ← Haskell package organization
│   ├── dsl/core/CLAUDE.md      ← Graph DSL reference (START HERE for handlers)
│   ├── effects/CLAUDE.md       ← Effect interpreters
│   │   ├── llm-interpreter/     ← Anthropic/OpenAI API
│   │   ├── worktree-interpreter/ ← Git worktree management
│   │   └── ...
│   ├── protocol/CLAUDE.md      ← Wire formats
│   └── tools/CLAUDE.md         ← Dev tools
├── rust/CLAUDE.md             ← Rust workspace overview (3 crates)
│   ├── exomonad/CLAUDE.md  ← MCP server + hook handler (binary)
│   ├── exomonad-core/      ← Unified library: framework, handlers, services, protocol, UI types
│   ├── exomonad-proto/     ← Proto-generated types (prost) for FFI + effects
│   └── exomonad-plugin/CLAUDE.md   ← Zellij WASM plugin (status + popups)
└── docs/audits/               ← Project audits and reports
```

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
| Add or modify an effect interpreter | `haskell/effects/CLAUDE.md` |

---

## Developing ExoMonad

### Package Inventory

All Haskell packages live under `haskell/`. See `haskell/CLAUDE.md` for full details.

**Core (`haskell/dsl/`):**
| Package | Purpose |
|---------|---------|
| `haskell/dsl/core` | Graph DSL, effects, templates, validation |

**Effect Interpreters (`haskell/effects/`):**
| Package | Purpose |
|---------|---------|
| `haskell/effects/llm-interpreter` | Anthropic/OpenAI API calls |
| `haskell/effects/observability-interpreter` | OpenTelemetry traces to Grafana |
| `haskell/effects/github-interpreter` | GitHub API integration |
| `haskell/effects/worktree-interpreter` | Git worktree management |
| `haskell/effects/justfile-interpreter` | Justfile execution |
| `haskell/effects/zellij-interpreter` | Zellij session management |

**Protocol (`haskell/protocol/`):**
| Package | Purpose |
|---------|---------|
| `haskell/protocol/wire-types` | Native protocol types |

**Tools (`haskell/tools/`):**
| Package | Purpose |
|---------|---------|
| `haskell/tools/training-generator` | Training data types for FunctionGemma |

### Where Things Go

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

1. **freer-simple for effects** — Reified continuations for WASM yield/resume
2. **Typed Jinja templates** — Compile-time validation via ginger
3. **OneOf sum type** — Fully typed dispatch without Dynamic
4. **IO-blind agents** — All IO in runners, enables WASM + deterministic testing
5. **Haskell WASM = embedded DSL** — All logic in Haskell, Rust handles I/O only

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

When reviewing handlers, grep for `_` prefixes in pattern matches. Each one is a potential bug where exit types capture info that never reaches the next node.

---

## Tech Lead Praxis

How to coordinate heterogeneous agent teams effectively. Proven patterns from real sessions.

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

---

## Agent DSL

ExoMonad is also a library for building LLM agents as typed state machines. Agents live in consuming repos (e.g., urchin). See `haskell/dsl/core/CLAUDE.md` for the full Graph DSL reference.

### Core Concepts

**Graphs** — Agents are typed state machine graphs. Nodes are LLM calls or pure logic. Edges derived from type annotations or explicit `Goto`. Compile-time validation via type families.

```haskell
data MyAgent mode = MyAgent
  { entry    :: mode :- Entry Message
  , classify :: mode :- LLMNode :@ Input Message :@ Schema Intent
  , route    :: mode :- LogicNode :@ Input Intent :@ UsesEffects [Goto "handle" Message, Goto Exit Response]
  , handle   :: mode :- LLMNode :@ Input Message :@ Schema Response
  , exit     :: mode :- Exit Response
  }
```

**Effects** — Agents yield effects; runners interpret them: `LLM`, `State s`, `Emit evt`, `RequestInput`, `Log`, `Time`, `Memory s`, `Goto target`.

**Templates** — Jinja with compile-time validation via ginger:
```haskell
myTemplate :: TypedTemplate MyContext SourcePos
myTemplate = $(typedTemplateFile ''MyContext "templates/my_prompt.jinja")
```

**Agent Turn Loop:**
1. Build context (State → TemplateContext)
2. Render template (Jinja → prompt)
3. Call LLM (prompt + schema + tools → result)
4. Apply structured output (result → State')
5. Handle transitions (Goto → next node)

### Consuming Repos

- **urchin** (`~/exomonad-labs/urchin`) — Context generation tooling: `urchin prime` (git/GitHub/LSP context), `urchin lsp` (impact analysis)

### Sleeptime

The evolution pattern for agents: agents run and produce traces → cron jobs in consuming repos observe runs → file issues and PRs to improve the agent (state fields, schemas, templates, tools). ExoMonad provides infrastructure; consuming repos implement the evolution loop.

### LSP Integration

```haskell
import ExoMonad.Effects.LSP
import ExoMonad.LSP.Interpreter (withLSPSession, runLSP)

withLSPSession "/path/to/project" $ \session -> do
  info <- runLSP session $ hover doc pos
```

---

## References

- [haskell/dsl/core/CLAUDE.md](haskell/dsl/core/CLAUDE.md) — Graph DSL reference
- [rust/exomonad/CLAUDE.md](rust/exomonad/CLAUDE.md) — MCP server + WASM host
- [freer-simple](https://hackage.haskell.org/package/freer-simple) — Effect system
- [Anthropic tool use](https://docs.anthropic.com/en/docs/tool-use)
