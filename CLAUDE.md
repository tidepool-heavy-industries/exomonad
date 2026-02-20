# ExoMonad

Type-safe LLM agent orchestration. Haskell defines all logic via Tidepool (Cranelift-compiled Haskell Core). Rust executes I/O effects. Zellij provides isolation and multiplexing. Agents are IO-blind state machines that yield typed effects.

---

## Rules

### Style

ALWAYS update CLAUDE.md files when you make changes. Adding new documentation is critical, as is removing stale documentation.

Comments should always focus on what is or will be. Never leave comments about why you deleted something, its in the git history which is enough.

The repository should be kept clean of dead code, placeholders, and half-done heuristics.

Always prefer failure to an undocumented heuristic or fallback.

### Single Code Path

Never maintain two code paths that do the same thing. Redundant paths cause bug risk — fixes applied to one path get missed on the other. If there's a "debug mode" or "legacy mode" that duplicates a primary path, cut it.

### All Tool Logic in Haskell

**Never add direct Rust MCP tools.** All MCP tools are defined in Haskell — tool schemas, argument parsing, dispatch logic, everything. Rust is the I/O runtime: it executes effects that the Haskell DSL yields. If a new tool needs new I/O capabilities, add a service in Rust and wire it through the `BridgeDispatcher` in `tidepool_backend.rs`. The Haskell tool definitions live in `rust/exomonad-core/haskell/`.

This is the entire architectural premise. Haskell is the single source of truth for tool definitions. Rust never defines tool schemas, never parses tool arguments, never contains tool logic.

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

The server must be running before Claude Code or Gemini can use MCP tools. Without it, every tool call fails. Init also writes `.claude/settings.local.json` with hooks (including `SessionStart` which registers the Claude session UUID for `--fork-session` context inheritance in spawned subtrees).

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
claude mcp add --transport http exomonad http://localhost:7432/agents/tl/root/mcp
gemini mcp add --transport http exomonad http://localhost:7432/agents/tl/root/mcp
```

### Building

```bash
# One-command install (recommended - uses debug build for fast iteration)
just install-all-dev

# Or install release build (optimized, slower compile)
just install-all

# Rust binary only
cargo build -p exomonad
```

**What `just install-all-dev` does:**
1. Builds exomonad Rust binary (debug mode)
2. Builds and installs Zellij plugin (wasm32-wasip1)
3. Copies binaries to `~/.cargo/bin/` and `~/.config/zellij/plugins/`

**Cargo feature flags (`exomonad-core`):**
- **`runtime`** (default): Full runtime with Tidepool backend (Cranelift-compiled Haskell Core), MCP server, and all service integrations.
- Without `runtime`: Only lightweight UI protocol types (`ui_protocol` module). Used by `exomonad-plugin`.

### Configuration

**Bootstrap:** `exomonad init` auto-creates `.exo/config.toml` and `.gitignore` entries if missing. Works in any project directory.

```toml
default_role = "tl"  # or "dev"
project_dir = "."
shell_command = "nix develop"  # optional: environment wrapper for TL tab + server
```

**Config hierarchy:**
- `config.toml` uses `default_role` (project-wide default)
- `config.local.toml` uses `role` (worktree-specific override)
- Resolution: `local.role > global.default_role`

**Hook configuration** is auto-generated in two places:
- **`exomonad init`**: Writes `.claude/settings.local.json` with all hooks (SessionStart, PreToolUse, etc.) for the root TL session
- **`spawn_subtree`**: Writes `.claude/settings.local.json` into each spawned Claude worktree

The `SessionStart` hook is critical — it registers the Claude session UUID in `ClaudeSessionRegistry`, which `spawn_subtree` reads to pass `--resume <uuid> --fork-session` for context inheritance. Without it, spawned subtrees start with no context.

Gemini agents get settings via `GEMINI_CLI_SYSTEM_SETTINGS_PATH` env var (NOT `.gemini/settings.json`).

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

**Ink paste problem:** The injected Enter keypress must be deferred by ~100ms after the text injection. React Ink (used by Claude Code and Gemini CLI) treats multi-byte stdin writes arriving in the same event loop tick as clipboard paste events, bypassing `key.return` detection. The Zellij plugin uses `set_timeout(0.1)` + `Timer` to send the CR byte (0x0D) as a separate write, ensuring Ink processes it as an isolated keypress and fires `onSubmit`. See `rust/exomonad-plugin/CLAUDE.md` for full details.

### PR Workflow

- **`file_pr`** — Create or update a PR for the current branch. Auto-detects base branch from dot-separated naming convention.
- **`merge_pr`** — Merge a child's PR (`gh pr merge` + `jj git fetch` for auto-rebase). TL role only.

### Interactive UI

- **`popup`** — Display interactive popup in Zellij. Supports two modes:
  - **Simple form**: All 7 component types (text, choice, checkbox, slider, textbox, multiselect, group) with visibility rules, Tab/Shift+Tab navigation, keyboard input.
  - **Multi-pane wizard**: Branching CYOA-style forms with named panes, conditional transitions (goto or branch by field value), breadcrumb navigation, Back key support. Returns values from all visited panes.

### Built Infrastructure

| Feature | Status |
|---------|--------|
| **Event router** (Zellij STDIN injection) | Built. `notify_parent` → `inject_input` into parent pane via Zellij plugin pipe. |
| **GitHub poller** (PR status → events) | Built. Background service polls PR/CI status and injects notifications into agent panes. |
| **Event log** (JSONL structured events) | Built. `.exo/events.jsonl` — append-only JSONL. Query with `duckdb -c "SELECT * FROM read_json_auto('.exo/events.jsonl')"` or `jq`. Events: `agent.spawned`, `agent.completed`, `pr.filed`, `pr.merged`, `copilot.review`, `ci.status_changed`. |

---

## Architecture

### Components

```
Human in Zellij session
    └── Claude Code + exomonad (Rust + Haskell via Tidepool)
            ├── MCP tools via TidepoolBackend (spawn_subtree, spawn_leaf_subtree, spawn_workers, file_pr, etc.)
            └── Agent tree:
                ├── worktree: main.feature-a (TL role, can spawn children)
                │   ├── worker: rust-impl (Gemini, in-place pane)
                │   └── worker: haskell-impl (Gemini, in-place pane)
                └── worktree: main.feature-b (TL role)
                    └── ...
```

**Haskell = All Logic (via Tidepool)**
- Defines tool schemas, handlers, decision logic
- Yields typed effects (no I/O)
- Compiled to Cranelift JIT via Haskell Core. Native speed, no WASM overhead.
- Effect types bridge via `FromCore`/`ToCore` — no protobuf.
- Tool definitions in `rust/exomonad-core/haskell/`

**Rust = Runtime**
- Hosts TidepoolBackend, executes all effects (git, GitHub API, filesystem, Zellij)
- Owns the process lifecycle
- MCP server (HTTP, started by `exomonad init`)

**Worktrees + Zellij = Isolation/Multiplexing**
- Git worktrees for code isolation (no Docker containers)
- Zellij tabs for Claude subtrees, panes for Gemini workers
- Each agent = worktree + tab (or pane), managed by Rust runtime

### Data Flows

**MCP Tool Call:**
```
Claude Code → HTTP request → exomonad serve
→ Arc<dyn RuntimeBackend>::call_tool(role, tool_name, args)
→ TidepoolBackend → EffectMachine::run_async(haskell_expr)
→ BridgeDispatcher routes effects to Rust services
→ Result returned via FromCore/ToCore bridge
```

**Hook Call:**
```
Claude Code → exomonad hook pre-tool-use (reads stdin JSON)
→ HTTP POST to server → Arc<dyn RuntimeBackend>::handle_hook(input)
→ TidepoolBackend → handle_hook returns HookEnvelope { stdout, exit_code }
→ Claude Code proceeds or blocks
```

**Fail-open:** If the server is unreachable, `exomonad hook` prints `{"continue":true}` and exits 0.

### MCP Tools Reference

All tools implemented in Haskell via Tidepool (`rust/exomonad-core/haskell/`):

| Tool | Role | Description |
|------|------|-------------|
| `spawn_subtree` | tl | Fork Claude agent into worktree + Zellij tab (TL role, can spawn children) |
| `spawn_leaf_subtree` | tl | Fork Gemini agent into worktree + Zellij tab (dev role, files PR) |
| `spawn_workers` | tl | Spawn Gemini agents as Zellij panes (ephemeral, no worktree) |
| `file_pr` | tl, dev | Create/update PR (auto-detects base branch from naming) |
| `merge_pr` | tl | Merge child PR (gh merge + jj fetch) |
| `popup` | tl | Interactive UI in Zellij (forms with all component types, multi-pane wizards) |
| `notify_parent` | all | Signal completion to parent. Auto-routed, injects into parent pane |

**Note**: Git operations (`git status`, `git log`, etc.) and GitHub operations (`gh pr list`, etc.) use the Bash tool with `git` and `gh` commands, not MCP tools.

---

## Documentation Tree

```
CLAUDE.md  ← YOU ARE HERE (project overview)
├── haskell/CLAUDE.md  ← Haskell package organization
│   ├── dsl/core/CLAUDE.md      ← Graph DSL reference (START HERE for handlers)
│   ├── effects/CLAUDE.md       ← Effect interpreters
│   │   ├── llm-interpreter/     ← Anthropic/OpenAI API
│   │   ├── worktree-interpreter/ ← Git worktree management
│   │   └── ...
│   ├── protocol/CLAUDE.md      ← Wire formats
│   └── tools/CLAUDE.md         ← Dev tools
├── rust/CLAUDE.md             ← Rust workspace overview (2 crates)
│   ├── exomonad/CLAUDE.md  ← MCP server + hook handler (binary)
│   ├── exomonad-core/      ← Unified library: Tidepool backend, services, protocol, UI types
│   └── exomonad-plugin/CLAUDE.md   ← Zellij WASM plugin (status + popups)
└── docs/audits/               ← Project audits and reports
```

| I want to... | Read this |
|--------------|-----------|
| Understand MCP tool architecture | `rust/exomonad/CLAUDE.md` |
| Work on the Tidepool backend | `rust/exomonad-core/src/tidepool_backend.rs` |
| Add or modify a service | `rust/exomonad-core/` (services/) |
| Understand shared protocol types | `rust/exomonad-core/` (protocol/) |
| Work with external service clients | `rust/exomonad-core/` (services/external/) |
| Modify popup UI protocol | `rust/exomonad-core/` (ui_protocol.rs) |
| Work on Zellij plugin | `rust/exomonad-plugin/CLAUDE.md` |
| Add a Haskell tool definition | `rust/exomonad-core/haskell/` |
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
| New MCP tool | `rust/exomonad-core/haskell/` (Tidepool .hs files) |
| Agents (consuming repos) | Separate repo (urchin, etc.) |

### Naming Conventions

- **Effect** (singular) = core infrastructure (`ExoMonad.Effect.*`)
- **Effects** (plural) = integrations/contrib (`ExoMonad.Effects.*`)
- **Interpreter** = effect implementation (replaces "executor" terminology)

### Building & Testing

```bash
cargo test --workspace     # Rust tests (from repo root)
just verify                # Full check (tests + cargo check)
cargo build -p exomonad    # Build binary only
```

### Task Tracking

GitHub Issues. Branch naming: `gh-{number}/{description}`. Reference issue in commits (`[#123] ...`). Issues closed via PR merges (`Closes #123`).

### Key Design Decisions

1. **Tidepool backend** — Cranelift-compiled Haskell Core, native speed, no WASM overhead
2. **Typed Jinja templates** — Compile-time validation via ginger
3. **OneOf sum type** — Fully typed dispatch without Dynamic
4. **IO-blind agents** — All IO in runners, enables deterministic testing
5. **Haskell = embedded DSL** — All logic in Haskell, Rust handles I/O only

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

How to coordinate heterogeneous agent teams. The TL is a compiler: it transforms high-level intent into leaf-executable specs, then gets out of the way.

### Intelligence Gradient

Claude (Opus) decomposes and dispatches. Gemini implements. Copilot reviews. The TL never implements directly and never manually reviews intermediate output.

**Cost model:** Opus tokens are 10-30x Gemini tokens. Every line of code the TL writes is expensive code. Every review cycle the TL performs is an expensive review cycle. The TL's job is producing specs sharp enough that the leaf + Copilot convergence loop handles quality without TL involvement.

### Fire-and-Forget Execution

The TL's workflow is: **decompose → spec → spawn → move on**. The TL does not wait, poll, review intermediate output, or re-spec. It spawns all leaves it can, then idles until `[CHILD COMPLETE]` notifications arrive.

**Convergence is leaf + Copilot, not TL:**
1. TL writes spec, spawns leaf (Gemini), returns immediately
2. Leaf works → commits → files PR
3. GitHub poller detects Copilot review comments → injects into leaf's pane
4. Leaf reads Copilot feedback, fixes, pushes
5. Copilot re-reviews; loop repeats until clean
6. Leaf calls `notify_parent` with status `success` → TL gets `[CHILD COMPLETE]` notification
7. TL merges the PR

**`notify_parent` means DONE** — not "I filed a PR." The leaf owns its quality. The TL only sees finished, review-clean work.

**Escalation, not iteration.** If a leaf fails after 3+ Copilot rounds, it calls `notify_parent` with `failure` status. The TL then decides: re-decompose, try a different approach, or flag for human intervention. The TL never manually fixes a leaf's code.

### Spec Quality (You Only Get One Shot)

Since the TL doesn't iterate on specs, the v1 spec must be production-quality. Every spec follows this structure:

```
1. ANTI-PATTERNS      — Known Gemini failure modes as explicit "DO NOT" rules (FIRST)
2. READ FIRST         — Exact files to read (CLAUDE.md, source files, proto files)
3. STEPS              — Numbered, each step = one concrete action with code snippets
4. VERIFY             — Exact build/test commands with env vars (PROTOC path, etc.)
5. DONE CRITERIA      — What "done" looks like (tests pass, PR filed, notify_parent called)
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
- **Include exact commands.** Not "run the tests" but `cargo test --workspace`.
- **Name every file.** Not "update the module" but "edit `rust/exomonad-core/src/services/git.rs`".
- **Specs are self-contained.** The leaf has no context from previous attempts. Every spec must stand alone with complete code snippets and full file paths.

### Parallelization

Spawn multiple leaves when tasks are independent (no file conflicts, no ordering dependency). The TL spawns a wave, returns, and gets poked as each leaf completes. Examples:
- Proto plumbing + nix shell wrapping — independent, parallel.
- Haskell tool changes + Rust handler changes — often dependent (proto-gen first), sequential.

### When TL Gets Notified

The TL is idle between spawning and receiving notifications. It wakes up for:
- `[CHILD COMPLETE: agent-id]` — leaf finished successfully. TL reviews the PR diff, merges, and verifies the merged result builds cleanly. This matters especially when multiple leaves land in parallel — their changes may interact.
- `[CHILD FAILED: agent-id]` — leaf exhausted retries. TL re-decomposes or escalates.
- GitHub poller notifications (CI status, PR merge conflicts).

The TL does NOT wake up for intermediate progress, Copilot comments, or partial results. The convergence loop (leaf + Copilot) runs without TL involvement.

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
- [rust/exomonad/CLAUDE.md](rust/exomonad/CLAUDE.md) — MCP server + Tidepool host
- [freer-simple](https://hackage.haskell.org/package/freer-simple) — Effect system
- [Anthropic tool use](https://docs.anthropic.com/en/docs/tool-use)
