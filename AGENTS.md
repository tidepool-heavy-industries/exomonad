# Tidepool - Agent Developer Guide

> Type-safe LLM agent framework with structured state and templates

This document guides AI coding agents working on the Tidepool project. It supplements `README.md` and `CLAUDE.md` with agent-specific workflow information.

## Project Overview

Tidepool is a Haskell library for building LLM agents as typed state machines. Agents are **IO-blind**: they express typed effects (LLM calls, state mutations, tool calls) that external interpreters handle, enabling sandboxed execution and WASM deployment.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Agent Turn Loop                         │
│                                                             │
│  1. Build context (Haskell: State → TemplateContext)        │
│  2. Render template (Jinja: Context → prompt)               │
│  3. Call LLM (prompt + schema + tools → result)             │
│  4. Apply structured output (result → State')               │
│  5. Handle transitions (Goto → next node)                   │
└─────────────────────────────────────────────────────────────┘
```

**Key Design Decisions:**
- **freer-simple** for effects - Reified continuations enable WASM yield/resume
- **Typed Jinja templates** - Compile-time validation via ginger fork
- **OneOf sum type** - Fully typed dispatch without Dynamic
- **IO-blind agents** - All IO in runners; enables WASM + deterministic testing

## Technology Stack

### Core Languages
- **Haskell** - Primary language (GHC 9.10+
- **TypeScript** - Cloudflare Worker harness, Claude Code++ MCP server
- **Rust** - Claude Code++ hook/MCP forwarding (`mantle-agent`)

### Build & Development
- **Cabal** - Haskell build system (monorepo with 30+ packages)
- **Just** - Task runner (`justfile` at root)
- **Nix** - Reproducible dev environments (flakes + fallback shell.nix)
- **Node.js/pnpm** - TypeScript tooling and deployment

### Testing
- **Cabal test** - Haskell unit/property tests
- **Vitest** - TypeScript tests for Cloudflare Worker
- **hlint** - Haskell linting (errors only, configured)
- **ESLint** - TypeScript linting

### Deployment
- **WASM** - Cross-compilation to wasm32-wasi (Cloudflare Workers)
- **Cloudflare** - Durable Objects + Workers
- **Native** - Servant server via `tidepool-native-server`

## Project Structure

All Haskell packages live in `haskell/`. See `cabal.project` for package list.

```
tidepool/
├── haskell/                    # All Haskell packages
│   ├── dsl/core/              # Graph DSL, effects, templates
│   ├── runtime/               # Execution backends (actor, WASM)
│   ├── effects/               # Effect interpreters (LLM, LSP, GitHub, etc.)
│   ├── agents/                # Production agents (semantic-scout)
│   ├── protocol/              # Wire protocols (Haskell ↔ TypeScript)
│   ├── tools/                 # Dev tools (sleeptime, training-generator)
│   ├── vendor/                # Vendored forks (freer-simple, ginger)
│   ├── control-server/        # Claude Code++ TCP server (OSCAR winner)
│   └── native-server/         # Native HTTP/WebSocket server
├── deploy/                    # Cloudflare Worker harness
├── rust/                      # Claude Code++ infrastructure
│   ├── mantle-agent/          # Hook/MCP forwarding (Rust ↔ Haskell TCP)
│   ├── mantle-shared/         # Protocol types
│   └── mantle-hub/            # Metrics (legacy)
├── typescript/                # TypeScript packages
│   ├── native-gui/            # Solid.js frontend
│   └── telegram-bot/          # Bot implementation
├── anemone/                   # Debug UI (Solid.js, in-repo)
├── tools/                     # Root-level analysis tools
├── docs/                      # Documentation
└── plans/                     # Design documents
```

## Essential Commands

### Building

```bash
# Build everything
cabal build all
just build           # Same, via Just

# Build with strict warnings
cabal build all --ghc-options="-Werror"
just build-strict

# Clean
just clean           # cabal clean
```

### Testing

```bash
# Run all tests
cabal test all
just test

# Graph validation only (fast)
just test-graph

# TypeScript tests
cd deploy && pnpm test

# Protocol conformance (Haskell → TypeScript)
just test-protocol-conformance

# Cross-boundary property tests (requires WASM)
just build-wasm
just test-roundtrip
```

### Development

```bash
# Native server (localhost:8080)
just native

# Cloudflare Worker local dev
cd deploy && pnpm dev

# Lint codebase
just lint            # hlint (Haskell) + ESLint (TypeScript)

# Pre-commit checks
just pre-commit      # build + lint + tests
just pre-commit-fast # build + lint only

# Install git hooks
just install-hooks   # Runs pre-commit on every commit
```

### Deployment

```bash
# Build WASM
just build-wasm      # Requires: nix develop .#wasm

# Deploy to Cloudflare
just deploy          # build-wasm + deploy-worker

# View logs
just logs            # Pretty format
just logs-json       # JSON format
```

## Development Workflows

### Claude Code++ (OSCAR) - Primary Development Mode

**Claude Code++** augments human-driven Claude Code sessions with Tidepool superpowers. **Not headless** - humans remain in control.

**Architecture:**
```
TTY (Zellij)
├── Pane 1: Claude Code
└── Pane 2: tidepool-control-server (logs)
              ↓
        mantle-agent (Rust)
              ↓ TCP (NDJSON)
        control-server (Haskell)
          • Hook passthrough
          • MCP scout tool (LSP + semantic exploration)
```

**Setup:**
```bash
# Start Zellij session with control server
nix develop .#claude-code-plus  # Auto-starts Zellij

# Or manually:
cd ~/tidepool-heavy-industries/tidepool
cabal run tidepool-control-server  # Terminal 1
claude                              # Terminal 2
```

**Key Files:**
- `haskell/control-server/CLAUDE.md` - Full data flow + implementation
- `rust/mantle-agent/CLAUDE.md` - Hook/MCP forwarding
- `haskell/agents/semantic-scout/CLAUDE.md` - Scout tool algorithm

### Effect Interpreter Development

**Where to add new effects:**
1. Type definition: `haskell/dsl/core/src/Tidepool/Effect/Types.hs`
2. Integration: `haskell/dsl/core/src/Tidepool/Effects/` (plural namespace)
3. Interpreter: `haskell/effects/<name>-interpreter/`

**Pattern:**
```haskell
-- Effect type (in core)
data MyEffect :: Effect where
  MyAction :: Input -> MyEffect m Output

-- Interpreter (separate package)
runMyEffect :: (MyEffect :< r) => Sem r a -> Sem r a
runMyEffect = interpret $ \case
  MyAction input -> do
    -- Implementation
    pure output
```

### Graph DSL Development

**Where to add new annotations:**
- `haskell/dsl/core/src/Tidepool/Graph/Types.hs`

**Validation:** The DSL uses type families for compile-time validation. See `haskell/dsl/core/src/Tidepool/Graph/Validate/`.

**Graph structure:**
```haskell
data MyAgent mode = MyAgent
  { entry    :: mode :- Entry Message
  , classify :: mode :- LLMNode :@ Input Message :@ Schema Intent
  , route    :: mode :- LogicNode :@ Input Intent :@ UsesEffects '[Goto "handle" Message, Goto Exit Response]
  , handle   :: mode :- LLMNode :@ Input Message :@ Schema Response
  , exit     :: mode :- Exit Response
  }
```

### Testing Strategy

**Unit tests:** Colocated in `test/` directories within packages
```bash
cabal test <package-name>
```

**Property tests:** Use `hedgehog` or `quickcheck` for protocol validation
```bash
just test-protocol-conformance  # Haskell ↔ TypeScript roundtrip
```

**Integration tests:** Cloudflare Worker tests with Vitest
```bash
cd deploy && pnpm test
```

**Manual testing:** Run agents in native server or Claude Code sessions
```bash
just native  # Interactive UI at localhost:8080
```

### WASM Development

**Requires:** `nix develop .#wasm` (cross-compilation toolchain)

**Build pipeline:**
```bash
nix develop .#wasm
wasm32-wasi-cabal build tidepool-reactor
wasm-opt -Oz <input.wasm> -o deploy/src/tidepool.wasm
```

**Limitations:**
- No Template Haskell in WASM target (use `src-platform-wasm/` variants)
- GHC 9.10 for stability (not 9.14 default)
- External interpreter for compile-time code generation

## Code Style Guidelines

### Haskell

**Language:** GHC 2021 (GHC 9.10+)

**Warnings:**
```cabal
ghc-options: -Wall -Wincomplete-patterns -Wredundant-constraints -Wunused-packages -haddock
```

**Formatting:** No strict formatter; follow existing style

**Documentation:** Haddock comments on all exported functions

**Effect naming:**
- `Effect` (singular) = core infrastructure (`Tidepool.Effect`)
- `Effects` (plural) = integrations (`Tidepool.Effects`)
- `Interpreter` = effect implementation (replaces "executor")

**Data flow principle:** **Never ignore captured data with `_` prefix**
- If a pattern binds `_field`, it's a data flow dead-end
- Thread data forward via input fields, memory, or context
- See `CLAUDE.md` section "Code Smells: Data Flow Dead-Ends"

### TypeScript

**Format:** ESLint configured in `deploy/.eslintrc.js`

**Type checking:** `tsc --noEmit`

**Module system:** ES Modules (`"type": "module"`)

### Jinja Templates

**Location:** `templates/` directories within packages

**Validation:** Compile-time via `typedTemplateFile` TH splice
```haskell
myTemplate :: TypedTemplate MyContext SourcePos
myTemplate = $(typedTemplateFile ''MyContext "templates/my_prompt.jinja")
```

## Task Tracking (Beads)

Git-native task tracking via **bd**. Database at `.beads/` (gitignored, shared across worktrees).

### Agent Workflow (REQUIRED)

When starting a new task (bead) as an agent, you MUST follow this workflow:

1.  **Branch Naming**: Create a branch following the `bd-{id}/{description}` convention.
    ```bash
    git checkout -b bd-69u/agent-onboarding
    ```
2.  **Bootstrap Context**: Run the context script immediately after checking out the branch to get task details and acceptance criteria.
    ```bash
    ./scripts/bead-context
    ```
3.  **Development**: Implement changes incrementally. Use `bd update <id> -s in_progress` to signal you are working on it.
4.  **Commit Format**: Use the bead ID in every commit message.
    ```bash
    git commit -m "[tidepool-69u] update AGENTS.md with onboarding workflow"
    ```
5.  **Pull Request**: File a PR using the GitHub CLI (`gh`).
    ```bash
    gh pr create --title "[tidepool-69u] <title>" --body "Closes tidepool-69u"
    ```
6.  **Cleanup**: After the PR is merged, close the bead.
    ```bash
    bd close tidepool-69u --reason "Merged: <PR URL>"
    ```

### Basic Commands

```bash
bd list --all              # List tasks
bd create -t task "..."    # Create task
bd update ID -s in_progress # Update status
bd show ID                 # View details
bd sync                    # Sync with git notes
```

**Workflow Summary:**
1. Mark task `in_progress` when starting
2. Reference bead ID in PR descriptions
3. Mark `closed` after merge
4. All worktrees share the same database

**Landing the plane:**
```bash
# 1. File issues for remaining work
bd create -t task "TODO: ..."

# 2. Run quality gates
just pre-commit

# 3. Update status
bd close <id> --reason "Merged: <PR URL>"

# 4. Push
bd sync
git pull --rebase
git push

# 5. Verify
git status  # Should show "up to date"
```

## Observability

**OpenTelemetry traces** to Grafana Tempo:

```bash
export OTLP_ENDPOINT="https://..."      # Grafana Tempo endpoint
export OTLP_USER="<instance-id>"        # GLC_...
export OTLP_TOKEN="glc_..."             # API token
```

Use `Execute.Instrumented` for automatic span emission on graph dispatch.

## Security Considerations

**No secrets in code** - use environment variables:
- `ANTHROPIC_API_KEY` - Required for LLM calls
- `OPENAI_API_KEY` - Optional, for OpenAI provider
- `GITHUB_TOKEN` - For GitHub interpreter
- `HABITICA_API_TOKEN` - For Habitica integration
- `OTLP_TOKEN` - For observability

**Secret management:**
- `.env` file gitignored (template: `.env.template` if needed)
- Cloudflare secrets via `wrangler secret`
- Nix shell loads from environment

**WASM sandboxing:**
- No native IO in WASM target
- All effects mediated by TypeScript harness
- Browser WASI shim for limited syscall support

## Common Pitfalls

### "Build succeeds but tests fail"

**Cause:** Protocol drift between Haskell and TypeScript

**Fix:**
```bash
just test-protocol-conformance  # Regenerate samples and verify
```

### "WASM build fails with TH error"

**Cause:** Using non-WASM-safe modules

**Fix:**
- Check for native dependencies in cabal `build-depends`
- Move platform-specific code to `src-platform-wasm/` or `src-platform-native/`
- Use CPP conditionals: `#ifndef __GHC_WASM__`

### "Claude Code++ hooks not working"

**Diagnosis:**
1. Check control server running: `cabal run tidepool-control-server`
2. Check mantle-agent in PATH: `which mantle-agent`
3. Verify `.claude/settings.local.json` has correct paths
4. Check TCP connection: `telnet localhost 7432`

**Test MCP:**
```bash
echo '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}' | mantle-agent mcp
```

### "Template compilation error"

**Cause:** Template references non-existent context field

**Fix:**
- Check template variables match context type
- Re-run TH splice to refresh template cache
- Verify template file path is correct

## Support & Documentation

**User-facing docs:** `README.md` (overview), `docs/`

**Developer docs:**
- `CLAUDE.md` - Project overview and navigation
- `haskell/CLAUDE.md` - Haskell package organization
- `haskell/dsl/core/CLAUDE.md` - Graph DSL reference
- `haskell/control-server/CLAUDE.md` - Claude Code++ implementation
- `rust/mantle-agent/CLAUDE.md` - Hook/MCP forwarding

**For detailed questions:** Follow the CLAUDE.md link tree based on your task (see CLAUDE.md "When to Read Which CLAUDE.md" table).

**Design documents:** `plans/` directory contains RFCs and architecture decisions.

<!-- bv-agent-instructions-v1 -->

---

## Beads Workflow Integration

This project uses [beads_viewer](https://github.com/Dicklesworthstone/beads_viewer) for issue tracking. Issues are stored in `.beads/` and tracked in git.

### Essential Commands

```bash
# View issues (launches TUI - avoid in automated sessions)
bv

# CLI commands for agents (use these instead)
bd ready              # Show issues ready to work (no blockers)
bd list --status=open # All open issues
bd show <id>          # Full issue details with dependencies
bd create --title="..." --type=task --priority=2
bd update <id> --status=in_progress
bd close <id> --reason="Completed"
bd close <id1> <id2>  # Close multiple issues at once
bd sync               # Commit and push changes
```

### Workflow Pattern

1. **Start**: Run `bd ready` to find actionable work
2. **Claim**: Use `bd update <id> --status=in_progress`
3. **Work**: Implement the task
4. **Complete**: Use `bd close <id>`
5. **Sync**: Always run `bd sync` at session end

### Key Concepts

- **Dependencies**: Issues can block other issues. `bd ready` shows only unblocked work.
- **Priority**: P0=critical, P1=high, P2=medium, P3=low, P4=backlog (use numbers, not words)
- **Types**: task, bug, feature, epic, question, docs
- **Blocking**: `bd dep add <issue> <depends-on>` to add dependencies

### Session Protocol

**Before ending any session, run this checklist:**

```bash
git status              # Check what changed
git add <files>         # Stage code changes
bd sync                 # Commit beads changes
git commit -m "..."     # Commit code
bd sync                 # Commit any new beads changes
git push                # Push to remote
```

### Best Practices

- Check `bd ready` at session start to find available work
- Update status as you work (in_progress → closed)
- Create new issues with `bd create` when you discover tasks
- Use descriptive titles and set appropriate priority/type
- Always `bd sync` before ending session

<!-- end-bv-agent-instructions -->
