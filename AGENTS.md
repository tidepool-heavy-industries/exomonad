# ExoMonad - Agent Developer Guide

> Type-safe LLM agent framework with structured state and templates

This document guides AI coding agents working on the ExoMonad project. It supplements `README.md` and `CLAUDE.md` with agent-specific workflow information.

## Project Overview

ExoMonad is a Haskell library for building LLM agents as typed state machines. Agents are **IO-blind**: they express typed effects (LLM calls, state mutations, tool calls) that external interpreters handle, enabling sandboxed execution.

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
- **Haskell** - Primary language (GHC 9.10+)
- **Rust** - Claude Code++ hook/MCP forwarding (`exomonad`)

### Build & Development
- **Cabal** - Haskell build system (monorepo with 30+ packages)
- **Just** - Task runner (`justfile` at root)
- **Nix** - Reproducible dev environments (flakes + fallback shell.nix)

### Testing
- **Cabal test** - Haskell unit/property tests
- **hlint** - Haskell linting (errors only, configured)

### Deployment
- **WASM** - Cross-compilation to wasm32-wasi
- **Native** - Servant server via `exomonad-native-server`

## Project Structure

All Haskell packages live in `haskell/`. See `cabal.project` for package list.

```
exomonad/
├── haskell/                    # All Haskell packages
│   ├── dsl/core/              # Graph DSL, effects, templates
│   ├── runtime/               # Execution backends (actor, WASM)
│   ├── effects/               # Effect interpreters (LLM, LSP, GitHub, etc.)
│   ├── agents/                # Production agents (semantic-scout)
│   ├── protocol/              # Wire protocols
│   ├── tools/                 # Dev tools (sleeptime, training-generator)
│   ├── vendor/                # Vendored forks (freer-simple, ginger)
│   ├── control-server/        # Claude Code++ TCP server (OSCAR winner)
│   └── native-server/         # Native HTTP/WebSocket server
├── rust/                      # Claude Code++ infrastructure
│   ├── exomonad/          # Hook/MCP forwarding (Rust ↔ Haskell TCP)
│   ├── exomonad-shared/         # Protocol types
│   └── exomonad-hub/            # Metrics (legacy)
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
```

### Development

```bash
# Native server (localhost:8080)
just native

# Lint codebase
just lint            # hlint (Haskell)

# Pre-commit checks
just pre-commit      # build + lint + tests
just pre-commit-fast # build + lint only

# Install git hooks
just install-hooks   # Runs pre-commit on every commit
```

## Development Workflows

### Claude Code++ (OSCAR) - Primary Development Mode

**Claude Code++** augments human-driven Claude Code sessions with ExoMonad superpowers. **Not headless** - humans remain in control.

**Architecture:**
```
TTY (Zellij)
├── Pane 1: Claude Code
└── Pane 2: exomonad-control-server (logs)
              ↓
        exomonad (Rust)
              ↓ TCP (NDJSON)
        control-server (Haskell)
          • Hook passthrough
          • MCP scout tool (LSP + semantic exploration)
```

**Setup:**
```bash
# Start Zellij session with control server
nix develop .#claude-code-plus  # Auto-starts Zellij

# Manual setup:
./ide                                    # Terminal 1: Starts Zellij with everything
# Or if running components manually:
cabal run exomonad-control-server    # Terminal 2: Control server
claude                               # Terminal 3: Claude Code session
```

**Key Files:**
- `haskell/control-server/CLAUDE.md` - Full data flow + implementation
- `rust/exomonad/CLAUDE.md` - Hook/MCP forwarding
- `haskell/agents/semantic-scout/CLAUDE.md` - Scout tool algorithm

### Effect Interpreter Development

**Where to add new effects:**
1. Type definition: `haskell/dsl/core/src/ExoMonad/Effect/Types.hs`
2. Integration: `haskell/dsl/core/src/ExoMonad/Effects/` (plural namespace)
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
- `haskell/dsl/core/src/ExoMonad/Graph/Types.hs`

**Validation:** The DSL uses type families for compile-time validation. See `haskell/dsl/core/src/ExoMonad/Graph/Validate/`.

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

**Manual testing:** Run agents in native server or Claude Code sessions
```bash
just native  # Interactive UI at localhost:8080
```

### WASM Development

**Requires:** `nix develop .#wasm` (cross-compilation toolchain)

**Build pipeline:**
```bash
nix develop .#wasm
wasm32-wasi-cabal build exomonad-reactor
wasm-opt -Oz <input.wasm> -o exomonad.wasm
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
- `Effect` (singular) = core infrastructure (`ExoMonad.Effect`)
- `Effects` (plural) = integrations (`ExoMonad.Effects`)
- `Interpreter` = effect implementation (replaces "executor")

**Data flow principle:** **Never ignore captured data with `_` prefix**
- If a pattern binds `_field`, it's a data flow dead-end
- Thread data forward via input fields, memory, or context
- See `CLAUDE.md` section "Code Smells: Data Flow Dead-Ends"

### Jinja Templates

**Location:** `templates/` directories within packages

**Validation:** Compile-time via `typedTemplateFile` TH splice
```haskell
myTemplate :: TypedTemplate MyContext SourcePos
myTemplate = $(typedTemplateFile ''MyContext "templates/my_prompt.jinja")
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
- Browser WASI shim for limited syscall support

## Common Pitfalls

### "WASM build fails with TH error"

**Cause:** Using non-WASM-safe modules

**Fix:**
- Check for native dependencies in cabal `build-depends`
- Move platform-specific code to `src-platform-wasm/` or `src-platform-native/`
- Use CPP conditionals: `#ifndef __GHC_WASM__`

### "Claude Code++ hooks not working"

**Diagnosis:**
1. Check control server running: `cabal run exomonad-control-server`
2. Check exomonad in PATH: `which exomonad`
3. Verify `.claude/settings.local.json` has correct paths
4. Check TCP connection: `telnet localhost 7432`

**Test MCP:**
```bash
echo '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}' | exomonad mcp
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
- `rust/exomonad/CLAUDE.md` - Hook/MCP forwarding

**For detailed questions:** Follow the CLAUDE.md link tree based on your task (see CLAUDE.md "When to Read Which CLAUDE.md" table).

**Design documents:** `plans/` directory contains RFCs and architecture decisions.
