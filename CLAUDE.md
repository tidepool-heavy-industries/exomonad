# Tidepool - Type-Safe LLM Agent Framework

A Haskell library for building LLM agents as typed state machines. Agents are IO-blind - they yield typed effects that runners interpret.

## Two Audiences

This doc serves two audiences:

1. **Using Tidepool** - Building agents in consuming repos (anemone, urchin)
2. **Developing Tidepool** - Working on the framework itself

## Documentation Tree

Navigate to the right docs for your task:

```
CLAUDE.md  ← YOU ARE HERE (project overview)
├── haskell/CLAUDE.md  ← Haskell package organization
│   ├── dsl/core/CLAUDE.md      ← Graph DSL reference (START HERE for handlers)
│   ├── effects/CLAUDE.md       ← Effect interpreters
│   │   ├── session-executor/   ← Claude Code subprocess (key for V3)
│   │   ├── llm-executor/       ← Anthropic/OpenAI API
│   │   └── ...
│   ├── runtime/CLAUDE.md       ← Execution backends
│   │   └── actor/CLAUDE.md     ← Actor model details
│   ├── protocol/CLAUDE.md      ← Wire formats
│   └── tools/CLAUDE.md         ← Dev tools (ghci-oracle, sleeptime)
├── rust/CLAUDE.md             ← Mantle: Claude Code session orchestration
│   ├── mantle/CLAUDE.md        ← Host-side session CLI
│   ├── mantle-agent/CLAUDE.md  ← Container-side hook handler + MCP
│   ├── mantle-hub/CLAUDE.md    ← Session visualization daemon
│   └── mantle-shared/CLAUDE.md ← Shared types and protocols
├── types-first-dev/CLAUDE.md   ← V3 TDD protocol project
├── deploy/CLAUDE.md            ← Cloudflare deployment
└── typescript/
    ├── native-gui/CLAUDE.md    ← Solid.js frontend
    └── telegram-bot/CLAUDE.md  ← Telegram integration
```

## When to Read Which CLAUDE.md

| I want to... | Read this |
|--------------|-----------|
| Define a graph, handlers, annotations | `haskell/dsl/core/CLAUDE.md` |
| Understand ClaudeCode execution | `haskell/effects/session-interpreter/CLAUDE.md` |
| Add or modify an effect interpreter | `haskell/effects/CLAUDE.md` |
| Understand actor execution model | `haskell/runtime/actor/CLAUDE.md` |
| Work on types-first-dev V3 protocol | `types-first-dev/CLAUDE.md` |
| Deploy to Cloudflare Workers | `deploy/CLAUDE.md` |
| Work on the native server | `haskell/native-server/CLAUDE.md` |
| Work on mantle session orchestration | `rust/CLAUDE.md` |
| Handle Claude Code hooks or MCP | `rust/mantle-agent/CLAUDE.md` |
| Track sessions in the hub | `rust/mantle-hub/CLAUDE.md` |

---

# Part 1: Using Tidepool

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
  , route    :: mode :- LogicNode :@ Input Intent :@ UsesEffects '[Goto "handle" Message, Goto Exit Response]
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

### Native (Development)
```bash
just native  # Starts at localhost:8080
```

Uses the native server with full effect composition. Good for development with real Anthropic API.

### WASM/Cloudflare (Production)
Compile to WASM, deploy to Cloudflare Durable Objects. TypeScript harness interprets effects.

```bash
cd deploy && pnpm dev  # Local CF worker
```

## Consuming Repos

Tidepool is a library. Agents live in separate repos:

### anemone (`~/tidepool-labs/anemone`)
Example consuming repo with working agents:
- Native and Cloudflare WIPs (both work e2e)
- Agent definitions using Graph DSL
- Templates and schemas

### urchin (`~/tidepool-labs/urchin`)
Context generation tooling for coding agents:
- `urchin prime` - Generate context from git/bd/LSP for agent bootstrap
- `urchin lsp` - LSP impact analysis for Haskell code

## Sleeptime

**Sleeptime** is the evolution pattern for agents:

1. Agents run and produce logs/traces
2. **Cron jobs in consuming repos** observe these runs
3. Cron jobs file issues and PRs to improve the agent
4. Changes: state fields, output schemas, templates, tools

The cron jobs live in the consuming repo (anemone, urchin), not in tidepool itself. Tidepool provides the infrastructure; consuming repos implement the evolution loop.

## ClaudeCode Interpreter (WIP)

Dockerized Claude Code orchestration via mantle.

```haskell
import Tidepool.LSP.Interpreter (withLSPSession, runLSP)
```

Spawn Claude Code as a graph node:
```haskell
"work" := LLM
    :@ Input TaskInfo
    :@ Schema WorkResult
    :@ ClaudeCode 'Sonnet ('Just "/path/to/worktree")
```

This renders a template, spawns `claude -p` via mantle, and parses JSON output. Enables Tidepool graphs that orchestrate Claude Code sessions.

**Status**: WIP in `tidepool-claude-code-executor`

---

# Part 2: Developing Tidepool

## Package Inventory

All Haskell packages now live under `haskell/`. See `haskell/CLAUDE.md` for full details.

### Core (`haskell/dsl/`, `haskell/runtime/`)
| Package | Purpose |
|---------|---------|
| `haskell/dsl/core` | Graph DSL, effects, templates, validation |
| `haskell/runtime/actor` | Actor runtime with graph-to-actor execution |
| `haskell/runtime/parallel` | Parallel fan-out/fan-in execution with ki |
| `haskell/runtime/wasm` | WASM deployment scaffolding |
| `haskell/platform` | Platform abstraction layer (deprecated) |

### Effect Interpreters (`haskell/effects/`)
| Package | Purpose |
|---------|---------|
| `haskell/native-server` | Servant + WebSocket server (facade) |
| `haskell/effects/llm-interpreter` | Anthropic/OpenAI API calls |
| `haskell/effects/bd-interpreter` | Beads integration + urchin CLI |
| `haskell/effects/claude-code-interpreter` | Claude Code subprocess via mantle |
| `haskell/effects/habitica-interpreter` | Habitica API |
| `haskell/effects/ui-interpreter` | WebSocket ↔ UI bridging |
| `haskell/effects/observability-interpreter` | OpenTelemetry traces to Grafana |
| `haskell/effects/lsp-interpreter` | LSP via lsp-client |
| `haskell/effects/ghci-interpreter` | GHCi Oracle thin client |
| `haskell/effects/github-interpreter` | GitHub API integration |
| `haskell/effects/worktree-interpreter` | Git worktree management |
| `haskell/effects/cabal-interpreter` | Cabal build operations |
| `haskell/effects/devlog-interpreter` | Devlog effect interpreter |

### Integrations (`haskell/effects/`, `haskell/protocol/`)
| Package | Purpose |
|---------|---------|
| `haskell/effects/habitica` | Habitica effect types (standalone) |
| `haskell/effects/telegram` | Telegram effect types (disabled) |
| `haskell/protocol/wire-types` | Native protocol types |
| `haskell/protocol/generated-ts` | TypeScript type generation |

### Tools (`haskell/tools/`)
| Package | Purpose |
|---------|---------|
| `haskell/tools/ghci-oracle` | Persistent GHCi session server |
| `haskell/tools/sleeptime` | Log analysis for agent evolution |
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
| New effect type | `haskell/dsl/core/src/Tidepool/Effect/Types.hs` |
| New integration | `haskell/dsl/core/src/Tidepool/Effects/` (plural) |
| New graph annotation | `haskell/dsl/core/src/Tidepool/Graph/Types.hs` |
| New interpreter | `haskell/effects/<name>-executor/` |
| TypeScript frontend/bot | `typescript/<name>/` |
| Agents | Separate repo (anemone, urchin, etc.) |

### Naming Conventions
- **Effect** (singular) = core infrastructure (`Tidepool.Effect.*`)
- **Effects** (plural) = integrations/contrib (`Tidepool.Effects.*`)
- **Interpreter** = effect implementation (replaces "executor" terminology)

## Task Tracking (Beads)

Git-native task tracking via BD. Database at `.beads/` (gitignored, shared across worktrees).

```bash
bd list --all              # List tasks
bd create -t task "..."    # Create task
bd update ID -s in_progress # Update status
bd show ID                 # View details
```

### Workflow
1. Mark task `in_progress` when starting
2. Reference bead ID in PR description
3. Mark `closed` after merge
4. All worktrees share the same db

### Landing the Plane

When ending a session:

1. File issues for remaining work (`bd create`)
2. Run quality gates (`just pre-commit`)
3. Update issue status
4. Push: `git pull --rebase && bd sync && git push`
5. Verify: `git status` shows "up to date"

## Building & Testing

```bash
cabal build all            # Build everything
just native                # Run native server
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

## Observability

OpenTelemetry traces to Grafana Tempo:

```bash
export OTLP_ENDPOINT="https://..."
export OTLP_USER="<instance-id>"
export OTLP_TOKEN="glc_..."
just native  # Traces exported automatically
```

Use `Execute.Instrumented` for automatic span emission on graph dispatch.

## LSP Integration

The `tidepool-lsp-executor` provides LSP for code intelligence:

```haskell
import Tidepool.Effects.LSP
import Tidepool.LSP.Executor (withLSPSession, runLSP)

withLSPSession "/path/to/project" $ \session -> do
  info <- runLSP session $ hover doc pos
  ...
```

## References

- [tidepool-core/CLAUDE.md](tidepool-core/CLAUDE.md) - Graph DSL reference
- [tidepool-native-gui/server/CLAUDE.md](tidepool-native-gui/server/CLAUDE.md) - Native server docs
- [deploy/CLAUDE.md](deploy/CLAUDE.md) - Cloudflare deployment
- [freer-simple](https://hackage.haskell.org/package/freer-simple) - Effect system
- [Anthropic tool use](https://docs.anthropic.com/en/docs/tool-use)
