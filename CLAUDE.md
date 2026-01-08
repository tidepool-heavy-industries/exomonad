# Tidepool - Type-Safe LLM Agent Framework

A Haskell library for building LLM agents as typed state machines. Agents are IO-blind - they yield typed effects that runners interpret.

## Two Audiences

This doc serves two audiences:

1. **Using Tidepool** - Building agents in consuming repos (anemone, urchin)
2. **Developing Tidepool** - Working on the framework itself

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

See `tidepool-core/CLAUDE.md` for the full Graph DSL reference.

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

### anemone (`~/dev/anemone`)
Example consuming repo with working agents:
- Native and Cloudflare WIPs (both work e2e)
- Agent definitions using Graph DSL
- Templates and schemas

### urchin (`~/dev/urchin`, currently in tidepool-bd-executor)
Context generation tooling for coding agents:
- `urchin prime` - Generate context from git/bd/LSP for agent bootstrap
- Will be extracted to its own repo

## Sleeptime

**Sleeptime** is the evolution pattern for agents:

1. Agents run and produce logs/traces
2. **Cron jobs in consuming repos** observe these runs
3. Cron jobs file issues and PRs to improve the agent
4. Changes: state fields, output schemas, templates, tools

The cron jobs live in the consuming repo (anemone, urchin), not in tidepool itself. Tidepool provides the infrastructure; consuming repos implement the evolution loop.

## ClaudeCode Executor (WIP)

Spawn Claude Code as a graph node:
```haskell
"work" := LLM
    :@ Input TaskInfo
    :@ Schema WorkResult
    :@ ClaudeCode 'Sonnet ('Just "/path/to/worktree")
```

This renders a template, spawns `claude -p` via zellij-cc, and parses JSON output. Enables Tidepool graphs that orchestrate Claude Code sessions.

**Status**: WIP in `tidepool-claude-code-executor`

---

# Part 2: Developing Tidepool

## Package Inventory

### Core
| Package | Purpose |
|---------|---------|
| `tidepool-core` | Graph DSL, effects, templates, validation |
| `tidepool-wasm` | WASM deployment scaffolding |
| `tidepool-platform` | Platform abstraction layer |

### Effect Executors (`tidepool-native-gui/`)
| Package | Purpose |
|---------|---------|
| `tidepool-native-server` | Servant + WebSocket server |
| `tidepool-llm-executor` | Anthropic/OpenAI API calls |
| `tidepool-ui-executor` | WebSocket ↔ UI bridging |
| `tidepool-habitica-executor` | Habitica API |
| `tidepool-bd-executor` | Beads integration + urchin CLI |
| `tidepool-lsp-executor` | LSP via lsp-client |
| `tidepool-observability-executor` | OpenTelemetry traces to Grafana |
| `tidepool-claude-code-executor` | Claude Code subprocess (WIP) |
| `tidepool-ghci-executor` | GHCi Oracle thin client |
| `tidepool-issue-executor` | Issue tracking |
| `tidepool-github-executor` | GitHub API integration |
| `tidepool-wire-types` | Shared wire format types |

### Integrations
| Package | Purpose |
|---------|---------|
| `tidepool-habitica` | Habitica effect types |
| `tidepool-mcp` | Model Context Protocol |
| `tidepool-telegram-hs` | Telegram bot (Haskell) |
| `tidepool-telegram-ts` | Telegram bot (TypeScript) |
| `tidepool-generated-ts` | Generated TypeScript types |

### Tools (`tools/`)
| Package | Purpose |
|---------|---------|
| `ghci-oracle` | Persistent GHCi session server (standalone) |
| `impact-analysis` | PR blast radius analysis |
| `tidepool-sleeptime-logs` | Log analysis for sleeptime |

### Deployment
| Directory | Purpose |
|-----------|---------|
| `deploy/` | Cloudflare Worker Durable Object harness |
| `template/` | Project template for new consuming repos |

## Where Things Go

| Thing | Location |
|-------|----------|
| New effect type | `tidepool-core/src/Tidepool/Effect/Types.hs` |
| New integration | `tidepool-core/src/Tidepool/Effects/` (plural) |
| New graph annotation | `tidepool-core/src/Tidepool/Graph/Types.hs` |
| New executor | `tidepool-native-gui/<name>-executor/` |
| Agents | Separate repo (anemone, urchin, etc.) |

### Naming Conventions
- **Effect** (singular) = core infrastructure (`Tidepool.Effect.*`)
- **Effects** (plural) = integrations/contrib (`Tidepool.Effects.*`)

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
