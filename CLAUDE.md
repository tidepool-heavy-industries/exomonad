# Tidepool - Type-Safe LLM Agent Framework

A Haskell library for building LLM agents as typed state machines. Agents are IO-blind - they yield typed effects that runners interpret.

## Two Audiences

This doc serves two audiences:

1. **Using Tidepool** - Building agents in consuming repos (anemone, urchin)
2. **Developing Tidepool** - Working on the framework itself

### HUMAN STYLE OVERRIDES

ALWAYS update CLAUDE.md files when you make changes. Adding new documentation is critical, as is removing stale documentation.

Comments should always focus on what is or will be. Never leave comments about why you deleted something, its in the git history which is enough.

The repository should be kept clean of dead code, placeholders, and half-done heuristics.

Always prefer failure to an undocumented heuristic or fallback.


## Documentation Tree

Navigate to the right docs for your task:

```
CLAUDE.md  ← YOU ARE HERE (project overview)
├── haskell/CLAUDE.md  ← Haskell package organization
│   ├── control-server/CLAUDE.md ⭐ Claude Code++ hub (hooks/MCP/scout)
│   ├── dsl/core/CLAUDE.md      ← Graph DSL reference (START HERE for handlers)
│   ├── agents/                 ← Production agents
│   │   └── semantic-scout/CLAUDE.md ← MERGED into control-server (redirect)
│   ├── effects/CLAUDE.md       ← Effect interpreters
│   │   ├── llm-interpreter/     ← Anthropic/OpenAI API
│   │   ├── lsp-interpreter/     ← Language Server Protocol
│   │   ├── mcp-server/          ← MCP tool server (expose agents to Claude)
│   │   └── ...
│   ├── runtime/CLAUDE.md       ← Execution backends
│   │   └── actor/CLAUDE.md     ← Actor model details
│   ├── protocol/CLAUDE.md      ← Wire formats
│   └── tools/CLAUDE.md         ← Dev tools (ghci-oracle, sleeptime, training-generator)
├── rust/CLAUDE.md             ← Claude Code++ (hook handler + MCP forwarding)
│   ├── mantle-agent/CLAUDE.md  ← Hook handler + MCP stdio server (IMPLEMENTED)
│   ├── mantle-hub/CLAUDE.md    ← Metrics hub (LEGACY, needs repurposing)
│   └── mantle-shared/CLAUDE.md ← Protocol types, TCP socket client
├── types-first-dev/CLAUDE.md   ← V3 TDD protocol project
├── deploy/CLAUDE.md            ← Cloudflare deployment
├── anemone/CLAUDE.md           ← Debug/diagnostic Solid.js UI (in-repo, not ~/tidepool-labs)
├── tools/CLAUDE.md             ← Root-level tools (micro-gastown, blast-radius)
└── typescript/
    ├── native-gui/CLAUDE.md    ← Solid.js frontend (alternative UI)
    └── telegram-bot/CLAUDE.md  ← Telegram integration
```

## When to Read Which CLAUDE.md

| I want to... | Read this |
|--------------|-----------|
| Work on Claude Code++ (hooks/MCP/scout) ⭐ | `haskell/control-server/CLAUDE.md` |
| Understand hook/MCP forwarding (Rust side) | `rust/mantle-agent/CLAUDE.md` |
| Define a graph, handlers, annotations | `haskell/dsl/core/CLAUDE.md` |
| Add or modify an effect interpreter | `haskell/effects/CLAUDE.md` |
| Understand actor execution model | `haskell/runtime/actor/CLAUDE.md` |
| Work on semantic-scout code exploration | `haskell/control-server/CLAUDE.md` (merged from agents/semantic-scout) |
| Expose agents as MCP tools | `haskell/effects/mcp-server/CLAUDE.md` |
| Work with LSP (Language Server Protocol) | `haskell/effects/lsp-interpreter/CLAUDE.md` |
| Generate training data for FunctionGemma | `haskell/tools/training-generator/CLAUDE.md` |
| Work on types-first-dev V3 protocol | `types-first-dev/CLAUDE.md` |
| Deploy to Cloudflare Workers | `deploy/CLAUDE.md` |
| Work on the native server | `haskell/native-server/CLAUDE.md` |
| Work on debug UI frontend | `anemone/CLAUDE.md` or `typescript/native-gui/CLAUDE.md` |
| Understand control protocol types | `rust/mantle-shared/CLAUDE.md` |

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

## Claude Code++ Integration

Human-driven Claude Code sessions augmented with Tidepool. **Not headless automation** - humans interact via TTY; we add superpowers.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ User TTY (Zellij 2-pane)                                    │
│  Pane 1: Claude Code  │  Pane 2: control-server (logs)      │
└───────────┬─────────────────────────────────────────────────┘
            │ Hooks/MCP
            ▼
┌─────────────────────────────────────────────────────────────┐
│ mantle-agent (Rust)                                         │
│  • hook subcommand: CC hooks → TCP                          │
│  • mcp subcommand: JSON-RPC stdio → TCP                     │
└───────────┬─────────────────────────────────────────────────┘
            │ TCP (NDJSON)
            ▼
┌─────────────────────────────────────────────────────────────┐
│ control-server (Haskell)                                    │
│  • Unix socket: .tidepool/control.sock                      │
│  • Long-lived LSP session (HLS)                             │
│  • Hook Handler: Passthrough (log and allow)                │
│  • MCP Handler: scout tool (semantic code exploration)      │
└─────────────────────────────────────────────────────────────┘
```

### Key Components

| Component | Location | Purpose |
|-----------|----------|---------|
| **mantle-agent** | `rust/mantle-agent/` | Hook/MCP forwarding to control server |
| **control-server** | `haskell/control-server/` | Haskell TCP server with LSP + scout tool |
| **Protocol types** | `rust/mantle-shared/protocol.rs` + `haskell/control-server/Protocol.hs` | Bidirectional message types (must match exactly) |

### Data Flow

**Hook Flow (PreToolUse):**
```
1. Claude Code wants to call Write tool
2. Generates hook JSON on stdin
3. mantle-agent hook pre-tool-use reads stdin
4. Forwards ControlMessage::HookEvent via TCP
5. control-server receives, routes to handleHook
6. Returns HookResponse (allow/deny)
7. mantle-agent prints to stdout
8. Claude Code proceeds or blocks
```

**MCP Tool Flow (scout):**
```
1. User: "What breaks if I add a variant to LLMKind?"
2. Claude plans to call scout MCP tool
3. Claude Code spawns mantle-agent mcp (JSON-RPC stdio)
4. mantle-agent forwards ControlMessage::McpToolCall via TCP
5. control-server routes to handleScoutTool
6. Exploration: LSP (workspace/symbol, hover, references) + Gemma scoring
7. Returns ScoutResponse (pointers, summary, training examples)
8. mantle-agent formats as JSON-RPC result to stdout
9. Claude analyzes and responds to user
```

### Configuration

In `.claude/settings.local.json`:
```json
{
  "hooks": {
    "PreToolUse": "mantle-agent hook pre-tool-use"
  },
  "mcpServers": {
    "tidepool": {
      "command": "mantle-agent",
      "args": ["mcp"],
      "env": {
        "MANTLE_CONTROL_HOST": "127.0.0.1",
        "MANTLE_CONTROL_PORT": "7432"
      }
    }
  }
}
```

### Running

```bash
# Pane 1: Start control server
cd /path/to/project
cabal run tidepool-control-server

# Pane 2: Start Claude Code (in same directory)
claude-code
```

### Status

- ✅ Hook forwarding (passthrough)
- ✅ MCP server + scout tool
- ✅ LSP integration (HLS via lsp-test)
- ✅ FunctionGemma scoring (heuristic + HTTP interpreters)
- 🔄 Training data generation (types ready, CLI pending)
- ❌ Daemon mode (not implemented, uses per-call TCP)
- ❌ Metrics hub (mantle-hub needs repurposing)
- ❌ Real hook logic (currently allow-all passthrough)

### See Also

- **[haskell/control-server/CLAUDE.md](haskell/control-server/CLAUDE.md)** - Complete data flow + implementation
- **[rust/CLAUDE.md](rust/CLAUDE.md)** - Rust workspace overview
- **[rust/mantle-agent/CLAUDE.md](rust/mantle-agent/CLAUDE.md)** - Hook/MCP implementation
- **[haskell/agents/semantic-scout/CLAUDE.md](haskell/agents/semantic-scout/CLAUDE.md)** - Scout exploration algorithm

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

### Agents (`haskell/agents/`)
| Package | Purpose |
|---------|---------|
| `haskell/agents/semantic-scout` | Code exploration MCP tool using LSP |

### Effect Interpreters (`haskell/effects/`)
| Package | Purpose |
|---------|---------|
| `haskell/native-server` | Servant + WebSocket server (facade) |
| `haskell/effects/llm-interpreter` | Anthropic/OpenAI API calls |
| `haskell/effects/bd-interpreter` | Beads integration + urchin CLI |
| `haskell/effects/mcp-server` | MCP tool server (expose agents to Claude) |
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
| `haskell/tools/training-generator` | Training data types for FunctionGemma |
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
| New interpreter | `haskell/effects/<name>-interpreter/` |
| New MCP tool/agent | `haskell/agents/<name>/` |
| TypeScript frontend/bot | `typescript/<name>/` |
| Agents (consuming repos) | Separate repo (anemone, urchin, etc.) |

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


## LSP Integration

The `tidepool-lsp-interpreter` provides LSP for code intelligence:

```haskell
import Tidepool.Effects.LSP
import Tidepool.LSP.Interpreter (withLSPSession, runLSP)

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
