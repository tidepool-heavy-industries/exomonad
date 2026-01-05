# Tidepool - Type-Safe LLM Agent Framework

## What This Is

A type-safe LLM agent loop library for building LLM agents as typed state machines.

The key insight: LLMs don't need raw IO - they need typed state they can read (via templates) and typed mutations they can express (via structured output).

**Status**: Core library compiles. Example agents live in separate repos (e.g., `~/dev/anemone`).

## Start Here

New to this codebase? Here's the reading order:

1. **Graph DSL** - Read `tidepool-core/CLAUDE.md` first. The Graph DSL is the core abstraction for defining LLM agents as typed state machines.

2. **Effect System** - See `Tidepool.Effect.Types` for effect definitions. Agents are IO-blind; all IO happens via effect interpreters.

3. **Tool Definitions** - See `Tidepool.Graph.Tool` for the `ToolDef` typeclass. Tools are typed actions LLMs can invoke.

4. **Example Agent** - See `tidepool-native-gui/server/src/Tidepool/Server/SimpleAgent.hs` for a minimal working agent.

5. **Run Something** - `just native` starts the native server at localhost:8080.

### Where Things Go

| Thing | Location |
|-------|----------|
| New effect type | `tidepool-core/src/Tidepool/Effect/Types.hs` |
| New integration (API) | `tidepool-core/src/Tidepool/Effects/` (plural) |
| New graph annotation | `tidepool-core/src/Tidepool/Graph/Types.hs` |
| Tool definitions | Use `ToolDef` from `Tidepool.Graph.Tool` |
| Agents | Create in a separate repo (e.g., `~/dev/anemone`) |

### Key Naming Conventions

- **Effect** (singular) = core infrastructure (`Tidepool.Effect.*`)
- **Effects** (plural) = integrations/contrib (`Tidepool.Effects.*`)
- **Graph.Tool** = canonical tool definitions (not the deprecated `Tidepool.Tool`)

## Task Tracking (Beads)

This repo uses BD (Beads) for git-native task tracking. The beads database is at `~/dev/tidepool/.beads/` (gitignored, shared by all worktrees).

### Quick Reference

```bash
# From any worktree, use bd at repo root
cd ~/dev/tidepool

# List all tasks
bd list --all

# Create an epic
bd create -t epic "Feature name"

# Create a child task under an epic
bd create -t task --parent tidepool-XXX "Task description"

# Update status
bd update tidepool-XXX.Y -s in_progress
bd update tidepool-XXX.Y -s closed

# View task details
bd show tidepool-XXX
```

### Workflow

1. **Starting work**: Mark your task `in_progress`
2. **Creating PRs**: Reference bead ID in PR description
3. **Completing work**: Mark task `closed` after PR merge
4. **Tracking across worktrees**: All worktrees share the same db

### Landing the Plane (Session Completion)

When ending a work session, complete ALL steps. Work is NOT complete until `git push` succeeds.

1. **File issues for remaining work** - Create beads for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **Push to remote**:
   ```bash
   git pull --rebase && bd sync && git push
   ```
5. **Verify** - `git status` shows "up to date with origin"

### Current Tasks

Run `bd list --all` from `~/dev/tidepool` to see current tasks.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Agent Turn Loop                         │
│                                                             │
│  1. Build context (Haskell: State → Context)                │
│  2. Render template (Jinja: Context → prompt)               │
│  3. Call LLM (runTurn: prompt + schema + tools → result)    │
│  4. Apply structured output (Output → State')               │
│  5. Handle user input (RequestInput for choices)            │
│  6. Return response                                         │
└─────────────────────────────────────────────────────────────┘
```

### Core Design Decisions

1. **freer-simple** for effects
   - Reified continuations enable yield/resume across FFI boundaries
   - Effects: `LLM`, `RequestInput`, `State`, `Emit`, `Random`, `Log`, `ChatHistory`, `Time`

2. **Typed Jinja templates** (via ginger library)
   - Compile-time validation against Haskell types
   - `$(typedTemplateFile ''MyContext "templates/turn.jinja")`
   - LLMs know Jinja from training data

3. **Type-safe tool lists**
   - `ToolList` GADT: `TCons (Proxy @MyTool) $ TCons (Proxy @OtherTool) $ TNil`
   - Tools have access to: `State`, `Emit`, `RequestInput`, `Random`

4. **Delta fields for mutations**
   - LLM outputs deltas, not absolute values
   - `because` fields on every mutation for training data

5. **Typed graph execution** (via `OneOf` sum type)
   - `GotoChoice` wraps `OneOf` for fully typed dispatch
   - `DispatchGoto` typeclass pattern matches to call handlers
   - No Dynamic or unsafeCoerce - exact types at every step

## Effect System

### IO-Blind Architecture

Agents are **IO-blind**: they cannot use `IOE` directly. All IO happens in the runner via effect interpreters. This enables:
- WASM compilation
- Deterministic testing
- Clear separation of pure logic from IO

```haskell
-- BaseEffects: What agents see (no IOE!)
type BaseEffects s evt =
  '[ LLM, State s, Emit evt, RequestInput, Log, ChatHistory, Random, Time ]

-- RunnerEffects: What interpreters use (includes IOE)
type RunnerEffects s event =
  '[ LLM, RequestInput, Log, ChatHistory, State s, Emit event, Random, Time, IOE ]
```

### Core Effects

```haskell
-- LLM effect - interpreter handles API, tools, retries
data LLM :: Effect where
  RunTurnOp :: Text -> [ContentBlock] -> Value -> [Value] -> LLM m (TurnOutcome (TurnResult Value))

-- RequestInput - for tools/game logic needing player input
data RequestInput :: Effect where
  RequestChoice :: Text -> [(Text, a)] -> RequestInput m a
  RequestText :: Text -> RequestInput m Text
  RequestTextWithPhoto :: Text -> RequestInput m (Text, [(Text, Text)])  -- For photo attachments

-- Time effect - IO-blind time access
data Time :: Effect where
  GetCurrentTime :: Time m UTCTime

-- Interpreted by runner:
runTime :: IOE :> es => Eff (Time : es) a -> Eff es a
```

## Project Structure (Monorepo)

```
tidepool-core/             # Core library (reusable)
├── src/
│   ├── Tidepool.hs        # Re-exports
│   ├── Tidepool/
│   │   ├── Effect.hs      # LLM, RequestInput, State, Emit, Random, Time effects
│   │   ├── Template.hs    # TypedTemplate, Template (jinja + schema + tools)
│   │   ├── Tool.hs        # Tool typeclass, ToolList GADT
│   │   ├── Schema.hs      # JSON Schema derivation (deriveSchema)
│   │   ├── Storage.hs     # SQLite persistence for game state
│   │   ├── Graph/         # Type-level DSL for agent graphs
│   │   ├── Template/      # Template analysis tools
│   │   │   └── DependencyTree.hs  # Jinja include/extends parsing, Mermaid generation
│   │   └── GUI/           # Generic threepenny-gui components
│   └── Delta/
│       └── Agent.hs       # Example delta routing agent
├── docs/                  # Core library documentation
├── templates/             # Example templates
└── test/                  # Graph validation tests

tidepool-native-gui/       # Native execution layer
├── server/                # Servant + WebSocket server
├── ui-executor/           # UI effect interpreter
├── llm-executor/          # LLM effect interpreter (Anthropic)
├── habitica-executor/     # Habitica API integration
├── observability-executor/# OTLP traces + Loki logs
├── claude-code-executor/  # Claude Code subprocess
├── lsp-executor/          # LSP client for code intelligence
├── bd-executor/           # Beads issue tracking
├── issue-executor/        # GitHub issues
└── wire-types/            # Shared wire protocol types

tidepool-wasm/             # WASM compilation target
deploy/                    # Cloudflare Worker Durable Object harness
├── src/                   # TypeScript harness for WASM state machines
└── wrangler.toml          # CF Worker configuration

tidepool-platform/         # CLI-enabled agent base
tools/                     # Agent 8 tooling
├── sleeptime-logs/        # Log analysis for agent evolution
└── impact-analysis/       # Code change impact analysis
```

## Implementation Status

### Complete
- Effect system with all core effects defined
- Type-safe tool list (ToolList GADT)
- Template system (ginger library with TH compile-time validation)
- JSON Schema derivation via Template Haskell (`deriveJSONSchema`)
- **Graph: OneOf sum type** - Type-indexed GADT for fully typed dispatch
- **Graph: GotoChoice** - Handler return type wrapping OneOf
- **Graph: DispatchGoto** - Typeclass for typed graph execution (no Dynamic/unsafeCoerce)
- **Native Server** - Servant + wai-websockets with SolidJS frontend
- **Effect Executors** - LLM, UI, Habitica, Observability, LSP, BD, ClaudeCode

## Running

```bash
# Build all packages
cabal build all

# Native server (SimpleAgent example)
just native  # Starts at localhost:8080

# Tests
cabal run test-llm         # LLM integration test (tidepool-core)
cabal test all             # Run all test suites
```

## What Sleeptime Would Evolve

The "sleeptime" learning agent reads run logs and evolves agent definitions:

1. **State fields** - Add new fields to agent state
2. **Output schema** - New mutation types in turn output
3. **Apply logic** - How mutations affect state
4. **Templates** - What context surfaces to LLM
5. **Tool behavior** - How tools execute

The loop structure, effect types, and infrastructure stay stable.

## Cloudflare Worker Deployment (`deploy/`)

The `deploy/` directory contains a Cloudflare Worker Durable Object harness for hosting compiled WASM state machines. This enables long-lived, WebSocket-based agent sessions running on the edge.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Cloudflare Durable Object                                  │
│  ┌───────────────────────────────────────────────────────┐  │
│  │  TypeScript Harness (deploy/src/)                     │  │
│  │  - WebSocket connection management                    │  │
│  │  - Effect interpreter (CF AI, fetch, logging)         │  │
│  │  - WASM instance lifecycle                            │  │
│  └─────────────────────┬─────────────────────────────────┘  │
│                        │ FFI (JSON)                         │
│  ┌─────────────────────▼─────────────────────────────────┐  │
│  │  WASM Module (GHC 9.10 → wasm32-wasi)                 │  │
│  │  - Effectful-based state machine                      │  │
│  │  - Yields serialized effect descriptions              │  │
│  │  - Pure business logic, no IO                         │  │
│  └───────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### WASM Standin

**Current status**: The WASM module (`tidepool.wasm`) comes from a separate standin implementation at `~/dev/tidepool`. This standin demonstrates the effect protocol but will be replaced by compiling the Haskell code in this repo once:

1. The freer-simple-based agent loop is WASM-ready
2. GHC WASM cross-compilation is integrated into the build

The protocol types in `deploy/src/protocol.ts` define the contract between TypeScript and Haskell.

### Files

- `src/index.ts` - Durable Object with WebSocket handling, effect loop
- `src/loader.ts` - GHC WASM loader with JSFFI and WASI polyfills
- `src/protocol.ts` - TypeScript types matching Haskell Serializable.hs
- `src/jsffi.ts` - GHC WASM JavaScript FFI implementation
- `wrangler.toml` - CF Worker configuration (AI binding, DO class)

### Running

```bash
cd deploy
pnpm install
pnpm dev          # Local dev server
pnpm typecheck    # Type check
pnpm lint         # ESLint
pnpm deploy       # Deploy to Cloudflare
```

### WebSocket Protocol

```typescript
// Client → Server
{ type: "init", input: { messageText: "..." } }
{ type: "resume", result: { type: "success", value: ... } }

// Server → Client
{ type: "progress", node: "classify", effect: "LlmComplete" }
{ type: "done", result: { responseText: "..." } }
{ type: "error", message: "..." }
```

## Native Server (`tidepool-native-gui/server/`)

For local development without Cloudflare Workers. Runs agents natively with full effect composition.

### Quick Start

```bash
just native  # Starts server at localhost:8080
# Or directly:
TIDEPOOL_DIST=tidepool-native-gui/solid-frontend/dist cabal run tidepool-native
```

### Architecture

- **Servant** for REST API (`/health`, `/sessions`)
- **wai-websockets** for WebSocket overlay
- **freer-simple** effect composition via `runEffects`
- **STM sessions** for per-connection state

### Effect Stack

```haskell
runEffects :: ExecutorEnv -> UIContext -> UICallback
           -> Eff '[UI, Habitica, LLMComplete, Observability, IO] a
           -> IO a
```

See `tidepool-native-gui/server/CLAUDE.md` for full documentation.

## Observability (OpenTelemetry Traces)

The native server exports OpenTelemetry traces to Grafana Tempo via OTLP HTTP.

### Quick Setup

```bash
# Set OTLP endpoint and credentials
export OTLP_ENDPOINT="https://otlp-gateway-prod-us-west-0.grafana.net/otlp/v1/traces"
export OTLP_USER="<instance-id>"
export OTLP_TOKEN="glc_..."

# Optional: custom service name (default: tidepool-native)
export SERVICE_NAME="my-agent"

# Run server - traces are automatically exported
just native
```

### Instrumented Graph Execution

Use `Execute.Instrumented` for automatic span emission on graph dispatch:

```haskell
import Tidepool.Graph.Execute.Instrumented

-- Wrap graph execution with a root span
result <- withGraphSpan "agent:handle-request" $
  runGraphWithSpans handlers inputValue
```

This creates a span hierarchy:
```
agent:handle-request (root span)
├── node:classify
├── node:route
└── node:respond
```

### Manual Spans

For custom spans within handlers:

```haskell
import Tidepool.Effects.Observability

myHandler input = do
  _ <- startSpan "custom-operation" SpanInternal
    [AttrText "input.type" "message"]
  result <- doWork input
  endSpan False [AttrInt "result.count" (length result)]
  pure result
```

### Key Files

| File | Purpose |
|------|---------|
| `Tidepool.Effects.Observability` | Span effect types and smart constructors |
| `Tidepool.Graph.Execute.Instrumented` | Traced graph dispatch (`runGraphWithSpans`) |
| `Tidepool.Observability.Executor` | OTLP HTTP exporter and Loki client |
| `Tidepool.Observability.Types` | OTLP span types and TraceContext |

See `.claude/skills/tidepool-observability/SKILL.md` for full documentation.

## LSP Integration (Gas Town)

Claude Code agents (polecats, witnesses, mayor) can use LSP for code intelligence.

### Enabling LSP

1. Set `ENABLE_LSP_TOOL=1` in your shell profile
2. Install the HLS plugin: `/plugin marketplace add m4dc4p/claude-hls`
3. Restart Claude Code

### LSP Operations

| Operation | Use Case |
|-----------|----------|
| `hover` | Get type info and documentation |
| `goToDefinition` | Find where a symbol is defined |
| `findReferences` | Find all usages (impact analysis) |
| `documentSymbol` | Get file structure overview |

### Example Usage

```
# Before editing a function, understand it:
LSP hover on function name → get type signature
LSP findReferences → see all call sites

# Validate changes:
Check diagnostics for type errors
```

See `docs/gas-town-lsp-integration.md` for full details.

## LSP Executor Package (`tidepool-native-gui/lsp-executor/`)

The `tidepool-lsp-executor` package provides an LSP effect interpreter using the `lsp-client` library from Hackage.

### Usage

```haskell
import Tidepool.Effects.LSP
import Tidepool.LSP.Executor (withLSPSession, runLSP)

-- Run LSP-enabled action (freer-simple based)
withLSPSession "/path/to/project" $ \session -> do
  let action = runLSP session $ do
        info <- hover doc pos
        defs <- definition doc pos
        pure (info, defs)
  result <- runM action  -- runM from Control.Monad.Freer
  print result
```

### Key Dependencies

- `lsp-client` - Session management and request/response handling
- `lsp-types` - LSP protocol types (version 2.3)

### lsp-types 2.3 Patterns

Result types use `X |? Null` (not `Maybe X`):

```haskell
fromHover :: L.Hover L.|? L.Null -> Maybe HoverInfo
fromHover (L.InR L.Null) = Nothing
fromHover (L.InL h) = Just HoverInfo { ... }
```

Newtypes wrap `|?` types:

```haskell
-- Definition is: newtype Definition = Definition (Location |? [Location])
-- MarkedString is: newtype MarkedString = MarkedString (Text |? MarkedStringWithLanguage)
```

### Running the Smoke Test

```bash
# Requires minimal test project at /tmp/lsp-test-project/
cabal run lsp-smoke-test
```

## References

- freer-simple: https://hackage.haskell.org/package/freer-simple
- Anthropic tool use: https://docs.anthropic.com/en/docs/tool-use
- GHC WASM: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html
