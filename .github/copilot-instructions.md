# Tidepool - Copilot Instructions

## Project Overview

Tidepool is a type-safe LLM agent framework written in Haskell. It demonstrates how to build LLM agents with:
- **Typed state** that LLMs read via templates
- **Typed mutations** that LLMs express via structured output
- **Typed tools** for mid-turn capabilities
- **IO-blind architecture** enabling deterministic testing and eventual WASM compilation

The framework includes two demonstration agents:
1. **DM Agent** - Blades in the Dark-style Dungeon Master with faction mechanics, clocks, and FitD dice
2. **Tidying Agent** - Executive function prosthetic for tackling overwhelming spaces with photo analysis

## Architecture

### Core Design Principles

1. **effectful for effects** - MTL-equivalent performance without INLINE pragma dance
   - Effects: `LLM`, `RequestInput`, `State`, `Emit`, `Random`, `Log`, `ChatHistory`, `Time`
   - Agents are **IO-blind**: they cannot use `IOE` directly
   - All IO happens in runners via effect interpreters

2. **Typed Jinja templates** (via ginger library fork)
   - Compile-time validation against Haskell types
   - Template Haskell: `$(typedTemplateFile ''DMContext "templates/dm_turn.jinja")`
   - LLMs know Jinja from training data

3. **Type-safe tool lists**
   - `ToolList` GADT ensures compile-time tool type safety
   - Example: `TCons (Proxy @ThinkAsDM) $ TCons (Proxy @SpeakAsNPC) $ TNil`
   - Tools have access to: `State`, `Emit`, `RequestInput`, `Random`

4. **Delta fields for mutations**
   - LLM outputs `+2 stress`, not `stress = 5`
   - Every mutation has a `because` field for explainability/training data

5. **Typed graph execution** (via `OneOf` sum type)
   - `GotoChoice` wraps `OneOf` for fully typed dispatch
   - `DispatchGoto` typeclass for pattern matching to call handlers
   - No `Dynamic` or `unsafeCoerce` - exact types at every step

## Project Structure

```
tidepool-core/              # Core library (reusable)
├── src/Tidepool/
│   ├── Effect.hs           # Core effects: LLM, RequestInput, State, Emit, Random, Time
│   ├── Template.hs         # TypedTemplate, Template (jinja + schema + tools)
│   ├── Tool.hs             # Tool typeclass, ToolList GADT
│   ├── Schema.hs           # JSON Schema derivation (deriveSchema)
│   ├── Storage.hs          # SQLite persistence
│   ├── Graph/              # Type-level DSL for agent graphs
│   └── GUI/                # Generic threepenny-gui components

tidepool-dm/                # DM agent (Blades in the Dark)
├── src/DM/
│   ├── State.hs            # WorldState, PlayerState, dice mechanics, factions
│   ├── Output.hs           # TurnOutput with delta fields, applyTurnOutput
│   ├── Tools.hs            # ThinkAsDM, SpeakAsNPC, AskPlayer, Choose
│   ├── Context.hs          # DMContext, DiceContext, Precarity
│   ├── Templates.hs        # dmTurnTemplate, compressionTemplate
│   ├── Loop.hs             # dmTurn, handleDiceSelection, runDMGame
│   └── GUI/                # DM-specific GUI (noir aesthetic)

tidepool-tidying/           # Tidying agent
├── src/Tidying/
│   ├── Agent.hs            # Agent definition, TidyingM monad
│   ├── State.hs            # SessionState, Phase, Piles, Photo
│   ├── Loop.hs             # OODA loop: tidyingTurn, photo analysis
│   ├── Decide.hs           # Pure routing: (State, Extract) → (Action, Phase)
│   ├── Action.hs           # Action ADT
│   └── GUI/                # Tidying-specific GUI

deploy/                     # Cloudflare Worker Durable Object harness
├── src/                    # TypeScript harness for WASM state machines
└── wrangler.toml           # CF Worker configuration
```

## Building and Testing

### Build Commands

```bash
# Build all packages
just build
# or: cabal build all

# Build with warnings as errors
just build-strict

# Clean build artifacts
just clean
```

### Testing

```bash
# Run all Haskell tests
just test
# or: cabal test all

# Run graph validation tests only
just test-graph

# Run TypeScript tests
just test-ts

# Verify protocol conformance (Haskell ↔ TypeScript)
just test-protocol-conformance
```

### Linting

```bash
# Lint Haskell (hlint)
just lint-hs

# Lint TypeScript (ESLint)
just lint-ts

# Typecheck TypeScript
just typecheck-ts

# Run all lints
just lint
```

### Pre-commit Checks

```bash
# Fast: build + lint
just pre-commit-fast

# Full: build + lint + tests
just pre-commit
```

### Running Applications

```bash
# DM CLI
just dm
# or: cabal run tidepool-dm

# DM GUI (browser opens at localhost:8023)
just dm-gui
# or: cabal run tidepool-dm-gui

# Tidying GUI (browser opens at localhost:8024)
just tidy-gui
# or: cabal run tidepool-tidy-gui
```

## Code Conventions

### Effect System Patterns

1. **Effect Definitions**
   ```haskell
   data MyEffect :: Effect where
     DoThing :: Arg -> MyEffect m Result
   
   type instance DispatchOf MyEffect = 'Dynamic
   
   doThing :: MyEffect :> es => Arg -> Eff es Result
   doThing = send . DoThing
   ```

2. **Effect Constraints**
   ```haskell
   myFunction 
     :: ( State WorldState :> es
        , Emit DMEvent :> es
        , Random :> es
        )
     => Eff es ()
   ```

3. **Effect Order Matters**
   - Outermost runs first
   - `runLLMWithTools` needs `State`, `Emit`, `RequestInput` below it
   ```haskell
   runEff
     . runRandom
     . runEmit eventHandler
     . runState initialState
     . runLLMWithTools config dispatcher
     $ computation
   ```

4. **IO-Blind Architecture**
   - Agent code uses `BaseEffects` (no `IOE`)
   - Runners use `RunnerEffects` (includes `IOE`)
   - This enables WASM compilation and deterministic testing

### Template System

1. **Typed Templates** use Template Haskell for compile-time validation:
   ```haskell
   dmTurnTemplate :: Template DMContext
   dmTurnTemplate = $(typedTemplateFile ''DMContext "templates/dm_turn.jinja")
   ```

2. **Template Context** types define what LLM sees:
   ```haskell
   data DMContext = DMContext
     { stress :: Int
     , clocks :: [Clock]
     , npcs :: [Npc]
     -- ... fields accessible in template
     }
   ```

3. **Template includes** and extends are supported via Jinja

### Tool System

1. **Tool Definition**
   ```haskell
   data ThinkAsDM = ThinkAsDM { thought :: Text }
   
   instance Tool ThinkAsDM where
     toolName _ = "think"
     toolDescription _ = "Internal reasoning"
     executeTool input = do
       emit (DMThought input.thought)
       return ()
   ```

2. **Tool Lists** are type-safe GADTs:
   ```haskell
   type DMTools = 
     TCons (Proxy @ThinkAsDM) $
     TCons (Proxy @SpeakAsNPC) $
     TNil
   ```

3. **ToolBreak** for state transitions:
   - Transition tools return `ToolBreak` to restart turn
   - Example: mood transitions (engage, resolve, accept)

### Delta Fields

Always include a `because` field for mutations:

```haskell
data PlayerDeltas = PlayerDeltas
  { stressDelta :: Int      -- +2, not "set to 5"
  , coinDelta :: Int
  , deltaBecause :: Text    -- "caught by guard"
  }
```

### GUI Architecture

1. **Polling-based updates** via `TVar` version numbers
2. **Communication**: `MVar` for blocking requests, `TVar` for state
3. **Widget pattern**: Create widget, return `Element`, update via polling
4. **Accessibility**: Full keyboard support, ARIA labels, focus states

```haskell
-- Widget creation
clocksPanel :: GUIBridge WorldState -> UI Element
clocksPanel bridge = do
  panel <- UI.div #. "clocks-panel"
  updateClocksPanel panel bridge
  pure panel

-- Widget update (called by polling loop)
updateClocksPanel :: Element -> GUIBridge WorldState -> UI ()
updateClocksPanel panel bridge = do
  void $ element panel # set children []
  -- Rebuild contents from state...
```

## Dependencies

### Haskell

- **effectful** - Effect system (no MTL)
- **ginger** - Jinja templating (custom fork with typed templates)
- **aeson** - JSON serialization
- **threepenny-gui** - Browser-based GUI
- **anthropic-sdk** - Anthropic API client
- **stm** - Software Transactional Memory (for GUI bridge)

### TypeScript (deploy/)

- **@cloudflare/workers-types** - Cloudflare Worker types
- **wrangler** - Cloudflare deployment tool

## Common Patterns

### Error Handling

- Use `Either` for recoverable errors
- Use exceptions for truly exceptional conditions
- Effect handlers catch and log errors

### Testing

- Graph validation tests check type-level graph structure
- Protocol conformance tests verify Haskell ↔ TypeScript JSON contract
- Property tests for pure functions (when present)

### FitD Dice Mechanics (DM Agent)

- Position: Controlled/Risky/Desperate
- Effect: Limited/Standard/Great
- Outcome: Critical/Success/Partial/Bad/Disaster
- **Precarity** scales narrative tone based on stress + heat + wanted

### Compression

Long scenes compress into durable state via dedicated template:
- SceneOutcome (summary, key beats)
- WorldDeltas (faction/NPC changes)
- Extractions (threads, rumors, promises)
- Decay (what fades)

## Development Workflow

1. **Make changes** to Haskell or TypeScript code
2. **Build**: `just build`
3. **Lint**: `just lint`
4. **Test**: `just test` (Haskell) and/or `just test-ts` (TypeScript)
5. **Run**: `just dm-gui` or `just tidy-gui` to test interactively

For TypeScript (deploy/):
```bash
cd deploy
pnpm install
pnpm dev          # Local dev server
pnpm deploy       # Deploy to Cloudflare
```

## Additional Resources

- [effectful documentation](https://hackage.haskell.org/package/effectful)
- [Anthropic tool use docs](https://docs.anthropic.com/en/docs/tool-use)
- [Blades in the Dark SRD](https://bladesinthedark.com/) - Inspiration for DM mechanics
- [GHC WASM docs](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)

## Notes

- **WASM compilation**: The deploy/ directory uses a standin WASM module from `~/dev/tidepool`. Full Haskell→WASM compilation is planned.
- **Claude skills**: The `.claude/skills/` directory contains specialized agent instructions for Claude AI (not Copilot).
- **Git hooks**: Run `just install-hooks` to add pre-commit checks.
