# ExoMonad - Copilot Instructions

## Project Overview

ExoMonad is a type-safe LLM agent framework library written in Haskell. It provides:
- **Typed state** that LLMs read via templates
- **Typed mutations** that LLMs express via structured output
- **Typed tools** for mid-turn capabilities
- **IO-blind architecture** enabling deterministic testing and WASM compilation

Agents are built in separate repos () using this library.

## Architecture

### Core Design Principles

1. **freer-simple for effects** - Reified continuations for yield/resume across FFI
   - Effects: `LLM`, `RequestInput`, `State`, `Emit`, `Random`, `Log`, `ChatHistory`, `Time`
   - Agents are **IO-blind**: they cannot use `IOE` directly
   - All IO happens in runners via effect interpreters

2. **Typed Jinja templates** (via ginger library fork)
   - Compile-time validation against Haskell types
   - Template Haskell: `$(typedTemplateFile ''MyContext "templates/turn.jinja")`
   - LLMs know Jinja from training data

3. **Type-safe tool lists**
   - `ToolList` GADT ensures compile-time tool type safety
   - Tools have access to: `State`, `Emit`, `RequestInput`, `Random`

4. **Delta fields for mutations**
   - LLM outputs deltas, not absolute values
   - Every mutation has a `because` field for explainability/training data

5. **Typed graph execution** (via `OneOf` sum type)
   - `GotoChoice` wraps `OneOf` for fully typed dispatch
   - `DispatchGoto` typeclass for pattern matching to call handlers
   - No `Dynamic` or `unsafeCoerce` - exact types at every step

## Project Structure

```
exomonad-core/              # Core library (Graph DSL, effects, templates)
├── src/ExoMonad/
│   ├── Effect.hs           # Core effects
│   ├── Template.hs         # TypedTemplate
│   ├── Tool.hs             # Tool typeclass, ToolList GADT
│   ├── Schema.hs           # JSON Schema derivation
│   └── Graph/              # Type-level DSL for agent graphs

exomonad-native-gui/        # Native execution layer
├── server/                 # Servant + WebSocket server
├── llm-executor/           # Anthropic API interpreter
├── ui-executor/            # UI effect interpreter
├── observability-executor/ # OTLP + Loki
└── ...                     # Other effect executors
```

## Building and Testing

```bash
# Build all packages
cabal build all

# Run native server
just native  # Starts at localhost:8080

# Run tests
cabal test all
```

## Code Conventions

### Effect System Patterns

1. **Effect Definitions**
   ```haskell
   data MyEffect :: Effect where
     DoThing :: Arg -> MyEffect m Result
   ```

2. **Effect Constraints**
   ```haskell
   myFunction
     :: ( State MyState :> es
        , Emit MyEvent :> es
        )
     => Eff es ()
   ```

3. **IO-Blind Architecture**
   - Agent code uses `BaseEffects` (no `IOE`)
   - Runners use `RunnerEffects` (includes `IOE`)
   - This enables WASM compilation and deterministic testing

### Template System

1. **Typed Templates** use Template Haskell for compile-time validation:
   ```haskell
   myTemplate :: Template MyContext
   myTemplate = $(typedTemplateFile ''MyContext "templates/turn.jinja")
   ```

2. **Template Context** types define what LLM sees

### Tool System

1. **Tool Definition**
   ```haskell
   data MyTool = MyTool { input :: Text }

   instance Tool MyTool where
     toolName _ = "my_tool"
     toolDescription _ = "Description"
     executeTool input = do
       emit (MyEvent input)
       return result
   ```

2. **Tool Lists** are type-safe GADTs

### Delta Fields

Always include a `because` field for mutations:

```haskell
data StateDelta = StateDelta
  { valueDelta :: Int       -- +2, not "set to 5"
  , deltaBecause :: Text    -- "reason for change"
  }
```

## Dependencies

### Haskell

- **freer-simple** - Effect system with reified continuations
- **ginger** - Jinja templating (custom fork with typed templates)
- **aeson** - JSON serialization
- **servant** - REST API + WebSocket

## Common Patterns

### Error Handling

- Use `Either` for recoverable errors
- Effect handlers catch and log errors

### Testing

- Graph validation tests check type-level graph structure

## Additional Resources

- [freer-simple documentation](https://hackage.haskell.org/package/freer-simple)
- [Anthropic tool use docs](https://docs.anthropic.com/en/docs/tool-use)
- [GHC WASM docs](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)