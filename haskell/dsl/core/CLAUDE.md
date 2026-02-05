# ExoMonad Core

Polysemy-based effect system for LLM tool handlers with compile-time validated structured output.

## What This Package Provides

1. **Effect types** for LLM workflows (State, LLM, Log, Emit, TUI, etc.)
2. **Structured output** with compile-time rejection of Anthropic-incompatible schemas
3. **LLM call configuration** (model selection, prompts, tools)
4. **Template rendering** via Ginger/Jinja
5. **Integration effects** for external services (GitHub, Git, Zellij, etc.)

## Effect Types

Core effects live in `ExoMonad.Effect.Types`:

```haskell
-- State management
get :: Member (State s) r => Sem r s
put :: Member (State s) r => s -> Sem r ()
modify :: Member (State s) r => (s -> s) -> Sem r ()

-- LLM calls with structured output
runTurn :: (Member LLM r, StructuredOutput out)
        => Text -> Text -> Value -> [Value] -> Sem r (TurnResult out)

-- Logging
logInfo, logWarn, logError :: Member Log r => Text -> Sem r ()

-- Event emission
emitEvent :: Member (Emit evt) r => evt -> Sem r ()

-- User input
requestChoice :: Member RequestInput r => Text -> [(Text, a)] -> Sem r a
requestText :: Member RequestInput r => Text -> Sem r Text

-- Popup forms (TUI effect)
showUI :: Member TUI r => PopupDefinition -> Sem r PopupResult

-- Terminate with value
returnValue :: Member (Return a) r => a -> Sem r a
```

### Running Effects

```haskell
import Polysemy (runM, embed)
import ExoMonad.Effect.Types

example :: IO ()
example = runM
        . runState initialState
        . runLog logHandler
        . runTime
        $ myComputation
```

## Structured Output

Types used as LLM output must derive `StructuredOutput`:

```haskell
data Response = Response
  { summary :: Text
  , confidence :: Double
  , tags :: [Text]
  }
  deriving (Generic, StructuredOutput)
```

### Compile-Time Schema Validation

Anthropic's structured outputs don't support `oneOf` schemas. ExoMonad rejects these at compile time:

```haskell
-- ❌ Won't compile - sum type with data generates oneOf
data Result = Success Text | Failure Error
  deriving (Generic, StructuredOutput)

-- ✅ Nullary enums are fine - generate string enum
data Priority = Low | Medium | High
  deriving (Generic, StructuredOutput)
-- Schema: {"type": "string", "enum": ["Low", "Medium", "High"]}

-- ✅ Records with Maybe fields work
data Result = Result
  { success :: Maybe Text
  , failure :: Maybe Error
  }
  deriving (Generic, StructuredOutput)
```

## LLM Configuration

```haskell
import ExoMonad.LLM.Types

-- Model selection
data Model = Haiku | Sonnet | Opus

-- Prompt wrappers (prevent positional errors)
newtype System = System Text
newtype User = User Text

-- Configuration
data CallConfig out tools = CallConfig
  { ccModel :: Model
  , ccMaxTokens :: Maybe Int
  , ccTools :: Maybe tools
  }
```

## Integration Effects

Effects for external services (in `ExoMonad.Effects.*`):

| Effect | Purpose |
|--------|---------|
| `Git` | Git operations (status, log, branch) |
| `GitHub` | GitHub API (issues, PRs) |
| `Worktree` | Git worktree management |
| `Zellij` | Terminal multiplexer control |
| `FileSystem` | File operations |
| `Env` | Environment variables |
| `LLMProvider` | LLM API abstraction |

Each effect has a corresponding interpreter in `haskell/effects/*/`.

## Template Rendering

Jinja templates via Ginger:

```haskell
import ExoMonad.Template.Render

-- Render a template with context
rendered <- renderTemplate template context
```

## Module Organization

```
ExoMonad/
├── Effect/
│   ├── Types.hs      -- Core effects (State, LLM, Log, Emit, etc.)
│   ├── TUI.hs        -- Popup form effect
│   ├── Log.hs        -- Logging infrastructure
│   └── Decision.hs   -- Decision logging for observability
├── Effects/          -- Integration effects (plural = external services)
│   ├── Git.hs
│   ├── GitHub.hs
│   ├── Zellij.hs
│   └── ...
├── LLM/
│   ├── Types.hs      -- Model, System, User, CallConfig
│   ├── Builder.hs    -- Fluent API for LLM calls
│   └── Tools.hs      -- Tool definition helpers
├── StructuredOutput/
│   ├── Class.hs      -- StructuredOutput typeclass
│   └── Generic.hs    -- Generic derivation
├── Schema.hs         -- JSON Schema generation
├── Template/
│   └── Render.hs     -- Jinja template rendering
└── Anthropic/
    └── Types.hs      -- Message, ContentBlock, etc.
```

## WASM Compatibility

Modules are split for WASM compatibility:
- Core types and effects work in WASM
- Native-only modules (TH, file paths) excluded via `if !os(wasi)` in cabal

See the cabal file for the exact split.
