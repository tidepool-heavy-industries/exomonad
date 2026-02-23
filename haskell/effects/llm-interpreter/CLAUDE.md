# LLM Interpreter - Anthropic Socket/HTTP Client

Interprets the `LLMComplete` effect by making requests to Anthropic via Unix Socket (preferred) or direct HTTP (teaching only).

## When to Read This

Read this if you're:
- Debugging LLM API calls
- Understanding how graph LLM nodes interpret
- Configuring API keys and socket paths

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Graph LLM Node                                                       │
│   complete SAnthropic config "prompt" (Just schema)                 │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ LLMComplete effect
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ LLM Interpreter (exomonad-socket-client)                             │
│   buildAnthropicRequest → ServiceRequest::AnthropicChat             │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ Unix Socket (NDJSON)
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Service Server (Rust)                                                │
│   Calls Anthropic API                                               │
└─────────────────────────────────────────────────────────────────────┘
```

## Supported Providers

| Provider | Singleton | Config Type | Implementation |
|----------|-----------|-------------|----------------|
| Anthropic | `SAnthropic` | `AnthropicConfig` | Socket-based |

## Usage

```haskell
import ExoMonad.LLM.Interpreter (runLLMComplete, mkLLMEnv, LLMConfig(..))
import ExoMonad.Effects.LLMProvider (LLMComplete, complete, SAnthropic)

config :: LLMConfig
config = LLMSocketConfig ".exo/sockets/service.sock"

main :: IO ()
main = do
  env <- mkLLMEnv config
  runM $ runLLMComplete env $ do
    response <- complete SAnthropic anthropicCfg "Explain monads" Nothing
    -- Handle response
```

## Configuration

### Config Types

```haskell
data LLMConfig
  = LLMSocketConfig
      { lcSocketPath :: FilePath
      }
```

### Per-Request Config

```haskell
data AnthropicConfig = AnthropicConfig
  { acModel :: Text              -- "claude-3-5-sonnet-latest"
  , acMaxTokens :: Int
  , acThinking :: ThinkingBudget  -- Extended thinking config
  , acSystemPrompt :: Maybe Text
  }
```

## Tools and Structured Output

The interpreter supports:

1. **Tool Use** - Passing tools to the LLM
2. **Structured Output** - JSON schema constraints (via forced respond tool)

Tool definitions use `AnthropicTool`:

```haskell
data AnthropicTool = AnthropicTool
  { atName :: Text
  , atDescription :: Text
  , atInputSchema :: Value
  }
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Interpreter.hs` | Effect interpreter, request building |
| `Types.hs` | Config types, wire types, environment |

## Related Documentation

- [dsl/core/CLAUDE.md](../../dsl/core/CLAUDE.md) - LLM effect type, handler patterns
- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern

