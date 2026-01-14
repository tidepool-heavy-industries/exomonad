# LLM Interpreter - Anthropic/OpenAI HTTP Client

Interprets the `LLMComplete` effect by making HTTP requests to LLM APIs.

## When to Read This

Read this if you're:
- Debugging LLM API calls
- Adding a new LLM provider
- Understanding how graph LLM nodes interpret
- Configuring API keys and endpoints

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Graph LLM Node                                                       │
│   complete SAnthropic config "prompt" (Just schema)                 │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ LLMComplete effect
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ LLM Interpreter (servant-client)                                        │
│   buildAnthropicRequest → POST /v1/messages                         │
│   or buildOpenAIRequest → POST /v1/chat/completions                 │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ HTTP
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Anthropic / OpenAI API                                               │
│   Returns: text, tool_use blocks, structured output                 │
└─────────────────────────────────────────────────────────────────────┘
```

## Supported Providers

| Provider | Singleton | Config Type | Endpoint |
|----------|-----------|-------------|----------|
| Anthropic | `SAnthropic` | `AnthropicConfig` | `/v1/messages` |
| OpenAI | `SOpenAI` | `OpenAIConfig` | `/v1/chat/completions` |

## Usage

```haskell
import Tidepool.LLM.Interpreter (runLLMComplete, mkLLMEnv, LLMConfig(..))
import Tidepool.Effects.LLMProvider (LLMComplete, complete, SAnthropic)

config :: LLMConfig
config = LLMConfig
  { lcAnthropicSecrets = Just $ AnthropicSecrets
      { asApiKey = "sk-ant-..."
      , asBaseUrl = Nothing  -- Use default
      }
  , lcOpenAISecrets = Nothing
  }

main :: IO ()
main = do
  env <- mkLLMEnv config
  runM $ runLLMComplete env $ do
    response <- complete SAnthropic anthropicCfg "Explain monads" Nothing
    -- Handle response
```

## Configuration

### API Keys

```haskell
data LLMConfig = LLMConfig
  { lcAnthropicSecrets :: Maybe AnthropicSecrets
  , lcOpenAISecrets :: Maybe OpenAISecrets
  }

data AnthropicSecrets = AnthropicSecrets
  { asApiKey :: Text
  , asBaseUrl :: Maybe BaseUrl  -- Override for proxies
  }
```

### Per-Request Config

```haskell
data AnthropicConfig = AnthropicConfig
  { acModel :: Text              -- "claude-sonnet-4-20250514"
  , acMaxTokens :: Int
  , acTemperature :: Maybe Double
  , acSystemPrompt :: Maybe Text
  }
```

## Tools and Structured Output

The interpreter supports:

1. **Tool Use** - Passing tools to the LLM
2. **Structured Output** - JSON schema constraints

```haskell
-- With schema
response <- complete SAnthropic cfg prompt (Just jsonSchema)

-- With tools
response <- completeWithTools SAnthropic cfg prompt tools
```

Tool definitions use `AnthropicTool`:

```haskell
data AnthropicTool = AnthropicTool
  { atName :: Text
  , atDescription :: Text
  , atInputSchema :: Value
  }
```

## Error Handling

```haskell
data LLMError
  = LLMNetworkError Text      -- Connection failed
  | LLMAPIError Int Text      -- API returned error status
  | LLMParseError Text        -- Response parse failure
  | LLMRateLimited            -- 429 Too Many Requests
  | LLMContextLength          -- Prompt too long
```

The interpreter maps HTTP errors to these typed errors.

## Key Modules

| Module | Purpose |
|--------|---------|
| `Interpreter.hs` | Effect interpreter, request building |
| `Types.hs` | Config types, environment |
| `API/Anthropic.hs` | Anthropic-specific serialization |
| `API/OpenAI.hs` | OpenAI-specific serialization |

## Testing

The interpreter can be mocked for testing:

```haskell
import Test.Tidepool.MockLLM (runMockLLM, mockResponse)

-- In tests, use mock instead of real HTTP
runMockLLM [mockResponse "Hello!"] $ do
  result <- complete SAnthropic cfg "Hi"
  -- result contains "Hello!"
```

## Related Documentation

- [dsl/core/CLAUDE.md](../../dsl/core/CLAUDE.md) - LLM effect type, handler patterns
- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern
- [session-interpreter/CLAUDE.md](../session-interpreter/CLAUDE.md) - Claude Code subprocess (alternative)
