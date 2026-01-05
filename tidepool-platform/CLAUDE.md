# tidepool-platform - Runtime Platform & Effect Interpreters

Core runtime support for running Tidepool agents: effect interpreters and LLM integration.

## What This Is

Shared runtime infrastructure providing:
- **Effect Runners**: Interpreters for LLM, RequestInput, Log, Time, etc.
- **LLM Integration**: Anthropic API client with retry logic and tool dispatch

## Key Modules

| Module | Purpose |
|--------|---------|
| `Tidepool.Effect.Runners` | Effect interpreters (runLLM, runRequestInput, runLog, etc.) |

## Effect Runners

```haskell
-- LLM effect with tool support and retry logic
runLLMWithToolsHooked :: LLMConfig -> ToolDispatcher -> Eff (LLM : es) a -> Eff es a

-- RequestInput for user prompts
runRequestInput :: (Text -> IO Text) -> Eff (RequestInput : es) a -> Eff es a

-- Time effect (IO-blind)
runTime :: IOE :> es => Eff (Time : es) a -> Eff es a
```

## Dependencies

Used by: agents in consuming repos (e.g., `~/dev/anemone`) that need LLM effects.

## Legacy Code (Deprecated)

The `Tidepool.GUI.*` modules contain threepenny-gui infrastructure that is no longer actively used.
New development should use the native server (`tidepool-native-gui/server`) or Cloudflare Worker (`deploy/`) instead.
