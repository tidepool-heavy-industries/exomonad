# tidepool-platform - Runtime Platform & Effect Interpreters

Core runtime support for running Tidepool agents: effect interpreters, LLM integration, GUI infrastructure.

## What This Is

Shared runtime infrastructure providing:
- **Effect Runners**: Interpreters for LLM, RequestInput, Log, Time, etc.
- **LLM Integration**: Anthropic API client with retry logic and tool dispatch
- **GUI Core**: GUIBridge pattern for threepenny-gui with TVar/MVar communication
- **Theme System**: Color palettes and CSS generation for agent UIs

## Key Modules

| Module | Purpose |
|--------|---------|
| `Tidepool.Effect.Runners` | Effect interpreters (runLLM, runRequestInput, runLog, etc.) |
| `Tidepool.GUI.Core` | GUIBridge type, state synchronization, polling pattern |
| `Tidepool.GUI.Theme` | Theme system with color palettes and CSS generation |
| `Tidepool.GUI.Widgets` | Generic widgets (textInput, choiceCards, narrativePane) |

## Effect Runners

```haskell
-- LLM effect with tool support and retry logic
runLLMWithToolsHooked :: LLMConfig -> ToolDispatcher -> Eff (LLM : es) a -> Eff es a

-- RequestInput for user prompts
runRequestInput :: (Text -> IO Text) -> Eff (RequestInput : es) a -> Eff es a

-- Time effect (IO-blind)
runTime :: IOE :> es => Eff (Time : es) a -> Eff es a
```

## GUI Pattern

The GUIBridge pattern enables game loops to communicate with threepenny-gui:

```haskell
data GUIBridge state = GUIBridge
  { gbState            :: TVar state
  , gbStateVersion     :: TVar Int
  , gbPendingRequest   :: TVar (Maybe PendingRequest)
  , gbRequestResponse  :: MVar RequestResponse
  , gbNarrativeLog     :: TVar (Seq Text)
  , gbDebugLog         :: TVar (Seq DebugEntry)
  , gbLLMActive        :: TVar Bool
  }
```

Game loops block on `gbRequestResponse`, GUI polls TVars for updates.

## Dependencies

Used by: agents in user repos (e.g., `~/dev/anemone`) that need GUI or LLM effects.
