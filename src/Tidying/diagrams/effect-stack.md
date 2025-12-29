# Effect Stack

Effect interpretation order and capabilities in the Tidying agent.

## Effect Interpretation Order

The agent code is **IO-blind** - all IO happens through effect interpreters.

```mermaid
flowchart TB
    subgraph Agent["Agent Code (IO-blind)"]
        A1["tidyingRun"]
    end

    subgraph Effects["Effect Stack (bottom = outermost)"]
        E1["IOE (root)"]
        E2["Time → getCurrentTime"]
        E3["Random → system random"]
        E4["Emit → event callback"]
        E5["State → runStateWithGUISync"]
        E6["ChatHistory → in-memory"]
        E7["Log → GUIBridge"]
        E8["RequestInput → GUIHandler"]
        E9["LLMWithToolsHooked → Anthropic API"]
        E10["QuestionUI → QuestionHandler"]
    end

    A1 --> E10
    E10 --> E9
    E9 --> E8
    E8 --> E7
    E7 --> E6
    E6 --> E5
    E5 --> E4
    E4 --> E3
    E3 --> E2
    E2 --> E1
```

## Effect Capabilities

| Effect | Capability | Interpreter |
|--------|-----------|-------------|
| `State SessionState` | Get/Put session state | `runStateWithGUISync` (syncs to TVar) |
| `LLM` | Vision, extraction, response | `runLLMWithToolsHooked` |
| `Emit TidyingEvent` | Send events to GUI | Callback to narrative log |
| `Log` | Debug/info/warning | `runLogWithBridge` |
| `Time` | Get current time | `runTime` (IO-blind) |
| `RequestInput` | User choice/text | `runRequestInput` |
| `QuestionUI` | Mid-turn questions | `runQuestionUI` |
| `Random` | Randomness | `runRandom` |

## Agent vs Runner Effect Types

```mermaid
flowchart LR
    subgraph Agent["BaseEffects (IO-blind)"]
        B1["LLM"]
        B2["State SessionState"]
        B3["Emit TidyingEvent"]
        B4["RequestInput"]
        B5["Log"]
        B6["ChatHistory"]
        B7["Random"]
        B8["Time"]
    end

    subgraph Runner["RunnerEffects (with IOE)"]
        R1["LLM"]
        R2["RequestInput"]
        R3["Log"]
        R4["ChatHistory"]
        R5["State SessionState"]
        R6["Emit TidyingEvent"]
        R7["Random"]
        R8["Time"]
        R9["IOE"]
    end

    Agent -->|inject| Runner
```

## State Synchronization

State effect syncs to GUI TVar after every `Put`:

```mermaid
sequenceDiagram
    participant Agent
    participant StateEffect
    participant GUIBridge

    Agent->>StateEffect: Put newState
    StateEffect->>StateEffect: runState (local update)
    StateEffect->>GUIBridge: updateState (TVar)
    StateEffect->>GUIBridge: gbStateVersion += 1
```

## LLM Effect with Tools

```mermaid
flowchart TD
    A["runLLMWithToolsHooked"]
    B["onTurnStart hook<br/>(show spinner)"]
    C["Call Anthropic API"]
    D{"Tool call<br/>requested?"}
    E["Dispatch to tool"]
    F["Execute tool<br/>(may use QuestionHandler)"]
    G["Return result to LLM"]
    H["Continue turn"]
    I["onTurnEnd hook<br/>(hide spinner)"]
    J["Return TurnOutcome"]

    A --> B --> C --> D
    D -->|yes| E --> F --> G --> H --> D
    D -->|no| I --> J
```

## Key Interpreters

### runStateWithGUISync

```haskell
runStateWithGUISync :: GUIBridge state -> state -> Eff (State state : es) a -> Eff es a
runStateWithGUISync bridge initial = reinterpret (runState initial) $ \_ -> \case
  Get -> EState.get
  Put s -> do
    EState.put s
    liftIO $ updateState bridge (const s)  -- Sync to TVar
```

### runLLMWithToolsHooked

```haskell
runLLMWithToolsHooked
  :: LLMHooks
  -> LLMConfig
  -> ToolDispatcher
  -> Eff (LLM : es) a
  -> Eff es a
```

### runQuestionUI

```haskell
runQuestionUI :: QuestionHandler -> Eff (QuestionUI : es) a -> Eff es a
runQuestionUI handler = interpret $ \_ (AskQuestion q) ->
  liftIO $ handler q
```

## Key Files

- `Tidepool/Effect.hs` - Core effects, BaseEffects, RunnerEffects
- `GUI/Runner.hs` - Effect stack wiring
- `GUI/Handler.hs` - GUIHandler, makeGUIHandler
