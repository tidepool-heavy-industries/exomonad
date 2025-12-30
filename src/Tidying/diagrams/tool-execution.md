# Tool Execution

How tools execute during LLM turns and communicate with the GUI.

## Tool → QuestionHandler → GUI Flow

```mermaid
flowchart TD
    subgraph LLM["LLM Turn"]
        L1["LLM requests tool call"]
        L2["toolDispatcher"]
        L3["executeProposeDisposition"]
    end

    subgraph Handler["QuestionHandler (IO)"]
        H1["askQuestion :: Question -> IO Answer"]
        H2["writeTVar gbPendingRequest"]
        H3["takeMVar (with 5min timeout)"]
        H4["Parse response"]
    end

    subgraph GUI["Browser"]
        G1["Poll detects request"]
        G2["renderQuestion"]
        G3["User taps disposition card"]
        G4["tryPutMVar response"]
    end

    subgraph Safety["Error Handling"]
        S1["try/catch around handler"]
        S2["Timeout → fallbackAnswer"]
        S3["Parse error → fallbackAnswer"]
    end

    L1 --> L2 --> L3
    L3 --> H1
    H1 --> H2 --> H3
    H3 --> G1 --> G2 --> G3 --> G4
    G4 --> H3
    H3 --> H4
    H4 --> L3

    L3 -.-> S1
    H3 -.-> S2
    H4 -.-> S3
```

## ProposeDisposition Tool Flow

The main tool for item disposition:

```mermaid
flowchart TD
    A["LLM calls propose_disposition<br/>{item, choices, reason}"]
    B["emit ItemProposed event"]
    C["Build Question<br/>ProposeDisposition item choices reason"]
    D["askQuestion (IO callback)"]
    E{"Response type?"}
    F["DispositionAnswer<br/>Trash/Keep/Place/Skip"]
    G["Parse location if Place"]
    H["Emit UserConfirmed or UserCorrected"]
    I["Return ProposeDispositionResult<br/>disposition + wasCorrection"]
    J["LLM continues turn<br/>with tool result"]

    A --> B --> C --> D --> E
    E --> F --> G --> H --> I --> J
```

## ItemDisposition Enum

Where an item can go (no "Unsure" option - that's an antipattern):

```haskell
data ItemDisposition
  = PlaceAt Text      -- Specific location: "kitchen counter"
  | Trash             -- Garbage
  | Donate            -- Give away
  | Recycle           -- Recycling bin
  | SkipForNow        -- Put it back, come back later
  | NeedMoreInfo      -- Agent needs more context
```

## Question Types and Answers

| Question Type | Answer Type | Fallback |
|---------------|-------------|----------|
| `ProposeDisposition item choices fallback` | `DispositionAnswer ItemDisposition` | `SkipForNow` |
| `Confirm prompt default` | `ConfirmAnswer Bool` | `default` |
| `Choose prompt qid options` | `ChoiceAnswer Text` | `""` |
| `FreeText prompt placeholder` | `TextAnswer Text` | `""` |

## QuestionHandler Implementation

```mermaid
flowchart TD
    A["makeQuestionHandler bridge"]
    B["Return: Question -> IO Answer"]
    C["Post to gbPendingRequest"]
    D["takeMVar with 5min timeout"]
    E{"Timeout?"}
    F["Return fallbackAnswer"]
    G["Parse CustomResponse"]
    H{"Parse success?"}
    I["Return Answer"]
    J["Return fallbackAnswer"]

    A --> B
    B --> C --> D --> E
    E -->|yes| F
    E -->|no| G --> H
    H -->|yes| I
    H -->|no| J
```

## Tool Dispatcher

Routes tool calls to their executors:

```mermaid
flowchart TD
    A["makeTidyingDispatcher questionHandler"]
    B["Return: ToolDispatcher"]
    C["Match tool name"]
    D{"Tool?"}
    E["propose_disposition"]
    F["executeProposeDisposition"]
    G["ask_space_function"]
    H["executeAskSpaceFunction"]
    I["confirm_done"]
    J["executeConfirmDone"]
    K["Unknown tool"]
    L["Return error result"]

    A --> B --> C --> D
    D -->|propose| E --> F
    D -->|ask_function| G --> H
    D -->|confirm| I --> J
    D -->|unknown| K --> L
```

## Error Handling Pattern

All tool execution wraps the QuestionHandler in try/catch:

```haskell
executeProposeDisposition askQuestion input = do
  emit $ ItemProposed ...

  -- Wrapped in try/catch for robustness
  result <- liftIO $ try @SomeException $ askQuestion question

  case result of
    Left err -> do
      logWarn "Question handler failed"
      pure fallback
    Right answer ->
      processAnswer answer
```

## Fallback Answers

When handler fails or times out:

```haskell
fallbackAnswer :: Question -> Answer
fallbackAnswer = \case
  ProposeDisposition _ _ _ -> DispositionAnswer SkipForNow
  Confirm _ defVal         -> ConfirmAnswer defVal
  Choose _ _ _             -> ChoiceAnswer ""
  FreeText _ _             -> TextAnswer ""
```

## TidyingEvent List (All 12 Events)

| Event | Description |
|-------|-------------|
| `PhotoAnalyzed Text` | Photo was analyzed |
| `SituationClassified Text` | Situation/intent classified |
| `ActionTaken Action` | Action was decided |
| `PhaseChanged Phase Phase` | Phase transition (from, to) |
| `SessionEnded Int` | Session ended with item count |
| `UserInputReceived Text` | User input for chat display |
| `ResponseGenerated Text` | Response for chat display |
| `ItemProposed Text [Text]` | Tool: item + dispositions |
| `UserConfirmed Text Text` | Tool: item + chosen disposition |
| `UserCorrected Text Text` | Tool: item + user-provided location |
| `FunctionChosen Text` | Tool: space function selected |
| `SessionConfirmedDone` | Tool: user confirmed done |

## Tool Schema Example

```json
{
  "name": "propose_disposition",
  "description": "Propose what to do with an item",
  "input_schema": {
    "type": "object",
    "properties": {
      "item": { "type": "string" },
      "choices": {
        "type": "array",
        "items": {
          "type": "object",
          "properties": {
            "label": { "type": "string" },
            "description": { "type": "string" }
          }
        }
      },
      "reason": { "type": "string" }
    },
    "required": ["item", "choices"]
  }
}
```

## Why IO Callback (not Effect)?

Tools run **inside** the LLM interpreter where `QuestionUI` has already been consumed:

```
runLLMWithToolsHooked  -- LLM effect interpreted here
  └── tool calls       -- Tools execute during interpretation
       └── need QuestionHandler (IO) not QuestionUI (Effect)
```

The `QuestionHandler` type bridges this:
```haskell
type QuestionHandler = Question -> IO Answer
```

## Key Files

- `Tools.hs` - ProposeDisposition, tool schemas, executors
- `Question.hs` - Question DSL, Answer types, fallbackAnswer
- `GUI/Runner.hs` - makeQuestionHandler, makeTidyingDispatcher
- `GUI/Widgets/Question.hs` - renderQuestion (GUI side)
