# Tool Execution

How tools execute during LLM turns, including transition tools and ToolBreak.

## Tool Result Types

Tools can return two types of results:

```haskell
data ToolResult
  = ToolSuccess Value    -- Continue current turn
  | ToolBreak Text       -- End turn, inject message, transition mode
```

```mermaid
flowchart TD
    T["Tool Executes"]
    T --> TS["ToolSuccess Value"]
    T --> TB["ToolBreak Text"]

    TS --> C["Return result to LLM<br/>LLM may call more tools"]

    TB --> E["End current turn immediately"]
    E --> M["Change Mode in state"]
    M --> I["Inject synthetic user message"]
    I --> N["Start new turn with new Mode"]
```

## Tool Categories

### Regular Tools (ToolSuccess)

Execute an action, return result, LLM continues:

| Tool | Mode | Purpose |
|------|------|---------|
| `propose_disposition` | Sorting | Propose item disposition, get user confirmation |

### Transition Tools (ToolBreak)

Change mode, end turn, start fresh:

| Tool | From → To | Initial Data |
|------|-----------|--------------|
| `begin_sorting` | Surveying → Sorting | SortingData {} |
| `need_to_clarify` | Sorting → Clarifying | ClarifyingData {item, photoContext, reason} |
| `user_seems_stuck` | Sorting → DecisionSupport | DecisionSupportData {stuckItem} |
| `time_to_wrap` | Sorting → WindingDown | WindingDownData {} |
| `resume_sorting` | Clarifying/DecisionSupport → Sorting | SortingData {} |
| `skip_item` | Clarifying → Sorting | SortingData {} |
| `end_session` | WindingDown → (end) | N/A |

## Transition Tool Flow

```mermaid
sequenceDiagram
    participant LLM
    participant Dispatcher
    participant Tool
    participant State
    participant Loop

    LLM->>Dispatcher: Tool call: need_to_clarify({item, context, reason})
    Dispatcher->>Tool: executeNeedToClarify(args)
    Tool->>State: mode = Clarifying(ClarifyingData {...})
    Tool->>Tool: emit ModeChanged event
    Tool->>Dispatcher: return ToolBreak("[Continue as: Clarifying]")
    Dispatcher->>Loop: ToolBreak received
    Loop->>Loop: End current turn
    Loop->>Loop: Append synthetic user message to history
    Loop->>LLM: Start new turn with Clarifying template/tools
```

## Tool Dispatcher

Routes tool calls to executors, handles ToolBreak:

```mermaid
flowchart TD
    D["toolDispatcher(toolName, args)"]
    D --> M{"Match tool name"}

    M -->|propose_disposition| E1["executeProposeDisposition"]
    M -->|need_to_clarify| E2["executeNeedToClarify"]
    M -->|resume_sorting| E3["executeResumeSorting"]
    M -->|user_seems_stuck| E4["executeUserSeemsStuck"]
    M -->|time_to_wrap| E5["executeTimeToWrap"]
    M -->|skip_item| E6["executeSkipItem"]
    M -->|end_session| E7["executeEndSession"]
    M -->|unknown| ERR["ToolSuccess (error message)"]

    E1 --> R1["ToolSuccess (result)"]
    E2 --> R2["ToolBreak"]
    E3 --> R2
    E4 --> R2
    E5 --> R2
    E6 --> R2
    E7 --> R2
```

## Transition Tool Implementation

```haskell
executeNeedToClarify
  :: (State SessionState :> es, Emit TidyingEvent :> es)
  => NeedToClarifyInput
  -> Eff es ToolResult
executeNeedToClarify input = do
  oldMode <- gets @SessionState mode

  -- Create new mode with initial data from tool args
  let newModeData = ClarifyingData
        { cdItem = input.item
        , cdPhotoContext = input.photoContext
        , cdReason = input.reason
        }

  -- Update state
  modify @SessionState $ \s -> s { mode = Clarifying newModeData }

  -- Emit event for debugging/GUI
  emit $ ModeChanged oldMode (Clarifying newModeData)

  -- Return ToolBreak to end turn and inject synthetic message
  pure $ ToolBreak $ "[Continue as: Clarifying. Describe: " <> input.item <> "]"
```

## Regular Tool Implementation

```haskell
executeProposeDisposition
  :: (State SessionState :> es, Emit TidyingEvent :> es, IOE :> es)
  => QuestionHandler
  -> ProposeDispositionInput
  -> Eff es ToolResult
executeProposeDisposition askQuestion input = do
  -- Emit event
  emit $ ItemProposed input.item input.choices

  -- Ask user via QuestionHandler (blocks for response)
  answer <- liftIO $ askQuestion (ProposeDispositionQ input.item input.choices)

  -- Process answer, update piles
  processDispositionAnswer input.item answer

  -- Return success - LLM continues turn
  pure $ ToolSuccess (toJSON result)
```

## QuestionHandler (for Regular Tools)

Regular tools that need user input use the QuestionHandler:

```mermaid
sequenceDiagram
    participant Tool
    participant QH as QuestionHandler
    participant Bridge as GUIBridge
    participant GUI as Browser

    Tool->>QH: askQuestion(ProposeDispositionQ ...)
    QH->>Bridge: writeTVar gbPendingRequest
    QH->>Bridge: takeMVar gbRequestResponse (BLOCKS)

    GUI->>Bridge: Poll detects request
    GUI->>GUI: Render disposition cards
    GUI->>GUI: User taps card
    GUI->>Bridge: tryPutMVar response

    Bridge->>QH: Unblock with response
    QH->>Tool: Return Answer
```

## Mode-Specific Tool Sets

```haskell
toolsForMode :: Mode -> [Value]
toolsForMode (Surveying _) =
  [ beginSortingTool
  ]

toolsForMode (Sorting _) =
  [ proposeDispositionTool
  , needToClarifyTool
  , userSeemsStuckTool
  , timeToWrapTool
  ]

toolsForMode (Clarifying _) =
  [ resumeSortingTool
  , skipItemTool
  ]

toolsForMode (DecisionSupport _) =
  [ resumeSortingTool
  ]

toolsForMode (WindingDown _) =
  [ endSessionTool
  ]
```

## Tool Schema Example

```json
{
  "name": "need_to_clarify",
  "description": "User can't identify the item. Transition to Clarifying mode to describe it in detail. This ENDS the current turn.",
  "input_schema": {
    "type": "object",
    "properties": {
      "item": {
        "type": "string",
        "description": "What item we're clarifying"
      },
      "photo_context": {
        "type": "string",
        "description": "What we observed in the photo (location, nearby objects)"
      },
      "reason": {
        "type": "string",
        "description": "Why user is confused (their response)"
      }
    },
    "required": ["item", "photo_context", "reason"]
  }
}
```

## Events Emitted

| Event | When | Data |
|-------|------|------|
| `ModeChanged` | Transition tool executed | oldMode, newMode |
| `ItemProposed` | propose_disposition called | item, choices |
| `UserConfirmed` | User accepted disposition | item, disposition |
| `UserCorrected` | User provided different location | item, location |
| `SessionEnded` | end_session called | itemsProcessed |

## Key Files

- `Tools.hs` - Tool definitions, executors, `toolsForMode`
- `Loop.hs` - ToolBreak handling, turn continuation
- `GUI/Runner.hs` - QuestionHandler implementation
- `Events.hs` - ModeChanged and other events
