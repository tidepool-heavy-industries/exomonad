# UI Interpreter - WebSocket Bridge for Agent UIs

Interprets the `UI` effect by bridging agent interactions to frontend clients over WebSocket.

## When to Read This

Read this if you're:
- Building agent UIs (native-gui, Telegram bot)
- Understanding the request/response flow between agents and frontends
- Debugging UI state synchronization issues
- Working with the native WebSocket server

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Agent Effects                                                        │
│   showText "Hello" / requestTextInput "Name?" / requestChoice [...]  │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ UI effect
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ UI Interpreter                                                          │
│   1. Accumulates messages in UIContext                              │
│   2. On Request*, builds UIState and calls callback                 │
│   3. Callback sends UIState → WebSocket → frontend                  │
│   4. Frontend sends UserAction → callback returns                   │
│   5. Effect resumes with user input                                 │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ UICallback
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ WebSocket / Frontend                                                 │
│   native-gui (Solid.js) / telegram-bot / other clients              │
└─────────────────────────────────────────────────────────────────────┘
```

## Usage

```haskell
import ExoMonad.UI.Interpreter (runUI, UICallback, newUIContext)
import ExoMonad.Effects.UI (showText, requestTextInput, requestChoice)
import ExoMonad.Wire.Types (UIState, UserAction(..))

-- Callback bridges to your transport (WebSocket, HTTP, etc.)
myCallback :: UICallback
myCallback uiState = do
  sendOverWebSocket (encode uiState)  -- Send state to frontend
  action <- receiveFromWebSocket      -- Wait for user action
  pure (decode action)

main :: IO ()
main = do
  ctx <- newUIContext "entry"  -- Start at "entry" node
  result <- runM $ runUI ctx myCallback $ do
    showText "Welcome to ExoMonad!"
    name <- requestTextInput "What's your name?"
    showText ("Hello, " <> name <> "!")

    choice <- requestChoice "What would you like to do?"
      [ ("explore", "Explore the codebase")
      , ("fix", "Fix a bug")
      , ("learn", "Learn something new")
      ]
    pure choice
  print result
```

## Effect Operations

### Display (non-blocking)

| Operation | Effect |
|-----------|--------|
| `showText msg` | Append message to conversation |
| `setThinking True` | Show loading indicator |

### Request (blocking)

| Operation | Waits For | Returns |
|-----------|-----------|---------|
| `requestTextInput prompt` | User text input | `Text` |
| `requestChoice prompt opts` | User selection | Selected key |
| `requestConfirmation prompt` | Yes/No | `Bool` |

## UIContext

Mutable state tracking accumulated UI:

```haskell
data UIContext = UIContext
  { ucMessages  :: IORef (Seq ChatMessage)  -- Conversation history
  , ucGraphNode :: IORef Text               -- Current graph node
  , ucThinking  :: IORef Bool               -- Loading indicator
  }

-- Create context
ctx <- newUIContext "entry"

-- Update graph node (for observability)
setGraphNode ctx "classify"
```

## UICallback Type

```haskell
type UICallback = UIState -> IO UserAction
```

The callback:
1. Receives `UIState` containing current conversation + input request
2. Sends it to the frontend
3. Waits for and returns `UserAction` from the frontend

## Wire Types

Defined in `protocol/wire-types/`:

```haskell
-- Agent → Frontend
data UIState = UIState
  { usMessages :: [ChatMessage]
  , usInputRequest :: Maybe InputRequest
  , usGraphNode :: Text
  , usThinking :: Bool
  }

-- Frontend → Agent
data UserAction
  = TextInput Text
  | ChoiceSelection Text
  | Confirmation Bool
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Interpreter.hs` | Effect interpreter, context management |

## Integration Points

- **native-server**: Wires UICallback to WebSocket handlers
- **native-gui**: Solid.js frontend consuming UIState
- **telegram-bot**: Telegram API consuming UIState

## Related Documentation

- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern
- [protocol/wire-types/CLAUDE.md](../../protocol/wire-types/CLAUDE.md) - Wire format types
- [native-server/CLAUDE.md](../../native-server/CLAUDE.md) - WebSocket server
- [../typescript/native-gui/CLAUDE.md](../../../typescript/native-gui/CLAUDE.md) - Solid.js frontend
