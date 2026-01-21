# TUI Interpreter - Bidirectional TUI Sidebar Communication

Interprets the `TUI` effect for graph-based interactive UI flows via TCP/Unix socket connection to Rust TUI sidebar.

## When to Read This

Read this if you're:
- Implementing graph handlers that use TUI effect
- Connecting to Rust TUI sidebar from Haskell
- Debugging TUI communication protocol
- Understanding the TUI effect interpreter pattern

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Graph Handler (Haskell)                                              │
│   showUI formSpec → updateUI progress → closeUI                     │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ TUI effect
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ TUI Interpreter                                                      │
│   1. Send UISpec → TUI sidebar                                      │
│   2. Wait for Interaction response                                  │
│   3. Send UIUpdate (non-blocking)                                   │
│   4. Close UI when done                                             │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ TCP NDJSON
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Rust TUI Sidebar (tui-sidebar)                                      │
│   - Renders UISpec as terminal UI                                   │
│   - Captures user interaction                                       │
│   - Sends Interaction back to interpreter                           │
└─────────────────────────────────────────────────────────────────────┘
```

## Usage

```haskell
import Tidepool.TUI.Interpreter (withTUIConnection, runTUI)
import Tidepool.Effect.TUI
import Network.Socket (withSocketsDo)

main :: IO ()
main = withSocketsDo $ do
  withTUIConnection "localhost" "7433" $ \tuiHandle -> do
    runM $ runTUI tuiHandle $ do
      -- Show a form
      interaction <- showUI $ UISpec
        { uiId = "config-form"
        , uiLayout = Vertical
            [ EInput "topic" "Topic" ""
            , EButton "submit" "Submit"
            , EButton "cancel" "Cancel"
            ]
        }

      case interaction of
        ButtonClicked _ "submit" -> pure "submitted"
        ButtonClicked _ "cancel" -> pure "cancelled"
        _ -> pure "unknown"
```

## Protocol

### Message Format

NDJSON (newline-delimited JSON) over TCP/Unix socket:

**Haskell → Rust (TUIMessage):**
```json
{"type": "PushUI", "spec": {"id": "form-1", "layout": {...}}}
{"type": "ReplaceUI", "spec": {"id": "form-1", "layout": {...}}}
{"type": "UpdateUI", "update": {"ui_id": "form-1", "element_id": "progress", "update": {...}}}
```

**Rust → Haskell (Interaction):**
```json
{"type": "ButtonClicked", "ui_id": "form-1", "button_id": "submit"}
{"type": "InputSubmitted", "ui_id": "form-1", "input_id": "name", "value": "Alice"}
```

**Note:** There is no `PopUI` message. The Rust TUI sidebar automatically pops its UI stack
after sending an Interaction. For multi-step UIs, just send another `PushUI`.

## API

### Connection Management

```haskell
-- Bracket-style (recommended)
withTUIConnection :: HostName -> ServiceName -> (TUIHandle -> IO a) -> IO a

-- Manual connection
connectTUI :: HostName -> ServiceName -> IO Socket
newTUIHandle :: Text -> Socket -> IO TUIHandle
closeTUIHandle :: TUIHandle -> IO ()
```

### Interpreter

```haskell
runTUI :: LastMember IO effs => TUIHandle -> Eff (TUI ': effs) a -> Eff effs a
```

## Implementation Details

### TUIHandle

Manages bidirectional communication:
- **Socket**: TCP/Unix socket connection to Rust TUI sidebar
- **Send channel**: Queue of outgoing messages (PushUI, ReplaceUI, UpdateUI)
- **Receive channel**: Queue of incoming Interaction events
- **Active UI tracker**: Which UI is currently shown (local state)
- **Session ID**: Identifier for this TUI session

### Background Threads

Two threads per connection:

1. **Sender thread**: Reads from send channel, writes JSON lines to socket
2. **Receiver thread**: Reads JSON lines from socket, parses Interaction, writes to receive channel

Both threads handle errors gracefully (connection loss, malformed JSON).

### Blocking vs Non-Blocking

- `showUI` - **Blocking**: sends UISpec, waits for Interaction
- `updateUI` - **Non-blocking**: sends UIUpdate, returns immediately
- `closeUI` - **Non-blocking**: updates local state only (no wire message)

## Example: Progress Bar Updates

```haskell
runExploration :: Members '[TUI, LSP] r => Query -> Sem r Result
runExploration query = do
  -- Show progress UI
  void $ showUI $ UISpec
    { uiId = "progress"
    , uiLayout = Vertical
        [ EText "status" "Exploring..."
        , EProgress "bar" "Progress" 0 100
        ]
    }

  -- Run exploration with progress updates
  result <- exploreWithProgress query $ \current total -> do
    updateUI $ UIUpdate "progress" "bar" (SetProgress current total)
    updateUI $ UIUpdate "progress" "status" (SetText $ "Found " <> show current <> " items")

  -- Close progress UI
  closeUI

  pure result
```

## Related Documentation

- **[dsl/core/src/Tidepool/Effect/TUI.hs](../../dsl/core/src/Tidepool/Effect/TUI.hs)** - TUI effect definition + protocol types
- **[rust/tui-sidebar/CLAUDE.md](../../../rust/tui-sidebar/CLAUDE.md)** - Rust TUI sidebar implementation
- **[haskell/control-server/CLAUDE.md](../../control-server/CLAUDE.md)** - Control server integration

## Status

✅ Implemented and building
⬜ Not yet tested end-to-end (requires Rust TUI sidebar connection)
⬜ No example graph (user will test outside this branch)
