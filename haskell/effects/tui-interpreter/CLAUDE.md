# TUI Interpreter - Bidirectional TUI Sidebar Communication

Interprets the `TUI` effect for graph-based interactive UI flows via Unix socket connection to Rust TUI sidebar.

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
│   showUI popupDefinition → process popupResult                       │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ TUI effect
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ TUI Interpreter                                                      │
│   1. Send PopupDefinition → TUI sidebar                              │
│   2. Wait for PopupResult response (blocking)                        │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ Unix Socket NDJSON
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Rust TUI Sidebar (tui-sidebar)                                      │
│   - Renders PopupDefinition as terminal UI (tui-realm)               │
│   - Captures user interaction (keyboard/mouse)                       │
│   - Sends PopupResult back to interpreter                            │
└─────────────────────────────────────────────────────────────────────┘
```

## Usage

```haskell
import Tidepool.TUI.Interpreter (withTUIUnixConnection, runTUI)
import Tidepool.Effect.TUI
import Network.Socket (withSocketsDo)

main :: IO ()
main = withSocketsDo $ do
  withTUIUnixConnection ".tidepool/sockets/tui.sock" $ \tuiHandle -> do
    runM $ runTUI tuiHandle $ do
      -- Show a popup form
      result <- showUI $ PopupDefinition
        { pdTitle = "Configure Options"
        , pdComponents =
            [ mkTextbox "topic" "Topic" Nothing Nothing Nothing
            , mkSlider "budget" "Budget" 10 100 50 Nothing
            , mkCheckbox "includeTests" "Include test files" False Nothing
            ]
        }

      -- Process result based on button pressed
      case result.prButton of
        "submit" -> do
          let budget = lookupNumber "budget" result.prValues
          pure $ "Submitted with budget: " <> show budget
        _ -> pure "Cancelled"
```

## Protocol

### Message Format

NDJSON (newline-delimited JSON) over Unix socket:

**Haskell → Rust (PopupDefinition):**
```json
{
  "title": "Configure Options",
  "components": [
    {"id": "topic", "type": "textbox", "label": "Topic"},
    {"id": "budget", "type": "slider", "label": "Budget", "min": 10, "max": 100, "default": 50},
    {"id": "includeTests", "type": "checkbox", "label": "Include tests", "default": false}
  ]
}
```

**Rust → Haskell (PopupResult):**
```json
{
  "button": "submit",
  "values": {
    "topic": "authentication",
    "budget": "7/10",
    "includeTests": true
  }
}
```

**Key differences from old protocol:**
- **Request-response pattern**: One PopupDefinition → One PopupResult (not streaming)
- **All values returned**: Result includes all visible component values, not just triggered element
- **No UI stack management**: No PushUI/PopUI/UpdateUI - just send definition, get result

## API

### Connection Management

```haskell
-- Bracket-style Unix socket (recommended)
withTUIUnixConnection :: FilePath -> (TUIHandle -> IO a) -> IO a

-- Bracket-style TCP (legacy)
withTUIConnection :: HostName -> ServiceName -> (TUIHandle -> IO a) -> IO a

-- Manual connection
connectTUIUnix :: FilePath -> IO Socket
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
- **Socket**: Unix socket connection to Rust TUI sidebar
- **Send channel**: Queue of outgoing PopupDefinition messages
- **Receive channel**: Queue of incoming PopupResult responses
- **Session ID**: Identifier for this TUI session

**Note**: popup-tui uses simple request-response. No UI stack, no active UI tracking.

### Background Threads

Two threads per connection:

1. **Sender thread**: Reads from send channel, writes JSON lines to socket
2. **Receiver thread**: Reads JSON lines from socket, parses PopupResult, writes to receive channel

Both threads handle errors gracefully (connection loss, malformed JSON).

### Blocking Behavior

`showUI` is **blocking**: sends PopupDefinition, waits for PopupResult.

No non-blocking operations - the popup-tui pattern is synchronous by design.

## Example: Configuration Form

```haskell
runConfiguration :: Members '[TUI, State AppState] r => Config -> Sem r ConfigResult
runConfiguration initialConfig = do
  -- Show configuration popup
  result <- showUI $ PopupDefinition
    { pdTitle = "Application Settings"
    , pdComponents =
        [ mkGroup "general" "General Settings" Nothing
        , mkTextbox "appName" "Application Name" (Just "My App") Nothing Nothing
        , mkChoice "theme" "Theme" ["Light", "Dark", "Auto"] (Just 0) Nothing
        , mkGroup "advanced" "Advanced" Nothing
        , mkCheckbox "enableDebug" "Enable debug logging" False Nothing
        , mkSlider "cacheSize" "Cache Size (MB)" 10 1000 100 (Just $ Checked "enableDebug")
        ]
    }

  case result.prButton of
    "submit" -> do
      -- Extract values from result
      let appName = lookupText "appName" result.prValues
      let theme = lookupChoice "theme" result.prValues
      let enableDebug = lookupBool "enableDebug" result.prValues
      let cacheSize = lookupNumber "cacheSize" result.prValues

      pure $ ConfigResult
        { crAppName = appName
        , crTheme = theme
        , crDebug = enableDebug
        , crCacheSize = cacheSize
        }
    _ -> pure $ ConfigCancelled
```

## Related Documentation

- **[dsl/core/src/Tidepool/Effect/TUI.hs](../../dsl/core/src/Tidepool/Effect/TUI.hs)** - TUI effect definition + popup-tui protocol types
- **[rust/tui-sidebar/CLAUDE.md](../../../rust/tui-sidebar/CLAUDE.md)** - Rust TUI sidebar implementation
- **[haskell/control-server/CLAUDE.md](../../control-server/CLAUDE.md)** - Control server integration

## Status

✅ Implemented with popup-tui protocol
✅ Building
⬜ Not yet tested end-to-end (requires Rust TUI sidebar connection)
⬜ No example graph (user will test outside this branch)
