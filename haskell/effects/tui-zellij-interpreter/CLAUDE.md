# TUI Zellij Interpreter

Interprets the `TUI` effect by spawning Zellij floating panes with the `tui-popup` binary.

## When to Read This

Read this if you're:
- Understanding how TUI popups are rendered in Zellij panes
- Debugging popup spawn issues
- Working on control-server MCP tool integration
- Comparing Unix socket vs Zellij pane approaches

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Graph Handler (Haskell)                                              │
│   showUI popupDefinition → process popupResult                       │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ TUI effect
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ TUI Zellij Interpreter (runTUIZellij)                                │
│   1. Serialize PopupDefinition to JSON                               │
│   2. Create temp file for result                                     │
│   3. Spawn Zellij floating pane with tui-popup                       │
│   4. Wait for process exit (blocking)                                │
│   5. Read PopupResult from temp file                                 │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ Zellij action new-pane
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Zellij Floating Pane                                                 │
│   sh -c 'tui-popup --spec "..." > /tmp/result.json'                  │
│                                                                       │
│   ┌─────────────────────────────────────┐                            │
│   │ tui-popup binary (Rust)              │                           │
│   │  - Renders PopupDefinition (ratatui) │                           │
│   │  - Captures keyboard/mouse           │                           │
│   │  - Outputs PopupResult to stdout     │                           │
│   └─────────────────────────────────────┘                            │
│                                                                       │
│   Pane closes when tui-popup exits                                   │
└─────────────────────────────────────────────────────────────────────┘
```

## Usage

```haskell
import Tidepool.TUI.ZellijInterpreter (runTUIZellij)
import Tidepool.Effect.TUI

main :: IO ()
main = do
  result <- runM $ runTUIZellij $ do
    popupResult <- showUI $ PopupDefinition
      { pdTitle = "Confirm Action"
      , pdComponents =
          [ mkText "action" "Delete 15 files" Nothing
          , mkText "warning" "This cannot be undone." Nothing
          ]
      }

    case popupResult.prButton of
      "submit" -> pure "Action confirmed"
      _ -> pure "Action cancelled"
```

## How It Works

### 1. Serialization

PopupDefinition is serialized to JSON:

```haskell
let defJson = LBS.toStrict $ encode definition
let defJsonStr = T.unpack $ T.decodeUtf8 defJson
```

### 2. Temp File Creation

Result file is created in system temp directory:

```haskell
tmpDir <- getCanonicalTemporaryDirectory
let resultFile = tmpDir </> "tui-popup-result.json"
```

### 3. Zellij Pane Spawn

Command is constructed and executed:

```haskell
let shCmd = "tui-popup --spec '" <> escapeShellArg defJsonStr <> "' > " <> resultFile
let zellijArgs =
  [ "action", "new-pane"
  , "--floating"
  , "--name", "tidepool-popup"
  , "--"
  , "sh", "-c", shCmd
  ]

(exitCode, stdout, stderr) <- readProcessWithExitCode "zellij" zellijArgs ""
```

**Why `sh -c`?** Allows output redirection (`>`) which Zellij doesn't support directly.

### 4. Blocking Wait

`readProcessWithExitCode` blocks until the Zellij pane closes (tui-popup exits).

### 5. Result Reading

After process exits, result is read from temp file:

```haskell
resultOrErr <- eitherDecodeFileStrict resultFile
removeFile resultFile  -- Cleanup
case resultOrErr of
  Right result -> pure result
  Left err -> pure $ PopupResult "error" (object [])
```

## Shell Escaping

JSON is embedded in shell command, so single quotes are escaped:

```haskell
escapeShellArg :: String -> String
escapeShellArg = concatMap $ \c ->
  case c of
    '\'' -> "'\\''"  -- End quote, escaped quote, start quote
    _    -> [c]
```

This prevents shell injection attacks.

## Error Handling

### Parse Errors

If PopupDefinition JSON is invalid, tui-popup exits with error:

```haskell
ExitFailure code -> do
  IO.hPutStrLn IO.stderr $ "Zellij pane failed with exit code " <> show code
  pure $ PopupResult "error" (object [])
```

### Zellij Not Running

If Zellij is not running or user is not in a session:

```
Zellij pane failed with exit code 1
stderr: Error: Zellij is not running
```

Control-server logs error and returns error result.

### Temp File Missing

If result file doesn't exist after process exit:

```haskell
Left err -> do
  IO.hPutStrLn IO.stderr $ "Error parsing popup result: " <> err
  pure $ PopupResult "error" (object [])
```

## API

### Interpreter Functions

```haskell
-- Use tui-popup from PATH
runTUIZellij :: LastMember IO effs => Eff (TUI ': effs) a -> Eff effs a

-- Use custom tui-popup binary path
runTUIZellijWithBinary :: LastMember IO effs
                       => FilePath
                       -> Eff (TUI ': effs) a
                       -> Eff effs a
```

### Internal

```haskell
-- Spawn popup pane and wait for result
spawnPopupPane :: FilePath -> PopupDefinition -> IO PopupResult

-- Escape shell argument
escapeShellArg :: String -> String
```

## Configuration

Currently hardcoded to use `tui-popup` from PATH. Future enhancement:

```haskell
-- Future: configurable binary path
data TUIConfig = TUIConfig
  { tcBinaryPath :: FilePath
  , tcTempDir :: Maybe FilePath
  , tcTimeout :: Maybe Int  -- Seconds
  }
```

## Comparison with Unix Socket Approach

| Aspect | Unix Socket (old) | Zellij Pane (new) |
|--------|-------------------|-------------------|
| **Architecture** | Long-lived tui-sidebar process | Fire-and-forget tui-popup binary |
| **Lifecycle** | Process runs continuously | Process spawns per popup |
| **Connection** | Bidirectional Unix socket | Unidirectional (temp file) |
| **Complexity** | Socket management, channels | Simple process spawn |
| **Visibility** | Hidden background process | Visible Zellij pane |
| **Cleanup** | Manual socket cleanup | Automatic on exit |
| **State** | Stateful (session tracking) | Stateless (fresh each time) |

**Why the switch?**

The Unix socket approach required managing a long-lived tui-sidebar process, socket lifecycle, and connection state. The Zellij pane approach is simpler: spawn, wait, read result, done. Zellij handles pane management and cleanup automatically.

## Integration with Control-Server

Control-server MCP tools use this interpreter:

```haskell
-- In Tidepool.Control.Handler.MCP

runTUIInterpreter :: LastMember IO effs => Eff (TUI ': effs) a -> Eff effs a
runTUIInterpreter = runTUIZellij

handleConfirmActionTool :: Logger -> Text -> Value -> IO ControlResponse
handleConfirmActionTool logger reqId args = do
  -- ...
  resultOrErr <- try $ runM
    $ runLog Debug
    $ runTUIInterpreter
    $ runReturn (confirmActionLogic caArgs)
  -- ...
```

## Dependencies

- **tidepool-core** - TUI effect types
- **freer-simple** - Effect system
- **process** - Process spawning
- **aeson** - JSON encoding/decoding
- **temporary** - Temp file creation
- **filepath** - Path manipulation
- **directory** - File operations

## Building

```bash
cabal build tidepool-tui-zellij-interpreter
```

## Testing

### Manual Test

1. Start Zellij session:
   ```bash
   zellij
   ```

2. In Zellij, run control-server:
   ```bash
   cabal run tidepool-control-server
   ```

3. Call MCP tool (via Claude Code or mantle-agent):
   ```json
   {
     "tool": "confirm_action",
     "arguments": {
       "action": "Delete files",
       "details": "This will delete 15 files"
     }
   }
   ```

4. Verify floating pane appears with popup

### Troubleshooting

**Popup doesn't appear:**
- Check Zellij is running: `zellij list-sessions`
- Check tui-popup is in PATH: `which tui-popup`
- Check control-server logs for spawn errors

**Popup appears but result is "error":**
- Check tui-popup binary works: `tui-popup --spec '...'`
- Check temp file permissions
- Check Zellij pane logs

**Popup appears but doesn't close:**
- tui-popup binary crashed (check with `Ctrl+C`)
- Terminal state corrupted (run `reset`)

## Future Enhancements

- ⏳ Configurable binary path (via control-server config)
- ⏳ Named pipe communication (instead of temp file)
- ⏳ Timeout handling (currently blocks indefinitely)
- ⏳ Custom pane options (size, position, direction)
- ⏳ Pane reuse (keep pane open for multiple popups)

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| **Zellij panes** | Native Zellij integration; visible to user |
| **Floating panes** | Modal popup overlay UX |
| **Temp file** | Simple IPC; works across process boundaries |
| **Blocking wait** | Matches TUI effect semantics (blocking for user input) |
| **Shell escaping** | Prevents injection; safe JSON embedding |
| **Auto-cleanup** | Temp file removed after read |
| **PATH lookup** | Standard Unix pattern; easy deployment |

## Related Documentation

- **[rust/tui-popup/CLAUDE.md](../../../rust/tui-popup/CLAUDE.md)** - Binary implementation
- **[rust/tui-sidebar/CLAUDE.md](../../../rust/tui-sidebar/CLAUDE.md)** - Component library
- **[haskell/dsl/core/src/Tidepool/Effect/TUI.hs](../../dsl/core/src/Tidepool/Effect/TUI.hs)** - TUI effect definition
- **[haskell/control-server/CLAUDE.md](../../control-server/CLAUDE.md)** - MCP tool integration
