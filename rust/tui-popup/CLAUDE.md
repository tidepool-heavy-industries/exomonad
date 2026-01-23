# TUI Popup Binary

Standalone binary that renders interactive TUI popups and communicates with control-server via WebSocket.

## Purpose

Provides a fire-and-forget popup UI for Zellij pane-based interactions. When control-server needs user input, it spawns a Zellij floating pane running `tui-popup`, which connects via WebSocket, receives the popup definition, renders the UI, captures input, and sends the result back on the same connection.

## Architecture (WebSocket Mode)

```
control-server                    Zellij Floating Pane
     |                                  |
     | 1. Spawn pane: tui-popup         |
     | 2. Register pending popup        |
     |------------------------------->  |
     | 3. WebSocket /tui/ws             |
     |<-------------------------------  | (connects)
     | 4. Send PopupDefinition          |
     |------------------------------->  |
     | 5. Block on TMVar                | (user interacts)
     |                                  |
     | 6. Receive PopupResult           |
     |<-------------------------------  |
     | 7. TMVar unblocks, return        |

CRITICAL: tui-popup maintains a single WebSocket connection throughout.
The connection must stay open between receive and send, otherwise the
server-side handler will clean up and the response will be lost.
```

## Usage

### Via CLI Argument

```bash
tui-popup --spec '{"title":"Confirm Action","components":[...]}'
```

### Via Stdin

```bash
echo '{"title":"Confirm Action","components":[...]}' | tui-popup --stdin
```

## Protocol

### Input: PopupDefinition

JSON structure defining the popup UI:

```json
{
  "title": "Confirm Action",
  "components": [
    {
      "id": "action",
      "type": "text",
      "content": "Action: Delete 15 files"
    },
    {
      "id": "details",
      "type": "text",
      "content": "This cannot be undone."
    }
  ]
}
```

### Output: PopupResult

JSON structure with user response:

```json
{
  "button": "submit",
  "values": {
    "choice": "Yes",
    "custom": "Additional notes here"
  }
}
```

**Button values:**
- `"submit"` - User pressed Enter
- `"decline"` - User pressed Esc
- `"timeout"` - Operation timed out (not implemented in binary, handled by caller)
- `"error"` - Error occurred

## Supported Components

All components from tui-sidebar are supported:

| Component | Description |
|-----------|-------------|
| `text` | Static text display |
| `slider` | Numeric input with min/max |
| `checkbox` | Boolean toggle |
| `textbox` | Text input (single or multiline) |
| `choice` | Single-select dropdown |
| `multiselect` | Multiple selection list |
| `group` | Section header |

See `rust/tui-sidebar/CLAUDE.md` for full component documentation.

## Keyboard Controls

- **Enter** - Submit form (button="submit")
- **Esc** - Cancel form (button="decline")
- **Tab / Shift+Tab** - Navigate between components
- **Arrow keys** - Adjust slider, cycle choice, navigate multiselect
- **Space** - Toggle checkbox, toggle multiselect item
- **Characters** - Type into textbox
- **Backspace** - Delete character in textbox

## Code Reuse

This binary reuses **all** rendering logic from `tui-sidebar`:

```rust
use tui_sidebar::popup::run_popup;
use tui_sidebar::protocol::PopupDefinition;

let definition: PopupDefinition = serde_json::from_str(&json)?;
let result = run_popup(definition)?;
println!("{}", serde_json::to_string(&result)?);
```

The `tui-sidebar` library provides:
- `popup::run_popup()` - Blocking event loop
- `protocol::PopupDefinition` - Input type
- `protocol::PopupResult` - Output type
- `realm::*` - All tui-realm component implementations

## Error Handling

Errors are logged to stderr (not stdout) to avoid polluting JSON output:

```bash
tui-popup --spec '...' > result.json 2> error.log
```

Exit codes:
- `0` - Success (popup completed, even if cancelled)
- `1` - Error (parse failure, rendering failure, etc.)

## Example: Zellij Integration

How control-server spawns popups:

```bash
# Create temp file for result
RESULT_FILE=/tmp/popup-result-$$.json

# Spawn Zellij floating pane
zellij action new-pane --floating --name "tidepool-popup" -- \
  sh -c "tui-popup --spec '$POPUP_JSON' > $RESULT_FILE"

# Wait for pane to close (process exits)
# Read result from temp file
```

The pane closes automatically when `tui-popup` exits.

## Building

```bash
cargo build --release -p tui-popup
```

Binary output: `target/release/tui-popup`

## Installation

The binary must be in PATH for control-server to spawn it:

```bash
# Option 1: Copy to system bin
cp target/release/tui-popup ~/.local/bin/

# Option 2: Copy to hangar runtime bin (recommended)
cp target/release/tui-popup ../runtime/bin/

# Option 3: Specify custom path in control-server config (future)
```

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| **Standalone binary** | Zellij panes need a simple executable, not a long-lived service |
| **JSON via CLI arg** | Simpler than pipes; shell escaping is handled by escapeShellArg |
| **Result to stdout** | Standard Unix pattern; easy to capture in temp file |
| **Reuse tui-sidebar** | All rendering logic already exists; zero duplication |
| **Logs to stderr** | Keeps stdout clean for JSON output |
| **Auto-exit** | Pane closes automatically; no cleanup needed |

## Troubleshooting

### "Binary not found"

Control-server can't find `tui-popup` in PATH.

**Fix:** Ensure binary is installed in PATH or specify location.

### "JSON parse error"

PopupDefinition JSON is malformed.

**Fix:** Validate JSON before passing to tui-popup:

```bash
echo "$JSON" | jq . > /dev/null && tui-popup --spec "$JSON"
```

### "Garbage on screen"

Terminal cleanup failed (panic or Ctrl+C).

**Fix:** Run `reset` in terminal, or close/reopen terminal.

### Popup doesn't appear

Zellij pane spawn failed.

**Fix:** Check:
- Is Zellij running?
- Is user in a Zellij session?
- Check control-server logs for spawn errors

## Future Enhancements

- ⏳ Configurable binary path (via control-server config)
- ⏳ Named pipe result communication (instead of temp file)
- ⏳ Timeout handling (currently relies on caller)
- ⏳ Validation rules (client-side validation before submit)

## Related Documentation

- **[rust/tui-sidebar/CLAUDE.md](../tui-sidebar/CLAUDE.md)** - Component library
- **[haskell/effects/tui-zellij-interpreter/CLAUDE.md](../../haskell/effects/tui-zellij-interpreter/CLAUDE.md)** - Zellij interpreter
- **[haskell/control-server/CLAUDE.md](../../haskell/control-server/CLAUDE.md)** - MCP tool integration
