# TUI Popup Binary

Standalone binary that renders interactive TUI popups in Zellij floating panes.

## Purpose

Provides a fire-and-forget popup UI for Zellij pane-based interactions. When control-server needs user input, it uses `tui-spawner` to spawn a Zellij floating pane running `tui-popup`, which reads the popup definition from a file, renders the UI to `/dev/tty`, captures input, and writes the result to a FIFO.

## Architecture (FIFO Mode)

```
control-server (Haskell)
    │ calls tui-spawner binary
    ▼
tui-spawner (Rust)
    ├─ 1. Write popup definition to /sockets/popup-{uuid}-in.json
    ├─ 2. mkfifo /sockets/popup-{uuid}.fifo
    ├─ 3. docker exec exomonad-zellij zellij action new-pane --floating -- \
    │      tui-popup --input /sockets/popup-{uuid}-in.json \
    │                --output /sockets/popup-{uuid}.fifo
    ├─ 4. Block reading from FIFO (kernel handles sync)
    │
    ▼
tui-popup (in zellij container)
    ├─ Reads PopupDefinition from --input file
    ├─ Renders UI to /dev/tty (NOT stdout - crucial!)
    ├─ User interacts (keyboard)
    ├─ Writes PopupResult to --output (FIFO)
    └─ Exits (--close-on-exit closes pane)
    │
    ▼
tui-spawner reads result from FIFO, returns to Haskell
```

**Key insight:** TUI renders to `/dev/tty` directly, not stdout. This allows the result to go to the FIFO while the UI displays in the terminal.

## Usage

```bash
# FIFO mode (production)
tui-popup --input /sockets/popup-123-in.json --output /sockets/popup-123.fifo

# Stdin mode (testing)
echo '{"title":"Test","elements":[...]}' | tui-popup --output /tmp/result.json
```

## CLI Arguments

| Argument | Description |
|----------|-------------|
| `--input <path>` | Read PopupDefinition from file (instead of stdin) |
| `--output <path>` | Write PopupResult to file/FIFO (required) |

## Protocol

### Input: PopupDefinition

JSON structure defining the popup UI:

```json
{
  "title": "Confirm Action",
  "elements": [
    {
      "id": "action",
      "text": "Action: Delete 15 files"
    },
    {
      "id": "details",
      "text": "This cannot be undone."
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
    "choice": "Yes"
  }
}
```

**Button values:**
- `"submit"` - User pressed Enter
- `"cancel"` - User pressed Esc
- `"error"` - Error occurred (check `values.message`)

## Keyboard Controls

- **Enter** - Submit form (button="submit")
- **Esc** - Cancel form (button="cancel")
- **Tab / Shift+Tab** - Navigate between components
- **Arrow keys** - Adjust slider, cycle choice
- **Space** - Toggle checkbox
- **Characters** - Type into textbox

## Code Structure

```rust
// main.rs - CLI entry point
fn main() -> Result<()> {
    let args = Args::parse();
    let definition = read_definition(&args)?;
    let result = run_popup_with_tty(definition)?;  // Uses /dev/tty
    write_result(&args.output, &result)?;
    Ok(())
}

// Uses tui-sidebar library for rendering
use tui_sidebar::popup::run_popup_with_tty;
```

## Building

```bash
cargo build --release -p tui-popup
```

Binary output: `target/release/tui-popup`

## Docker Installation

The binary is installed in the `exomonad-zellij` container via Dockerfile:

```dockerfile
# docker/zellij/Dockerfile
COPY --from=rust-builder /usr/local/bin/tui-popup /usr/local/bin/tui-popup
```

## Troubleshooting

### "Failed to open /dev/tty"

The binary requires access to a real TTY. This happens when:
- Running in a pipe without TTY (e.g., `echo | tui-popup`)
- Running in a container without TTY allocation

**Fix:** Ensure running in a TTY context (Zellij pane provides this).

### "No such file" for --input

The input file doesn't exist or path is wrong.

**Fix:** Check that tui-spawner wrote the file before spawning.

### Popup appears but result not captured

The FIFO wasn't created before tui-popup started writing.

**Fix:** tui-spawner must `mkfifo` before spawning the pane.

## Related Documentation

- **[rust/tui-sidebar/CLAUDE.md](../tui-sidebar/CLAUDE.md)** - Component library (popup.rs)
- **[rust/tui-spawner/CLAUDE.md](../tui-spawner/CLAUDE.md)** - FIFO-based spawning
- **[haskell/control-server/CLAUDE.md](../../haskell/control-server/CLAUDE.md)** - MCP tool integration
