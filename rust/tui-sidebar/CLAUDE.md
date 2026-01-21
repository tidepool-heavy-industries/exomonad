# TUI Sidebar

Unix socket client that renders interactive terminal UIs for Tidepool graph handlers.

**Purpose:** Provides a TUI effect implementation for Haskell agents. Connects to `control-server` via Unix socket, receives UISpec messages, renders them using ratatui, captures keyboard input, and sends Interaction events back.

**Status:** Phase 1 MVP - Text, Button, Input, Progress rendering with Tab+Enter navigation.

## Architecture

```
Haskell Graph Handler                    Rust TUI Sidebar
        │                                       │
        │ showUI formSpec                       │
        ▼                                       │
tidepool-tui-interpreter ─────UISpec───────▶ Unix Socket
 (Part of control-server)    (NDJSON)           │
        │                                       ▼
        │                                  Event Loop
        │                                       │
        │                                  ┌────┴────┐
        │                                  │         │
        │                               Render   Keyboard
        │                                (ratatui) (crossterm)
        │                                       │
        ◀────────────Interaction────────────────┘
              (ButtonClicked, etc.)
```

### Data Flow

1. **Haskell → Rust:** UISpec via Unix socket NDJSON (lines 200-445 of TUI.hs)
2. **Event Loop:** tokio::select! over Unix stream messages and keyboard polling
3. **Rendering:** UISpec → ratatui widgets (recursive layout)
4. **Input:** Tab (cycle focus), Enter (trigger interaction)
5. **Rust → Haskell:** Interaction via Unix socket NDJSON

### Protocol Reference

All types match `/Users/inannamalick/dev/tidepool/haskell/dsl/core/src/Tidepool/Effect/TUI.hs` exactly.

**Critical constraints:**
- `#[serde(tag = "type")]` for all enums (tagged union serialization)
- Field names use snake_case in JSON (Rust convention)
- Split ratio is u8 (0-100)
- Progress uses `value` and `max` fields in JSON

## Running

### Standalone (testing)
```bash
cargo run -p tui-sidebar --release -- --socket .tidepool/sockets/tui.sock
# Connects to control-server, waits for commands
```

### Hybrid Tidepool (recommended)
```bash
cd /Users/inannamalick/dev/tidepool
./start-augmented.sh
```

This launches the Hybrid Tidepool architecture:
- **process-compose**: Orchestrates services with dependency management
  - control-server: Starts first, supports Unix socket health check
  - tui-sidebar: Starts after control-server is healthy, connects to it
  - mcp-server-bridge: socat Unix socket bridge for MCP visibility
- **Zellij**: 3-pane layout for visualization
  - Pane 1: Welcome screen with instructions → **start Claude Code manually in your project**
  - Pane 2: process-compose TUI dashboard (live service status)
  - Pane 3: control-server logs

After Zellij launches, in Pane 1:
```bash
cd /path/to/your/project  # Navigate to your project
claude-code               # Start Claude Code
```

**Orchestration features (process-compose):**
- Unix socket health checks ensure control-server is ready before starting dependencies
- Declarative dependency DAG (tui-sidebar waits for control-server health)
- Automatic restart on failure with exponential backoff (2s initial, max 3 restarts)
- Centralized logging to `.tidepool/logs/`
- Unix socket readiness probes for service validation

**vs. Previous Zellij-only approach:**
- Replaces bash logic in KDL (port polling, backgrounding, error handling)
- Declarative YAML config instead of embedded bash scripts
- process-compose dashboard provides real-time service status
- Unified logging and monitoring for all services

### Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `RUST_LOG` | info | Log level (trace, debug, info, warn, error) |

## Module Structure

| Module | Lines | Purpose |
|--------|-------|---------|
| `protocol.rs` | 150 | UISpec, Layout, Element, Interaction types |
| `server.rs` | 160 | Unix client, NDJSON framing, I/O tasks, health listener |
| `ui_stack.rs` | 90 | Stack of active UIs (nested dialogs) |
| `app.rs` | 160 | Main event loop (tokio::select!) |
| `render.rs` | 250 | Recursive layout → ratatui widgets |
| `input.rs` | 120 | Keyboard → Interaction conversion |
| `main.rs` | 110 | CLI entry, logging setup, health listener init, retry logic |

**Total:** ~1040 lines

## Phase 1 MVP (Implemented)

### Supported Elements
- ✅ Text - Static text display
- ✅ Button - Focusable, sends ButtonClicked
- ✅ Input - Focusable, sends InputSubmitted (no editing yet)
- ✅ Progress - Progress bar display

### Supported Layouts
- ✅ Vertical - Stack elements vertically
- ✅ Horizontal - Stack elements horizontally
- ✅ Split - Left/right with ratio

### Keyboard Navigation
- ✅ Tab - Cycle focus through focusable elements
- ✅ Enter - Trigger interaction (ButtonClicked, InputSubmitted)
- ✅ Ctrl+C - Exit immediately

### Rendering
- ✅ Recursive layout rendering (handles nested Split)
- ✅ Focus highlighting (green background for buttons, cyan for inputs)
- ✅ ratatui widgets (Paragraph, Block, Gauge)

## Phase 2/3 (Planned)

### Additional Elements (Phase 2)
- ❌ Table - Data table with row selection
- ❌ Select - Dropdown with arrow key navigation
- ❌ List - Simple list display

### Additional Interactions (Phase 2)
- ❌ TableRowSelected - User selected table row
- ❌ SelectionChanged - Dropdown selection changed

### Dynamic Updates (Phase 2)
- ❌ UpdateUI messages (modify elements without full re-render)
- ❌ Progress bar updates
- ❌ Text content updates

### Input Editing (Phase 3)
- ❌ Character accumulation (type into inputs)
- ❌ Backspace support
- ❌ Cursor position display
- ❌ InputChanged events (keystroke)

### Error Handling (Phase 3)
- ❌ Malformed JSON recovery
- ❌ Connection loss recovery
- ❌ File logging (tracing appender)
- ❌ Graceful Ctrl+C shutdown

## Testing

```bash
# Protocol roundtrip tests
cargo test -p tui-sidebar

# Manual Unix Socket test (requires control-server)
# Terminal 1:
cabal run tidepool-control-server

# Terminal 2:
cargo run -p tui-sidebar
```

## Integration with Haskell

### TUI Effect (Haskell Side)

Graph handlers use the TUI effect:

```haskell
import Tidepool.Effect.TUI

showFormHandler :: Input -> Eff (TUI ': effs) (GotoChoice targets)
showFormHandler input = do
  interaction <- showUI $ UISpec
    { uiId = "config-form"
    , uiLayout = Vertical
        [ EText "title" "Configure Options"
        , EInput "name" "Name" ""
        , EButton "submit" "Submit"
        , EButton "cancel" "Cancel"
        ]
    }

  case interaction of
    ButtonClicked _ "submit" -> pure $ gotoChoice @"process" config
    ButtonClicked _ "cancel" -> pure $ gotoExit cancelled
```

### TUI Interpreter (Haskell Side)

The `tidepool-control-server` package provides the Unix socket server:

```haskell
-- In Tidepool.Control.Server.hs
-- Listens on .tidepool/sockets/tui.sock
-- Accepts connections from tui-sidebar
```

**Connection behavior:**
- control-server starts and listens on `tui.sock`
- tui-sidebar starts and connects to `tui.sock`
- Bidirectional NDJSON over Unix socket

## Known Limitations (Phase 1)

1. **No input editing** - Input fields display value but don't support typing (Phase 3)
2. **Auto-pop after interaction** - UI automatically pops after sending Interaction; multi-step flows use repeated PushUI
3. **No dynamic updates** - Can't update progress bars mid-render (Phase 2)
4. **Fixed element sizing** - All elements get equal height (Phase 2: smart sizing)
5. **No Table/Select/List** - These widgets not implemented (Phase 2)
6. **Polling keyboard** - 100ms sleep (crossterm doesn't integrate with tokio)
7. **No error recovery** - Malformed JSON logged but not handled gracefully (Phase 3)

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| Unix Socket | Improved security and simpler local orchestration vs TCP |
| Client-Server (Sidebar connects to Control) | Sidebar can be restarted independently; better alignment with service dependencies |
| NDJSON framing | Human-readable, easy debugging, matches control-server protocol |
| tokio async | Required for event loop (Unix stream + keyboard concurrently) |
| Polling keyboard | crossterm::event::read() blocks, incompatible with tokio::select! |
| Immediate UI close | Phase 1 simplification; Phase 2 adds lifecycle management |
| ratatui | Most popular Rust TUI library, well-maintained |
| UI stack | Supports nested dialogs (Phase 2+), even though Phase 1 doesn't use it |

## Troubleshooting

### "Connection refused" when starting tui-sidebar

**Problem:** control-server not running or not listening.

**Fix:** Start control-server first: `cabal run tidepool-control-server`

**Or:** Use Zellij layout to start all components: `./start-augmented.sh`

### UI renders but keyboard doesn't work

**Problem:** Terminal not in raw mode or focus in wrong pane.

**Fix:** Click on the tui-sidebar pane (Pane 3) to focus it.

### Button press does nothing

**Problem:** Button not focused.

**Fix:** Press Tab to cycle focus until button is highlighted (green background).

### Garbage on screen after exit

**Problem:** Terminal cleanup failed (panic or Ctrl+C during rendering).

**Fix:** Run `reset` command in terminal, or close/reopen terminal.

## Future Enhancements

1. **Daemon mode** - Long-lived process that accepts multiple connections
2. **Session management** - Reuse connections, reduce latency
3. **Smart layout sizing** - Calculate element heights based on content
4. **Mouse support** - Click buttons directly (crossterm supports this)
5. **Themes** - Customizable colors, styles
6. **Logging to file** - Avoid polluting terminal output
7. **Metrics** - Interaction latency, render times

## References

- **Protocol source:** `/Users/inannamalick/dev/tidepool/haskell/dsl/core/src/Tidepool/Effect/TUI.hs`
- **TUI interpreter:** `/Users/inannamalick/dev/tidepool/haskell/effects/tui-interpreter/`
- **ratatui docs:** https://docs.rs/ratatui/
- **crossterm docs:** https://docs.rs/crossterm/
