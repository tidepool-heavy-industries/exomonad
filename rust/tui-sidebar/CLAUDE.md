# TUI Sidebar

TCP server (port 7433) that renders interactive terminal UIs for Tidepool graph handlers.

**Purpose:** Provides a TUI effect implementation for Haskell agents. Receives UISpec messages, renders them using ratatui, captures keyboard input, and sends Interaction events back.

**Status:** Phase 1 MVP - Text, Button, Input, Progress rendering with Tab+Enter navigation.

## Architecture

```
Haskell Graph Handler                    Rust TUI Sidebar
        │                                       │
        │ showUI formSpec                       │
        ▼                                       │
tidepool-tui-interpreter ─────UISpec───────▶ TCP Server (7433)
   (TCP client)              (NDJSON)           │
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

1. **Haskell → Rust:** UISpec via TCP NDJSON (lines 200-445 of TUI.hs)
2. **Event Loop:** tokio::select! over TCP messages and keyboard polling
3. **Rendering:** UISpec → ratatui widgets (recursive layout)
4. **Input:** Tab (cycle focus), Enter (trigger interaction)
5. **Rust → Haskell:** Interaction via TCP NDJSON

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
cargo run -p tui-sidebar --release
# Listens on port 7433, waits for connection
```

### Hybrid Tidepool (recommended)
```bash
cd /Users/inannamalick/dev/tidepool
./start-augmented.sh
```

This launches the Hybrid Tidepool architecture:
- **process-compose**: Orchestrates services with dependency management
  - control-server: Starts first, exposes HTTP health check (port 7434)
  - tui-sidebar: Starts after control-server is healthy
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
- HTTP health checks ensure control-server is ready before starting dependencies
- Declarative dependency DAG (tui-sidebar waits for control-server health)
- Automatic restart on failure with exponential backoff (2s initial, max 3 restarts)
- Centralized logging to `.tidepool/logs/`
- TCP port readiness probes for service validation

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
| `protocol.rs` | 140 | UISpec, Layout, Element, Interaction types |
| `server.rs` | 120 | TCP listener, NDJSON framing, I/O tasks |
| `ui_stack.rs` | 90 | Stack of active UIs (nested dialogs) |
| `app.rs` | 130 | Main event loop (tokio::select!) |
| `render.rs` | 240 | Recursive layout → ratatui widgets |
| `input.rs` | 120 | Keyboard → Interaction conversion |
| `main.rs` | 50 | CLI entry, logging setup |

**Total:** ~890 lines

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

# Manual TCP test (no Haskell)
# Terminal 1:
cargo run -p tui-sidebar

# Terminal 2:
echo '{"id":"test","layout":{"type":"Vertical","elements":[{"type":"Button","id":"b1","label":"OK"}]}}' | nc localhost 7433
# Expected: Button appears, press Enter → ButtonClicked JSON printed
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

The `tidepool-tui-interpreter` package provides the TCP client:

```haskell
import Tidepool.TUI.Interpreter

withTUIConnection "localhost" "7433" $ \handle -> do
  result <- runM $ runTUI handle $ runGraph handlers input
  print result
```

**Connection behavior:**
- Blocks until tui-sidebar is running on port 7433
- Sends UISpec as NDJSON
- Waits for Interaction response
- Closes connection after interaction

## Known Limitations (Phase 1)

1. **No input editing** - Input fields display value but don't support typing (Phase 3)
2. **Immediate UI close** - UI pops after first interaction (no multi-step dialogs yet)
3. **No dynamic updates** - Can't update progress bars mid-render (Phase 2)
4. **Fixed element sizing** - All elements get equal height (Phase 2: smart sizing)
5. **No Table/Select/List** - These widgets not implemented (Phase 2)
6. **Polling keyboard** - 100ms sleep (crossterm doesn't integrate with tokio)
7. **No error recovery** - Malformed JSON logged but not handled gracefully (Phase 3)

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| TCP (not Unix socket) | Consistent with mantle-agent pattern, cross-platform |
| NDJSON framing | Human-readable, easy debugging, matches control-server protocol |
| tokio async | Required for event loop (TCP + keyboard concurrently) |
| Polling keyboard | crossterm::event::read() blocks, incompatible with tokio::select! |
| Immediate UI close | Phase 1 simplification; Phase 2 adds lifecycle management |
| ratatui | Most popular Rust TUI library, well-maintained |
| UI stack | Supports nested dialogs (Phase 2+), even though Phase 1 doesn't use it |

## Troubleshooting

### "Connection refused" when starting control-server

**Problem:** tui-sidebar not running.

**Fix:** Start tui-sidebar first: `cargo run -p tui-sidebar --release`

**Or:** Use Zellij layout to start all components: `zellij --layout .zellij/tidepool.kdl`

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
