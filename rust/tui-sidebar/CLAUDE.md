# TUI Sidebar

Unix socket client that renders interactive terminal UIs for Tidepool graph handlers using popup-tui.

**Purpose:** Provides a TUI effect implementation for Haskell agents. Connects to `control-server` via Unix socket, receives PopupDefinition messages, renders them using tui-realm + ratatui, captures keyboard/mouse input, and sends PopupResult back.

**Status:** Ported to popup-tui - Full form support with Slider, Checkbox, Textbox, Choice, Multiselect components.

## Architecture

```
Haskell Graph Handler                    Rust TUI Sidebar
        │                                       │
        │ showUI popupDef                       │
        ▼                                       │
tidepool-tui-interpreter ─PopupDefinition──▶ Unix Socket
 (Part of control-server)    (NDJSON)           │
        │                                       ▼
        │                                  Blocking Event Loop
        │                                 (tokio::spawn_blocking)
        │                                       │
        │                                  ┌────┴────┐
        │                                  │         │
        │                               Render   Keyboard/Mouse
        │                              (tui-realm) (crossterm)
        │                                       │
        ◀────────────PopupResult────────────────┘
              (button + all form values)
```

### Data Flow

1. **Haskell → Rust:** PopupDefinition via Unix socket NDJSON
   - Title + list of components (Text, Slider, Checkbox, Textbox, Choice, Multiselect, Group)
   - Each component has ID, type, and optional visibility rules
2. **Rust Processing:**
   - Deserialize PopupDefinition
   - Build tui-realm components from definition
   - Run blocking event loop (crossterm::event::read())
3. **Rendering:** PopupComponent renders all visible components with tui-realm MockComponent trait
4. **Input:** Tab/Shift+Tab (navigate), Arrow keys (adjust/select), Enter (submit), Esc (cancel), Mouse (click to focus/activate)
5. **Rust → Haskell:** PopupResult via Unix socket NDJSON
   - button: "submit" or "decline"
   - values: JSON object with all form values (only visible, interactive components)

### Protocol Reference

All types defined in `/Users/inannamalick/hangars/tidepool/repo/rust/tui-sidebar/src/protocol.rs`.

**Key differences from old protocol:**
- **Request-response pattern:** One PopupDefinition → One PopupResult (not streaming interactions)
- **Flat component list:** All components in a single array, not nested layouts
- **Component-based:** Each component is self-contained with ID, type, label, and optional visibility rule
- **Visibility rules:** Components can be hidden based on state of other components (checked, value comparisons)
- **All values returned:** Result includes all visible interactive component values, not just triggered element

## Running

### Standalone (testing)
```bash
# Connect to control-server (uses $TIDEPOOL_TUI_SOCKET by default)
cargo run -p tui-sidebar --release

# Connect to specific socket
cargo run -p tui-sidebar --release -- --socket /path/to/tui.sock
```

### Hybrid Tidepool (recommended)
```bash
cd /Users/inannamalick/hangars/tidepool/repo
./start-augmented.sh
```

This launches the Hybrid Tidepool architecture:
- **process-compose**: Orchestrates services with dependency management
  - control-server: Starts first, supports Unix socket health check
  - tui-sidebar: Starts after control-server is healthy, connects to it
- **Zellij**: 3-pane layout for visualization

**Orchestration features:**
- Unix socket health checks ensure control-server is ready
- Declarative dependency DAG
- Automatic restart on failure with exponential backoff
- Centralized logging to `.tidepool/logs/`

### Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `RUST_LOG` | info | Log level (trace, debug, info, warn, error) |

## Module Structure

| Module | Lines | Purpose |
|--------|-------|---------|
| `protocol.rs` | 236 | PopupDefinition, Component, ComponentSpec, VisibilityRule, PopupState, ElementValue, PopupResult |
| `server.rs` | 176 | Unix client, NDJSON framing, I/O tasks, health listener |
| `app.rs` | 234 | Async wrapper + blocking popup event loop (tokio::spawn_blocking) |
| `realm/mod.rs` | 468 | PopupComponent, visibility evaluation, focus management, mouse handling |
| `realm/builder.rs` | 144 | Build tui-realm components from PopupDefinition |
| `realm/components/` | ~1000 | 7 tui-realm MockComponent implementations (text, slider, checkbox, textbox, choice, multiselect) |
| `main.rs` | 110 | CLI entry, logging setup, health listener init, retry logic |

**Total:** ~2400 lines

## Supported Components

### Interactive Components
- ✅ **Slider** - Numeric input with min/max, arrow keys to adjust
- ✅ **Checkbox** - Boolean toggle with space/enter
- ✅ **Textbox** - Text input with typing, backspace, optional multiline
- ✅ **Choice** - Single-select dropdown, arrow keys to cycle
- ✅ **Multiselect** - Multiple selection list, space to toggle items

### Display Components
- ✅ **Text** - Static text display
- ✅ **Group** - Section header (rendered as "--- Label ---")

### Keyboard Navigation
- ✅ **Tab** / **Shift+Tab** - Navigate between focusable components
- ✅ **Enter** - Submit form (returns all values with button="submit")
- ✅ **Esc** - Cancel form (returns all values with button="decline")
- ✅ **Arrow keys** - Adjust slider, cycle choice, navigate multiselect, **move cursor in textbox**
- ✅ **Home / End** - Move to start/end of content (slider, choice, multiselect, textbox)
- ✅ **Space** - Toggle checkbox, toggle multiselect item (if focused)
- ✅ **Characters** - Type into textbox (if focused)
- ✅ **Backspace** - Delete character in textbox

### Mouse Support
- ✅ **Click** - Focus component and trigger action (checkbox toggle, choice cycle, multiselect toggle)

### Visibility Rules
- ✅ **Checked(id)** - Show if checkbox is checked
- ✅ **Equals({id: value})** - Show if choice equals value
- ✅ **GreaterThan/LessThan** - Show if slider value meets condition
- ✅ **CountEquals/CountGreaterThan** - Show if multiselect has N items selected

## Rendering Architecture (tui-realm)

The popup-tui pattern uses **tui-realm** for component-based TUI rendering:

### MockComponent Trait
Each component implements `tuirealm::MockComponent`:

```rust
pub trait MockComponent {
    fn view(&mut self, frame: &mut Frame, area: Rect);
    fn query(&self, attr: Attribute) -> Option<AttrValue>;
    fn attr(&mut self, attr: Attribute, value: AttrValue);
    fn state(&self) -> State;
    fn perform(&mut self, cmd: Cmd) -> CmdResult;
}
```

### Component Lifecycle
1. **Build** - `builder.rs` creates MockComponent instances from ComponentSpec
2. **Render** - `view()` draws component in allocated Rect
3. **Command** - `perform(cmd)` handles keyboard/mouse commands (Move, Submit, Type, Cancel)
4. **State sync** - `state()` returns current value, synced to PopupState after each command

### Focus Management
- PopupComponent tracks `focused_component` index
- Focus visible in rendering (borders, colors)
- Tab/Shift+Tab cycles through focusable components
- Mouse click changes focus + triggers action

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
  result <- showUI $ PopupDefinition
    { pdTitle = "Configure Options"
    , pdComponents =
        [ Component "topic" (Text "Choose a topic to explore") Nothing
        , Component "budget" (Slider "Budget" 10 100 50) Nothing
        , Component "includeTests" (Checkbox "Include test files" False) Nothing
        , Component "submit" (Textbox "Submit" Nothing Nothing) Nothing
        ]
    }

  case result.button of
    "submit" -> do
      let budget = lookupNumber "budget" result.values
      let includeTests = lookupBool "includeTests" result.values
      pure $ gotoChoice @"process" (Config budget includeTests)
    _ -> pure $ gotoExit cancelled
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

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| **popup-tui pattern** | Proven component model with visibility rules and mouse support |
| **tui-realm** | Component abstraction (MockComponent trait) enables reusable widgets |
| **Request-response** | Simpler than streaming interactions, fits form-based workflows |
| **Blocking event loop** | popup-tui uses crossterm::event::read() which is blocking; wrapped in tokio::spawn_blocking |
| **Flat component list** | Easier to reference components by ID, supports visibility rules |
| **Visibility rules** | Enable dynamic forms (show/hide based on other inputs) |
| **All values in result** | Haskell gets complete form state, not just triggered element |
| **Unix Socket** | Improved security and simpler local orchestration vs TCP |
| **NDJSON framing** | Human-readable, easy debugging |

## Troubleshooting

### "Connection refused" when starting tui-sidebar

**Problem:** control-server not running or not listening.

**Fix:** Start control-server first: `cabal run tidepool-control-server`

**Or:** Use Zellij layout to start all components: `./start-augmented.sh`

### Components not rendering

**Problem:** Component visibility rule hiding it.

**Fix:** Check visibility rules - components may be hidden based on other component states.

### Mouse clicks not working

**Problem:** Clicking outside component area or on non-interactive component.

**Fix:** Ensure clicking within component bounds. Text and Group are not interactive.

### Garbage on screen after exit

**Problem:** Terminal cleanup failed (panic or Ctrl+C during rendering).

**Fix:** Run `reset` command in terminal, or close/reopen terminal.

## Future Enhancements

1. **Nested groups** - Hierarchical component organization
2. **Validation rules** - Client-side validation before submit
3. **Custom themes** - Configurable colors, styles
4. **Logging to file** - Avoid polluting terminal output
5. **Metrics** - Interaction latency, render times

## References

- **Protocol source:** `/Users/inannamalick/hangars/tidepool/repo/haskell/dsl/core/src/Tidepool/Effect/TUI.hs`
- **TUI interpreter:** `/Users/inannamalick/hangars/tidepool/repo/haskell/effects/tui-interpreter/`
- **popup-tui reference:** `~/dev/popup-tui` (source implementation)
- **tuirealm docs:** https://docs.rs/tuirealm/
- **ratatui docs:** https://docs.rs/ratatui/
- **crossterm docs:** https://docs.rs/crossterm/
