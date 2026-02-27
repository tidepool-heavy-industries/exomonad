# exomonad-plugin

Zellij plugin for ExoMonad agent status and popup UI.

## Overview

A Zellij WASM plugin that provides:
- **Status display**: Agent state (IDLE, RUNNING, WAITING, ERROR)
- **Popup UI**: Interactive forms for RequestInput effect
- **Wizard UI**: Multi-pane branching wizard forms (CYOA-style)

## Architecture

```
Haskell WASM → Sidecar → pipe_message_to_plugin("exomonad") → Zellij plugin
                                                                    ↓
                                                            Renders popup UI
                                                                    ↓
                                                            Handles keyboard
                                                                    ↓
                                                            Returns PopupResult
```

## Plugin Messages

Plugin subscribes to:
- **CustomMessage**: Receives PopupDefinition and Status updates from sidecar
- **Key**: Handles keyboard navigation and submission

Message variants (internal PluginMessage enum):
- `Status { state, message }`: Update status display
- `Popup { request_id, definition }`: Show interactive popup
- `ClosePopup`: Dismiss current popup

## Status Display

The plugin maintains and displays agent status:

| State | Description |
|-------|-------------|
| IDLE | Agent ready for input |
| RUNNING | Agent processing request |
| WAITING | Agent waiting for user input |
| ERROR | Agent encountered an error |

**Implementation:** Status bar shows `[EXOMONAD: <state>] <message>`

## Popup Rendering

Components rendered as ASCII UI:
- **Text**: Static display (no interaction)
- **Slider**: `[====|----] 50` with arrow keys (left/right)
- **Checkbox**: `[x]` or `[ ]` with space toggle
- **Textbox**: Editable text field (type to input, backspace to delete)
- **Choice**: `> Option 1` with arrow keys (up/down)
- **Multiselect**: `[x] Option 1` with space toggles
- **Group**: Visual grouping (indentation)

### Navigation

| Key | Action |
|-----|--------|
| Tab / Shift+Tab | Move between fields |
| Enter | Submit form |
| Esc | Cancel form |
| Arrow keys (Up/Down) | Adjust choice selection |
| Arrow keys (Left/Right) | Adjust slider value |
| Space | Toggle checkbox/multiselect |
| Type characters | Input text in textbox |
| Backspace | Delete character in textbox |

### Visibility Rules

Components can be conditionally shown based on visibility rules:
- `Checked(id)`: Show if checkbox is checked
- `Equals {id, value}`: Show if choice equals value
- `GreaterThan {id, min_value}`: Show if slider >= min_value
- `LessThan {id, max_value}`: Show if slider <= max_value
- `CountEquals {id, exact_count}`: Show if multiselect has exact count
- `CountGreaterThan {id, min_count}`: Show if multiselect has >= min_count

**Implementation:** Plugin evaluates visibility rules on every state change and only renders visible components.

## Multi-Pane Wizard

The plugin supports multi-pane wizard forms (CYOA-style branching). A wizard is a JSON object with named panes, a start pane, and transition rules between panes.

### Wizard Schema

```json
{
  "title": "Design Architecture",
  "panes": {
    "approach": {
      "title": "Choose Approach",
      "elements": [
        { "type": "choice", "id": "strategy", "label": "Strategy", "options": ["Simple", "Scalable"] }
      ],
      "then_transition": { "strategy": { "Simple": "confirm", "Scalable": "details" } }
    },
    "details": {
      "title": "Details",
      "elements": [ { "type": "textbox", "id": "notes", "label": "Notes" } ],
      "then_transition": "confirm"
    },
    "confirm": {
      "title": "Confirm",
      "elements": [ { "type": "text", "id": "msg", "content": "Ready to proceed." } ]
    }
  },
  "start": "approach"
}
```

### Transition Rules

- **`then_transition` absent** (terminal pane): Enter submits the wizard, collecting all values from all visited panes.
- **`then_transition` is string**: Unconditional goto (always advance to that pane).
- **`then_transition` is object `{ "field_id": { "OptionA": "pane_a", "OptionB": "pane_b" } }`**: Branch based on the selected option label (for Choice) or `"true"`/`"false"` (for Checkbox).

### Wizard Navigation

| Key | Action |
|-----|--------|
| Enter | Next pane (via transition) or Submit (on terminal pane) |
| Esc | Cancel wizard — returns partial results with `button: "cancelled"` |
| Backspace | Go back to previous pane (when not in a textbox) |
| Tab/Shift+Tab | Move between fields within current pane |

### Wizard Response

The wizard returns a `WizardResult` with values from all visited panes:

```json
{
  "button": "submit",
  "values": {
    "approach": { "strategy": 0 },
    "details": { "notes": "..." }
  },
  "panes_visited": ["approach", "details", "confirm"]
}
```

### Implementation

- `ActiveWizard` wraps an `ActiveForm` for the current pane with wizard-specific state (pane history, collected values, transition resolution).
- Plugin detects wizard vs form by trying to parse pipe payload as `WizardRequest` (has `"wizard"` key) before `PopupRequest`.
- Breadcrumb trail shown above form content: `approach > [details]`.
- Values from each pane are collected when advancing and restored when going back.
- `handle_form_key()` is shared between form and wizard modes for component interaction (Tab, Up/Down, Space, etc.).

## Building

```bash
cargo build -p exomonad-plugin --target wasm32-wasi --release
```

**Output:** `target/wasm32-wasi/release/exomonad-plugin.wasm`

**Requirements:**
- `wasm32-wasi` target installed: `rustup target add wasm32-wasi`
- Zellij plugin API dependencies (zellij-tile crate)

## Deployment

Install to Zellij plugins directory:
```bash
mkdir -p ~/.config/zellij/plugins
cp target/wasm32-wasi/release/exomonad-plugin.wasm ~/.config/zellij/plugins/
```

Load in Zellij layout (KDL):
```kdl
plugin location="file:~/.config/zellij/plugins/exomonad-plugin.wasm"
```

**Note:** The plugin is typically loaded via KDL layouts generated by `zellij-gen`, not manually added to layouts.

## Testing

```bash
# Build and test locally
cargo build -p exomonad-plugin --target wasm32-wasi
cargo test -p exomonad-plugin

# Manual testing in Zellij:
# 1. Build plugin
# 2. Copy to ~/.config/zellij/plugins/
# 3. Start Zellij session
# 4. Send CustomMessage via exomonad
```

**Integration testing:**
1. Run exomonad with Haskell WASM
2. Trigger RequestInput effect
3. Verify popup renders correctly
4. Interact with form (keyboard navigation)
5. Verify PopupResult returned to WASM

## Stdin Injection and the Ink Paste Problem

**Critical:** When injecting text into panes running Ink-based TUIs (Claude Code, Gemini CLI), the Enter keypress MUST be sent as a separate, delayed write. Without this, Ink treats the entire payload (text + CR byte) as a clipboard paste and `key.return` never fires.

**Root cause:** React Ink's `useInput` hook detects multi-byte stdin writes as paste events. When `write_chars_to_pane_id("text")` and `write_to_pane_id(vec![13])` arrive in the same Node.js event loop tick, they coalesce into one `data` event buffer. Ink sees >1 character, enters paste mode, and the CR byte (0x0D) is treated as a literal newline — not a submission trigger.

**Solution:** Defer the Enter keypress via Zellij's `set_timeout(0.1)` + `Timer` event:
1. `write_chars_to_pane_id(text, pane_id)` — sends text immediately
2. Store `pane_id` in `pending_enter` vec, call `set_timeout(0.1)`
3. On `Event::Timer`, drain `pending_enter` and send `write_to_pane_id(vec![13], pane_id)`

The 100ms delay ensures the OS kernel flushes the text write before the CR byte arrives, so Node.js processes them as separate `data` events. Ink then sees an isolated `\r` byte, sets `key.return = true`, and `onSubmit` fires.

**This applies to ANY Ink-based application** (Claude Code, Gemini CLI, Wrangler, etc.) — not just our stack. The distinction is between `key.return` (physical Enter, byte 0x0D in isolation) and `key.enter` (newline, byte 0x0A) — only `key.return` triggers `onSubmit` in `ink-text-input`.

## Design Notes

- **Single plugin**: Handles both status bar and popups (not separate plugins)
- **Protocol types**: Uses exomonad-core (default-features=false) for shared ui_protocol types
- **Event subscription**: CustomMessage, Key, Timer (for deferred Enter), TabUpdate, PaneUpdate
- **WASM target**: wasm32-wasi (Zellij plugin API requirement)
- **Stateful**: Maintains popup state across render cycles
- **ASCII rendering**: No graphics, pure text-based UI (Zellij limitation)
- **Keyboard-only**: No mouse support (Zellij limitation)

## Communication Protocol

**Popups use Zellij CLI pipes (bidirectional, synchronous):**

```
Rust PopupService                              Zellij Plugin
       │                                              │
       ├─ Action::CliPipe { floating: false, ... } ───►│
       │   via ZellijIpc (direct Unix socket)          │
       │                                              │
       │   (IPC reads response loop)                  ├─ block_cli_pipe_input(&pipe_id)
       │                                              ├─ (tiled pane appears, user interacts)
       │                                              │
       │◄────────── cli_pipe_output(&pipe_id, resp) ──┤
       │◄────────── UnblockCliPipeInput(pipe_id) ─────┤
       │                                              ├─ close_self()
       ├─ CliPipeOutput parsed from IPC responses     │
```

Requires permissions: `ReadCliPipes`, `ChangeApplicationState`. Both are requested in `load()`. On first launch after a fresh install, the user must accept the permission prompt in the plugin pane.

**Status updates use CustomMessage (one-way):**
```rust
pipe_message_to_plugin(
    MessageToPlugin::new("exomonad")
        .with_payload(serde_json::to_string(&PluginMessage::Status { ... })?)
);
```

## Related Documentation

- [exomonad-core](../exomonad-core/) - Shared ui_protocol types (used with default-features=false)
- [Zellij Plugin API](https://zellij.dev/documentation/plugins.html) - Zellij plugin docs
- [Root CLAUDE.md](../../CLAUDE.md) - Project overview
