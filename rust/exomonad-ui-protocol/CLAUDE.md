# exomonad-ui-protocol

Shared UI protocol types for ExoMonad popup forms.

## Overview

Defines protocol types for interactive popup UI:
- **PopupDefinition**: Form structure with components
- **Component**: UI elements (text, slider, checkbox, textbox, choice, multiselect, group)
- **VisibilityRule**: Conditional component visibility
- **PopupState**: Runtime state management
- **PopupResult**: User submission result

Used by:
- **Haskell WASM**: Generates popup definitions (ExoMonad.Effects.RequestInput)
- **Zellij plugin**: Renders popups and collects user input

## Key Types

| Type | Purpose |
|------|---------|
| PopupDefinition | Top-level form structure (title + components) |
| Component | UI element (7 variants: text, slider, checkbox, textbox, choice, multiselect, group) |
| VisibilityRule | Conditional visibility (6 variants: Checked, Equals, GreaterThan, LessThan, CountEquals, CountGreaterThan) |
| PopupState | Runtime state (values, button_clicked) |
| ElementValue | Typed value storage (Number, Boolean, Text, Choice, MultiChoice) |
| PopupResult | Submission result (button, values, time_spent_seconds) |

## Component Variants

| Component | Fields | Use Case |
|-----------|--------|----------|
| Text | id, content, visible_when? | Display static or conditional text |
| Slider | id, label, min, max, default, visible_when? | Numeric input with range |
| Checkbox | id, label, default, visible_when? | Boolean toggle |
| Textbox | id, label, placeholder?, rows?, visible_when? | Freeform text input |
| Choice | id, label, options, default?, visible_when? | Single-select dropdown |
| Multiselect | id, label, options, default?, visible_when? | Multi-select checkboxes |
| Group | id, label, visible_when? | Logical grouping |

## Visibility Rules

| Rule | Example | Meaning |
|------|---------|---------|
| Checked("id") | "visible_when": "checkbox1" | Show if checkbox1 is checked |
| Equals({"id": "val"}) | "visible_when": {"choice1": "option2"} | Show if choice1 equals "option2" |
| GreaterThan { id, min_value } | {"id": "s1", "min_value": 10.0} | Show if s1 >= 10.0 |
| LessThan { id, max_value } | {"id": "s1", "max_value": 50.0} | Show if s1 <= 50.0 |
| CountEquals { id, exact_count } | {"id": "m1", "exact_count": 2} | Show if exactly 2 items selected in m1 |
| CountGreaterThan { id, min_count } | {"id": "m1", "min_count": 1} | Show if at least 1 item selected in m1 |

## Data Flow

```
Haskell WASM
    ↓ yields RequestInput effect with PopupDefinition
WASM guest
    ↓ returns PopupDefinition JSON
Rust sidecar
    ↓ sends to Zellij plugin via pipe
Zellij plugin
    ↓ renders UI, handles keyboard input
User interaction
    ↓ submits or cancels
Zellij plugin
    ↓ returns PopupResult JSON
Rust sidecar
    ↓ passes to WASM
Haskell WASM
    ↓ resumes with user input
```

## Serialization Format

**Haskell uses flat structure with "type" field:**
```json
{
  "id": "slider1",
  "type": "slider",
  "label": "Confidence",
  "min": 0.0,
  "max": 100.0,
  "default": 50.0
}
```

**Not nested:**
```json
// WRONG (Haskell doesn't use this)
{
  "id": "slider1",
  "spec": {
    "type": "slider",
    ...
  }
}
```

**VisibilityRule is untagged** - matches by JSON structure for backward compatibility.

## PopupState Helpers

Type-safe getters/setters:
```rust
let mut state = PopupState::new(&definition);
state.set_boolean("confirm", true);
state.set_number("confidence", 75.0);

if let Some(val) = state.get_boolean("confirm") {
    // ...
}
```

**Available methods:**
- `get_number(id)` / `set_number(id, value)` - For sliders
- `get_boolean(id)` / `set_boolean(id, value)` - For checkboxes
- `get_text(id)` / `set_text(id, value)` - For textboxes
- `get_choice(id)` / `set_choice(id, index)` - For single-select
- `get_multi_choice(id)` / `set_multi_choice(id, selections)` - For multiselect

## PopupResult

Returned when user submits or cancels:

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PopupResult {
    pub button: String,  // "submit", "cancel", or custom button ID
    pub values: HashMap<String, Value>,  // Component values as JSON
    pub time_spent_seconds: Option<u32>,  // Optional telemetry
}
```

**Example JSON:**
```json
{
  "button": "submit",
  "values": {
    "confidence": 75.0,
    "confirm": true,
    "notes": "This looks good"
  },
  "time_spent_seconds": 12
}
```

## Building

```bash
cargo build -p exomonad-ui-protocol
cargo test -p exomonad-ui-protocol
```

## Testing

```bash
cargo test -p exomonad-ui-protocol
# Tests verify:
# - Component deserialization from Haskell JSON format
# - VisibilityRule untagged matching
# - PopupState type-safe getters/setters
```

## Design Notes

- **Flat structure**: Matches Haskell ToJSON format (not nested Rust enum)
- **Untagged VisibilityRule**: Matches by structure, no "type" field
- **PopupState helpers**: Type-safe accessors prevent runtime type errors
- **time_spent_seconds**: Optional telemetry for UX analysis
- **Backward compatibility**: Multiselect default field kept for backward compat, though unused

## Related Documentation

- [exomonad-plugin](../exomonad-plugin/CLAUDE.md) - Zellij plugin that renders these definitions
- [haskell/dsl/core](../../haskell/dsl/core/CLAUDE.md) - Haskell RequestInput effect
- [Root CLAUDE.md](../../CLAUDE.md) - Project overview
