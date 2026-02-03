# WASM Guest Plugins (Haskell)

This directory contains the Haskell source for the WASM plugins run by `exomonad-runtime`.

## Plugins

### `wasm-guest-tl` (Tech Lead)
- **Role**: Orchestration, planning, team management.
- **Entry**: `TL/Main.hs`
- **Tools**:
  - `spawn_agents`: Create parallel worktrees for subtasks.
  - `cleanup_agents`: Close tabs/worktrees.
  - `list_agents`: View active agents.
  - `popup`: Interactive TUI forms for human input.

### `wasm-guest-dev` (Developer)
- **Role**: Coding, testing, implementation.
- **Entry**: `Dev/Main.hs`
- **Tools**:
  - `git_*`: Full git access.
  - `fs_*`: File read/write.
  - `file_pr`: Create/update PRs.
  - `github_*`: Issue/PR management.

## Architecture

```
Haskell (Guest)                     Rust (Host)
----------------                    -----------
Effect (UI.ShowPopup)      --->     Host Function (ui_show_popup)
  ↓                                       ↓
Interpreter (runUI)                 UIService::show_popup()
  ↓                                       ↓
FFI (host_ui_show_popup)            Zellij Plugin (Popup Render)
```

## UI Effect (Interactive Popups)

The `UI` effect allows agents to request structured input from the human user via a TUI popup in Zellij.

**Effect Definition:** `ExoMonad.Guest.Effects.UI`

```haskell
data UI m a where
  ShowPopup :: PopupDefinition -> UI m PopupResult
```

**Popup Definition:**
```haskell
data PopupDefinition = PopupDefinition
  { title :: Text
  , components :: [Component]
  }

data Component
  = TextComponent { ... }
  | SliderComponent { ... }
  | CheckboxComponent { ... }
  | ChoiceComponent { ... }
  -- ... (see ExoMonad.Guest.TUI for full list)
```

**Usage Example:**
```haskell
import ExoMonad.Guest.Effects.UI
import ExoMonad.Guest.TUI

askConfirmation :: Member UI r => Sem r Bool
askConfirmation = do
  let def = PopupDefinition
        { title = "Confirm Action"
        , components = 
            [ text "msg" "Do you want to proceed?"
            , checkbox "confirm" "Yes, I am sure" False
            ]
        }
  result <- showPopup def
  -- Parse result...
```

**Note:** Due to cross-compilation limitations with Template Haskell in GHC 9.12 WASM backend, the `UI` effect uses manually written smart constructors and interpreters instead of `makeSem`.

## Building

See root `CLAUDE.md` or `justfile`.
```bash
# Build all plugins
nix build .#wasm-guest-tl
nix build .#wasm-guest-dev
```
