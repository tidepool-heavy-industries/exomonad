# zellij-gen

Generate Zellij KDL layout files with zjstatus (Solarized Dark theme) and exomonad-plugin.

## Overview

Library and binary for generating Zellij layout files with consistent styling.

**Features:**
- zjstatus status bar with Solarized Dark color scheme
- Git branch display in status bar
- exomonad-plugin for agent status display
- Tab-bar and status-bar plugins
- Login shell for environment inheritance
- Auto-cleanup on exit (`close_on_exit true`)

## Library Usage

The library exposes `generate_agent_layout` for programmatic layout generation:

```rust
use zellij_gen::{generate_agent_layout, AgentTabParams};
use std::path::Path;

let params = AgentTabParams {
    tab_name: "🤖 473-refactor",
    pane_name: "Agent",
    command: "claude --prompt 'test'",
    cwd: Path::new("/path/to/worktree"),
    shell: "/bin/zsh",
    focus: true,
};

let layout = generate_agent_layout(&params)?;
```

**Integration:** `exomonad-runtime` uses this library to generate layouts for spawned agents.

## CLI Usage

```bash
# Generate subagent layout
zellij-gen subagent \
    --tab-name "🤖 473-refactor" \
    --command "claude --prompt 'test'" \
    --cwd /path/to/worktree
```

**Options:**
- `--tab-name`: Display name for the tab (supports emoji)
- `--command`: Command to run in the pane
- `--cwd`: Working directory (absolute path)
- `--shell`: Shell to use (default: $SHELL or /bin/zsh)
- `--output`: Output file path (default: /tmp/exomonad-layouts/<name>.kdl)

## Templates

| Template | Purpose |
|----------|---------|
| `agent_tab.kdl.j2` | Single agent tab with exomonad-plugin |
| `subagent.kdl.j2` | Layout wrapper with zjstatus |
| `main.kdl.j2` | Multi-tab layout with zjstatus |
| `includes/zjstatus.kdl.j2` | Shared zjstatus config (reference) |

**Template location:** `rust/zellij-gen/templates/`

## Plugins

The generated layouts use these Zellij plugins (install to `~/.config/zellij/plugins/`):

| Plugin | Purpose | Keybinding |
|--------|---------|------------|
| **zjstatus** | Solarized Dark status bar with git branch | (always visible) |
| **room** | Quick tab switcher (fuzzy search) | `Ctrl+Space` |
| **ghost** | Floating command terminal | `Ctrl+\` |
| **zellij-forgot** | Keybind cheatsheet | `Ctrl+/` |
| **exomonad-plugin** | Agent status display | (in agent tabs) |

**Install all plugins:**
```bash
cd ~/.config/zellij/plugins
curl -sL "https://github.com/dj95/zjstatus/releases/latest/download/zjstatus.wasm" -o zjstatus.wasm
curl -sL "https://github.com/rvcas/room/releases/latest/download/room.wasm" -o room.wasm
curl -sL "https://github.com/vdbulcke/ghost/releases/latest/download/ghost.wasm" -o ghost.wasm
curl -sL "https://github.com/karimould/zellij-forgot/releases/latest/download/zellij_forgot.wasm" -o zellij_forgot.wasm
```

**Security note:** These commands download plugins from `releases/latest` URLs without checksum verification. For production environments, consider pinning to specific versioned releases and verifying checksums. The plugins run with access to your terminal session.

## Color Scheme: Solarized Dark

| Role | Color | Hex | Usage |
|------|-------|-----|-------|
| bg | Base03 | `#002b36` | Background |
| text | Base0 | `#839496` | Body text, inactive tabs |
| normal | Cyan | `#2aa198` | Normal mode, active tab |
| tab | Green | `#859900` | Tab mode, rename mode, git branch |
| pane | Yellow | `#b58900` | Pane mode, resize mode |
| search | Red | `#dc322f` | Search mode, errors |
| locked | Magenta | `#d33682` | Locked mode, tmux mode |
| scroll | Blue | `#268bd2` | Scroll mode |
| session | Orange | `#cb4b16` | Session mode |

## zjstatus Widgets

The status bar shows:
- **Left:** Mode indicator + session name
- **Center:** Tab list
- **Right:** Git branch (green) + time

**Git branch widget config:**
```kdl
command_git_branch_command     "git rev-parse --abbrev-ref HEAD 2>/dev/null || echo ''"
command_git_branch_format      "#[fg=#859900] {stdout}"
command_git_branch_interval    "5"
command_git_branch_rendermode  "static"
```

## Example Output

```kdl
layout {
    default_tab_template {
        pane size=1 borderless=true {
            plugin location="zellij:tab-bar"
        }
        children
        pane size=1 borderless=true {
            plugin location="file:~/.config/zellij/plugins/zjstatus.wasm" {
                format_right  "{command_git_branch} #[fg=#839496]{datetime}"
                // ... mode colors, git branch widget ...
            }
        }
    }

    tab name="🤖 473-refactor" focus=true {
        pane name="Agent" focus=true {
            command "/bin/zsh"
            args "-l" "-c" "claude --prompt 'test'"
            cwd "/path/to/worktree"
            close_on_exit true
        }
        pane size=3 borderless=true {
            plugin location="file:~/.config/zellij/plugins/exomonad-plugin.wasm"
        }
    }
}
```

## Building

```bash
cargo build -p zellij-gen
cargo test -p zellij-gen
```

## Design Notes

- **Library-first:** Core functionality in lib.rs, CLI wraps it
- **zjstatus integration:** Consistent Solarized Dark theme across all layouts
- **Git branch widget:** Shows current branch in status bar (updates every 5s)
- **Login shell:** Uses `-l` flag to ensure PATH and env vars are inherited
- **Emoji support:** Tab names support emoji for visual agent differentiation
- **Askama templates:** Type-safe template rendering with compile-time checks

## Related Documentation

- [exomonad-runtime](../exomonad-runtime/CLAUDE.md) - Uses this library for agent spawning
- [zjstatus](https://github.com/dj95/zjstatus) - Status bar plugin
- [room](https://github.com/rvcas/room) - Tab switcher plugin
- [ghost](https://github.com/vdbulcke/ghost) - Floating terminal plugin
- [zellij-forgot](https://github.com/karimould/zellij-forgot) - Keybind cheatsheet
- [Zellij KDL Layouts](https://zellij.dev/documentation/layouts.html) - Layout documentation
