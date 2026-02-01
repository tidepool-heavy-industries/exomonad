# zellij-gen

Generate Zellij KDL layout files with baked-in environment variables.

## Overview

Binary that generates Zellij layout files with commands as string literals, solving environment variable propagation issues.

**Problem:** Zellij doesn't propagate environment variables to spawned panes when using CLI arguments.

**Solution:** Generate KDL layouts with baked-in command strings that include env vars as literals.

## Commands

```bash
# Generate main layout (TL/PM/Infrastructure tabs)
zellij-gen main

# Generate subagent layout
zellij-gen subagent <issue_id> <container_id>
```

## Templates

Uses Askama templates (Jinja-like syntax):

| Template | Output | Use Case |
|----------|--------|----------|
| main.kdl.j2 | Main session layout | TL session with multiple tabs |
| agent_tab.kdl.j2 | Single agent tab | Spawned agent worktree |
| subagent.kdl.j2 | Subagent wrapper | Single subagent tab |

**Template location:** `rust/zellij-gen/templates/`

## Output

Layouts written to `/tmp/exomonad-layouts/<name>.kdl` and consumed by:
```bash
zellij action new-tab --layout /tmp/exomonad-layouts/<name>.kdl
```

**Generated files:**
- `main.kdl` - Main session layout (TL, PM, Infrastructure tabs)
- `subagent-<issue_id>.kdl` - Single agent tab for spawned issue

## Example Output

```kdl
layout {
    default_tab_template {
        pane size=1 borderless=true {
            plugin location="zellij:tab-bar"
        }
        children
        pane size=1 borderless=true {
            plugin location="zellij:status-bar"
        }
    }
    tab name="gh-433" {
        pane command="sh" {
            args "-c" "claude"
            cwd "/path/to/worktree"
            close_on_exit true
        }
    }
}
```

**Key features:**
- Includes tab-bar and status-bar plugins (native Zellij UI)
- Wraps command in shell (`sh -c`) for environment inheritance
- Sets `close_on_exit true` for automatic cleanup
- Absolute paths for cwd (no reliance on shell state)

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `EXOMONAD_CONTROL_SOCKET` | Control socket path | `/sockets/control.sock` |

**Baked into layouts:** Environment variables are read at layout generation time and written as string literals into the KDL file. This ensures they're available when Zellij spawns panes.

## Building

```bash
cargo build -p zellij-gen
cargo install --path rust/zellij-gen  # Install to ~/.cargo/bin
```

## Testing

```bash
# Generate main layout
zellij-gen main
cat /tmp/exomonad-layouts/main.kdl

# Generate subagent layout
zellij-gen subagent 123 exomonad-agent-123
cat /tmp/exomonad-layouts/subagent-123.kdl

# Test in Zellij
zellij action new-tab --layout /tmp/exomonad-layouts/main.kdl
```

## Design Notes

- **Baked-in commands**: Environment variables become string literals in KDL
- **Askama templates**: Type-safe template rendering (compile-time checks)
- **Temp directory**: Layouts are ephemeral, cleaned up after use
- **Shell wrapping**: Ensures PATH and env vars are inherited
- **Tab templates**: Consistent UI (tab-bar + status-bar) across all layouts
- **close_on_exit**: Automatic cleanup when Claude exits (no orphaned tabs)

## Template Structure

**agent_tab.kdl.j2:**
```kdl
tab name="{{ tab_name }}" {
    pane command="sh" {
        args "-c" "{{ command }}"
        cwd "{{ cwd }}"
        close_on_exit true
    }
}
```

**Variables:**
- `tab_name`: Display name for tab (e.g., "gh-433")
- `command`: Literal command string (e.g., "claude")
- `cwd`: Absolute path to working directory

## Integration with AgentControlService

AgentControlService uses zellij-gen during spawn:

```rust
// Generate layout
let output = Command::new("zellij-gen")
    .args(["subagent", &issue_id, &container_id])
    .output()?;

// Create tab with generated layout
let layout_path = format!("/tmp/exomonad-layouts/subagent-{}.kdl", issue_id);
Command::new("zellij")
    .args(["action", "new-tab", "--layout", &layout_path])
    .status()?;
```

**Flow:**
1. Generate KDL layout with baked-in vars
2. Write to /tmp/exomonad-layouts/
3. Call `zellij action new-tab --layout <path>`
4. Zellij creates tab with proper environment

## Related Documentation

- [exomonad-runtime](../exomonad-runtime/CLAUDE.md) - AgentControlService calls zellij-gen
- [Zellij KDL Layouts](https://zellij.dev/documentation/layouts.html) - KDL layout docs
- [Root CLAUDE.md](../../CLAUDE.md) - Project overview
