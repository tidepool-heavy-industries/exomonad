---
paths:
  - "**/tmux*.rs"
  - "**/session_boot.rs"
---

# tmux Gotchas

## `resize-window` sets `window-size = manual`

Any `tmux resize-window` invocation — including a transient +1/-1 wiggle to fire
SIGWINCH — permanently sets that window's `window-size` option to `manual`
(documented in tmux(1)). The window then freezes at its current size and stops
tracking the attached client, so terminal resizes no longer propagate to its
panes.

If you resize a window programmatically, follow up with:

```
tmux set-option -w -t <target> -u window-size
```

to restore the default (`latest`, track the newest client). `TmuxIpc::wake_pane`
does this; keep the pattern for any new resize-based wake or layout code.

Diagnose a stuck window with `tmux show-options -w -t <target> window-size` —
any output means an override is set.

## Detached sessions default to 80x24

`new-session -d` without `-x`/`-y` creates an 80x24 window and the TUI launched
into it doesn't repaint on attach. `boot_root_session` sizes the root from the
controlling terminal via `stty size`; child windows inherit the attached
client's size and don't need this.

## Buffer-paste for input injection

Never `send-keys` long text directly — use the buffer pattern
(`load-buffer` from a temp file + `paste-buffer` + debounce + `send-keys Enter`),
session-qualified targets, per-target mutex. Implemented once in
`TmuxIpc::inject_input`; route all delivery through it.
