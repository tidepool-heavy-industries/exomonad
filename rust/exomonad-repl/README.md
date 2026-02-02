# ExoMonad MCP TUI REPL

Interactive TUI for testing MCP tools without needing a full Claude session.

## Usage

```bash
cargo run -p exomonad-repl -- --wasm ~/.exomonad/wasm/wasm-guest-dev.wasm
```

## Keybindings

- `Ctrl+C` or `q`: Quit
- `Arrows Up/Down`: Navigate tool list or scroll results
- `Enter`: Select tool (in Tool List) or Execute tool (in Form)
- `Tab / Shift+Tab`: Move focus between form fields
- `Ctrl+L`: Focus Tool List
- `Ctrl+F`: Focus Form
- `Ctrl+R`: Focus Results

## Architecture

- **tuirealm**: TUI framework
- **exomonad-runtime**: Loads and runs WASM plugin
- **exomonad-sidecar**: Reuses MCP routing and tool execution logic
- **schema_parser**: Dynamically generates form fields from JSON schema
