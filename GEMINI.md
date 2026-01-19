# Gemini Code Intelligence

This project uses `CLAUDE.md` files for documentation and context. 

## Setup

Before using Gemini with this project, you must build the MCP server and link it to the location expected by `settings.json`.

Run the following command:

```bash
just setup-gemini
```

This will:
1. Build the `tidepool-mcp-server` executable.
2. Create a symlink at `./result/bin/mcp-server`.

## Documentation

Please refer to the following files for information:
- `CLAUDE.md` - Root project overview and architecture.
- `haskell/CLAUDE.md` - Haskell package organization and details.
- `rust/CLAUDE.md` - Rust workspace overview.
- Other `CLAUDE.md` files in subdirectories as linked from the root.

Gemini-specific configuration is located in `settings.json` at the project root.
