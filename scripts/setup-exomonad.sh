#!/bin/bash
# Setup local exomonad environment for Claude Code.
#
# Creates .exomonad/config.toml and .mcp.json in the current directory.
# Run this from your project root before starting Claude Code.
#
# Usage:
#   ./scripts/setup-exomonad.sh [role]
#
# Roles:
#   tl  - Tech Lead (default)
#   pm  - Product Manager
#   dev - Developer

set -e

ROLE="${1:-tl}"

# Validate role
case "$ROLE" in
    tl|pm|dev)
        ;;
    *)
        echo "Error: Invalid role '$ROLE'. Must be one of: tl, pm, dev" >&2
        exit 1
        ;;
esac

# Create .exomonad directory
mkdir -p .exomonad

# Create config.toml
cat > .exomonad/config.toml << EOF
# ExoMonad sidecar configuration
# This file is gitignored

role = "$ROLE"
EOF

# Create .mcp.json for Claude Code
cat > .mcp.json << EOF
{
  "mcpServers": {
    "exomonad": {
      "type": "stdio",
      "command": "exomonad-sidecar",
      "args": ["mcp-stdio"]
    }
  }
}
EOF

echo "ExoMonad setup complete!"
echo ""
echo "  Role: $ROLE"
echo "  Config: .exomonad/config.toml"
echo "  MCP: .mcp.json"
echo ""
echo "To start Claude Code with ExoMonad tools:"
echo "  claude"
echo ""
echo "Verify with:"
echo "  /mcp    # Should show 'exomonad' connected"
echo "  /tools  # Should show git_* and github_* tools"
