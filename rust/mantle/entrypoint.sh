#!/bin/bash
# Entrypoint script for mantle Docker container.
#
# Registers the decision MCP server with Claude Code if MANTLE_DECISION_TOOLS_FILE
# is set (points to a mounted JSON file), then executes the provided command.

set -e

# Register decision MCP server if tools file is provided
if [ -n "$MANTLE_DECISION_TOOLS_FILE" ]; then
    echo "[entrypoint] Registering decision MCP server with Claude Code..." >&2
    echo "[entrypoint] Tools file: $MANTLE_DECISION_TOOLS_FILE" >&2
    echo "[entrypoint] Control server: $MANTLE_CONTROL_HOST:$MANTLE_CONTROL_PORT" >&2

    # Write MCP config directly to ~/.claude.json
    # (claude mcp add writes here but the file isn't persisted in our volume setup)
    cat > /home/user/.claude.json << EOF
{
  "mcpServers": {
    "decision": {
      "type": "stdio",
      "command": "/home/user/.local/bin/mantle-agent",
      "args": ["mcp"],
      "env": {
        "MANTLE_CONTROL_HOST": "$MANTLE_CONTROL_HOST",
        "MANTLE_CONTROL_PORT": "$MANTLE_CONTROL_PORT",
        "MANTLE_DECISION_TOOLS_FILE": "$MANTLE_DECISION_TOOLS_FILE"
      }
    }
  }
}
EOF
    echo "[entrypoint] MCP server configured in /home/user/.claude.json" >&2
fi

# Execute the actual command
exec "$@"
