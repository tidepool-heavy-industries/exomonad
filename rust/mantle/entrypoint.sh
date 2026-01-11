#!/bin/bash
# Entrypoint script for mantle Docker container.
#
# Registers the decision MCP server with Claude Code if MANTLE_DECISION_TOOLS
# environment variable is set, then executes the provided command.

set -e

# Register decision MCP server if tools are provided
if [ -n "$MANTLE_DECISION_TOOLS" ]; then
    echo "[entrypoint] Registering decision MCP server with Claude Code..." >&2
    if ! mcp_output=$(claude mcp add --user decision /usr/local/bin/mantle-agent mcp 2>&1); then
        echo "[entrypoint] Warning: Failed to register MCP server: $mcp_output" >&2
        echo "[entrypoint] Continuing anyway..." >&2
    fi
fi

# Execute the actual command
exec "$@"
