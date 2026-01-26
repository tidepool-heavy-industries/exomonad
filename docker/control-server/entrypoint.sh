#!/bin/bash
set -euo pipefail

# =============================================================================
# Control Server Container Entrypoint
# =============================================================================
# MCP server for agent containers. Provides:
#   - MCP tools (find_callers, teach-graph, etc.)
#   - Hook handling (passthrough)
#   - Zellij tab creation for subagents
# =============================================================================

echo "Starting Tidepool Control Server..."

# --- Fix volume ownership ---
[ -d /sockets ] && chown 1000:1000 /sockets
chmod 755 /sockets

# --- XDG_RUNTIME_DIR for Zellij ---
mkdir -p /run/user/1000
chown 1000:1000 /run/user/1000
chmod 755 /run/user/1000

# --- Cleanup stale sockets ---
rm -f /sockets/control.sock /sockets/tui.sock 2>/dev/null

# --- Fix gh CLI auth directory ownership ---
# Volume may have been created by root; ensure user can write to it
mkdir -p /home/user/.config/gh
[ -d /home/user/.config/gh ] && chown -R 1000:1000 /home/user/.config/gh

# --- Environment for user ---
export HOME=/home/user
export USER=user

# --- Start server as non-root ---
echo "Starting control-server as user..."
exec gosu 1000:1000 tidepool-control-server
