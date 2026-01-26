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
chown 1000:1000 /sockets 2>/dev/null || true
chmod 755 /sockets

# --- XDG_RUNTIME_DIR for Zellij ---
mkdir -p /run/user/1000
chown 1000:1000 /run/user/1000
chmod 755 /run/user/1000

# --- Cleanup stale sockets ---
rm -f /sockets/control.sock /sockets/tui.sock 2>/dev/null || true

# --- Docker socket access (for spawner communication) ---
if [ -S /var/run/docker.sock ]; then
    SOCKET_GID=$(stat -c '%g' /var/run/docker.sock)

    if ! getent group "$SOCKET_GID" > /dev/null 2>&1; then
        groupadd -g "$SOCKET_GID" docker-host 2>/dev/null || true
    fi

    DOCKER_GROUP=$(getent group "$SOCKET_GID" | cut -d: -f1)

    if ! groups user | grep -q "\b$DOCKER_GROUP\b" 2>/dev/null; then
        usermod -aG "$DOCKER_GROUP" user 2>/dev/null || true
    fi
fi

# --- Fix gh CLI auth directory ownership ---
# Volume may have been created by root; ensure user can write to it
mkdir -p /home/user/.config/gh
chown -R 1000:1000 /home/user/.config/gh 2>/dev/null || true

# --- Environment for user ---
export HOME=/home/user
export USER=user
# UTF-8 locale required for Haskell to decode gh CLI output (contains Unicode like ✓)
export LANG=C.UTF-8
export LC_ALL=C.UTF-8

# --- Start server as non-root ---
echo "Starting control-server as user..."
exec gosu 1000:1000 tidepool-control-server
