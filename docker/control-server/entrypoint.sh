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

echo "Starting ExoMonad Control Server..."

# --- Docker socket access for gosu ---
# group_add in docker-compose.yml doesn't persist through gosu (which recalculates
# supplemental groups from /etc/group). We must add user to the docker group here.
#
# Always detect GID from the mounted socket - DOCKER_GID from host is unreliable
# for remote Docker (e.g., NixOS via SSH where host has no local socket).
if [ -S /var/run/docker.sock ]; then
    SOCKET_GID=$(stat -c '%g' /var/run/docker.sock)

    if ! getent group "$SOCKET_GID" > /dev/null 2>&1; then
        groupadd -g "$SOCKET_GID" docker-host
    fi
    DOCKER_GROUP=$(getent group "$SOCKET_GID" | cut -d: -f1)
    if ! id -nG user | grep -qw "$DOCKER_GROUP"; then
        usermod -aG "$DOCKER_GROUP" user
    fi
fi

# --- Fix volume ownership ---
[ -d /sockets ] && chown 1000:1000 /sockets
chmod 755 /sockets
[ -d /worktrees ] && chown 1000:1000 /worktrees
chmod 755 /worktrees 2>/dev/null || true

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

# --- Fix repo bind mount access ---
# Create .exomonad directory structure in the repo before dropping privileges.
# This is needed because the bind mount preserves host file permissions.
if [ -d /repo ]; then
    echo "Creating .exomonad structure in /repo..."
    if mkdir -p /repo/.exomonad/sockets /repo/.exomonad/logs; then
        chown -R 1000:1000 /repo/.exomonad
        chmod -R 755 /repo/.exomonad
        echo "✓ .exomonad directory created"
    else
        echo "⚠️  Could not create .exomonad in /repo (bind mount may be read-only)"
        echo "   Creating in /home/user instead..."
        mkdir -p /home/user/.exomonad/sockets /home/user/.exomonad/logs
        chown -R 1000:1000 /home/user/.exomonad
        # Override EXOMONAD_PROJECT_DIR to use writable location
        export EXOMONAD_PROJECT_DIR=/home/user
    fi
fi

# --- Git safe.directory for shared volumes ---
# Repo volume may be owned by root; allow git operations
# Must set as 'user' since that's who runs the server via gosu
gosu user git config --global --add safe.directory '*'

# --- Environment for user ---
export HOME=/home/user
export USER=user
# UTF-8 locale required for Haskell to decode gh CLI output (contains Unicode like ✓)
export LANG=C.UTF-8
export LC_ALL=C.UTF-8

# --- Start server as non-root ---
# Use username (not UID:GID) so gosu calls initgroups() and picks up
# supplementary groups like docker-host from /etc/group
echo "Starting control-server as user..."
exec gosu user exomonad-control-server
