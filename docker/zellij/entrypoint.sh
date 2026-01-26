#!/bin/bash
set -euo pipefail

# =============================================================================
# Zellij Container Entrypoint
# =============================================================================
# Minimal container for visual multiplexing. Panes run docker attach to agents.
#
# IDEMPOTENT SESSION MANAGEMENT:
#   docker exec -it exomonad-zellij zellij --layout /etc/zellij/layouts/main.kdl attach --create main
#
# This single command:
#   - Creates session with layout if it doesn't exist
#   - Attaches to existing session if it does
#   - Resurrects session with layout if container was restarted
# =============================================================================

echo "Starting Zellij multiplexer..."

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

# --- XDG Runtime ---
mkdir -p /run/user/1000
chown user:user /run/user/1000
chmod 755 /run/user/1000

# --- Cleanup stale state ---
rm -rf /tmp/zellij-* 2>/dev/null

# --- Drop privileges and run ---
export HOME=/home/user
export USER=user
export TERM=xterm-256color
export XDG_RUNTIME_DIR=/run/user/1000
export ZELLIJ_CONFIG_DIR=/etc/zellij

echo ""
echo "Zellij container ready."
echo ""
echo "Connect with (idempotent - works for create, attach, or resurrect):"
echo "  ./ide"
echo ""
echo "Or manually:"
echo "  docker exec -it exomonad-zellij zellij --layout /etc/zellij/layouts/main.kdl attach --create main"
echo ""

# Keep container alive - user connects via docker exec
exec tail -f /dev/null
