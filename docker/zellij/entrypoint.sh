#!/bin/bash
set -euo pipefail

# =============================================================================
# Zellij Container Entrypoint
# =============================================================================
# Minimal container for visual multiplexing. Panes run docker attach to agents.
#
# IDEMPOTENT SESSION MANAGEMENT:
#   docker exec -it tidepool-zellij zellij --layout /etc/zellij/layouts/main.kdl attach --create tidepool
#
# This single command:
#   - Creates session with layout if it doesn't exist
#   - Attaches to existing session if it does
#   - Resurrects session with layout if container was restarted
# =============================================================================

echo "Starting Zellij multiplexer..."

# --- Docker Socket Access ---
if [ -S /var/run/docker.sock ]; then
    SOCKET_GID=$(stat -c '%g' /var/run/docker.sock)

    if ! getent group "$SOCKET_GID" > /dev/null 2>&1; then
        echo "Creating docker-host group (GID $SOCKET_GID)"
        groupadd -g "$SOCKET_GID" docker-host
    fi

    DOCKER_GROUP=$(getent group "$SOCKET_GID" | cut -d: -f1)

    if ! groups user | grep -q "\b$DOCKER_GROUP\b"; then
        echo "Adding user to $DOCKER_GROUP group"
        usermod -aG "$DOCKER_GROUP" user
    fi
fi

# --- XDG Runtime ---
mkdir -p /run/user/1000
chown user:user /run/user/1000
chmod 755 /run/user/1000

# --- Cleanup stale state ---
rm -rf /tmp/zellij-* 2>/dev/null || true

# --- Wait for agent containers ---
echo "Waiting for agent containers..."
TIMEOUT=60
ELAPSED=0
while [ $ELAPSED -lt $TIMEOUT ]; do
    TL_READY=$(docker inspect -f '{{.State.Running}}' tidepool-tl 2>/dev/null || echo "false")
    PM_READY=$(docker inspect -f '{{.State.Running}}' tidepool-pm 2>/dev/null || echo "false")

    if [ "$TL_READY" = "true" ] && [ "$PM_READY" = "true" ]; then
        echo "Agent containers ready"
        break
    fi

    sleep 1
    ELAPSED=$((ELAPSED + 1))
done

if [ $ELAPSED -ge $TIMEOUT ]; then
    echo "Warning: Timeout waiting for agents, starting anyway..."
fi

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
echo "  docker exec -it tidepool-zellij zellij --layout /etc/zellij/layouts/main.kdl attach --create tidepool"
echo ""

# Keep container alive - user connects via docker exec
exec tail -f /dev/null
