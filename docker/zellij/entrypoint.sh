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
echo "  docker exec -it tidepool-zellij zellij --layout /etc/zellij/layouts/main.kdl attach --create tidepool"
echo ""

# Keep container alive - user connects via docker exec
exec tail -f /dev/null
