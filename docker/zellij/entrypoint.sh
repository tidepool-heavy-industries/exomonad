#!/bin/bash
set -euo pipefail

# =============================================================================
# Zellij Container Entrypoint
# =============================================================================
# Service-specific setup before calling common entrypoint:
#   - Stale Zellij state cleanup
#   - Terminal environment
#
# Delegates to entrypoint-common.sh for:
#   - Docker socket GID detection
#   - Git safe.directory config
#   - Dropping privileges via gosu
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

# --- Cleanup stale state ---
rm -rf /tmp/zellij-* 2>/dev/null

# --- Generate layout dynamically ---
# This bakes in container names and socket paths as literals, avoiding
# the issue where env vars don't propagate to pane processes
echo "Generating Zellij layout..."
LAYOUT_PATH=$(zellij-gen main)
echo "Generated layout: $LAYOUT_PATH"

# Symlink to expected location for compatibility with ./ide script
mkdir -p /etc/zellij/layouts
ln -sf "$LAYOUT_PATH" /etc/zellij/layouts/main.kdl

echo ""
echo "Zellij container ready."
echo ""
echo "Connect with (idempotent - works for create, attach, or resurrect):"
echo "  ./ide"
echo ""
echo "Or manually:"
echo "  docker exec -it exomonad-zellij zellij --layout /etc/zellij/layouts/main.kdl attach --create main"
echo ""

# --- Delegate to common entrypoint ---
# Common entrypoint handles Docker GID, git config, and drops privileges
# For zellij, we keep the container alive - user connects via docker exec
exec /usr/local/bin/entrypoint-common.sh tail -f /dev/null
