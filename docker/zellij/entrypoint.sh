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
