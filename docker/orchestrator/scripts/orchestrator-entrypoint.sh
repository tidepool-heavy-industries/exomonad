#!/bin/bash
set -euo pipefail

# =============================================================================
# ExoMonad Orchestrator Entrypoint
# =============================================================================
# Service-specific setup before delegating to common entrypoint:
#   Phase 1 (root): Fix permissions, configure Claude Code
#   Phase 2: Delegate to common entrypoint for privilege drop
#
# Delegates to entrypoint-common.sh for:
#   - Docker socket GID detection
#   - Git safe.directory config
#   - Dropping privileges via gosu
# =============================================================================

echo "ExoMonad Orchestrator starting..."

# =============================================================================
# PHASE 1: Root Setup (service-specific)
# =============================================================================

# --- 1.1 Volume Permissions ---
# Named volumes are created as root; fix ownership
echo "Fixing volume permissions..."
[ -d /home/user/.claude ] && chown -R user:user /home/user/.claude
[ -d /home/user/.ssh ] && chown -R user:user /home/user/.ssh
[ -d /sockets ] && chown -R user:user /sockets
[ -d /var/log/exomonad ] && chown -R user:user /var/log/exomonad

# --- 1.2 Socket Symlinks ---
# Create symlinks so local dev paths work in Docker
mkdir -p /worktrees/.exomonad/sockets
ln -sf /sockets/control.sock /worktrees/.exomonad/sockets/control.sock
ln -sf /sockets/tui.sock /worktrees/.exomonad/sockets/tui.sock
chown -R user:user /worktrees/.exomonad

# --- 1.3 Claude Code Configuration ---
echo "Configuring Claude Code..."

CLAUDE_DIR="${CLAUDE_CONFIG_DIR:-/home/user/.claude}"
mkdir -p "$CLAUDE_DIR"

# Skip onboarding
cat > /home/user/.claude.json <<'EOF'
{"hasCompletedOnboarding": true}
EOF

# MCP config (HTTP transport to control-server)
cat > /worktrees/.exomonad/mcp.json <<'EOF'
{
  "mcpServers": {
    "exomonad": {
      "type": "http",
      "url": "http://exomonad-control-server:7432/role/tl/mcp"
    }
  }
}
EOF
ln -sf .exomonad/mcp.json /worktrees/.mcp.json

# Settings with hooks
cat > "$CLAUDE_DIR/settings.json" <<'EOF'
{
  "permissions": {
    "additionalDirectories": ["/worktrees"]
  },
  "hooks": {
    "PreToolUse": [{
      "matcher": "*",
      "hooks": [{
        "type": "command",
        "command": "exomonad hook pre-tool-use",
        "timeout": 300
      }]
    }]
  }
}
EOF

chown user:user /home/user/.claude.json "$CLAUDE_DIR/settings.json"
chown -h user:user /worktrees/.mcp.json
echo "Claude Code configured"

# --- 1.4 Runtime Cleanup ---
# Remove stale Zellij state that might be root-owned from previous runs
rm -rf /tmp/zellij-* 2>/dev/null
# Clear Zellij session cache to prevent restoring suspended pane states
rm -rf /home/user/.cache/zellij 2>/dev/null
rm -rf /home/user/.local/share/zellij/sessions 2>/dev/null

# --- 1.5 Control Server Check ---
SOCKET_PATH="${EXOMONAD_CONTROL_SOCKET:-/sockets/control.sock}"
if [ -S "$SOCKET_PATH" ]; then
    echo "Control server socket ready"
else
    echo "Control server socket not found (will retry)" >&2
fi

# =============================================================================
# PHASE 2: Delegate to Common Entrypoint
# =============================================================================

echo "Delegating to common entrypoint..."

# Handle different commands
case "${1:-orchestrator}" in
    orchestrator)
        # Default: Run Zellij session manager via common entrypoint
        echo "Starting Zellij orchestrator session..."

        # Small delay for filesystem readiness (prevents race condition)
        sleep 0.5

        # Create the orchestrator script that will run as user
        cat > /tmp/run-orchestrator.sh <<'ORCHESTRATOR_SCRIPT'
#!/bin/bash
SESSION_NAME="orchestrator"

create_session() {
    zellij \
        --config /etc/exomonad/zellij/config.kdl \
        --layout /etc/exomonad/zellij/layouts/exomonad.kdl \
        attach "$SESSION_NAME" --create-background
}

session_exists() {
    zellij list-sessions 2>/dev/null | sed "s/\x1b\[[0-9;]*m//g" | grep -q "^$SESSION_NAME"
}

# Create initial session
create_session
echo "Orchestrator running. Attach via:"
echo "   docker exec -it exomonad-orchestrator zellij attach $SESSION_NAME"

# Monitor and recreate if needed
# Uses "sleep & wait" idiom for responsive signal handling during docker stop
while true; do
    if ! session_exists; then
        echo "Session died, recreating..." >&2
        rm -rf /tmp/zellij-*
        sleep 0.5
        create_session
    fi
    sleep 5 & wait $!
done
ORCHESTRATOR_SCRIPT
        chmod +x /tmp/run-orchestrator.sh

        # Delegate to common entrypoint which handles Docker GID and privilege drop
        exec /usr/local/bin/entrypoint-common.sh /tmp/run-orchestrator.sh
        ;;
    *)
        # Pass through any other command via common entrypoint
        exec /usr/local/bin/entrypoint-common.sh "$@"
        ;;
esac
