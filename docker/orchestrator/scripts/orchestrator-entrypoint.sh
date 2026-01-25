#!/bin/bash
set -euo pipefail

# =============================================================================
# Tidepool Orchestrator Entrypoint
# =============================================================================
# Follows "Root Setup / User Runtime" pattern:
#   Phase 1 (root): Fix permissions, configure environment
#   Phase 2 (user): Drop privileges, run Zellij
# =============================================================================

echo "🚀 Tidepool Orchestrator starting..."

# =============================================================================
# PHASE 1: Root Setup
# =============================================================================

# --- 1.1 Docker Socket Access ---
# Detect host's docker GID and add user to matching group
if [ -S /var/run/docker.sock ]; then
    SOCKET_GID=$(stat -c '%g' /var/run/docker.sock)

    # Create group if it doesn't exist
    if ! getent group "$SOCKET_GID" > /dev/null 2>&1; then
        echo "📦 Creating docker-host group (GID $SOCKET_GID)"
        groupadd -g "$SOCKET_GID" docker-host
    fi

    DOCKER_GROUP=$(getent group "$SOCKET_GID" | cut -d: -f1)

    # Add user to docker group
    if ! groups user | grep -q "\b$DOCKER_GROUP\b"; then
        echo "📦 Adding user to $DOCKER_GROUP group"
        usermod -aG "$DOCKER_GROUP" user
    fi
fi

# --- 1.2 Volume Permissions ---
# Named volumes are created as root; fix ownership
echo "🔧 Fixing volume permissions..."
chown -R user:user /home/user/.claude 2>/dev/null || true
chown -R user:user /home/user/.ssh 2>/dev/null || true
chown -R user:user /sockets 2>/dev/null || true
chown -R user:user /var/log/tidepool 2>/dev/null || true

# --- 1.3 Socket Symlinks ---
# Create symlinks so local dev paths work in Docker
mkdir -p /worktrees/.tidepool/sockets
ln -sf /sockets/control.sock /worktrees/.tidepool/sockets/control.sock 2>/dev/null || true
ln -sf /sockets/tui.sock /worktrees/.tidepool/sockets/tui.sock 2>/dev/null || true
chown -R user:user /worktrees/.tidepool

# --- 1.4 Claude Code Configuration ---
echo "🔧 Configuring Claude Code..."

CLAUDE_DIR="${CLAUDE_CONFIG_DIR:-/home/user/.claude}"
mkdir -p "$CLAUDE_DIR"

# Skip onboarding
cat > /home/user/.claude.json <<'EOF'
{"hasCompletedOnboarding": true}
EOF

# MCP config (HTTP transport to control-server)
cat > /worktrees/.tidepool/mcp.json <<'EOF'
{
  "mcpServers": {
    "tidepool": {
      "type": "http",
      "url": "http://tidepool-control-server:7432/role/tl/mcp"
    }
  }
}
EOF
ln -sf .tidepool/mcp.json /worktrees/.mcp.json

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
        "command": "mantle-agent hook pre-tool-use",
        "timeout": 300
      }]
    }]
  }
}
EOF

chown user:user /home/user/.claude.json "$CLAUDE_DIR/settings.json"
chown -h user:user /worktrees/.mcp.json
echo "✓ Claude Code configured"

# --- 1.5 Runtime Cleanup ---
# Remove stale Zellij state that might be root-owned from previous runs
rm -rf /tmp/zellij-* 2>/dev/null || true
# Clear Zellij session cache to prevent restoring suspended pane states
rm -rf /home/user/.cache/zellij 2>/dev/null || true
rm -rf /home/user/.local/share/zellij/sessions 2>/dev/null || true
# Don't remove Zellij sockets - they may be in use by cross-container connections

# Ensure XDG_RUNTIME_DIR exists for user
# Uses chmod 755 (not 700) to allow cross-container Zellij access from control-server
# Both containers share this volume via tidepool-zellij named volume
mkdir -p /run/user/1000
chown user:user /run/user/1000
chmod 755 /run/user/1000

# --- 1.6 Control Server Check ---
SOCKET_PATH="${TIDEPOOL_CONTROL_SOCKET:-/sockets/control.sock}"
if [ -S "$SOCKET_PATH" ]; then
    echo "✓ Control server socket ready"
else
    echo "⚠️  Control server socket not found (will retry)"
fi

# =============================================================================
# PHASE 2: Drop Privileges and Run
# =============================================================================

echo "🔑 Dropping privileges to user..."

# Set user environment
export HOME=/home/user
export USER=user
export SHELL=/bin/bash
export TERM=xterm-256color
export XDG_RUNTIME_DIR=/run/user/1000

# Handle different commands
case "${1:-orchestrator}" in
    orchestrator)
        # Default: Run Zellij session manager
        echo "🖥️  Starting Zellij orchestrator session..."

        # Small delay for filesystem readiness (prevents race condition)
        sleep 0.5

        # Create background session with layout, then monitor
        exec gosu user /bin/bash -c '
            SESSION_NAME="orchestrator"

            create_session() {
                zellij \
                    --config /etc/tidepool/zellij/config.kdl \
                    --layout /etc/tidepool/zellij/layouts/exomonad.kdl \
                    attach "$SESSION_NAME" --create-background
            }

            session_exists() {
                zellij list-sessions 2>/dev/null | sed "s/\x1b\[[0-9;]*m//g" | grep -q "^$SESSION_NAME"
            }

            # Create initial session
            create_session
            echo "✅ Orchestrator running. Attach via:"
            echo "   docker exec -it tidepool-orchestrator zellij attach $SESSION_NAME"

            # Monitor and recreate if needed
            while true; do
                sleep 5
                if ! session_exists; then
                    echo "⚠️  Session died, recreating..."
                    rm -rf /tmp/zellij-*
                    sleep 0.5
                    create_session
                fi
            done
        '
        ;;
    *)
        # Pass through any other command
        exec gosu user "$@"
        ;;
esac
