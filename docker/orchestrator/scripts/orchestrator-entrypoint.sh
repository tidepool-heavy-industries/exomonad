#!/bin/bash
set -euo pipefail

SESSION_NAME="orchestrator"

# Cleanup sibling containers on exit
cleanup() {
    echo "Cleaning up..."
    docker ps -q --filter "label=tidepool.orchestrator=$HOSTNAME" | xargs -r docker kill 2>/dev/null || true
    zellij kill-session "$SESSION_NAME" 2>/dev/null || true
    exit 0
}
trap cleanup SIGTERM SIGINT EXIT

# Ensure /sockets has correct permissions
chown -R user:user /sockets 2>/dev/null || true

# Create symlink so local dev .mcp.json paths work in Docker
# Local dev: .tidepool/sockets/control.sock → Docker: /sockets/control.sock
mkdir -p /worktrees/.tidepool/sockets
ln -sf /sockets/control.sock /worktrees/.tidepool/sockets/control.sock 2>/dev/null || true
ln -sf /sockets/tui.sock /worktrees/.tidepool/sockets/tui.sock 2>/dev/null || true

# Ensure log file exists
touch /var/log/tidepool/control-server.log
chown user:user /var/log/tidepool/control-server.log

# Generate Claude Code configuration
# CLAUDE_CONFIG_DIR is a persistent volume (/home/user/.claude)
echo "🔧 Configuring Claude Code for Docker environment..."

CLAUDE_DIR="${CLAUDE_CONFIG_DIR:-/home/user/.claude}"
mkdir -p "$CLAUDE_DIR"
chown -R user:user "$CLAUDE_DIR"

# Skip onboarding (user home, not config dir)
cat > /home/user/.claude.json <<'EOF'
{"hasCompletedOnboarding": true}
EOF

# MCP config comes from repo's .mcp.json (points to .tidepool/sockets/control.sock)
# Symlinks above make that path work in Docker

# Settings with hooks (inside persistent config dir)
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
echo "✓ Claude Code configured (persistent volume: $CLAUDE_DIR)"

# Set environment for user context
export HOME=/home/user
export SHELL=/bin/bash
export TERM=xterm-256color

# Verify control-server socket
SOCKET_PATH="${TIDEPOOL_CONTROL_SOCKET:-/sockets/control.sock}"
if [ ! -S "$SOCKET_PATH" ]; then
    echo "⚠️  WARNING: control-server socket not found at $SOCKET_PATH"
fi

# Function to check if session exists (strip ANSI codes from zellij output)
session_exists() {
    gosu user zellij list-sessions 2>/dev/null | sed 's/\x1b\[[0-9;]*m//g' | grep -q "^$SESSION_NAME"
}

# Function to create session using native --create-background flag
# This is the proper way to create headless Zellij sessions (v0.40.0+)
create_session() {
    echo "🚀 Creating Zellij '$SESSION_NAME' session..."

    # Clean stale sockets
    rm -f /tmp/zellij-*.sock

    # Create background session with layout
    # --create-background: Creates session without attaching (like tmux new -d)
    # --layout: Pre-loads the layout into the headless session
    # Note: --config and --layout must come BEFORE the subcommand
    gosu user zellij \
        --config /etc/tidepool/zellij/config.kdl \
        --layout /etc/tidepool/zellij/layouts/exomonad.kdl \
        attach "$SESSION_NAME" --create-background

    sleep 2
}

# Create initial session
create_session

echo "✅ Orchestrator running. Attach via:"
echo "   docker exec -it --user user tidepool-orchestrator zellij attach $SESSION_NAME"

# Monitor loop - keeps container alive and restarts session if needed
while true; do
    sleep 5
    if ! session_exists; then
        echo "⚠️  Session '$SESSION_NAME' not found, recreating..."
        create_session
        echo "✅ Session recreated"
    fi
done
