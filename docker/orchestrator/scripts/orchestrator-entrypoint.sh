#!/bin/bash
set -euo pipefail

# Cleanup sibling containers on exit
# We use the label tidepool.orchestrator=$HOSTNAME to identify containers 
# started by this specific orchestrator instance.
cleanup() {
    echo "Cleaning up sibling containers..."
    # Using docker ps with filter to find siblings
    # xargs -r avoids errors if no containers match
    docker ps -q --filter "label=tidepool.orchestrator=$HOSTNAME" | xargs -r docker kill
}

trap cleanup EXIT

# Clean up old sockets and ensure correct permissions
rm -f /sockets/control.sock /sockets/tui.sock
chown -R user:user /sockets

# Ensure log file exists so tail doesn't complain
touch /var/log/tidepool/control-server.log
chown user:user /var/log/tidepool/control-server.log

# Generate Claude Code configuration for non-root user
echo "🔧 Configuring Claude Code for Docker environment..."

# Use CLAUDE_CONFIG_DIR from environment (defaults to /home/user/.claude-orchestrator)
CLAUDE_DIR="${CLAUDE_CONFIG_DIR:-/home/user/.claude-orchestrator}"

# Ensure directory exists with correct ownership
mkdir -p "$CLAUDE_DIR"
chown -R user:user "$CLAUDE_DIR"

# 1. Skip onboarding
cat > /home/user/.claude.json <<'EOF'
{"hasCompletedOnboarding": true}
EOF

# 2. MCP configuration with absolute socket path
# /sockets/control.sock → URL-encoded: %2Fsockets%2Fcontrol.sock
cat > /home/user/.mcp.json <<'EOF'
{
  "mcpServers": {
    "tidepool": {
      "transport": {
        "type": "http",
        "url": "http+unix://%2Fsockets%2Fcontrol.sock"
      }
    }
  }
}
EOF

# 3. Hook configuration in isolated config directory
cat > "$CLAUDE_DIR/settings.json" <<'EOF'
{
  "permissions": {
    "additionalDirectories": [
      "/worktrees"
    ]
  },
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "*",
        "hooks": [
          {
            "type": "command",
            "command": "mantle-agent hook pre-tool-use",
            "timeout": 300
          }
        ]
      }
    ]
  }
}
EOF

# Set ownership for all Claude config files
chown user:user /home/user/.claude.json /home/user/.mcp.json "$CLAUDE_DIR/settings.json"

echo "✓ Claude Code configured"

# Ensure Zellij config directory exists (if using home-based config)
# mkdir -p ~/.config/zellij

# Generate self-signed SSL certificate for web server
# Zellij requires SSL when binding to non-localhost addresses
# Store in /etc/ssl to match config.kdl paths
mkdir -p /etc/ssl/zellij
openssl req -x509 -newkey rsa:4096 -nodes \
    -keyout /etc/ssl/zellij/key.pem \
    -out /etc/ssl/zellij/cert.pem \
    -days 365 -subj "/CN=tidepool-orchestrator" 2>/dev/null

# Make certificates readable by non-root user
chmod 644 /etc/ssl/zellij/key.pem /etc/ssl/zellij/cert.pem

echo "🌐 Starting Zellij web server on 0.0.0.0:8080"
echo "📋 Web server will create 'orchestrator' session on first browser connection"
echo "📋 After startup, get login token with: docker exec tidepool-orchestrator gosu user zellij web --create-token"
echo "🌍 Then open: https://nixos:8080/orchestrator"

# Ensure HOME and SHELL are set for user context
export HOME=/home/user
export SHELL=/bin/bash

# Cleanup old sockets to prevent "Address in use" or stale session errors
rm -f /tmp/zellij-*.sock

# Start web server in daemon mode to decouple from PID 1 lifecycle
# This prevents the container from exiting when the web server process completes initialization
gosu user zellij web --daemonize

# Keep PID 1 alive - container stays running while web server runs in background
# This is the recommended pattern when using zellij web --daemonize
exec tail -f /dev/null
