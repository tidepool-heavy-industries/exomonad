#!/usr/bin/env bash
# Claude Code Agent Entrypoint
set -e

# 1. Link worktree to shared git alternates if provided
# This saves disk space by sharing the object store with a host-side cache
if [ -n "$GIT_ALTERNATES_OBJECT_DIR" ] && [ -d "/workspace/.git" ]; then
    echo "Linking worktree to shared git alternates: $GIT_ALTERNATES_OBJECT_DIR"
    mkdir -p /workspace/.git/objects/info
    echo "$GIT_ALTERNATES_OBJECT_DIR" > /workspace/.git/objects/info/alternates
fi

# 1.5 Handle Auth Isolation
# If credentials are provided via mount, link them into the per-container config dir.
# We only link credentials and settings to avoid sharing history/db.
echo "🔐 Setting up auth isolation..."
CONFIG_DIR="${CLAUDE_CONFIG_DIR:-/home/agent/.claude}"
mkdir -p "$CONFIG_DIR"

for f in ".credentials.json" "settings.json"; do
    if [ -f "/mnt/secrets/$f" ]; then
        ln -sf "/mnt/secrets/$f" "$CONFIG_DIR/$f"
        echo "✓ Linked $f"
    fi
done
echo "✓ Auth isolated in $CONFIG_DIR"

# 2. Configure Claude Code hooks
# We point hooks to mantle-agent which forwards them to the control-server
mkdir -p /home/agent/.claude
cat > /home/agent/.claude/settings.json <<EOF
{
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

# 3. Configure MCP
# Claude Code uses MCP (Model Context Protocol) for tool extension.
# We use the http+unix protocol to talk to the control socket.
# The path must be URL-encoded (/ -> %2F).
if [ -z "${MANTLE_CONTROL_SOCKET:-}" ]; then
    echo "Error: MANTLE_CONTROL_SOCKET is not set or empty; cannot configure MCP control socket." >&2
    exit 1
fi
SOCKET_PATH_ENCODED=$(echo "$MANTLE_CONTROL_SOCKET" | sed 's/\//%2F/g')
cat > /home/agent/.mcp.json <<EOF
{
  "mcpServers": {
    "control": {
      "type": "http",
      "url": "http+unix://$SOCKET_PATH_ENCODED/mcp"
    }
  }
}
EOF


# 4. Handle mounted socket permissions (if any)
# If TIDEPOOL_CONTROL_SOCKET is mounted, ensure we can read it.
# (Usually handled by UID/GID matching on host/container)

# Execute the CMD via tini
exec tini -- "$@"
