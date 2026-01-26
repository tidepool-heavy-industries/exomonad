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
# Only create settings.json if it wasn't linked from secrets
mkdir -p /home/agent/.claude
if [ ! -L /home/agent/.claude/settings.json ] && [ ! -f /home/agent/.claude/settings.json ]; then
    echo "Creating default Claude Code settings with hooks..."
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
else
    echo "Using existing/linked settings.json"
fi

# 3. Configure MCP
# Claude Code uses MCP (Model Context Protocol) for tool extension.
# Each agent gets its own workspace subdirectory to avoid config conflicts
ROLE="${TIDEPOOL_ROLE:-agent}"
AGENT_WORKSPACE="/workspace/${ROLE}"
mkdir -p "$AGENT_WORKSPACE"

if [ -n "${CONTROL_SERVER_URL:-}" ]; then
    # TCP MCP transport (container separation architecture)
    echo "Configuring MCP via TCP: ${CONTROL_SERVER_URL}/role/${ROLE}/mcp"
    cat > "$AGENT_WORKSPACE/.mcp.json" <<EOF
{
  "mcpServers": {
    "tidepool": {
      "type": "http",
      "url": "${CONTROL_SERVER_URL}/role/${ROLE}/mcp"
    }
  }
}
EOF
elif [ -n "${MANTLE_CONTROL_SOCKET:-}" ]; then
    # Unix socket transport (legacy)
    echo "Configuring MCP via Unix socket: ${MANTLE_CONTROL_SOCKET}"
    SOCKET_PATH_ENCODED=$(echo "$MANTLE_CONTROL_SOCKET" | sed 's/\//%2F/g')
    cat > "$AGENT_WORKSPACE/.mcp.json" <<EOF
{
  "mcpServers": {
    "control": {
      "type": "http",
      "url": "http+unix://$SOCKET_PATH_ENCODED/mcp"
    }
  }
}
EOF
else
    echo "Warning: No MCP configuration (neither CONTROL_SERVER_URL nor MANTLE_CONTROL_SOCKET set)" >&2
fi

# Change to agent-specific workspace
cd "$AGENT_WORKSPACE"


# 4. Handle mounted socket permissions (if any)
# If TIDEPOOL_CONTROL_SOCKET is mounted, ensure we can read it.
# (Usually handled by UID/GID matching on host/container)

# Execute the CMD via tini
exec tini -- "$@"
