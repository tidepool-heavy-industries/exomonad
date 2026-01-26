#!/usr/bin/env bash
# Claude Code Agent Entrypoint
set -e

# --- Fix XDG_RUNTIME_DIR for Zellij ---
sudo mkdir -p /run/user/1000
sudo chown 1000:1000 /run/user/1000
sudo chmod 755 /run/user/1000
export XDG_RUNTIME_DIR=/run/user/1000

# --- Git safe.directory for shared volumes ---
# Repo volume is owned by root but we run as agent
git config --global --add safe.directory '*'

# 1. Link worktree to shared git alternates if provided
if [ -n "$GIT_ALTERNATES_OBJECT_DIR" ] && [ -d "/workspace/.git" ]; then
    echo "Linking worktree to shared git alternates: $GIT_ALTERNATES_OBJECT_DIR"
    mkdir -p /workspace/.git/objects/info
    echo "$GIT_ALTERNATES_OBJECT_DIR" > /workspace/.git/objects/info/alternates
fi

# 1.5 Initialize .claude directory if empty (new volume)
CONFIG_DIR="/home/agent/.claude"
mkdir -p "$CONFIG_DIR"

# 2. Configure Claude Code hooks
if [ ! -f "$CONFIG_DIR/settings.json" ]; then
    echo "Creating default Claude Code settings with hooks..."
    cat > "$CONFIG_DIR/settings.json" <<EOF
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

# 3. Detect Subagent Mode vs Root Agent Mode
if [ -f "process-compose.yaml" ]; then
    echo "Starting local control-server (Subagent Mode)..."

    # Ensure socket directory exists
    if [ -n "$TIDEPOOL_SOCKET_DIR" ]; then
        mkdir -p "$TIDEPOOL_SOCKET_DIR"
    fi

    # Clean up stale sockets
    if [ -n "$TIDEPOOL_CONTROL_SOCKET" ]; then
        rm -f "$TIDEPOOL_CONTROL_SOCKET"
    fi
    if [ -n "$TIDEPOOL_TUI_SOCKET" ]; then
        rm -f "$TIDEPOOL_TUI_SOCKET"
    fi

    # Start control-server in background
    mkdir -p .tidepool/logs
    echo "Launching tidepool-control-server --no-tui..."
    # Run as background process. We don't use process-compose anymore, so we supervise manually.
    tidepool-control-server --no-tui > .tidepool/logs/control-server.log 2>&1 &
    SERVER_PID=$!
    
    # Wait briefly for socket to appear
    echo "Waiting for control socket..."
    for i in {1..10}; do
        if [ -S "$TIDEPOOL_CONTROL_SOCKET" ]; then
            echo "Control server started (PID $SERVER_PID)"
            break
        fi
        sleep 0.5
    done
fi

# 4. Configure MCP
ROLE="${TIDEPOOL_ROLE:-agent}"
# Subagents might already have .mcp.json written by SpawnAgents to their worktree root.
# Root agents might need it in their specific workspace.
AGENT_WORKSPACE="/workspace/${ROLE}"

# Create workspace directory (may need sudo for bind mounts)
if ! mkdir -p "$AGENT_WORKSPACE" 2>/dev/null; then
    sudo mkdir -p "$AGENT_WORKSPACE"
    sudo chown agent:agent "$AGENT_WORKSPACE"
fi

# Write MCP config (use sudo for bind mounts that may have host permissions)
write_mcp_config() {
    local content="$1"
    local target="$AGENT_WORKSPACE/.mcp.json"

    # Use sudo tee to handle bind-mounted directories with host permissions
    # This works even when the directory is owned by a different UID
    echo "$content" | sudo tee "$target" > /dev/null
    sudo chown agent:agent "$target" 2>/dev/null || true
    echo "✓ MCP config written to $target"
}

if [ -n "${TIDEPOOL_CONTROL_SOCKET:-}" ]; then
    echo "Configuring MCP via Unix socket: ${TIDEPOOL_CONTROL_SOCKET}"
    SOCKET_PATH_ENCODED=$(echo "$TIDEPOOL_CONTROL_SOCKET" | sed 's/\//%2F/g')
    write_mcp_config '{
  "mcpServers": {
    "control": {
      "type": "http",
      "url": "http+unix://'"$SOCKET_PATH_ENCODED"'/mcp"
    }
  }
}'
elif [ -n "${CONTROL_SERVER_URL:-}" ]; then
    echo "Configuring MCP via TCP: ${CONTROL_SERVER_URL}/role/${ROLE}/mcp"
    write_mcp_config '{
  "mcpServers": {
    "tidepool": {
      "type": "http",
      "url": "'"${CONTROL_SERVER_URL}/role/${ROLE}/mcp"'"
    }
  }
}'
else
    echo "Warning: No MCP configuration" >&2
fi

# Change to agent-specific workspace
cd "$AGENT_WORKSPACE"

# Execute the CMD (init: true in docker-compose.yml provides signal handling)
exec "$@"
