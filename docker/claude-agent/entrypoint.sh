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

# 3. Configure MCP
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

if [ -n "${CONTROL_SERVER_URL:-}" ]; then
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
    echo "Warning: No MCP configuration (CONTROL_SERVER_URL not set)" >&2
fi

# Change to agent-specific workspace
cd "$AGENT_WORKSPACE"

# Execute the CMD (init: true in docker-compose.yml provides signal handling)
exec "$@"
