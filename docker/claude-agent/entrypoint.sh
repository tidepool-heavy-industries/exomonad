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

# --- Configure git authentication ---
# Use GH_TOKEN env var if available (preferred), otherwise try gh CLI
if [ -n "${GH_TOKEN:-}" ]; then
    echo "Configuring git to use GH_TOKEN..."
    # Use a credential helper that reads the environment variable directly
    # This avoids persisting the token in .gitconfig
    git config --global credential.helper '!f() { echo username=x-access-token; echo password=$GH_TOKEN; }; f'
elif command -v gh &> /dev/null && gh auth status &> /dev/null; then
    echo "Configuring git to use gh CLI for authentication..."
    gh auth setup-git
else
    echo "Warning: No git authentication configured, operations may fail"
fi


# --- Determine workspace ---
# Subagents (EXOMONAD_ISSUE_ID set) use their worktree as working directory
# Root agents (TL, PM) use /workspace/${ROLE} and clone the repo there
ROLE="${EXOMONAD_ROLE:-agent}"

if [ -n "${EXOMONAD_ISSUE_ID:-}" ]; then
    # Subagent: stay in current directory (the worktree set by docker-ctl)
    AGENT_WORKSPACE="$(pwd)"
    echo "Subagent mode (issue ${EXOMONAD_ISSUE_ID}): using worktree at $AGENT_WORKSPACE"

    # Verify we're in a git worktree
    if [ ! -d ".git" ] && [ ! -f ".git" ]; then
        echo "ERROR: Subagent started but not in a git worktree. Current dir: $AGENT_WORKSPACE"
        exit 1
    fi
else
    # Root agent: use /workspace/${ROLE} and initialize repo if needed
    AGENT_WORKSPACE="/workspace/${ROLE}"
    REPO_URL="${EXOMONAD_REPO_URL:-https://github.com/tidepool-heavy-industries/exomonad.git}"
    REPO_BRANCH="${EXOMONAD_REPO_BRANCH:-main}"

    if [ ! -d "$AGENT_WORKSPACE/.git" ]; then
        echo "Initializing repository in $AGENT_WORKSPACE..."
        # Create workspace if needed
        sudo mkdir -p "$AGENT_WORKSPACE"
        sudo chown agent:agent "$AGENT_WORKSPACE"

        # Handle non-empty directory (stale volume content)
        if [ "$(ls -A "$AGENT_WORKSPACE" 2>/dev/null)" ]; then
            echo "Directory not empty, initializing git in place..."
            cd "$AGENT_WORKSPACE"
            git init
            git remote add origin "$REPO_URL"
            git fetch origin "$REPO_BRANCH"
            git checkout -B "$REPO_BRANCH" "origin/$REPO_BRANCH"
            echo "Repository initialized from existing directory"
        else
            # Clone the repo (use HTTPS for simplicity, SSH would need key setup)
            git clone --branch "$REPO_BRANCH" "$REPO_URL" "$AGENT_WORKSPACE"
            echo "Repository cloned to $AGENT_WORKSPACE"
        fi
    else
        echo "Repository exists at $AGENT_WORKSPACE"
    fi
fi

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
            "command": "exomonad hook pre-tool-use",
            "timeout": 300
          }
        ]
      }
    ],
    "SessionStart": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "exomonad hook session-start"
          }
        ]
      }
    ],
    "SessionEnd": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "exomonad hook session-end"
          }
        ]
      }
    ],
    "SubagentStart": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "exomonad hook subagent-start"
          }
        ]
      }
    ],
    "SubagentStop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "exomonad hook subagent-stop"
          }
        ]
      }
    ],
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "exomonad hook stop"
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

# 2.5 Configure Gemini hooks & MCP
GEMINI_CONFIG_DIR="/home/agent/.gemini"
if [ ! -f "$GEMINI_CONFIG_DIR/settings.json" ]; then
    echo "Creating default Gemini settings with hooks..."
    mkdir -p "$GEMINI_CONFIG_DIR"

    # Prepare MCP config fragment if URL is available
    MCP_CONFIG=""
    if [ -n "${CONTROL_SERVER_URL:-}" ]; then
        echo "Adding Gemini MCP config for: ${CONTROL_SERVER_URL}/role/${ROLE}/mcp"
        MCP_CONFIG=",
  \"mcpServers\": {
    \"exomonad\": {
      \"type\": \"http\",
      \"url\": \"${CONTROL_SERVER_URL}/role/${ROLE}/mcp\"
    }
  }"
    fi

    cat > "$GEMINI_CONFIG_DIR/settings.json" <<EOF
{
  "hooks": {
    "startup": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "exomonad hook session-start --runtime=gemini"
          }
        ]
      }
    ],
    "SessionStart": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "exomonad hook session-start --runtime=gemini"
          }
        ]
      }
    ],
    "PreToolUse": [
      {
        "matcher": "*",
        "hooks": [
          {
            "type": "command",
            "command": "exomonad hook pre-tool-use --runtime=gemini",
            "timeout": 300
          }
        ]
      }
    ]
  }${MCP_CONFIG}
}
EOF
    chown -R agent:agent "$GEMINI_CONFIG_DIR"
fi

# 3. Configure MCP
# ROLE and AGENT_WORKSPACE already defined above during repo init
# Subagents might already have .mcp.json written by SpawnAgents to their worktree root.
# Root agents might need it in their specific workspace.

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
    "exomonad": {
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
