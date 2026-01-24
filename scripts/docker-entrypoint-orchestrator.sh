#!/bin/bash
set -euo pipefail

# Claude auth is persisted via named volume (tidepool-claude-auth → /home/user/.claude)
echo "🔐 Setting up Claude config directory..."
mkdir -p "$CLAUDE_CONFIG_DIR"
chown -R user:user "$CLAUDE_CONFIG_DIR"
echo "✓ Config dir ready: $CLAUDE_CONFIG_DIR"

# SSH keys are in named volume (tidepool-ssh-keys → /home/user/.ssh)
# Populated once via: ./scripts/setup-docker-ssh.sh
echo "🔑 Checking SSH keys..."
if [ -f /home/user/.ssh/id_ed25519 ]; then
    chmod 700 /home/user/.ssh
    chmod 600 /home/user/.ssh/id_ed25519
    chown -R user:user /home/user/.ssh
    echo "✓ SSH key ready"
else
    echo "❌ No SSH key found in tidepool-ssh-keys volume"
    echo ""
    echo "   Run the setup script to copy your SSH key:"
    echo "   ./scripts/setup-docker-ssh.sh"
    echo ""
    exit 1
fi

# Clone or update repo
REPO_URL="${TIDEPOOL_REPO_URL:-git@github.com:tidepool-heavy-industries/tidepool.git}"
REPO_BRANCH="${TIDEPOOL_REPO_BRANCH:-main}"
WORKTREES_DIR="/worktrees"

echo "📦 Setting up repository..."
if [ -d "$WORKTREES_DIR/.git" ]; then
    echo "  Repository exists, pulling latest..."
    cd "$WORKTREES_DIR"
    gosu user git fetch origin || echo "  Fetch failed (offline?)"
    gosu user git checkout "$REPO_BRANCH" 2>/dev/null || true
    gosu user git pull origin "$REPO_BRANCH" || echo "  Pull failed (may have local changes)"
    echo "✓ Repository updated"
else
    echo "  Cloning $REPO_URL (branch: $REPO_BRANCH)..."
    gosu user git clone --branch "$REPO_BRANCH" "$REPO_URL" "$WORKTREES_DIR"
    echo "✓ Repository cloned"
fi

# Ensure repo is owned by user
chown -R user:user "$WORKTREES_DIR"

# Start the orchestrator entrypoint
exec /usr/local/bin/orchestrator-entrypoint.sh
