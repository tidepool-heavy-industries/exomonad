#!/bin/bash
set -euo pipefail

# Claude auth is persisted via named volume (tidepool-claude-auth → /home/user/.claude)
# No symlinks needed - the volume survives container restarts

echo "🔐 Setting up Claude config directory..."
mkdir -p "$CLAUDE_CONFIG_DIR"
chown -R user:user "$CLAUDE_CONFIG_DIR"
echo "✓ Config dir ready: $CLAUDE_CONFIG_DIR"

# Start the orchestrator entrypoint
exec /usr/local/bin/orchestrator-entrypoint.sh
