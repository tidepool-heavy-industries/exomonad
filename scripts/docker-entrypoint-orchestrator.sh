#!/bin/bash
set -euo pipefail

# 1. Handle Auth Isolation
# Claude Code stores credentials in ~/.claude/.credentials.json
# We mount the real credentials and settings to /mnt/secrets/
# and symlink them into the per-container config dir to prevent 
# history.jsonl or __store.db corruption from shared access.

echo "🔐 Setting up auth isolation..."
mkdir -p "$CLAUDE_CONFIG_DIR"

for f in ".credentials.json" "settings.json"; do
    if [ -f "/mnt/secrets/$f" ]; then
        ln -sf "/mnt/secrets/$f" "$CLAUDE_CONFIG_DIR/$f"
        echo "✓ Linked $f"
    fi
done

echo "✓ Auth isolated in $CLAUDE_CONFIG_DIR"

# 2. Start the original entrypoint logic
exec /usr/local/bin/orchestrator-entrypoint.sh