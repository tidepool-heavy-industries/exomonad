#!/bin/bash
set -e

# Tidepool Orchestrator Entrypoint
# This script handles UID/GID mapping and Auth Isolation

# Default to UID/GID 1000 if not provided
USER_ID=${PUID:-1000}
GROUP_ID=${PGID:-1000}

echo "Starting Tidepool Orchestrator with UID: $USER_ID, GID: $GROUP_ID"

# Create tidepool user if it doesn't exist
if ! id -u tidepool >/dev/null 2>&1; then
    groupadd -g "$GROUP_ID" tidepool
    useradd -u "$USER_ID" -g "$GROUP_ID" -m -s /bin/bash tidepool
fi

# Set up Auth Isolation for Claude Code
# We expect the host's ~/.claude to be mounted at /mnt/host-auth
# We symlink only the necessary credential files to /home/tidepool/.claude
# to prevent the container from modifying other host settings.
CLAUDE_CONFIG_DIR="/home/tidepool/.claude"
HOST_AUTH_DIR="/mnt/host-auth"

mkdir -p "$CLAUDE_CONFIG_DIR"
chown -R tidepool:tidepool "$CLAUDE_CONFIG_DIR"

if [ -d "$HOST_AUTH_DIR" ]; then
    echo "Setting up auth isolation from $HOST_AUTH_DIR..."
    # Symlink config files if they exist
    for f in "config.json" "session.json" "auth.json"; do
        if [ -f "$HOST_AUTH_DIR/$f" ]; then
            ln -sf "$HOST_AUTH_DIR/$f" "$CLAUDE_CONFIG_DIR/$f"
            echo "Linked $f"
        fi
    done
    chown -h tidepool:tidepool "$CLAUDE_CONFIG_DIR"/* 2>/dev/null || true
else
    echo "Warning: Host auth directory not found at $HOST_AUTH_DIR. Claude Code might not be authenticated."
fi

# Ensure sockets and logs directories are writable
mkdir -p /sockets /var/log/tidepool
chown -R tidepool:tidepool /sockets /var/log/tidepool /worktrees

# Execute the main command as the tidepool user
if [ "$1" = "orchestrator" ]; then
    shift
    exec gosu tidepool /usr/local/bin/orchestrator-entrypoint.sh "$@"
else
    exec "$@"
fi
