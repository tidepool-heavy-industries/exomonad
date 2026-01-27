#!/bin/bash
set -euo pipefail

# =============================================================================
# Control Server Container Entrypoint
# =============================================================================
# Service-specific setup before calling common entrypoint:
#   - Volume ownership fixes
#   - Stale socket cleanup
#   - gh CLI auth directory
#   - UTF-8 locale for Haskell
#
# Delegates to entrypoint-common.sh for:
#   - Docker socket GID detection
#   - Git safe.directory config
#   - Dropping privileges via gosu
# =============================================================================

echo "Starting ExoMonad Control Server..."

# --- Fix volume ownership ---
# Named volumes are created as root; fix ownership
[ -d /sockets ] && chown 1000:1000 /sockets && chmod 755 /sockets
[ -d /worktrees ] && chown 1000:1000 /worktrees && chmod 755 /worktrees 2>/dev/null || true

# --- Cleanup stale sockets ---
rm -f /sockets/control.sock /sockets/tui.sock 2>/dev/null

# --- Fix gh CLI auth directory ownership ---
# Volume may have been created by root; ensure user can write to it
mkdir -p /home/user/.config/gh
[ -d /home/user/.config/gh ] && chown -R 1000:1000 /home/user/.config/gh

# --- Fix repo bind mount access ---
# Create .exomonad directory structure in the repo before dropping privileges.
# This is needed because the bind mount preserves host file permissions.
if [ -d /repo ]; then
    echo "Creating .exomonad structure in /repo..."
    if mkdir -p /repo/.exomonad/sockets /repo/.exomonad/logs; then
        chown -R 1000:1000 /repo/.exomonad
        chmod -R 755 /repo/.exomonad
        echo "Created .exomonad directory"
    else
        echo "Could not create .exomonad in /repo (bind mount may be read-only)" >&2
        echo "Creating in /home/user instead..." >&2
        mkdir -p /home/user/.exomonad/sockets /home/user/.exomonad/logs
        chown -R 1000:1000 /home/user/.exomonad
        # Override EXOMONAD_PROJECT_DIR to use writable location
        export EXOMONAD_PROJECT_DIR=/home/user
    fi
fi

# --- UTF-8 locale ---
# Required for Haskell to decode gh CLI output (contains Unicode)
export LANG=C.UTF-8
export LC_ALL=C.UTF-8

# --- Delegate to common entrypoint ---
# Common entrypoint handles Docker GID, git config, and drops privileges
echo "Starting control-server as user..."
exec /usr/local/bin/entrypoint-common.sh exomonad-control-server
