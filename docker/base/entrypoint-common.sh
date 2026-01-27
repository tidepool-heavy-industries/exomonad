#!/bin/bash
# =============================================================================
# Common Entrypoint for ExoMonad Containers
# =============================================================================
# Minimal runtime setup before dropping privileges:
#   1. Docker socket GID detection (runtime-only, varies by host)
#   2. Git safe.directory config
#   3. Drop to non-root via gosu (app becomes PID 1, receives signals)
#
# Build-time config (in Dockerfile):
#   - User/group creation
#   - XDG_RUNTIME_DIR creation
#   - Static environment variables
# =============================================================================

set -euo pipefail

# --- Docker socket access for gosu ---
# group_add in docker-compose.yml doesn't persist through gosu (which recalculates
# supplemental groups from /etc/group). We must add user to the docker group here.
#
# Always detect GID from the mounted socket - DOCKER_GID from host is unreliable
# for remote Docker (e.g., NixOS via SSH where host has no local socket).
if [ -S /var/run/docker.sock ]; then
    SOCKET_GID=$(stat -c '%g' /var/run/docker.sock)

    if ! getent group "$SOCKET_GID" > /dev/null 2>&1; then
        groupadd -g "$SOCKET_GID" docker-host 2>/dev/null || true
    fi
    DOCKER_GROUP=$(getent group "$SOCKET_GID" | cut -d: -f1)
    if ! id -nG user | grep -qw "$DOCKER_GROUP" 2>/dev/null; then
        usermod -aG "$DOCKER_GROUP" user 2>/dev/null || true
    fi
fi

# --- XDG_RUNTIME_DIR permissions ---
# Ensure XDG_RUNTIME_DIR exists and is owned by user
# Uses chmod 755 (not 700) to allow cross-container Zellij access
if [ -n "${XDG_RUNTIME_DIR:-}" ]; then
    mkdir -p "$XDG_RUNTIME_DIR"
    chown 1000:1000 "$XDG_RUNTIME_DIR"
    chmod 755 "$XDG_RUNTIME_DIR"
fi

# --- Git safe.directory ---
# Apply git safe.directory for shared volumes owned by different UID
# Must set as 'user' since that's who runs the app via gosu
if [ -n "${GIT_SAFE_DIRECTORY:-}" ]; then
    gosu user git config --global --add safe.directory "$GIT_SAFE_DIRECTORY"
else
    # Default: allow all directories (safer for bind mounts)
    gosu user git config --global --add safe.directory '*'
fi

# --- Drop privileges and exec ---
# Use username (not UID:GID) so gosu calls initgroups() and picks up
# supplementary groups like docker-host from /etc/group
exec gosu user "$@"
