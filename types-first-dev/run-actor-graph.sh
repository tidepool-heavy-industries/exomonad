#!/usr/bin/env bash
set -euo pipefail

# Discover git repo root (works from any subdirectory)
GIT_ROOT="$(git rev-parse --show-toplevel)"

# Source .env from types-first-dev (for CLAUDE_CODE_OAUTH_TOKEN)
if [ -f "$GIT_ROOT/types-first-dev/.env" ]; then
    set -a
    source "$GIT_ROOT/types-first-dev/.env"
    set +a
fi

# ════════════════════════════════════════════════════════════════
# Pre-flight checks
# ════════════════════════════════════════════════════════════════

# Check Docker auth container is running and has valid credentials
check_auth() {
    local auth_container="tidepool-auth-shell"

    # Check container is running
    if ! docker ps --format '{{.Names}}' | grep -q "^${auth_container}$"; then
        echo "❌ Auth container not running. Start it with:"
        echo "   cd types-first-dev && just services-up"
        return 1
    fi

    # Check Claude Code is authenticated (look for credentials)
    # Note: credentials file is .credentials.json (with leading dot)
    if ! docker exec "$auth_container" test -f /home/user/.claude/.credentials.json 2>/dev/null && \
       ! docker exec "$auth_container" test -d /home/user/.claude/local 2>/dev/null; then
        echo "❌ Claude Code not authenticated. Run:"
        echo "   cd types-first-dev && just auth-login"
        return 1
    fi

    return 0
}

if ! check_auth; then
    exit 1
fi

# Enable Rust logging for mantle visibility
export RUST_LOG="${RUST_LOG:-info}"

# Default spec file (can override via $1)
SPEC_FILE="${1:-$GIT_ROOT/types-first-dev/specs/url-shortener.yaml}"

# Optional target directory override (via $2)
TARGET_DIR="${2:-}"

# Run from git root (for cabal.project)
cd "$GIT_ROOT"

# Suppress cabal build output (only show errors)
if [ -n "$TARGET_DIR" ]; then
    cabal run -v0 types-first-dev:exe:types-first-dev-runner -- "$SPEC_FILE" "$TARGET_DIR"
else
    cabal run -v0 types-first-dev:exe:types-first-dev-runner -- "$SPEC_FILE"
fi
