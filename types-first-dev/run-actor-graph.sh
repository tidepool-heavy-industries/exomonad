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
