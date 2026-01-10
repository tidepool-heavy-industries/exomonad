#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

# Source .env if it exists (for CLAUDE_CODE_OAUTH_TOKEN)
if [ -f .env ]; then
    set -a
    source .env
    set +a
fi

# Enable Rust logging for mantle visibility
export RUST_LOG="${RUST_LOG:-info}"

# Run the actor runtime
cd types-first-dev
cabal run types-first-dev:exe:types-first-dev-runner
