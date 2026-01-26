#!/usr/bin/env bash
# Health check for control-server via exomonad
set -euo pipefail

# Ensure we're checking from project root
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
export EXOMONAD_PROJECT_DIR="$PROJECT_ROOT"

# Use prebuilt binary (much faster than cargo run)
exec "$PROJECT_ROOT/rust/target/debug/exomonad" health
