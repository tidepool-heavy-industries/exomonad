#!/usr/bin/env bash
# Health check for control-server via mantle-agent
set -euo pipefail

# Ensure we're checking from project root
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
export TIDEPOOL_PROJECT_DIR="$PROJECT_ROOT"

# Use prebuilt binary (much faster than cargo run)
exec "$PROJECT_ROOT/rust/target/debug/mantle-agent" health
