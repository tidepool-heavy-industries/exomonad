#!/usr/bin/env bash
# Health check for control-server via mantle-agent
set -euo pipefail

# Ensure we're checking from project root
cd "$(dirname "$0")/.."
export TIDEPOOL_PROJECT_DIR="$(pwd)"

# Run health check
cargo run --bin mantle-agent --quiet -- health
