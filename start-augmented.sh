#!/usr/bin/env bash
set -e

cd "$(dirname "$0")"

# Validate .env
if [ ! -f .env ]; then
    echo "ERROR: .env file not found"
    exit 1
fi

source .env

if [ -z "$ANTHROPIC_API_KEY" ]; then
    echo "ERROR: ANTHROPIC_API_KEY not set in .env"
    exit 1
fi

# Create runtime directories
mkdir -p .tidepool/{sockets,logs}

# Build binaries if needed (fast if already built)
echo "Checking binaries..."
cabal build tidepool-control-server --enable-optimization=0 >/dev/null 2>&1 || {
    echo "ERROR: Failed to build control-server"
    exit 1
}
(cd rust && cargo build --bin mantle-agent --bin tui-sidebar) >/dev/null 2>&1 || {
    echo "ERROR: Failed to build Rust binaries"
    exit 1
}

# Check dependencies
if ! command -v process-compose &> /dev/null; then
    echo "ERROR: process-compose not installed"
    echo "Install with: brew install process-compose"
    echo "Or download from: https://github.com/F1bonacc1/process-compose/releases"
    exit 1
fi

# Check for existing process-compose session
if curl -sf http://localhost:8080/hostname > /dev/null 2>&1; then
    echo "⚠️  Existing Tidepool session detected, shutting down..."
    ~/.local/bin/process-compose down --ordered-shutdown
    sleep 2
fi

echo "Starting Hybrid Tidepool..."
echo "  Teaching: ${TEACHING_ENABLED:-false}"
echo "  Logs: .tidepool/logs/"
echo ""
echo "Note: MCP server connects to control-server via TCP."
echo "      If MCP shows 'failed' initially, use /mcp → Reconnect after services start."
echo ""

# Launch Zellij with the simplified layout
zellij --layout .zellij/tidepool.kdl
