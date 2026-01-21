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

# Canonical socket paths - Central source of truth
export TIDEPOOL_CONTROL_SOCKET="${TIDEPOOL_CONTROL_SOCKET:-.tidepool/sockets/control.sock}"
export TIDEPOOL_TUI_SOCKET="${TIDEPOOL_TUI_SOCKET:-.tidepool/sockets/tui.sock}"

# Create runtime directories
mkdir -p .tidepool/{sockets,logs}

# Detect and clean up stale process-compose sessions via UDS
# This eliminates port 8080 conflicts in parallel worktrees
PC_SOCKET=".tidepool/sockets/process-compose.sock"
if [ -S "$PC_SOCKET" ]; then
    if process-compose process list -u "$PC_SOCKET" > /dev/null 2>&1; then
        echo "⚠️  Found stale process-compose session. Shutting it down..."
        process-compose down -u "$PC_SOCKET" || true
        sleep 0.5
    fi
fi

# Clean up any other stale sockets from previous runs
# We use a loop to avoid deleting the process-compose socket if it was just recreated/preserved
shopt -s nullglob
for sock in .tidepool/sockets/*.sock; do
    if [ "$sock" != "$PC_SOCKET" ]; then
        rm -f "$sock"
    fi
done
shopt -u nullglob

# Discover Hangar by walking up from current directory
HANGAR_ROOT=$(pwd)
while [ ! -f "$HANGAR_ROOT/Hangar.toml" ] && [ "$HANGAR_ROOT" != "/" ] && [ "$HANGAR_ROOT" != "$HOME" ]; do
    HANGAR_ROOT=$(dirname "$HANGAR_ROOT")
done

if [ ! -f "$HANGAR_ROOT/Hangar.toml" ]; then
    echo "ERROR: Hangar.toml not found (searched up from $(pwd))"
    exit 1
fi

echo "📦 Discovered Hangar at: $HANGAR_ROOT"

# Add all Hangar binaries (including mantle-agent) to PATH for Claude and related tools
export PATH="$HANGAR_ROOT/runtime/bin:$PATH"

# Check dependencies
if ! command -v process-compose &> /dev/null; then
    echo "ERROR: process-compose not installed"
    echo "Install with: brew install process-compose"
    exit 1
fi

if ! command -v zellij &> /dev/null; then
    echo "ERROR: zellij not installed"
    echo "Install with: brew install zellij"
    exit 1
fi

echo "🌊 Starting Hybrid Tidepool..."
echo ""
echo "  Layout:"
echo "    ├─ Claude Code (left)     - waits for MCP"
echo "    ├─ TUI Sidebar (top-right) - waits for TUI socket"
echo "    └─ Backend (bottom-right)  - process-compose dashboard"
echo ""
echo "  Logs: .tidepool/logs/"
echo ""

# Launch Zellij with the hybrid layout
exec zellij --layout .zellij/tidepool.kdl
