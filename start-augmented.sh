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

# Clean up any stale sockets from previous runs
rm -f .tidepool/sockets/*.sock

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
