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
export EXOMONAD_CONTROL_SOCKET="${EXOMONAD_CONTROL_SOCKET:-.exomonad/sockets/control.sock}"
export EXOMONAD_TUI_SOCKET="${EXOMONAD_TUI_SOCKET:-.exomonad/sockets/tui.sock}"

# Create runtime directories
mkdir -p .exomonad/{sockets,logs}

# Detect and clean up stale process-compose sessions via UDS
# This eliminates port 8080 conflicts in parallel worktrees
PC_SOCKET=".exomonad/sockets/process-compose.sock"
if [ -S "$PC_SOCKET" ]; then
    if process-compose process list -u "$PC_SOCKET" > /dev/null 2>&1; then
        echo "⚠️  Found stale process-compose session. Shutting it down..."
        
        # Try graceful shutdown with a timeout (bg + wait loop)
        process-compose down -u "$PC_SOCKET" &
        DOWN_PID=$!
        
        # Wait up to 5 seconds
        for i in {1..10}; do
            if ! kill -0 $DOWN_PID 2>/dev/null; then
                break
            fi
            sleep 0.5
        done
        
        # If down command is still running or socket exists, force cleanup
        if kill -0 $DOWN_PID 2>/dev/null; then
            echo "⚠️  Shutdown timed out. Forcing cleanup..."
            kill -9 $DOWN_PID 2>/dev/null || true
            
            # Find and kill the server process holding the socket
            if command -v lsof >/dev/null; then
                SERVER_PID=$(lsof -t "$PC_SOCKET" 2>/dev/null)
                if [ -n "$SERVER_PID" ]; then
                    kill -9 $SERVER_PID 2>/dev/null || true
                fi
            fi
        fi
    fi
    # Ensure socket is gone so new instance can bind
    rm -f "$PC_SOCKET"
fi

# Clean up any other stale sockets from previous runs
# We use a loop to avoid deleting the process-compose socket if it was just recreated/preserved
shopt -s nullglob
for sock in .exomonad/sockets/*.sock; do
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

# Export HANGAR_ROOT for control-server and subagents
export HANGAR_ROOT

# Add all Hangar binaries (including exomonad) to PATH for Claude and related tools
export PATH="$HANGAR_ROOT/runtime/bin:$PATH"

# Export EXOMONAD_BIN_DIR for SessionStart hooks in settings.json
export EXOMONAD_BIN_DIR="$HANGAR_ROOT/runtime/bin"

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

echo "🌊 Starting Hybrid ExoMonad..."
echo ""
echo "  Layout:"
echo "    ├─ Claude Code (left)     - waits for MCP"
echo "    ├─ TUI Sidebar (top-right) - waits for TUI socket"
echo "    └─ Backend (bottom-right)  - process-compose dashboard"
echo ""
echo "  Logs: .exomonad/logs/"
echo ""

# Launch Zellij with the hybrid layout
exec zellij --layout .zellij/exomonad.kdl
