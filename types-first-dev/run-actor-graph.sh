#!/usr/bin/env bash
# run-actor-graph.sh - Run the types-first-dev 20-node actor graph
#
# This script manages the full execution environment:
# 1. Builds mantle and mantle-hub if needed
# 2. Starts mantle-hub in the background
# 3. Runs types-first-dev-runner
# 4. Cleans up on exit
#
# Usage:
#   ./run-actor-graph.sh [target-dir]
#
# Environment:
#   SKIP_BUILD=1     - Skip cargo build step
#   VERBOSE=1        - Show more output
#   HUB_PORT=8765    - Port for mantle-hub (default: 8765)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TIDEPOOL_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RUST_DIR="$TIDEPOOL_ROOT/rust"
TARGET_DIR="${1:-$SCRIPT_DIR/test-repo}"
HUB_PORT="${HUB_PORT:-8765}"
HUB_PID=""
MANTLE_BIN="$RUST_DIR/target/debug/mantle"
HUB_BIN="$RUST_DIR/target/debug/mantle-hub"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() { echo -e "${BLUE}[$(date +%H:%M:%S)]${NC} $*"; }
warn() { echo -e "${YELLOW}[$(date +%H:%M:%S)] WARNING:${NC} $*"; }
error() { echo -e "${RED}[$(date +%H:%M:%S)] ERROR:${NC} $*" >&2; }
success() { echo -e "${GREEN}[$(date +%H:%M:%S)]${NC} $*"; }

# Cleanup on exit
cleanup() {
    log "Cleaning up..."
    if [[ -n "$HUB_PID" ]] && kill -0 "$HUB_PID" 2>/dev/null; then
        log "Stopping mantle-hub (PID: $HUB_PID)"
        kill "$HUB_PID" 2>/dev/null || true
        wait "$HUB_PID" 2>/dev/null || true
    fi
    log "Cleanup complete"
}
trap cleanup EXIT INT TERM

# ════════════════════════════════════════════════════════════════════════════
# BUILD
# ════════════════════════════════════════════════════════════════════════════

build_rust() {
    if [[ "${SKIP_BUILD:-}" == "1" ]]; then
        log "Skipping rust build (SKIP_BUILD=1)"
        return
    fi

    log "Building mantle and mantle-hub..."
    cd "$RUST_DIR"
    cargo build --package mantle --package mantle-hub 2>&1 | tail -5
    success "Rust build complete"
}

build_haskell() {
    if [[ "${SKIP_BUILD:-}" == "1" ]]; then
        log "Skipping haskell build (SKIP_BUILD=1)"
        return
    fi

    log "Building types-first-dev-runner..."
    cd "$TIDEPOOL_ROOT"
    cabal build types-first-dev-runner 2>&1 | tail -10
    success "Haskell build complete"
}

# ════════════════════════════════════════════════════════════════════════════
# MANTLE-HUB
# ════════════════════════════════════════════════════════════════════════════

check_hub_running() {
    if curl -s "http://localhost:$HUB_PORT/api/sessions" >/dev/null 2>&1; then
        return 0
    fi
    return 1
}

start_hub() {
    if check_hub_running; then
        success "mantle-hub already running on port $HUB_PORT"
        return
    fi

    if [[ ! -x "$HUB_BIN" ]]; then
        error "mantle-hub binary not found at $HUB_BIN"
        error "Run: cargo build --package mantle-hub"
        exit 1
    fi

    log "Starting mantle-hub on port $HUB_PORT..."

    # Create hub data directory
    HUB_DATA_DIR="$TIDEPOOL_ROOT/.mantle-hub-data"
    mkdir -p "$HUB_DATA_DIR"

    # Start hub in background (uses `serve` subcommand)
    "$HUB_BIN" serve \
        --port "$HUB_PORT" \
        --db "$HUB_DATA_DIR/sessions.db" \
        --socket "$HUB_DATA_DIR/mantle.sock" \
        > "$HUB_DATA_DIR/hub.log" 2>&1 &
    HUB_PID=$!

    log "mantle-hub started (PID: $HUB_PID), waiting for ready..."

    # Wait for hub to be ready
    for i in {1..30}; do
        if check_hub_running; then
            success "mantle-hub ready on http://localhost:$HUB_PORT"
            return
        fi
        sleep 0.5
    done

    error "mantle-hub failed to start. Check $HUB_DATA_DIR/hub.log"
    cat "$HUB_DATA_DIR/hub.log" | tail -20
    exit 1
}

# ════════════════════════════════════════════════════════════════════════════
# MAIN
# ════════════════════════════════════════════════════════════════════════════

main() {
    echo ""
    echo "══════════════════════════════════════════════════════════════════"
    echo "  types-first-dev Actor Graph Runner"
    echo "══════════════════════════════════════════════════════════════════"
    echo ""
    echo "  Tidepool root:  $TIDEPOOL_ROOT"
    echo "  Target dir:     $TARGET_DIR"
    echo "  Hub port:       $HUB_PORT"
    echo ""

    # Check target directory
    if [[ ! -d "$TARGET_DIR" ]]; then
        warn "Target directory doesn't exist: $TARGET_DIR"
        log "Creating test-repo scaffold..."
        mkdir -p "$TARGET_DIR/src/Data"
        mkdir -p "$TARGET_DIR/test/Data"
        echo "-- Placeholder" > "$TARGET_DIR/src/Data/Stack.hs"
        echo "-- Placeholder" > "$TARGET_DIR/test/Data/StackSpec.hs"
        success "Created scaffold at $TARGET_DIR"
    fi

    # Build
    build_rust
    build_haskell

    # Start hub
    start_hub

    # Set up PATH for mantle binary
    export PATH="$RUST_DIR/target/debug:$PATH"

    # Verify mantle is accessible
    if ! command -v mantle >/dev/null 2>&1; then
        error "mantle not found in PATH after setup"
        exit 1
    fi
    log "mantle binary: $(which mantle)"

    # Run the graph!
    echo ""
    echo "══════════════════════════════════════════════════════════════════"
    echo "  Starting Graph Execution"
    echo "══════════════════════════════════════════════════════════════════"
    echo ""
    echo "  Hub UI:    http://localhost:$HUB_PORT"
    echo "  Hub API:   http://localhost:$HUB_PORT/api/sessions"
    echo ""
    echo "  Press Ctrl+C to stop"
    echo ""

    cd "$TIDEPOOL_ROOT"

    # Run with explicit timeout and output
    if [[ "${VERBOSE:-}" == "1" ]]; then
        cabal run types-first-dev-runner -- "$TARGET_DIR"
    else
        cabal run types-first-dev-runner -- "$TARGET_DIR" 2>&1 | tee "$SCRIPT_DIR/run.log"
    fi

    EXIT_CODE=$?

    if [[ $EXIT_CODE -eq 0 ]]; then
        success "Graph execution completed successfully!"
    else
        error "Graph execution failed with exit code $EXIT_CODE"
        echo ""
        echo "Check logs:"
        echo "  - Runner output: $SCRIPT_DIR/run.log"
        echo "  - Hub logs: $TIDEPOOL_ROOT/.mantle-hub-data/hub.log"
    fi

    return $EXIT_CODE
}

# ════════════════════════════════════════════════════════════════════════════
# SUBCOMMANDS
# ════════════════════════════════════════════════════════════════════════════

case "${1:-}" in
    hub-only)
        # Just start the hub and keep it running
        log "Starting hub only (Ctrl+C to stop)..."
        build_rust
        HUB_DATA_DIR="$TIDEPOOL_ROOT/.mantle-hub-data"
        start_hub
        log "Hub running at http://localhost:$HUB_PORT"
        log "  API:    http://localhost:$HUB_PORT/api/sessions"
        log "  Socket: $HUB_DATA_DIR/mantle.sock"
        log "  Logs:   $HUB_DATA_DIR/hub.log"
        log "Press Ctrl+C to stop."
        wait "$HUB_PID"
        ;;
    status)
        # Check status
        if check_hub_running; then
            success "mantle-hub is running on port $HUB_PORT"
            curl -s "http://localhost:$HUB_PORT/api/sessions" | head -5
        else
            warn "mantle-hub is not running"
        fi
        ;;
    build)
        # Just build
        build_rust
        build_haskell
        success "Build complete"
        ;;
    *)
        main
        ;;
esac
