#!/bin/bash
set -euo pipefail

# Main Integration Test Runner for Tidepool Docker Stack

# Ensure we are in the repo root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$REPO_ROOT"

# 1. Setup - Copy binaries to build contexts
echo "Preparing build contexts in $REPO_ROOT..."
mkdir -p docker/orchestrator/bin
mkdir -p docker/claude-agent/bin

# Find hangar root
HANGAR_ROOT="$REPO_ROOT"
while [ ! -f "$HANGAR_ROOT/Hangar.toml" ] && [ "$HANGAR_ROOT" != "/" ] && [ "$HANGAR_ROOT" != "$HOME" ]; do
    HANGAR_ROOT=$(dirname "$HANGAR_ROOT")
done

RUNTIME_BIN="$HANGAR_ROOT/runtime/bin"

if [ ! -d "$RUNTIME_BIN" ]; then
    echo "ERROR: Runtime binaries not found at $RUNTIME_BIN"
    exit 1
fi

echo "Copying binaries from $RUNTIME_BIN..."
cp "$RUNTIME_BIN/tidepool-control-server" docker/orchestrator/bin/
cp "$RUNTIME_BIN/tui-sidebar" docker/orchestrator/bin/
cp "$RUNTIME_BIN/mantle-agent" docker/claude-agent/bin/

# 2. Build images
echo "Building test images..."
docker-compose -f docker/test/docker-compose.test.yml build

# 3. Run containers
echo "Starting test environment..."
docker-compose -f docker/test/docker-compose.test.yml up -d

# Cleanup function
cleanup() {
    echo "Cleaning up..."
    docker-compose -f docker/test/docker-compose.test.yml down -v
    # Clean up any leftover test containers from sibling-cleanup test
    docker ps -aq --filter "label=tidepool.test.cleanup=true" | xargs -r docker rm -f
}

trap cleanup EXIT

# 4. Run tests
echo "Running integration tests..."

failed=0

run_test() {
    local test_script=$1
    echo "────────────────────────────────────────────────────────────────"
    echo "RUNNING: $test_script"
    if bash "$test_script"; then
        echo "✅ PASSED: $test_script"
    else
        echo "❌ FAILED: $test_script"
        failed=$((failed + 1))
    fi
}

run_test "docker/test/scripts/test-sibling-cleanup.sh"
run_test "docker/test/scripts/test-socket-perms.sh"
run_test "docker/test/scripts/test-zellij-panes.sh"
run_test "docker/test/scripts/test-zombies.sh"
run_test "docker/test/scripts/test-half-open.sh"

echo "────────────────────────────────────────────────────────────────"
if [ $failed -eq 0 ]; then
    echo "ALL INTEGRATION TESTS PASSED"
    exit 0
else
    echo "$failed TESTS FAILED"
    exit 1
fi
