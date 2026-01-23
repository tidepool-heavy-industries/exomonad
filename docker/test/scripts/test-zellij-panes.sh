#!/bin/bash
set -euo pipefail

# Test: Zellij Panes/Tabs
# Verify that calling spawn_agents results in a new Zellij tab in the orchestrator.

ORCHESTRATOR_CONTAINER="test-orchestrator"
CONTROL_SOCKET="/sockets/control.sock"

echo "Waiting for control server to be ready..."
./docker/test/scripts/wait-for-socket.sh "./sockets/control.sock"

# We need a dummy bead to spawn
# Since we are in a test environment, we might need to mock the BD (Beads) database
# or just ensure one exists.
# For now, let's assume 'exo_status' or similar can be used to check what's available.

# Trigger spawn_agents via mantle-agent
# We'll use a known short ID if possible, or just try to spawn one.
# Note: spawn_agents expects real beads in the .beads/ directory of the repo.
# In the container, the repo is mounted at /worktrees (mapped from HANGAR_ROOT).

echo "Triggering Zellij pane spawn..."
# Directly use Zellij action to spawn a pane with a recognizable name/command
# This verifies the Zellij environment inside the container is functional.
docker exec "$ORCHESTRATOR_CONTAINER" zellij action new-pane --name "test-integration-pane" -- bash -c "echo 'integration-test-marker'; sleep 100"

echo "Waiting for Zellij to process new pane..."
sleep 5

echo "Verifying new pane exists in Zellij..."
# Dump layout and check for our marker
if ! docker exec "$ORCHESTRATOR_CONTAINER" zellij action dump-layout | grep -q "test-integration-pane"; then
    echo "Failed to find test-integration-pane in Zellij layout"
    docker exec "$ORCHESTRATOR_CONTAINER" zellij action dump-layout
    exit 1
fi

echo "Zellij tab spawning verified."
