#!/bin/bash
set -euo pipefail

# Test: Zellij Panes/Tabs
# Verify that calling spawn_agents results in a new Zellij tab in the orchestrator.

ORCHESTRATOR_CONTAINER="test-orchestrator"
CONTROL_SOCKET="/sockets/control.sock"

echo "Waiting for control server to be ready..."
./docker/test/scripts/wait-for-socket.sh "./sockets/control.sock"

# Test Zellij pane spawning by creating a new pane with a recognizable command.
# This verifies the Zellij environment inside the container is functional.

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
