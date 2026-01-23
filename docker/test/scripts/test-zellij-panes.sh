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

echo "Triggering spawn_agents..."
# Using HTTP over Unix socket via curl
docker exec "$ORCHESTRATOR_CONTAINER" curl -s --unix-socket "$CONTROL_SOCKET" -X POST http://localhost/mcp/call \
    -H "Content-Type: application/json" \
    -d '{"id":"test-1","tool_name":"spawn_agents","arguments":{"bead_ids":["8sz"]}}'

echo "Waiting for Zellij to process new tab..."
sleep 5

echo "Verifying new tab exists in Zellij..."
# Dump layout and check for the tab name '8sz'
docker exec "$ORCHESTRATOR_CONTAINER" zellij action dump-layout | grep -q "8sz"

echo "Zellij tab spawning verified."
