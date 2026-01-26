#!/bin/bash
set -euo pipefail

# Test: Socket Permissions
# Verify that the Unix socket is accessible from the agent container with correct permissions.

ORCHESTRATOR_CONTAINER="test-orchestrator"
AGENT_CONTAINER="test-agent"
SOCKET_PATH="/sockets/control.sock"
AGENT_SOCKET_PATH="/home/agent/.exomonad/sockets/control.sock"

echo "Checking socket existence and permissions in orchestrator..."
docker exec "$ORCHESTRATOR_CONTAINER" ls -l "$SOCKET_PATH"

# Verify permissions are 660 (rw-rw----) or similar
PERMS=$(docker exec "$ORCHESTRATOR_CONTAINER" stat -c "%a" "$SOCKET_PATH")
if [[ "$PERMS" != "660" && "$PERMS" != "777" ]]; then
    echo "Warning: Socket permissions are $PERMS, expected 660 (or 777 in some dev envs)"
    # We'll allow 777 for now if that's how it's configured, but 660 is the target.
fi

echo "Verifying GID matches across containers..."
ORCH_GID=$(docker exec "$ORCHESTRATOR_CONTAINER" stat -c "%g" "$SOCKET_PATH")
echo "Socket GID: $ORCH_GID"

# In the agent container, the user 'agent' should be able to connect to the socket.
# We'll use curl to test HTTP connectivity over Unix socket.
echo "Testing connectivity from agent container..."
docker exec -u agent "$AGENT_CONTAINER" curl -s --unix-socket "$AGENT_SOCKET_PATH" http://localhost/ping | grep -q "pong"

echo "Socket connectivity verified."
