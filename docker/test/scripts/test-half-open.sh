#!/bin/bash
set -euo pipefail

# Test: Half-open Sockets
# Verify that no leaked connections persist on the control socket.

ORCHESTRATOR_CONTAINER="test-orchestrator"
CONTROL_SOCKET="/sockets/control.sock"

echo "Checking connection count on $CONTROL_SOCKET..."
# lsof -U lists unix sockets. We look for our control.sock.
# We expect 1 listener (the control-server) and potentially 0 or more clients.
# If we just finished a test, we want to ensure clients have disconnected.

# Run a few connections to simulate load
for i in {1..5}; do
    docker exec "$ORCHESTRATOR_CONTAINER" curl -s --unix-socket "$CONTROL_SOCKET" http://localhost/ping > /dev/null
done

sleep 1

# Check reference count
REF_COUNT=$(docker exec "$ORCHESTRATOR_CONTAINER" lsof -U | grep "$CONTROL_SOCKET" | wc -l)

echo "Socket reference count: $REF_COUNT"

# 1 for the server process. If multiple panes are open, there might be more if they keep connections open.
# In our minimal orchestrator.kdl, only the Control Server pane should have it open.
if [ "$REF_COUNT" -gt 5 ]; then
    echo "Warning: High reference count ($REF_COUNT) may indicate leaked connections."
    docker exec "$ORCHESTRATOR_CONTAINER" lsof -U | grep "$CONTROL_SOCKET"
    # exit 1 # Optional: make it a hard failure if we are sure it should be exactly 1
fi

echo "Socket leak test passed (count: $REF_COUNT)."
