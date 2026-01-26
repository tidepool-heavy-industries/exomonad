#!/bin/bash
set -euo pipefail

# Test: Sibling cleanup
# Verify that the orchestrator kills sibling containers labeled with its hostname on exit.

TEST_LABEL="exomonad.test.cleanup=true"
ORCHESTRATOR_ID="test-orch-$(date +%s)"
AGENT_LABEL="exomonad.orchestrator=$ORCHESTRATOR_ID"

echo "Starting orchestrator $ORCHESTRATOR_ID..."
docker run -d --name "$ORCHESTRATOR_ID" \
    --hostname "$ORCHESTRATOR_ID" \
    -v /var/run/docker.sock:/var/run/docker.sock \
    -l "$TEST_LABEL" \
    exomonad-orchestrator:test

echo "Spawning dummy agents..."
docker run -d --name "${ORCHESTRATOR_ID}-agent1" \
    -l "$AGENT_LABEL" \
    -l "$TEST_LABEL" \
    alpine tail -f /dev/null

docker run -d --name "${ORCHESTRATOR_ID}-agent2" \
    -l "$AGENT_LABEL" \
    -l "$TEST_LABEL" \
    alpine tail -f /dev/null

echo "Verifying agents are running..."
RUNNING_AGENTS=$(docker ps -q --filter "label=$AGENT_LABEL" | wc -l)
if [ "$RUNNING_AGENTS" -ne 2 ]; then
    echo "Agents failed to start (expected 2, found $RUNNING_AGENTS)"
    exit 1
fi

echo "Stopping orchestrator..."
docker stop "$ORCHESTRATOR_ID"

echo "Waiting for cleanup..."
sleep 5

echo "Verifying agents are gone..."
REMAINING=$(docker ps -aq --filter "label=$AGENT_LABEL")
if [ -n "$REMAINING" ]; then
    echo "Leaked containers found:"
    docker ps -a --filter "label=$AGENT_LABEL"
    docker rm -f $REMAINING || true
    docker rm -f "$ORCHESTRATOR_ID" || true
    exit 1
fi

echo "Cleanup successful."
docker rm -f "$ORCHESTRATOR_ID" || true
