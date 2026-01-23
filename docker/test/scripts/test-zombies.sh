#!/bin/bash
set -euo pipefail

# Test: Zombie Detection
# Verify that tini reaps orphans and no zombie processes persist.

ORCHESTRATOR_CONTAINER="test-orchestrator"

echo "Checking for zombie processes in $ORCHESTRATOR_CONTAINER..."
ZOMBIES=$(docker exec "$ORCHESTRATOR_CONTAINER" ps aux | awk '$8=="Z" {print $0}')

if [ -n "$ZOMBIES" ]; then
    echo "Zombie processes detected:"
    echo "$ZOMBIES"
    exit 1
fi

echo "No zombie processes found."
