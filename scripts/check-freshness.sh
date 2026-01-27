#!/usr/bin/env bash
# Check if running containers match local git state
# Usage: ./scripts/check-freshness.sh [service] [compose-file]

set -euo pipefail

SERVICE_NAME="${1:-control-server}"
COMPOSE_FILE="${2:-docker-compose.yml}"
LABEL_NAME="org.opencontainers.image.revision"

echo ">>> [Freshness Check] Service: $SERVICE_NAME"

# Check for dirty working directory
if [ -n "$(git status --porcelain)" ]; then
    echo "WARNING: Local working directory is DIRTY (uncommitted changes)."
    DIRTY_STATE=true
else
    DIRTY_STATE=false
fi

# Get local git SHA
LOCAL_SHA=$(git rev-parse --short HEAD)
echo "    Local Git SHA:   $LOCAL_SHA"

# Get container ID
CONTAINER_ID=$(docker compose -f "$COMPOSE_FILE" ps -q "$SERVICE_NAME" 2>/dev/null)

if [ -z "$CONTAINER_ID" ]; then
    echo "CRITICAL: No running container found for '$SERVICE_NAME'."
    echo "          Action: Run 'just dev-up $SERVICE_NAME'"
    exit 1
fi

# Get container's git SHA from label
CONTAINER_SHA=$(docker inspect --format "{{ index .Config.Labels \"$LABEL_NAME\" }}" "$CONTAINER_ID" 2>/dev/null)

if [ -z "$CONTAINER_SHA" ]; then
    echo "WARNING: Container lacks '$LABEL_NAME' label."
    echo "         Rebuild with: just dev-up $SERVICE_NAME"
    exit 2
fi

echo "    Container SHA:   $CONTAINER_SHA"

# Compare
if [ "$LOCAL_SHA" != "$CONTAINER_SHA" ]; then
    echo "!!! STALE DETECTED !!!"
    echo "    Running code does not match local git revision."
    echo "    Action: Run 'just dev-up $SERVICE_NAME'"
    exit 1
elif [ "$DIRTY_STATE" = true ]; then
    echo ">>> FRESH (base commit) - BUT LOCAL IS DIRTY"
    echo "    Container matches commit, but uncommitted changes won't be reflected."
    exit 0
else
    echo ">>> FRESH: System is synchronized."
    exit 0
fi
