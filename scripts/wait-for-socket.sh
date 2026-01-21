#!/usr/bin/env bash
# scripts/wait-for-socket.sh <socket-path> [timeout-seconds] [label]

SOCKET_PATH=$1
TIMEOUT=${2:-60}
LABEL=${3:-"socket"}

if [ -z "$SOCKET_PATH" ]; then
  echo "Usage: $0 <socket-path> [timeout-seconds] [label]" >&2
  exit 1
fi

ELAPSED=0
WAIT=1

while [ ! -S "$SOCKET_PATH" ]; do
  if [ $ELAPSED -ge $TIMEOUT ]; then
    echo "ERROR: $LABEL ($SOCKET_PATH) not created after ${TIMEOUT}s. Check Backend pane for errors." >&2
    exit 1
  fi
  echo "⏳ Waiting for $LABEL... (${ELAPSED}s/${TIMEOUT}s)"
  sleep $WAIT
  ELAPSED=$((ELAPSED + WAIT))
  if [ $WAIT -lt 5 ]; then
    WAIT=$((WAIT + 1))
  fi
done

echo "✓ $LABEL ready!"
