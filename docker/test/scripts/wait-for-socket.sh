#!/bin/bash
set -euo pipefail

SOCKET_PATH=$1
TIMEOUT=${2:-30}
INTERVAL=0.5

echo "Waiting for socket $SOCKET_PATH (timeout: ${TIMEOUT}s)..."

count=0
while [ ! -S "$SOCKET_PATH" ]; do
    sleep $INTERVAL
    count=$((count + 1))
    if [ $(echo "$count * $INTERVAL >= $TIMEOUT" | bc) -eq 1 ]; then
        echo "Timeout waiting for $SOCKET_PATH"
        exit 1
    fi
done

echo "Socket $SOCKET_PATH is ready."
