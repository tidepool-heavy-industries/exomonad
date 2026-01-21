#!/bin/bash
cd "$(dirname "$0")/.." || exit 1

cleanup() {
    echo "🛑 Tidepool Runner: Shutting down services..."

    # Try graceful API shutdown first via UDS
    PC_SOCKET=".tidepool/sockets/process-compose.sock"
    if process-compose down --ordered-shutdown -u "$PC_SOCKET" 2>/dev/null; then
        echo "✓ Graceful shutdown via API"
    else
        # Fallback: Send SIGTERM if API unreachable
        echo "⚠️  API unavailable, sending SIGTERM..."
        if [ -n "$PC_PID" ]; then
            kill -TERM "$PC_PID" 2>/dev/null || true
        fi
    fi
}

trap cleanup EXIT SIGINT SIGTERM SIGHUP

export PC_NO_SERVER=true PC_USE_UDS=true PC_SOCKET_PATH=.tidepool/sockets/process-compose.sock

echo "🚀 Starting Tidepool Orchestrator..."
process-compose "$@" &
PC_PID=$!
wait $PC_PID
