#!/bin/bash
set -euo pipefail

OPENOBSERVE_URL="${OPENOBSERVE_URL:-http://localhost:5080}"
AUTH_HEADER="${OPENOBSERVE_AUTH_HEADER:-$(echo -n 'admin@exomonad.local:exomonad-dev' | base64)}"

echo "=== OpenObserve Verification ==="

# 1. Health check
echo -n "Health check... "
curl -sf "${OPENOBSERVE_URL}/healthz" > /dev/null && echo "✓" || echo "✗"

# 2. Auth check
echo -n "Authentication... "
curl -sf -H "Authorization: Basic ${AUTH_HEADER}" \
  "${OPENOBSERVE_URL}/api/default/streams" > /dev/null && echo "✓" || echo "✗"

# 3. Traces stream exists
echo -n "Traces stream... "
STREAMS=$(curl -sf -H "Authorization: Basic ${AUTH_HEADER}" \
  "${OPENOBSERVE_URL}/api/default/streams" | jq -r '.list[].name' 2>/dev/null)
echo "$STREAMS" | grep -q "traces" && echo "✓ (exists)" || echo "⚠ (will be created on first span)"

echo ""
echo "Web UI: ${OPENOBSERVE_URL}"
echo "OTLP gRPC: ${OPENOBSERVE_URL%:5080}:5081"
