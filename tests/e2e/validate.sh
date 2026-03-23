#!/usr/bin/env bash
set -euo pipefail

# E2E Validation Script
# Run from WITHIN the e2e tmux session (TL window) to verify the pipeline.

SOCK=".exo/server.sock"

echo "=== E2E Validation ==="
echo ""

# Health check
echo ">>> Health check..."
if curl -sf --unix-socket "$SOCK" http://localhost/health | jq .; then
    echo "  OK"
else
    echo "  FAILED — is the server running?"
    exit 1
fi
echo ""

# List tools
echo ">>> Available tools (root role)..."
curl -sf --unix-socket "$SOCK" http://localhost/agents/root/root/tools | jq '.tools[].name'
echo ""

# Create a feature branch + commit, then file_pr
echo ">>> Creating test branch + filing PR via tool call..."
git config user.name "Exomonad E2E" 2>/dev/null || true
git config user.email "e2e@example.com" 2>/dev/null || true
git checkout -b main.test-feature 2>/dev/null || git checkout main.test-feature
git commit --allow-empty -m "test feature commit"
git push origin main.test-feature 2>/dev/null || true

RESPONSE=$(curl -sf --unix-socket "$SOCK" -X POST http://localhost/agents/dev/test-feature/tools/call \
    -H 'Content-Type: application/json' \
    -d '{"name":"file_pr","arguments":{"title":"Test PR","body":"E2E test"}}')
echo "  file_pr response:"
echo "$RESPONSE" | jq . 2>/dev/null || echo "$RESPONSE"
echo ""

# Check mock received the call
echo ">>> Mock GitHub log:"
MOCK_LOG="${MOCK_LOG:-}"
if [[ -n "$MOCK_LOG" && -f "$MOCK_LOG" ]]; then
    cat "$MOCK_LOG" | tail -5
else
    echo "  (MOCK_LOG not set or file not found — check from parent shell)"
fi
echo ""

# Check gh mock log
echo ">>> GH CLI mock log:"
GH_MOCK_LOG="${GH_MOCK_LOG:-}"
if [[ -n "$GH_MOCK_LOG" && -f "$GH_MOCK_LOG" ]]; then
    cat "$GH_MOCK_LOG" | tail -5
else
    echo "  (GH_MOCK_LOG not set or file not found — check from parent shell)"
fi
echo ""

# Merge via tool call
echo ">>> Merging PR #1 via tool call..."
RESPONSE=$(curl -sf --unix-socket "$SOCK" -X POST http://localhost/agents/root/root/tools/call \
    -H 'Content-Type: application/json' \
    -d '{"name":"merge_pr","arguments":{"pr_number":1}}')
echo "  merge_pr response:"
echo "$RESPONSE" | jq . 2>/dev/null || echo "$RESPONSE"
echo ""

echo "=== Validation Complete ==="
