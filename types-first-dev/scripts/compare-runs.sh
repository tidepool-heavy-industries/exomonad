#!/usr/bin/env bash
# Compare baseline runs across branches
#
# Usage:
#   ./scripts/compare-runs.sh run1 run2 [run3 ...]
#
# Compares automated scores and produces a summary table.

set -euo pipefail

BASELINE_DIR="$HOME/tidepool-labs/dev-runs/baseline"

if [ $# -lt 2 ]; then
  echo "Usage: $0 run1 run2 [run3 ...]"
  echo ""
  echo "Available runs:"
  ls -1 "$BASELINE_DIR" 2>/dev/null || echo "  (none)"
  exit 1
fi

echo "════════════════════════════════════════════════════════════════"
echo "Baseline Comparison"
echo "════════════════════════════════════════════════════════════════"
echo ""

# Header
printf "%-25s | %-8s | %-6s | %-6s | %-10s | %-8s\n" \
  "Run" "Branch" "Build" "Tests" "Endpoints" "Duration"
printf "%-25s-+-%-8s-+-%-6s-+-%-6s-+-%-10s-+-%-8s\n" \
  "-------------------------" "--------" "------" "------" "----------" "--------"

# Data rows
for RUN in "$@"; do
  RUN_DIR="$BASELINE_DIR/$RUN"

  if [ ! -f "$RUN_DIR/automated-scores.json" ]; then
    printf "%-25s | %-8s | %-6s | %-6s | %-10s | %-8s\n" \
      "$RUN" "?" "?" "?" "?" "?"
    continue
  fi

  BRANCH=$(jq -r '.branch // "?"' "$RUN_DIR/automated-scores.json")
  BUILD=$(jq -r 'if .compiles then "PASS" else "FAIL" end' "$RUN_DIR/automated-scores.json")
  TESTS=$(jq -r 'if .tests_pass then "PASS" else "FAIL" end' "$RUN_DIR/automated-scores.json")
  ENDPOINTS=$(jq -r '.endpoint_count // 0' "$RUN_DIR/automated-scores.json")
  DURATION=$(jq -r '.duration_seconds // 0' "$RUN_DIR/automated-scores.json")

  printf "%-25s | %-8s | %-6s | %-6s | %-10s | %-8s\n" \
    "$RUN" "${BRANCH:0:8}" "$BUILD" "$TESTS" "$ENDPOINTS/3" "${DURATION}s"
done

echo ""
echo "════════════════════════════════════════════════════════════════"
echo ""
echo "To inspect a run:"
echo "  ls $BASELINE_DIR/<run>/artifacts/"
echo "  cat $BASELINE_DIR/<run>/workflow.log"
