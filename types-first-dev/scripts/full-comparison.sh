#!/usr/bin/env bash
# Full comparison report with automated + quality scores
#
# Usage:
#   ./scripts/full-comparison.sh run1 run2 [run3 ...]

set -euo pipefail

BASELINE_DIR="$HOME/tidepool-labs/dev-runs/baseline"

if [ $# -lt 1 ]; then
  echo "Usage: $0 run1 [run2 ...]"
  exit 1
fi

echo "# Baseline Comparison Report"
echo ""
echo "_Generated: $(date)_"
echo ""

echo "## Automated Scores"
echo ""
echo "| Run | Branch | Build | Tests | Endpoints | Duration |"
echo "|-----|--------|-------|-------|-----------|----------|"

for RUN in "$@"; do
  RUN_DIR="$BASELINE_DIR/$RUN"

  if [ ! -f "$RUN_DIR/automated-scores.json" ]; then
    echo "| $RUN | ? | ? | ? | ? | ? |"
    continue
  fi

  BRANCH=$(jq -r '.branch // "?"' "$RUN_DIR/automated-scores.json")
  BUILD=$(jq -r 'if .compiles then "PASS" else "**FAIL**" end' "$RUN_DIR/automated-scores.json")
  TESTS=$(jq -r 'if .tests_pass then "PASS" else "**FAIL**" end' "$RUN_DIR/automated-scores.json")
  ENDPOINTS=$(jq -r '.endpoint_count // 0' "$RUN_DIR/automated-scores.json")
  DURATION=$(jq -r '.duration_seconds // 0' "$RUN_DIR/automated-scores.json")

  echo "| $RUN | $BRANCH | $BUILD | $TESTS | $ENDPOINTS/3 | ${DURATION}s |"
done

echo ""
echo "## Quality Scores"
echo ""
echo "| Run | Spec | Types | Tests | Impl | Coherence | Idiom | **Aggregate** |"
echo "|-----|------|-------|-------|------|-----------|-------|---------------|"

for RUN in "$@"; do
  RUN_DIR="$BASELINE_DIR/$RUN"

  if [ ! -f "$RUN_DIR/quality-scores.json" ]; then
    echo "| $RUN | - | - | - | - | - | - | _not scored_ |"
    continue
  fi

  SF=$(jq -r '.spec_fidelity.score // "-"' "$RUN_DIR/quality-scores.json")
  TD=$(jq -r '.type_design.score // "-"' "$RUN_DIR/quality-scores.json")
  TQ=$(jq -r '.test_quality.score // "-"' "$RUN_DIR/quality-scores.json")
  IQ=$(jq -r '.impl_quality.score // "-"' "$RUN_DIR/quality-scores.json")
  CO=$(jq -r '.coherence.score // "-"' "$RUN_DIR/quality-scores.json")
  ID=$(jq -r '.idiomaticity.score // "-"' "$RUN_DIR/quality-scores.json")
  AGG=$(jq -r '.aggregate // "-"' "$RUN_DIR/quality-scores.json")

  echo "| $RUN | $SF | $TD | $TQ | $IQ | $CO | $ID | **$AGG** |"
done

echo ""
echo "## Dimension Legend"
echo ""
echo "- **Spec**: Implements acceptance criteria"
echo "- **Types**: Quality of data type design"
echo "- **Tests**: Coverage and strategy"
echo "- **Impl**: Clean implementation"
echo "- **Coherence**: Types/tests/impl fit together"
echo "- **Idiom**: Idiomatic Haskell patterns"
echo ""
echo "Scale: 1-5 (5 = excellent)"
echo ""
echo "Weights: Spec 25%, Types 20%, Tests 15%, Impl 15%, Coherence 15%, Idiom 10%"
