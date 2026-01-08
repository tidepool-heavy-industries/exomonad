#!/usr/bin/env bash
# Score quality dimensions for a baseline run
#
# Usage:
#   ./scripts/score-quality.sh <run-name>
#
# Interactive prompt for each dimension, saves to quality-scores.json

set -euo pipefail

RUN="${1:-}"
BASELINE_DIR="$HOME/tidepool-labs/dev-runs/baseline"

if [ -z "$RUN" ]; then
  echo "Usage: $0 <run-name>"
  exit 1
fi

RUN_DIR="$BASELINE_DIR/$RUN"
ARTIFACTS="$RUN_DIR/artifacts"

if [ ! -d "$RUN_DIR" ]; then
  echo "ERROR: Run not found: $RUN_DIR"
  exit 1
fi

echo "════════════════════════════════════════════════════════════════"
echo "Quality Scoring: $RUN"
echo "════════════════════════════════════════════════════════════════"
echo ""
echo "Review the generated code, then score each dimension 1-5."
echo ""
echo "Scale:"
echo "  1 = Poor (major issues)"
echo "  2 = Below average"
echo "  3 = Adequate"
echo "  4 = Good"
echo "  5 = Excellent"
echo ""
echo "Generated files:"
echo "  $ARTIFACTS/src/UrlShortener.hs"
echo "  $ARTIFACTS/test/Main.hs"
echo ""
read -p "Press Enter to continue..."
echo ""

# Show code summary
echo "════════════════════════════════════════════════════════════════"
echo "Code Summary"
echo "════════════════════════════════════════════════════════════════"
echo ""
if [ -f "$ARTIFACTS/src/UrlShortener.hs" ]; then
  echo "=== Types ==="
  grep -E "^data |^newtype |^type [A-Z]" "$ARTIFACTS/src/UrlShortener.hs" 2>/dev/null || echo "(none)"
  echo ""
  echo "=== API ==="
  grep -E "type.*API" "$ARTIFACTS/src/UrlShortener.hs" 2>/dev/null | head -5 || echo "(none)"
  echo ""
  echo "=== Functions ==="
  grep -E "^[a-z][a-zA-Z]+ ::" "$ARTIFACTS/src/UrlShortener.hs" 2>/dev/null | head -10 || echo "(none)"
fi
echo ""

# Score each dimension
score_dimension() {
  local name="$1"
  local description="$2"
  local score
  local rationale

  echo "────────────────────────────────────────────────────────────────"
  echo "$name"
  echo "────────────────────────────────────────────────────────────────"
  echo "$description"
  echo ""

  while true; do
    read -p "Score (1-5): " score
    if [[ "$score" =~ ^[1-5]$ ]]; then
      break
    fi
    echo "Please enter 1-5"
  done

  read -p "Rationale (brief): " rationale
  echo ""

  echo "{\"score\": $score, \"rationale\": \"$rationale\"}"
}

echo ""
spec_fidelity=$(score_dimension "Spec Fidelity" \
  "Does it implement all 3 endpoints? Match acceptance criteria?")

type_design=$(score_dimension "Type Design" \
  "Newtypes for ShortCode/Url? Good record design? Smart constructors?")

test_quality=$(score_dimension "Test Quality" \
  "Covers acceptance criteria? Property-based where sensible? Edge cases?")

impl_quality=$(score_dimension "Implementation Quality" \
  "No stubs/undefined? Handles errors? Clean code?")

coherence=$(score_dimension "Coherence" \
  "Do types/tests/impl fit together? Tests exercise actual API?")

idiomaticity=$(score_dimension "Idiomaticity" \
  "Idiomatic Haskell? Proper patterns? No anti-patterns?")

# Calculate aggregate
calc_aggregate() {
  local sf=$(echo "$spec_fidelity" | jq '.score')
  local td=$(echo "$type_design" | jq '.score')
  local tq=$(echo "$test_quality" | jq '.score')
  local iq=$(echo "$impl_quality" | jq '.score')
  local co=$(echo "$coherence" | jq '.score')
  local id=$(echo "$idiomaticity" | jq '.score')

  # Weights: sf=25%, td=20%, tq=15%, iq=15%, co=15%, id=10%
  echo "scale=2; ($sf * 0.25 + $td * 0.20 + $tq * 0.15 + $iq * 0.15 + $co * 0.15 + $id * 0.10)" | bc
}

aggregate=$(calc_aggregate)

# Save scores
cat > "$RUN_DIR/quality-scores.json" << EOF
{
  "spec_fidelity": $spec_fidelity,
  "type_design": $type_design,
  "test_quality": $test_quality,
  "impl_quality": $impl_quality,
  "coherence": $coherence,
  "idiomaticity": $idiomaticity,
  "aggregate": $aggregate,
  "scored_at": "$(date -Iseconds)"
}
EOF

echo ""
echo "════════════════════════════════════════════════════════════════"
echo "Scores Saved"
echo "════════════════════════════════════════════════════════════════"
echo ""
echo "Aggregate Score: $aggregate / 5.0"
echo ""
echo "Saved to: $RUN_DIR/quality-scores.json"
echo ""
cat "$RUN_DIR/quality-scores.json"
