#!/usr/bin/env bash
# Run url-shortener baseline with full evaluation capture
#
# Usage:
#   ./scripts/run-baseline.sh              # Run with auto-generated name
#   ./scripts/run-baseline.sh my-run       # Run with custom name
#
# Results saved to: ~/tidepool-labs/dev-runs/baseline/<run-name>/
#
# Requires:
#   - Zellij session (set ZELLIJ_SESSION or default: types-first-baseline)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
BRANCH_NAME=$(git -C "$PROJECT_DIR" rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
RUN_NAME="${1:-run-$(date +%Y%m%d-%H%M%S)}"

# Output directories
BASELINE_DIR="$HOME/tidepool-labs/dev-runs/baseline"
RUN_DIR="$BASELINE_DIR/$RUN_NAME"
TEST_REPO="/tmp/baseline-$RUN_NAME"

echo "════════════════════════════════════════════════════════════════"
echo "URL Shortener Baseline Run"
echo "════════════════════════════════════════════════════════════════"
echo ""
echo "Run:      $RUN_NAME"
echo "Branch:   $BRANCH_NAME"
echo "Output:   $RUN_DIR"
echo "Project:  $TEST_REPO"
echo ""

# Setup directories
mkdir -p "$RUN_DIR/artifacts"
mkdir -p "$TEST_REPO"

# Copy url-shortener template if exists
if [ -d "$PROJECT_DIR/experiments/url-shortener/template" ]; then
  cp -r "$PROJECT_DIR/experiments/url-shortener/template/"* "$TEST_REPO/"
fi

# Initialize git repo
cd "$TEST_REPO"
git init -b main
git config user.email "baseline@test.local"
git config user.name "Baseline Runner"

# Minimal cabal file for ServantServer
cat > url-shortener.cabal << 'EOF'
cabal-version: 3.0
name:          url-shortener
version:       0.1.0.0
build-type:    Simple

library
    exposed-modules: UrlShortener
    build-depends:
        base >= 4.17 && < 5,
        text >= 2.0 && < 3,
        bytestring >= 0.11 && < 1,
        aeson >= 2.1 && < 3,
        containers >= 0.6 && < 1,
        mtl >= 2.2 && < 3,
        hashable >= 1.4 && < 2,
        servant >= 0.20 && < 1,
        servant-server >= 0.20 && < 1,
        servant-client >= 0.20 && < 1,
        wai >= 3.2 && < 4,
        warp >= 3.3 && < 4,
        http-client >= 0.7 && < 1,
        http-types >= 0.12 && < 1
    hs-source-dirs: src
    default-language: GHC2024
    default-extensions:
        DataKinds
        DeriveGeneric
        OverloadedStrings
        TypeOperators
    ghc-options: -Wall

test-suite url-shortener-test
    type: exitcode-stdio-1.0
    main-is: Main.hs
    build-depends:
        base >= 4.17 && < 5,
        url-shortener,
        text >= 2.0 && < 3,
        bytestring >= 0.11 && < 1,
        aeson >= 2.1 && < 3,
        containers >= 0.6 && < 1,
        hspec >= 2.10 && < 3,
        hspec-wai >= 0.11 && < 1,
        QuickCheck >= 2.14 && < 3,
        servant >= 0.20 && < 1,
        servant-server >= 0.20 && < 1,
        servant-client >= 0.20 && < 1,
        wai >= 3.2 && < 4,
        warp >= 3.3 && < 4,
        http-client >= 0.7 && < 1,
        http-types >= 0.12 && < 1
    hs-source-dirs: test
    default-language: GHC2024
    default-extensions:
        OverloadedStrings
    ghc-options: -threaded -rtsopts -with-rtsopts=-N
EOF

# Create placeholder source
mkdir -p src test
cat > src/UrlShortener.hs << 'EOF'
module UrlShortener where

-- Placeholder - types-first-dev will replace this
placeholder :: ()
placeholder = ()
EOF

cat > test/Main.hs << 'EOF'
module Main where

main :: IO ()
main = putStrLn "Placeholder test"
EOF

git add -A
git commit -m "Initial project setup"

# Record run metadata
cat > "$RUN_DIR/run-info.json" << EOF
{
  "run_name": "$RUN_NAME",
  "branch": "$BRANCH_NAME",
  "started": "$(date -Iseconds)",
  "experiment": "url-shortener",
  "test_repo": "$TEST_REPO"
}
EOF

# Zellij session check
SESSION="${ZELLIJ_SESSION:-types-first-baseline}"
echo "Zellij session: $SESSION"

if ! zellij list-sessions 2>/dev/null | grep -q "$SESSION"; then
  echo ""
  echo "ERROR: Zellij session '$SESSION' not found."
  echo ""
  echo "Create it with:"
  echo "  zellij --session $SESSION"
  echo ""
  exit 1
fi

# Build
echo ""
echo "Building types-first-dev..."
cd "$PROJECT_DIR"
cabal build exe:types-first-dev 2>&1 | tee "$RUN_DIR/build.log"

# Run workflow
echo ""
echo "════════════════════════════════════════════════════════════════"
echo "Running Workflow"
echo "════════════════════════════════════════════════════════════════"
echo ""

START_TIME=$(date +%s)

PROJECT_PATH="$TEST_REPO" \
  ZELLIJ_SESSION="$SESSION" \
  cabal run exe:types-first-dev -- --experiment url-shortener 2>&1 | tee "$RUN_DIR/workflow.log"

WORKFLOW_EXIT=$?
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# Capture artifacts
echo ""
echo "Capturing artifacts..."
cp -r "$TEST_REPO/src" "$RUN_DIR/artifacts/" 2>/dev/null || true
cp -r "$TEST_REPO/test" "$RUN_DIR/artifacts/" 2>/dev/null || true
git -C "$TEST_REPO" log --oneline --all --graph > "$RUN_DIR/artifacts/git-log.txt" 2>/dev/null || true
git -C "$TEST_REPO" diff HEAD~3..HEAD > "$RUN_DIR/artifacts/git-diff.txt" 2>/dev/null || true

# Final validation
cd "$TEST_REPO"
BUILD_OK="false"
TESTS_OK="false"

if cabal build all > "$RUN_DIR/final-build.log" 2>&1; then
  BUILD_OK="true"
fi

if cabal test > "$RUN_DIR/final-test.log" 2>&1; then
  TESTS_OK="true"
fi

# Count endpoints (heuristic)
ENDPOINT_COUNT=0
if [ -f "$TEST_REPO/src/UrlShortener.hs" ]; then
  # Count :<|> operators + 1 for first endpoint
  SEP_COUNT=$(grep -o ":<|>" "$TEST_REPO/src/UrlShortener.hs" 2>/dev/null | wc -l || echo "0")
  if [ "$SEP_COUNT" -gt 0 ]; then
    ENDPOINT_COUNT=$((SEP_COUNT + 1))
  else
    # Maybe single endpoint or different structure
    ENDPOINT_COUNT=$(grep -cE "type.*API|:[A-Z].*:>" "$TEST_REPO/src/UrlShortener.hs" 2>/dev/null || echo "0")
  fi
fi

# Generate scores
cat > "$RUN_DIR/automated-scores.json" << EOF
{
  "compiles": $BUILD_OK,
  "tests_pass": $TESTS_OK,
  "endpoint_count": $ENDPOINT_COUNT,
  "duration_seconds": $DURATION,
  "workflow_exit": $WORKFLOW_EXIT,
  "branch": "$BRANCH_NAME",
  "completed": "$(date -Iseconds)"
}
EOF

# Summary
echo ""
echo "════════════════════════════════════════════════════════════════"
echo "Results"
echo "════════════════════════════════════════════════════════════════"
echo ""
echo "Build:      $([ "$BUILD_OK" = "true" ] && echo "PASS" || echo "FAIL")"
echo "Tests:      $([ "$TESTS_OK" = "true" ] && echo "PASS" || echo "FAIL")"
echo "Endpoints:  $ENDPOINT_COUNT/3"
echo "Duration:   ${DURATION}s"
echo ""
echo "Artifacts:  $RUN_DIR/artifacts/"
echo ""
echo "To review generated code:"
echo "  cat $RUN_DIR/artifacts/src/UrlShortener.hs"
echo "  cat $RUN_DIR/artifacts/test/Main.hs"
echo ""
echo "════════════════════════════════════════════════════════════════"
