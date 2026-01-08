#!/usr/bin/env bash
set -e

# Usage: ./run-experiment.sh url-shortener
EXPERIMENT="${1:-url-shortener}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EXPERIMENT_DIR="$SCRIPT_DIR/experiments/$EXPERIMENT"

if [ ! -d "$EXPERIMENT_DIR" ]; then
  echo "ERROR: Experiment '$EXPERIMENT' not found at $EXPERIMENT_DIR"
  echo "Available experiments:"
  ls -1 "$SCRIPT_DIR/experiments/"
  exit 1
fi

# Create fresh experiment directory in /tmp
TEST_DIR="/tmp/types-first-$EXPERIMENT-$$"
LOG_FILE="$TEST_DIR/workflow.log"

echo "════════════════════════════════════════════════════════════════════════════"
echo "Types-First Development Experiment: $EXPERIMENT"
echo "════════════════════════════════════════════════════════════════════════════"
echo ""
echo "Test dir: $TEST_DIR"
echo "Log file: $LOG_FILE"
echo "Experiment: $EXPERIMENT_DIR"
echo ""

mkdir -p "$TEST_DIR"/{src,test}

# Initialize log file
echo "=== Types-First Experiment Log ===" > "$LOG_FILE"
echo "Experiment: $EXPERIMENT" >> "$LOG_FILE"
echo "Started: $(date)" >> "$LOG_FILE"
echo "Test dir: $TEST_DIR" >> "$LOG_FILE"
echo "" >> "$LOG_FILE"

# Copy template files (cabal file, etc.) if they exist
if [ -d "$EXPERIMENT_DIR/template" ]; then
  echo "Copying template files from $EXPERIMENT_DIR/template..."
  cp -r "$EXPERIMENT_DIR/template/"* "$TEST_DIR/"
fi

# Initialize git repo with 'main' as default branch
echo "Initializing git repository..."
( cd "$TEST_DIR" && git init -b main && git add . && git commit -m "Initial project from template" )

echo ""
echo "Running types-first-dev workflow..."
echo "Project path: $TEST_DIR"
echo ""

# Build the project first if needed
echo "Building types-first-dev..."
cabal build exe:types-first-dev 2>&1 | tee -a "$LOG_FILE"

# Run the workflow with experiment-specific spec
# Environment variables:
#   PROJECT_PATH: Path to the test project
#   PROJECT_TYPE: Type of project (ServantServer, PureLibrary, etc.)
#   MODULE_NAME: Module name to generate
#   ZELLIJ_SESSION: Zellij session for Claude Code
PROJECT_PATH="$TEST_DIR" \
  PROJECT_TYPE="ServantServer" \
  MODULE_NAME="UrlShortener" \
  ZELLIJ_SESSION="${ZELLIJ_SESSION:-types-first-$EXPERIMENT}" \
  TYPES_FIRST_LOG_FILE="$LOG_FILE" \
  cabal run exe:types-first-dev -- --experiment "$EXPERIMENT" 2>&1 | tee -a "$LOG_FILE"

echo ""
echo "════════════════════════════════════════════════════════════════════════════"
echo "Results in: $TEST_DIR"
echo "════════════════════════════════════════════════════════════════════════════"
echo ""

# Show git history
echo "=== Git History ==="
( cd "$TEST_DIR" && git log --oneline --all --graph ) || true
echo ""

# Show what files exist
echo "=== Generated Files ==="
find "$TEST_DIR/src" -name "*.hs" -type f 2>/dev/null | while read -r f; do
  echo "$f:"
  head -30 "$f"
  echo "..."
  echo ""
done

find "$TEST_DIR/test" -name "*.hs" -type f 2>/dev/null | while read -r f; do
  echo "$f:"
  head -30 "$f"
  echo "..."
  echo ""
done

# Try to build
echo "=== Build ==="
( cd "$TEST_DIR" && cabal build all 2>&1 ) | tee -a "$LOG_FILE" || echo "Build failed"

# Try to run tests
echo "=== Tests ==="
( cd "$TEST_DIR" && cabal test 2>&1 ) | tee -a "$LOG_FILE" || echo "Tests failed"

echo ""
echo "════════════════════════════════════════════════════════════════════════════"
echo "COMPLETE"
echo "════════════════════════════════════════════════════════════════════════════"
echo ""
echo "Full log saved to: $LOG_FILE"
echo "Test directory: $TEST_DIR"
echo ""
echo "To review:"
echo "  cat $LOG_FILE"
echo "  ls -la $TEST_DIR/src/"
echo ""
