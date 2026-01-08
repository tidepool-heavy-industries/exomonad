#!/usr/bin/env bash
set -e

# Create fresh test repo in /tmp
TEST_DIR="/tmp/types-first-test-$$"
LOG_FILE="$TEST_DIR/workflow.log"

echo "════════════════════════════════════════════════════════════════════════════"
echo "Types-First Development Workflow Test"
echo "════════════════════════════════════════════════════════════════════════════"
echo ""
echo "Test dir: $TEST_DIR"
echo "Log file: $LOG_FILE"
echo ""

mkdir -p "$TEST_DIR"/{src,test}

# Initialize log file
echo "=== Types-First Workflow Log ===" > "$LOG_FILE"
echo "Started: $(date)" >> "$LOG_FILE"
echo "Test dir: $TEST_DIR" >> "$LOG_FILE"
echo "" >> "$LOG_FILE"

cat > "$TEST_DIR/test-repo.cabal" << 'EOF'
cabal-version: 3.0
name:          test-repo
version:       0.1.0.0
build-type:    Simple

library
    exposed-modules:  Data.Stack
    hs-source-dirs:   src
    default-language: GHC2021
    build-depends:
        base >= 4.17 && < 5

test-suite test-repo-test
    type:             exitcode-stdio-1.0
    main-is:          Main.hs
    hs-source-dirs:   test
    default-language: GHC2021
    build-depends:
        base >= 4.17 && < 5,
        QuickCheck >= 2.14,
        test-repo
EOF

# Initialize git repo with 'main' as default branch (worktree executor expects 'main')
( cd "$TEST_DIR" && git init -b main && git add . && git commit -m "Initial cabal project" )

echo ""
echo "Running types-first-dev workflow..."
echo "Test repo: $TEST_DIR"
echo ""

# Run the workflow with logging enabled
# Logs go to both stdout and LOG_FILE via TYPES_FIRST_LOG_FILE env var
# Also tee stdout to capture everything
PROJECT_PATH="$TEST_DIR" \
  ZELLIJ_SESSION="${ZELLIJ_SESSION:-types-first-dev}" \
  TYPES_FIRST_LOG_FILE="$LOG_FILE" \
  cabal run exe:types-first-dev 2>&1 | tee -a "$LOG_FILE"

echo ""
echo "════════════════════════════════════════════════════════════════════════════"
echo "Results in: $TEST_DIR"
echo "════════════════════════════════════════════════════════════════════════════"
echo ""

# Show git history to verify agent commits
echo "=== Git History ==="
( cd "$TEST_DIR" && git log --oneline --all --graph )
echo ""

# Show what files exist
echo "=== Generated Files ==="
ls -la "$TEST_DIR/src/Data/" 2>/dev/null || echo "(no src/Data/ dir)"
ls -la "$TEST_DIR/test/" 2>/dev/null || echo "(no test/ dir)"
echo ""

# Show implementation if it exists
if [ -f "$TEST_DIR/src/Data/Stack.hs" ]; then
  echo "=== Implementation (src/Data/Stack.hs) ==="
  head -60 "$TEST_DIR/src/Data/Stack.hs"
  echo ""
fi

# Show tests if they exist
if [ -f "$TEST_DIR/test/Main.hs" ]; then
  echo "=== Tests (test/Main.hs) ==="
  head -60 "$TEST_DIR/test/Main.hs"
  echo ""
fi

# Try to build and test
echo "=== Build & Test ==="
( cd "$TEST_DIR" && cabal build all 2>&1 ) | tee -a "$LOG_FILE" || echo "Build failed"
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
echo "  ls -la $TEST_DIR"
echo ""
