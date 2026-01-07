#!/usr/bin/env bash
set -euo pipefail

# E2E test for types-first-dev parallel agent workflow
# Creates a test repo and runs the workflow

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_REPO="$SCRIPT_DIR/test-repo"

echo "=== Types-First Dev E2E Test ==="
echo ""

# Clean up any previous test repo
if [ -d "$TEST_REPO" ]; then
  echo "Cleaning up previous test repo..."
  rm -rf "$TEST_REPO"
fi

# Create test repo
echo "Creating test repo at $TEST_REPO..."
mkdir -p "$TEST_REPO/src"

cd "$TEST_REPO"
git init
git config user.email "test@example.com"
git config user.name "Test User"

# Create minimal cabal file
cat > test-repo.cabal << 'EOF'
cabal-version: 3.0
name:          test-repo
version:       0.1.0.0
build-type:    Simple

library
    exposed-modules: Data.Stack
    hs-source-dirs:  src
    build-depends:   base >= 4.17 && < 5
    default-language: GHC2021
    ghc-options: -Wall

test-suite test-repo-test
    type:             exitcode-stdio-1.0
    main-is:          Main.hs
    hs-source-dirs:   test
    build-depends:
        base >= 4.17 && < 5,
        test-repo,
        QuickCheck >= 2.14 && < 3
    default-language: GHC2021
EOF

# Create placeholder module (will be replaced by impl agent)
mkdir -p src/Data
cat > src/Data/Stack.hs << 'EOF'
-- Placeholder - will be replaced by types-first-dev
module Data.Stack where
EOF

# Create placeholder test dir
mkdir -p test
cat > test/Main.hs << 'EOF'
-- Placeholder - will be replaced by types-first-dev
module Main where
main :: IO ()
main = putStrLn "No tests yet"
EOF

# Initial commit
git add -A
git commit -m "Initial commit"

echo ""
echo "Test repo created at: $TEST_REPO"
echo ""

# Go back to types-first-dev dir
cd "$SCRIPT_DIR"

# Check for zellij session
if [ -z "${ZELLIJ_SESSION_NAME:-}" ]; then
  echo "ERROR: Not running inside a zellij session"
  echo "Please run this from within zellij"
  exit 1
fi

echo "Using zellij session: $ZELLIJ_SESSION_NAME"
echo ""

# Build first
echo "Building types-first-dev..."
cabal build exe:types-first-dev

echo ""
echo "Running types-first-dev workflow..."
echo "==================================="
echo ""

# Run the workflow with the test repo
export ZELLIJ_SESSION="$ZELLIJ_SESSION_NAME"
export PROJECT_PATH="$TEST_REPO"

echo "Running: PROJECT_PATH=$TEST_REPO cabal run exe:types-first-dev"
echo ""

cabal run exe:types-first-dev 2>&1 || {
  echo ""
  echo "Workflow failed - check output above"
  exit 1
}

echo ""
echo "=== E2E Test Complete ==="
