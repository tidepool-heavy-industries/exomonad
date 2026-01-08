#!/usr/bin/env bash
# Run types-first-dev baseline in a fresh test repo
#
# Usage:
#   ./scripts/run-baseline.sh              # Uses default stack spec
#   ./scripts/run-baseline.sh url-shortener # Uses url-shortener spec
#
# Requires:
#   - Zellij session named "types-first-dev" (or set ZELLIJ_SESSION)
#   - cabal build types-first-dev already done

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TYPES_FIRST_DEV_DIR="$(dirname "$SCRIPT_DIR")"

SPEC="${1:-stack}"
SESSION="${ZELLIJ_SESSION:-types-first-dev}"
TIMESTAMP=$(date +%Y%m%d-%H%M%S)
TEST_REPO="/tmp/test-repo-${TIMESTAMP}"

echo "=== Creating test repo at ${TEST_REPO} ==="

mkdir -p "${TEST_REPO}"
cd "${TEST_REPO}"

# Initialize git repo
git init
git config user.email "test@example.com"
git config user.name "Test User"

# Create minimal cabal project
cat > test-repo.cabal << 'EOF'
cabal-version: 3.0
name:          test-repo
version:       0.1.0.0
build-type:    Simple

library
    exposed-modules: Lib
    build-depends:
        base >= 4.17 && < 5,
        QuickCheck >= 2.14 && < 3,
        text >= 2.0 && < 3
    hs-source-dirs: src
    default-language: GHC2021
    default-extensions:
        OverloadedStrings
    ghc-options: -Wall

test-suite test-repo-test
    type: exitcode-stdio-1.0
    main-is: Main.hs
    build-depends:
        base >= 4.17 && < 5,
        test-repo,
        QuickCheck >= 2.14 && < 3,
        hspec >= 2.10 && < 3
    hs-source-dirs: test
    default-language: GHC2021
    ghc-options: -threaded -rtsopts -with-rtsopts=-N
EOF

# Create placeholder lib
mkdir -p src
cat > src/Lib.hs << 'EOF'
module Lib where

-- Placeholder - will be replaced by types-first-dev
placeholder :: ()
placeholder = ()
EOF

# Create placeholder test
mkdir -p test
cat > test/Main.hs << 'EOF'
module Main where

main :: IO ()
main = putStrLn "Placeholder - will be replaced by types-first-dev"
EOF

# Initial commit
git add -A
git commit -m "Initial test repo setup"

echo ""
echo "=== Test repo created at ${TEST_REPO} ==="
echo ""
echo "=== Running baseline with spec: ${SPEC} ==="
echo ""

# Run the baseline
cd "${TYPES_FIRST_DEV_DIR}"
PROJECT_PATH="${TEST_REPO}" ZELLIJ_SESSION="${SESSION}" cabal run types-first-dev -- baseline --spec "${SPEC}"

echo ""
echo "=== Done ==="
echo "Test repo: ${TEST_REPO}"
echo "Results: ~/tidepool-labs/dev-runs/baseline/"
