# Tidepool Justfile
# Run `just` to see available recipes

set shell := ["bash", "-uc"]

# Default: show available recipes
default:
    @just --list

# ─────────────────────────────────────────────────────────────
# Building
# ─────────────────────────────────────────────────────────────

# Build all packages
build:
    cabal build all

# Build with warnings as errors
build-strict:
    cabal build all --ghc-options="-Werror"

# Clean build artifacts
clean:
    cabal clean

# ─────────────────────────────────────────────────────────────
# Testing
# ─────────────────────────────────────────────────────────────

# Run all tests
test:
    cabal test all

# Run graph validation tests only
test-graph:
    cabal test graph-validation-tests

# Run cross-language protocol conformance tests (Haskell → TypeScript)
test-protocol:
    @echo "── Generating fixtures from Haskell ──"
    cabal run generate-fixtures
    @echo ""
    @echo "── Running TypeScript conformance tests ──"
    cd deploy && pnpm test

# ─────────────────────────────────────────────────────────────
# Pre-commit checks
# ─────────────────────────────────────────────────────────────

# Fast pre-commit check (just build)
pre-commit-fast:
    @echo "── Building all packages ──"
    cabal build all

# Full pre-commit check (build + tests)
pre-commit:
    @echo "── Building all packages ──"
    cabal build all
    @echo ""
    @echo "── Running tests ──"
    cabal test all

# ─────────────────────────────────────────────────────────────
# Running
# ─────────────────────────────────────────────────────────────

# Run DM CLI
dm:
    cabal run tidepool-dm

# Run DM GUI
dm-gui:
    cabal run tidepool-dm-gui

# Run Tidying GUI
tidy-gui:
    cabal run tidepool-tidy-gui

# ─────────────────────────────────────────────────────────────
# Git hooks
# ─────────────────────────────────────────────────────────────

# Install git hooks
install-hooks:
    @echo "Installing git hooks..."
    @mkdir -p .git/hooks
    @echo '#!/bin/bash' > .git/hooks/pre-commit
    @echo 'just pre-commit' >> .git/hooks/pre-commit
    @chmod +x .git/hooks/pre-commit
    @echo "✓ Installed pre-commit hook (runs 'just pre-commit' - build + tests)"

# Uninstall git hooks
uninstall-hooks:
    @rm -f .git/hooks/pre-commit
    @echo "✓ Removed pre-commit hook"
