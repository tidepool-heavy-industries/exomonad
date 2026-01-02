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

# Verify protocol conformance between Haskell and TypeScript
# Requires: node/npm installed in deploy/
test-protocol-conformance:
    @echo "── Generating JSON samples from Haskell ──"
    cabal run generate-golden-samples
    @echo ""
    @echo "── Verifying TypeScript can parse samples ──"
    cd deploy && npx tsx test/verify-protocol.ts

# ─────────────────────────────────────────────────────────────
# Linting
# ─────────────────────────────────────────────────────────────

# Run hlint on Haskell sources (errors only, not suggestions)
lint-hs:
    @echo "── Running hlint ──"
    hlint tidepool-core tidepool-dm tidepool-tidying tidepool-platform tidepool-wasm --ignore-suggestions

# Run ESLint on TypeScript sources
lint-ts:
    @echo "── Running pnpm lint ──"
    cd deploy && pnpm lint

# Run pnpm typecheck on TypeScript sources
typecheck-ts:
    @echo "── Running pnpm typecheck ──"
    cd deploy && pnpm typecheck

# Run TypeScript tests
test-ts:
    @echo "── Running pnpm test ──"
    cd deploy && pnpm test

# Run all lints
lint: lint-hs lint-ts

# ─────────────────────────────────────────────────────────────
# Pre-commit checks
# ─────────────────────────────────────────────────────────────

# Fast pre-commit check (build + lint)
pre-commit-fast: build lint

# Full pre-commit check (build + lint + tests)
pre-commit: build lint test test-ts

# ─────────────────────────────────────────────────────────────
# WASM Roundtrip Testing
# ─────────────────────────────────────────────────────────────

# Build roundtrip WASM (requires: nix develop .#wasm)
build-roundtrip-wasm:
    @echo "── Building roundtrip WASM ──"
    @echo "Note: This must be run inside 'nix develop .#wasm'"
    wasm32-wasi-cabal build tidepool-wasm
    @echo ""
    @echo "── Copying WASM to deploy/ ──"
    find dist-newstyle -name '*.wasm' -exec cp {} deploy/tidepool-roundtrip.wasm \;
    @echo "✓ Built: deploy/tidepool-roundtrip.wasm"

# Run cross-boundary property tests (requires WASM built first)
test-roundtrip:
    @echo "── Running cross-boundary property tests ──"
    cd deploy && pnpm test -- --grep "roundtrip"

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
    @echo 'export PATH="$HOME/.local/bin:$HOME/.ghcup/bin:$PATH"' >> .git/hooks/pre-commit
    @echo 'exec just pre-commit' >> .git/hooks/pre-commit
    @chmod +x .git/hooks/pre-commit
    @echo "✓ Installed pre-commit hook (runs 'just pre-commit' - build + lint + tests)"

# Uninstall git hooks
uninstall-hooks:
    @rm -f .git/hooks/pre-commit
    @echo "✓ Removed pre-commit hook"

# ─────────────────────────────────────────────────────────────
# WASM & Deployment
# ─────────────────────────────────────────────────────────────

# Build WASM blob (requires nix develop .#wasm)
build-wasm:
    @echo "── Building tidepool-reactor with wasm32-wasi-ghc ──"
    nix develop .#wasm --command bash -c "wasm32-wasi-cabal build tidepool-reactor"
    @echo ""
    @echo "── Copying WASM to deploy/src/tidepool.wasm ──"
    cp "$(find dist-newstyle/build/wasm32-wasi -name 'tidepool-reactor.wasm' | head -1)" deploy/src/tidepool.wasm
    @ls -lh deploy/src/tidepool.wasm
    @echo "✓ WASM blob ready"

# Deploy to Cloudflare Workers
deploy-worker:
    @echo "── Deploying to Cloudflare Workers ──"
    cd deploy && pnpm run deploy
    @echo "✓ Deployed"

# Build WASM and deploy to Cloudflare (full pipeline)
deploy: build-wasm deploy-worker
    @echo ""
    @echo "✓ Full deploy complete"
