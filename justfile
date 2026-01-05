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
    hlint tidepool-core tidepool-wasm --ignore-suggestions

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
    wasm32-wasi-cabal build tidepool-reactor
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

# Run native server (REST + WebSocket + static files)
native:
    TIDEPOOL_DIST=tidepool-native-gui/solid-frontend/dist cabal run tidepool-native

# NOTE: dm, dm-gui, tidy-gui disabled - see cabal.project for details

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
    #!/usr/bin/env bash
    set -euo pipefail
    echo "── Building tidepool-reactor with wasm32-wasi-ghc ──"
    nix develop .#wasm --command bash -c ' \
        wasm32-wasi-cabal build tidepool-reactor && \
        echo "" && \
        echo "── Optimizing WASM with wasm-opt -Oz ──" && \
        WASM=$(find dist-newstyle/build/wasm32-wasi -name "tidepool-reactor.wasm" | head -1) && \
        ls -lh "$WASM" && \
        wasm-opt -Oz "$WASM" -o deploy/src/tidepool.wasm && \
        echo "  ↓ optimized to:" && \
        ls -lh deploy/src/tidepool.wasm \
    '
    echo "✓ WASM blob ready"

# Deploy to Cloudflare Workers
deploy-worker:
    @echo "── Deploying to Cloudflare Workers ──"
    cd deploy && pnpm run deploy
    @echo "✓ Deployed"

# Build WASM and deploy to Cloudflare (full pipeline)
deploy: build-wasm deploy-worker
    @echo ""
    @echo "✓ Full deploy complete"

# ─────────────────────────────────────────────────────────────
# Observability
# ─────────────────────────────────────────────────────────────

# Tail Cloudflare Worker logs (pretty format)
logs:
    cd deploy && npx wrangler tail --format pretty

# Tail logs with JSON format (for parsing)
logs-json:
    cd deploy && npx wrangler tail --format json
