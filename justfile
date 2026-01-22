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
    hlint haskell/dsl/core haskell/runtime/wasm --ignore-suggestions

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
    TIDEPOOL_DIST=tidepool-native-gui/solid-frontend/dist cabal run haskell/native-server:tidepool-native-server

# NOTE: dm, dm-gui, tidy-gui disabled - see cabal.project for details

# ─────────────────────────────────────────────────────────────
# Hangar Runtime
# ─────────────────────────────────────────────────────────────

# Discover Hangar root from current directory
_hangar-root:
    #!/usr/bin/env bash
    HANGAR_ROOT=$(pwd)
    while [ ! -f "$HANGAR_ROOT/Hangar.toml" ] && [ "$HANGAR_ROOT" != "/" ] && [ "$HANGAR_ROOT" != "$HOME" ]; do
        HANGAR_ROOT=$(dirname "$HANGAR_ROOT")
    done
    if [ ! -f "$HANGAR_ROOT/Hangar.toml" ]; then
        echo "ERROR: Hangar.toml not found" >&2
        exit 1
    fi
    echo "$HANGAR_ROOT"

# Rebuild runtime binaries from current repo
rebuild-runtime:
    #!/usr/bin/env bash
    set -euo pipefail

    # Find Hangar root for binary installation
    HANGAR_ROOT=$(just _hangar-root)
    RUNTIME_BIN="$HANGAR_ROOT/runtime/bin"

    # Build from current repo directory
    REPO_ROOT=$(pwd)

    echo "── Hangar: $HANGAR_ROOT ──"
    echo "── Building from current repo: $REPO_ROOT ──"

    echo ""
    echo "── Building Haskell (tidepool-control-server) ──"
    cabal build tidepool-control-server

    echo ""
    echo "── Building Rust (mantle-agent, tui-sidebar) ──"
    cd "$REPO_ROOT/rust" && cargo build --release -p mantle-agent -p tui-sidebar

    echo ""
    echo "── Installing to $RUNTIME_BIN ──"
    cd "$REPO_ROOT"
    cp "$(cabal list-bin tidepool-control-server)" "$RUNTIME_BIN/"
    cp "$REPO_ROOT/rust/target/release/mantle-agent" "$RUNTIME_BIN/"
    cp "$REPO_ROOT/rust/target/release/tui-sidebar" "$RUNTIME_BIN/"

    echo ""
    echo "── Code signing binaries (macOS) ──"
    codesign -s - --force "$RUNTIME_BIN/tidepool-control-server" 2>/dev/null || true
    codesign -s - --force "$RUNTIME_BIN/mantle-agent" 2>/dev/null || true
    codesign -s - --force "$RUNTIME_BIN/tui-sidebar" 2>/dev/null || true

    echo ""
    echo "✓ Runtime binaries rebuilt from current repo:"
    ls -lh "$RUNTIME_BIN"

# Rebuild runtime without signing (deprecated - use rebuild-runtime)
rebuild-runtime-local:
    @echo "NOTE: rebuild-runtime-local is deprecated. Use 'just rebuild-runtime' instead."
    @echo "      rebuild-runtime now builds from current repo by default."
    @just rebuild-runtime

# ─────────────────────────────────────────────────────────────
# Gemini
# ─────────────────────────────────────────────────────────────

# Setup Gemini environment (builds MCP server + symlink)
setup-gemini:
    @echo "── Building tidepool-mcp-server ──"
    cabal build tidepool-mcp-server
    @echo ""
    @echo "── Creating symlink at result/bin/mcp-server ──"
    @mkdir -p result/bin
    @BINARY_PATH=$(cabal list-bin tidepool-mcp-server) && ln -sf "$$BINARY_PATH" result/bin/mcp-server
    @echo "✓ Setup complete. Gemini configuration is ready."

# ─────────────────────────────────────────────────────────────
# Git hooks
# ─────────────────────────────────────────────────────────────

# Install git hooks
install-hooks:
    @echo "Installing git hooks..."
    @mkdir -p .git/hooks
    @echo '#!/bin/bash' > .git/hooks/pre-commit
    @echo 'export PATH="$HOME/.nix-profile/bin:$HOME/.local/bin:$HOME/.ghcup/bin:$PATH"' >> .git/hooks/pre-commit
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
# Analysis
# ─────────────────────────────────────────────────────────────

# Analyze blast radius of current branch vs base (default: main)
blast-radius base="main":
    @./tools/blast-radius.sh {{base}}

# ─────────────────────────────────────────────────────────────
# Observability
# ─────────────────────────────────────────────────────────────

# Tail Cloudflare Worker logs (pretty format)
logs:
    cd deploy && npx wrangler tail --format pretty

# Tail logs with JSON format (for parsing)
logs-json:
    cd deploy && npx wrangler tail --format json
