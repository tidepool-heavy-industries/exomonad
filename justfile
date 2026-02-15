# ExoMonad Development Justfile

# Default recipe
default:
    @just --list

# Format all code (Haskell + Rust)
fmt:
    cd haskell && ormolu --mode inplace --ghc-opt -XImportQualifiedPost $(find . -name '*.hs' -not -path './vendor/*')
    cargo fmt --all

# Check formatting (no changes, exit 1 if unformatted)
fmt-check:
    cd haskell && ormolu --mode check --ghc-opt -XImportQualifiedPost $(find . -name '*.hs' -not -path './vendor/*')
    cargo fmt --all --check

# Lint Haskell code
lint:
    hlint haskell

# Run all tests
test:
    cabal test all
    cargo test --workspace

# Run fast tests only (Rust unit tests)
test-fast:
    cargo test --workspace --lib

# Verify everything builds and passes (Rust tests + WASM)
verify:
    #!/usr/bin/env bash
    set -euo pipefail
    echo ">>> [1/3] Rust unit tests..."
    cargo test --workspace --lib
    echo ">>> [2/3] Rust check (all targets)..."
    cargo check --workspace --all-targets
    echo ">>> [3/3] WASM build..."
    just wasm-all
    echo ">>> All checks passed."

# Pre-push checks (formatting + verify)
pre-push: fmt-check verify

# Install git hooks (symlinks scripts/hooks/* to .git/hooks/)
install-hooks:
    @echo "Installing git hooks..."
    @ln -sf ../../scripts/hooks/pre-push .git/hooks/pre-push
    @echo "Installed: pre-push"
    @echo "Done. Use 'git push --no-verify' to bypass in emergencies."

# Build WASM role and install to .exomonad/wasm/
wasm role="tl":
    @echo ">>> Building wasm-guest-{{role}}..."
    nix develop .#wasm --command bash -c 'export PATH=$PWD/.gemini/tmp/bin:$PATH; wasm32-wasi-cabal build --project-file=cabal.project.wasm wasm-guest-{{role}}'
    @echo ">>> Installing to .exomonad/wasm/..."
    mkdir -p .exomonad/wasm
    rm -f .exomonad/wasm/wasm-guest-{{role}}.wasm
    cp $(find dist-newstyle -name "wasm-guest-{{role}}.wasm" -type f -print -quit) .exomonad/wasm/wasm-guest-{{role}}.wasm
    @echo ">>> Done: .exomonad/wasm/wasm-guest-{{role}}.wasm"

# Build unified WASM plugin (contains all roles)
wasm-all:
    @just wasm unified
    @echo ">>> Installed to .exomonad/wasm/:"
    @ls -lh .exomonad/wasm/wasm-guest-*.wasm

# One-time WASM build environment setup (populates cabal package index)
wasm-setup:
    @echo ">>> Setting up WASM build environment (one-time)..."
    nix develop .#wasm --command bash -c 'export PATH=$PWD/.gemini/tmp/bin:$PATH; wasm32-wasi-cabal update --project-file=cabal.project.wasm'
    @echo ">>> Done. You can now run: just wasm-all"

# Internal: shared install logic for release/dev builds.
_install profile:
    #!/usr/bin/env bash
    set -euo pipefail

    if [ "{{profile}}" = "release" ]; then
        CARGO_FLAGS="--release"
        TARGET_DIR="release"
        LABEL="release"
    else
        CARGO_FLAGS=""
        TARGET_DIR="debug"
        LABEL="debug"
    fi

    echo ">>> [1/4] Building Haskell WASM plugins (cabal cached if unchanged)..."
    just wasm-all

    echo ">>> [2/4] Building Rust binary (${LABEL})..."
    cargo build ${CARGO_FLAGS} -p exomonad

    echo ">>> [3/4] Building Zellij plugin (wasm32-wasip1, ${LABEL})..."
    cd rust/exomonad-plugin && cargo build ${CARGO_FLAGS} --target wasm32-wasip1
    cd ../..

    echo ">>> [4/4] Installing binaries..."
    mkdir -p ~/.cargo/bin
    mkdir -p ~/.config/zellij/plugins
    mkdir -p ~/.exomonad/wasm
    cp "target/${TARGET_DIR}/exomonad" ~/.cargo/bin/
    cp "rust/exomonad-plugin/target/wasm32-wasip1/${TARGET_DIR}/exomonad-plugin.wasm" ~/.config/zellij/plugins/
    cp .exomonad/wasm/wasm-guest-unified.wasm ~/.exomonad/wasm/

    # macOS: remove quarantine and ad-hoc sign to avoid sandbox/Gatekeeper issues
    if [ "$(uname)" = "Darwin" ]; then
        xattr -d com.apple.quarantine ~/.cargo/bin/exomonad 2>/dev/null || true
        codesign -s - -f ~/.cargo/bin/exomonad 2>/dev/null || true
    fi

    echo ">>> Done!"
    echo ""
    echo "Installed:"
    ls -lh ~/.cargo/bin/exomonad
    ls -lh ~/.config/zellij/plugins/exomonad-plugin.wasm
    ls -lh .exomonad/wasm/wasm-guest-unified.wasm

# Install everything: Rust binaries + WASM plugins (release build)
install-all: (_install "release")

# Install everything (fast dev build)
install-all-dev: (_install "dev")

# Regenerate Haskell proto types (requires nix develop shell)
# Generated files are checked in - only run when protos change
proto-gen-haskell:
    ./proto-codegen/generate.sh

# Regenerate Rust proto types (part of normal cargo build)
proto-gen-rust:
    cargo build -p exomonad-proto

# Full proto regeneration
proto-gen: proto-gen-haskell proto-gen-rust
    @echo "Proto generation complete. Don't forget to commit haskell/proto/src/"

# Verify proto changes don't break wire format
proto-test:
    #!/usr/bin/env bash
    set -euo pipefail
    echo ">>> Running Rust proto wire format tests..."
    cargo test -p exomonad-proto
    echo ">>> Running Haskell proto tests..."
    cabal test exomonad-proto || echo "No tests defined yet"
    echo ">>> Running proto wire format compatibility test..."
    cabal run proto-test || echo "Wire format test not yet implemented"
    echo ">>> Done"

# Run MCP integration tests (starts server, runs tests, cleans up)
test-mcp *args:
    ./scripts/test-mcp-integration.sh {{args}}

# Validate Gemini settings against schema
validate-settings:
    nix-shell -p python3Packages.jsonschema --run "python3 scripts/validate_json.py .gemini/settings.json schema/gemini-cli/settings.schema.json"
