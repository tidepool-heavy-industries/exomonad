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
    cargo test --workspace

# Run fast tests only (Rust unit tests)
test-fast:
    cargo test --workspace --lib

# Verify everything builds and passes
verify:
    #!/usr/bin/env bash
    set -euo pipefail
    echo ">>> [1/2] Rust unit tests..."
    cargo test --workspace --lib
    echo ">>> [2/2] Rust check (all targets)..."
    cargo check --workspace --all-targets
    echo ">>> All checks passed."

# Pre-push checks (formatting + verify)
pre-push: fmt-check verify

# Install git hooks (symlinks scripts/hooks/* to .git/hooks/)
install-hooks:
    @echo "Installing git hooks..."
    @ln -sf ../../scripts/hooks/pre-push .git/hooks/pre-push
    @echo "Installed: pre-push"
    @echo "Done. Use 'git push --no-verify' to bypass in emergencies."

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

    echo ">>> [1/3] Building Rust binary (${LABEL})..."
    cargo build ${CARGO_FLAGS} -p exomonad

    echo ">>> [2/3] Building Zellij plugin (wasm32-wasip1, ${LABEL})..."
    cd rust/exomonad-plugin && cargo build ${CARGO_FLAGS} --target wasm32-wasip1
    cd ../..

    echo ">>> [3/3] Installing binaries..."
    mkdir -p ~/.cargo/bin
    mkdir -p ~/.config/zellij/plugins
    cp "target/${TARGET_DIR}/exomonad" ~/.cargo/bin/
    cp "rust/exomonad-plugin/target/wasm32-wasip1/${TARGET_DIR}/exomonad-plugin.wasm" ~/.config/zellij/plugins/

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

# Install everything: Rust binaries + Zellij plugin (release build)
install-all: (_install "release")

# Install everything (fast dev build)
install-all-dev: (_install "dev")

# Run MCP integration tests (starts server, runs tests, cleans up)
test-mcp *args:
    ./scripts/test-mcp-integration.sh {{args}}

# Validate Gemini settings against schema
validate-settings:
    nix-shell -p python3Packages.jsonschema --run "python3 scripts/validate_json.py .gemini/settings.json schema/gemini-cli/settings.schema.json"
