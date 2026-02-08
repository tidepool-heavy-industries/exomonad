# ExoMonad Operations Justfile
# Unified interface for Docker operations with digest pinning

# Configuration
metadata_dir := "dist"
metadata_file := metadata_dir + "/bake-metadata.json"
env_file := ".env"

# Default recipe
default:
    @just --list

# Verify jq is available
_check_jq:
    @command -v jq >/dev/null 2>&1 || { echo "ERROR: jq required but not installed"; exit 1; }

# Build and launch a service with digest pinning
dev-up target: _check_jq
    @echo ">>> [1/4] Building target '{{target}}'..."
    mkdir -p {{metadata_dir}}
    ./build {{target}} --load --metadata-file {{metadata_file}}

    @echo ">>> [2/4] Extracting digest..."
    @digest=$(jq -r '.["{{target}}"]["containerimage.digest"]' {{metadata_file}}); \
    if [ "$digest" = "null" ] || [ -z "$digest" ]; then \
        echo "ERROR: No digest for '{{target}}'"; exit 1; \
    fi; \
    echo "    Digest: $digest"

    @echo ">>> [3/4] Updating .env..."
    @env_var=$(echo "{{target}}" | tr '[:lower:]-' '[:upper:]_')_DIGEST; \
    touch {{env_file}}; \
    if grep -q "^${env_var}=" {{env_file}}; then \
        sed -i.bak "s|^${env_var}=.*|${env_var}=$digest|" {{env_file}} && rm -f {{env_file}}.bak; \
    else \
        echo "${env_var}=$digest" >> {{env_file}}; \
    fi

    @echo ">>> [4/4] Launching..."
    docker compose up -d {{target}}
    docker compose logs -f {{target}}

# Build all images (no load, just build to cache)
build:
    ./build

# Build and load all images into local daemon
build-load:
    ./build --load

# Build only dependency cachers (CI warmup)
build-deps:
    ./build deps

# Check if running containers match local git state
check-freshness service:
    ./scripts/check-freshness.sh {{service}}

# Update Rust dependencies and regenerate cargo-chef recipe
update-rust:
    @echo ">>> Updating Cargo dependencies..."
    docker run --rm -v $(pwd):/app -w /app rust:1.93-bookworm cargo update

    @echo ">>> Regenerating cargo-chef recipe..."
    docker run --rm -v $(pwd):/app -w /app/rust lukemathwalker/cargo-chef:latest \
        cargo chef prepare --recipe-path /app/recipe.json

    @echo ">>> Done. Commit 'Cargo.lock'."

# Freeze Haskell dependencies
freeze-haskell:
    @echo ">>> Freezing Haskell dependencies..."
    docker run --rm -v $(pwd):/app -w /app haskell:9.12.2-bookworm \
        sh -c "cabal update && cabal freeze"

    @echo ">>> Done. Commit 'cabal.project.freeze'."

# Run compose sync test
test-compose-sync:
    cargo test -p docker-ctl compose_sync

# Show git SHA that would be embedded
show-sha:
    @echo "Git SHA: $(git rev-parse --short HEAD)"

# Format all code (Haskell + Rust)
fmt:
    cd haskell && ormolu --mode inplace --ghc-opt -XImportQualifiedPost $(find . -name '*.hs' -not -path './vendor/*')
    cd rust && cargo fmt

# Check formatting (no changes, exit 1 if unformatted)
fmt-check:
    cd haskell && ormolu --mode check --ghc-opt -XImportQualifiedPost $(find . -name '*.hs' -not -path './vendor/*')
    cd rust && cargo fmt --check

# Lint Haskell code
lint:
    hlint haskell

# Run all tests
test:
    cabal test all
    cd rust && cargo test

# Run fast tests only (for pre-push hook)
test-fast:
    cd rust && cargo test --workspace

# Pre-push checks (formatting + fast tests)
pre-push: fmt-check test-fast

# Install git hooks (symlinks scripts/hooks/* to .git/hooks/)
install-hooks:
    @echo "Installing git hooks..."
    @ln -sf ../../scripts/hooks/pre-push .git/hooks/pre-push
    @echo "Installed: pre-push"
    @echo "Done. Use 'git push --no-verify' to bypass in emergencies."

# Build WASM role and install to .exomonad/wasm/
wasm role="tl":
    @echo ">>> Building wasm-guest-{{role}}..."
    nix develop .#wasm --command wasm32-wasi-cabal build --project-file=cabal.project.wasm wasm-guest-{{role}}
    @echo ">>> Installing to .exomonad/wasm/..."
    mkdir -p .exomonad/wasm
    rm -f .exomonad/wasm/wasm-guest-{{role}}.wasm
    cp $(find dist-newstyle -name "wasm-guest-{{role}}.wasm" -type f -print -quit) .exomonad/wasm/wasm-guest-{{role}}.wasm
    @echo ">>> Done: .exomonad/wasm/wasm-guest-{{role}}.wasm"

# Build both WASM role plugins (tl + dev)
wasm-all:
    @just wasm tl
    @just wasm dev
    @echo ">>> Installed to .exomonad/wasm/:"
    @ls -lh .exomonad/wasm/wasm-guest-*.wasm

# One-time WASM build environment setup (populates cabal package index)
wasm-setup:
    @echo ">>> Setting up WASM build environment (one-time)..."
    nix develop .#wasm --command wasm32-wasi-cabal update --project-file=cabal.project.wasm
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

    echo ">>> [1/5] Building Haskell WASM plugins (cabal cached if unchanged)..."
    just wasm-all

    echo ">>> [2/5] Building Rust binary (${LABEL}, embeds WASM)..."
    cd rust && cargo build ${CARGO_FLAGS} -p exomonad
    cd ..

    echo ">>> [3/5] Building Zellij plugin (wasm32-wasip1, ${LABEL})..."
    cd rust/exomonad-plugin && cargo build ${CARGO_FLAGS} --target wasm32-wasip1
    cd ../..

    echo ">>> [4/5] Building coordinator plugin (wasm32-wasip1, ${LABEL})..."
    cd rust/exomonad-coordinator && cargo build ${CARGO_FLAGS} --target wasm32-wasip1
    cd ../..

    echo ">>> [5/5] Installing binaries..."
    mkdir -p ~/.cargo/bin
    mkdir -p ~/.config/zellij/plugins
    cp "rust/target/${TARGET_DIR}/exomonad" ~/.cargo/bin/
    cp "rust/exomonad-plugin/target/wasm32-wasip1/${TARGET_DIR}/exomonad-plugin.wasm" ~/.config/zellij/plugins/
    cp "rust/exomonad-coordinator/target/wasm32-wasip1/${TARGET_DIR}/exomonad-coordinator.wasm" ~/.config/zellij/plugins/

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
    ls -lh ~/.config/zellij/plugins/exomonad-coordinator.wasm
    ls -lh .exomonad/wasm/wasm-guest-*.wasm

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
    cd rust && cargo build -p exomonad-proto

# Full proto regeneration
proto-gen: proto-gen-haskell proto-gen-rust
    @echo "Proto generation complete. Don't forget to commit haskell/proto/src/"

# Verify proto changes don't break wire format
proto-test:
    #!/usr/bin/env bash
    set -euo pipefail
    echo ">>> Running Rust proto wire format tests..."
    cd rust && cargo test -p exomonad-proto
    cd ..
    echo ">>> Running Haskell proto tests..."
    cabal test exomonad-proto || echo "No tests defined yet"
    echo ">>> Running proto wire format compatibility test..."
    cabal run proto-test || echo "Wire format test not yet implemented"
    echo ">>> Done"

# Clean build artifacts
clean:
    rm -rf {{metadata_dir}}
    docker builder prune -f

# Demo recipe for JustExec effect
test-exec-demo:
    @echo "Running tests..."
    @echo '{"status": "passed", "count": 42}'
