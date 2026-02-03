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

# Build WASM guest and install to ~/.exomonad/wasm/
wasm role="tl":
    @echo ">>> Building wasm-guest-{{role}} via Nix..."
    nix build .#wasm-guest-{{role}}
    @echo ">>> Installing to ~/.exomonad/wasm/..."
    mkdir -p ~/.exomonad/wasm
    rm -f ~/.exomonad/wasm/wasm-guest-{{role}}.wasm
    cp result/wasm-guest-{{role}}.wasm ~/.exomonad/wasm/
    @echo ">>> Setting up WASM cache..."
    mkdir -p ~/.exomonad/wasm-cache
    echo "[cache]" > ~/.exomonad/wasm-cache.toml
    echo "directory = '${HOME}/.exomonad/wasm-cache'" >> ~/.exomonad/wasm-cache.toml
    @echo ">>> Warming up cache..."
    cd rust && cargo run -q -p exomonad-sidecar -- warmup ~/.exomonad/wasm/wasm-guest-{{role}}.wasm
    @echo ">>> Done: ~/.exomonad/wasm/wasm-guest-{{role}}.wasm (cached)"

# Build both WASM guest plugins (tl + dev)
wasm-all:
    @just wasm tl
    @just wasm dev
    @echo ">>> Installed to ~/.exomonad/wasm/:"
    @ls -lh ~/.exomonad/wasm/wasm-guest-*.wasm

# Build WASM guest using local shell (no hash updates needed, impure)
wasm-dev role="tl":
    @echo ">>> Building wasm-guest-{{role}} (Impure/Dev Mode)..."
    nix develop .#wasm --command bash -c "\
        wasm32-wasi-cabal build --project-file=cabal.project.wasm wasm-guest-{{role}} && \
        mkdir -p ~/.exomonad/wasm && \
        cp \$(find dist-newstyle -name wasm-guest-{{role}}.wasm -type f | head -n 1) ~/.exomonad/wasm/"
    @echo ">>> Setting up WASM cache..."
    mkdir -p ~/.exomonad/wasm-cache
    echo "[cache]" > ~/.exomonad/wasm-cache.toml
    echo "directory = '${HOME}/.exomonad/wasm-cache'" >> ~/.exomonad/wasm-cache.toml
    @echo ">>> Warming up cache..."
    cd rust && cargo run -q -p exomonad-sidecar -- warmup ~/.exomonad/wasm/wasm-guest-{{role}}.wasm
    @echo ">>> Done: ~/.exomonad/wasm/wasm-guest-{{role}}.wasm (cached)"

# Install everything: Rust binaries + WASM plugins (uses release build)
install-all:
    @echo ">>> [1/3] Building Rust binaries (release)..."
    cd rust && cargo build --release -p exomonad-sidecar
    @echo ">>> [2/3] Installing binaries to ~/.cargo/bin/..."
    mkdir -p ~/.cargo/bin
    cp rust/target/release/exomonad-sidecar ~/.cargo/bin/
    @echo ">>> [3/3] Building and installing WASM plugins..."
    @just wasm-all
    @echo ">>> Done!"
    @echo ""
    @echo "Installed:"
    @ls -lh ~/.cargo/bin/exomonad-sidecar
    @ls -lh ~/.exomonad/wasm/wasm-guest-*.wasm

# Install everything (fast dev build)
install-all-dev:
    @echo ">>> [1/4] Building Rust sidecar (debug)..."
    cd rust && cargo build -p exomonad-sidecar
    @echo ">>> [2/4] Building Zellij plugin (wasm32-wasip1)..."
    cd rust/exomonad-plugin && cargo build --target wasm32-wasip1
    @echo ">>> [3/4] Installing binaries..."
    mkdir -p ~/.cargo/bin
    mkdir -p ~/.config/zellij/plugins
    cp rust/target/debug/exomonad-sidecar ~/.cargo/bin/
    cp rust/exomonad-plugin/target/wasm32-wasip1/debug/exomonad-plugin.wasm ~/.config/zellij/plugins/
    @echo ">>> [4/4] Building and installing Haskell WASM plugins..."
    @just wasm-all
    @echo ">>> Done!"
    @echo ""
    @echo "Installed:"
    @ls -lh ~/.cargo/bin/exomonad-sidecar
    @ls -lh ~/.config/zellij/plugins/exomonad-plugin.wasm
    @ls -lh ~/.exomonad/wasm/wasm-guest-*.wasm

# Clean build artifacts
clean:
    rm -rf {{metadata_dir}}
    docker builder prune -f

# Demo recipe for JustExec effect
test-exec-demo:
    @echo "Running tests..."
    @echo '{"status": "passed", "count": 42}'
