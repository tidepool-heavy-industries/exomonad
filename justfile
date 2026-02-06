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
# Roles are built from .exomonad/flake.nix (user-defined roles per issue #508)
wasm role="tl":
    @echo ">>> Building wasm-guest-{{role}} from .exomonad/flake.nix..."
    cd .exomonad && nix build .#{{role}}
    @echo ">>> Installing to .exomonad/wasm/..."
    mkdir -p .exomonad/wasm
    rm -f .exomonad/wasm/wasm-guest-{{role}}.wasm
    cp .exomonad/result/wasm-guest-{{role}}.wasm .exomonad/wasm/
    @echo ">>> Setting up WASM cache..."
    mkdir -p .exomonad/wasm-cache
    echo "[cache]" > .exomonad/wasm-cache.toml
    echo "directory = '$(pwd)/.exomonad/wasm-cache'" >> .exomonad/wasm-cache.toml
    @echo ">>> Warming up cache..."
    cd rust && cargo run -q -p exomonad -- warmup ../.exomonad/wasm/wasm-guest-{{role}}.wasm
    @echo ">>> Done: .exomonad/wasm/wasm-guest-{{role}}.wasm (cached)"

# Build both WASM role plugins (tl + dev)
wasm-all:
    @just wasm tl
    @just wasm dev
    @echo ">>> Installed to .exomonad/wasm/:"
    @ls -lh .exomonad/wasm/wasm-guest-*.wasm

# Fetch WASM dependencies for incremental builds (one-time, or when deps change)
wasm-deps:
    @echo ">>> Fetching WASM dependencies inside nix shell..."
    nix develop .#wasm -c bash -c 'wasm32-wasi-cabal update && wasm32-wasi-cabal build --project-file=cabal.project.wasm --only-download all'
    @echo ">>> Dependencies cached."

# Incremental WASM build for a role (faster than hermetic nix build)
# Uses nix develop .#wasm for wasm32-wasi-cabal with cabal's incremental compilation.
# First run: `just wasm-deps` to pre-fetch dependencies.
wasm-dev role="tl":
    #!/usr/bin/env bash
    set -euo pipefail
    ROLE="{{role}}"
    ROLE_DIR=".exomonad/roles/${ROLE}"

    echo ">>> [1/4] Generating role scaffolding for '${ROLE}'..."
    mkdir -p "${ROLE_DIR}"

    # Generate Main.hs (deterministic, matches mkWasmRole in flake.nix)
    cat > "${ROLE_DIR}/Main.hs" <<'MAIN_EOF'
    {-# LANGUAGE ForeignFunctionInterface #-}
    {-# LANGUAGE TypeApplications #-}
    module Main where

    import Foreign.C.Types (CInt(..))
    import ExoMonad.Guest.Tool.Runtime (hookHandler, listHandlerRecord, mcpHandlerRecord, wrapHandler)
    import Role (config, Tools)
    import ExoMonad.Types (RoleConfig(..))

    foreign export ccall handle_mcp_call :: IO CInt
    foreign export ccall handle_list_tools :: IO CInt
    foreign export ccall handle_pre_tool_use :: IO CInt

    handle_mcp_call :: IO CInt
    handle_mcp_call = wrapHandler $ mcpHandlerRecord (tools config)

    handle_list_tools :: IO CInt
    handle_list_tools = wrapHandler $ listHandlerRecord @Tools

    handle_pre_tool_use :: IO CInt
    handle_pre_tool_use = wrapHandler $ hookHandler (hooks config)

    main :: IO ()
    main = pure ()
    MAIN_EOF

    # Discover lib modules (e.g. StopHook.hs -> StopHook)
    LIB_DIR=".exomonad/lib"
    LIB_MODULES=""
    if [ -d "${LIB_DIR}" ]; then
        for f in "${LIB_DIR}"/*.hs; do
            [ -f "$f" ] || continue
            mod=$(basename "$f" .hs)
            LIB_MODULES="${LIB_MODULES}
            ${mod}"
        done
    fi

    # Generate .cabal file (matches mkWasmRole in flake.nix)
    cat > "${ROLE_DIR}/${ROLE}.cabal" <<CABAL_EOF
    cabal-version: 3.4
    name: ${ROLE}
    version: 0.1.0.0
    build-type: Simple

    common shared
        default-language: GHC2021
        default-extensions:
            AllowAmbiguousTypes
            DataKinds
            DeriveAnyClass
            DeriveGeneric
            DerivingStrategies
            FlexibleContexts
            FlexibleInstances
            ForeignFunctionInterface
            GADTs
            LambdaCase
            OverloadedStrings
            ScopedTypeVariables
            TypeApplications
            TypeFamilies
            TypeOperators
            UndecidableInstances
        build-depends:
            base >= 4.16 && < 5,
            bytestring,
            text,
            aeson,
            polysemy >= 1.9,
            unordered-containers,
            time,
            directory,
            filepath

        if !arch(wasm32)
            build-depends: polysemy-plugin >= 0.4

    executable wasm-guest-${ROLE}
        import: shared
        hs-source-dirs: . ../../lib
        main-is: Main.hs
        other-modules:
            Role${LIB_MODULES}
        ghc-options: -no-hs-main -optl-mexec-model=reactor -optl-Wl,--export=hs_init -optl-Wl,--export=handle_mcp_call -optl-Wl,--export=handle_pre_tool_use -optl-Wl,--export=handle_list_tools -optl-Wl,--allow-undefined
        build-depends:
            base,
            wasm-guest:wasm-guest-internal,
            exomonad-pdk
    CABAL_EOF

    echo ">>> [2/4] Generating project file..."
    # cabal.project.wasm.dev = cabal.project.wasm + role package
    cp cabal.project.wasm cabal.project.wasm.dev
    echo "" >> cabal.project.wasm.dev
    echo "packages: .exomonad/roles/${ROLE}" >> cabal.project.wasm.dev

    echo ">>> [3/4] Building (incremental)..."
    nix develop .#wasm -c wasm32-wasi-cabal build \
        --project-file=cabal.project.wasm.dev \
        "wasm-guest-${ROLE}"

    echo ">>> [4/4] Installing..."
    mkdir -p .exomonad/wasm
    WASM_FILE=$(find dist-newstyle -name "wasm-guest-${ROLE}.wasm" -print -quit)
    if [ -z "$WASM_FILE" ]; then
        echo "ERROR: wasm-guest-${ROLE}.wasm not found in dist-newstyle" >&2
        exit 1
    fi
    rm -f ".exomonad/wasm/wasm-guest-${ROLE}.wasm"
    cp "$WASM_FILE" ".exomonad/wasm/wasm-guest-${ROLE}.wasm"

    # Setup WASM cache and warmup
    mkdir -p .exomonad/wasm-cache
    echo "[cache]" > .exomonad/wasm-cache.toml
    echo "directory = '$(pwd)/.exomonad/wasm-cache'" >> .exomonad/wasm-cache.toml
    cd rust && cargo run -q -p exomonad -- warmup "../.exomonad/wasm/wasm-guest-${ROLE}.wasm"

    echo ">>> Done: .exomonad/wasm/wasm-guest-${ROLE}.wasm (incremental, cached)"

# Install everything: Rust binaries + WASM plugins (uses release build)
install-all:
    @echo ">>> [1/5] Building Rust binaries (release)..."
    cd rust && cargo build --release -p exomonad
    @echo ">>> [2/5] Building Zellij plugin (wasm32-wasip1, release)..."
    cd rust/exomonad-plugin && cargo build --release --target wasm32-wasip1
    @echo ">>> [3/5] Building coordinator plugin (wasm32-wasip1, release)..."
    cd rust/exomonad-coordinator && cargo build --release --target wasm32-wasip1
    @echo ">>> [4/5] Installing binaries..."
    mkdir -p ~/.cargo/bin
    mkdir -p ~/.config/zellij/plugins
    cp rust/target/release/exomonad ~/.cargo/bin/
    cp rust/exomonad-plugin/target/wasm32-wasip1/release/exomonad-plugin.wasm ~/.config/zellij/plugins/
    cp rust/exomonad-coordinator/target/wasm32-wasip1/release/exomonad-coordinator.wasm ~/.config/zellij/plugins/
    @echo ">>> [5/5] Building and installing WASM plugins..."
    @just wasm-all
    @echo ">>> Done!"
    @echo ""
    @echo "Installed:"
    @ls -lh ~/.cargo/bin/exomonad
    @ls -lh ~/.config/zellij/plugins/exomonad-plugin.wasm
    @ls -lh ~/.config/zellij/plugins/exomonad-coordinator.wasm
    @ls -lh ~/.exomonad/wasm/wasm-guest-*.wasm

# Install everything (fast dev build)
install-all-dev:
    @echo ">>> [1/4] Building Rust sidecar (debug)..."
    cd rust && cargo build -p exomonad
    @echo ">>> [2/5] Building Zellij plugin (wasm32-wasip1)..."
    cd rust/exomonad-plugin && cargo build --target wasm32-wasip1
    @echo ">>> [3/5] Building coordinator plugin (wasm32-wasip1)..."
    cd rust/exomonad-coordinator && cargo build --target wasm32-wasip1
    @echo ">>> [4/5] Installing binaries..."
    mkdir -p ~/.cargo/bin
    mkdir -p ~/.config/zellij/plugins
    cp rust/target/debug/exomonad ~/.cargo/bin/
    cp rust/exomonad-plugin/target/wasm32-wasip1/debug/exomonad-plugin.wasm ~/.config/zellij/plugins/
    cp rust/exomonad-coordinator/target/wasm32-wasip1/debug/exomonad-coordinator.wasm ~/.config/zellij/plugins/
    @echo ">>> [5/5] Building and installing Haskell WASM plugins..."
    @just wasm-all
    @echo ">>> Done!"
    @echo ""
    @echo "Installed:"
    @ls -lh ~/.cargo/bin/exomonad
    @ls -lh ~/.config/zellij/plugins/exomonad-plugin.wasm
    @ls -lh ~/.config/zellij/plugins/exomonad-coordinator.wasm
    @ls -lh ~/.exomonad/wasm/wasm-guest-*.wasm

# Clean build artifacts
clean:
    rm -rf {{metadata_dir}}
    docker builder prune -f

# Demo recipe for JustExec effect
test-exec-demo:
    @echo "Running tests..."
    @echo '{"status": "passed", "count": 42}'
