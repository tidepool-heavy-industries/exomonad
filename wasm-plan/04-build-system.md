# Build System

GHC WASM compilation, quicktype codegen, CI integration.

## GHC WASM Toolchain

### Installation

```bash
# Option 1: ghcup (if wasm32-wasi-ghc available)
ghcup install ghc wasm32-wasi-9.10

# Option 2: Nix (from ghc-wasm-meta)
nix develop github:AuroraFoundation/ghc-wasm-meta

# Option 3: Docker (isolated build environment)
# Use a Dockerfile with GHC WASM pre-installed
```

### Compiler Invocation

```bash
# Direct compilation
wasm32-wasi-ghc -no-hs-main \
  -optl-mexec-model=reactor \
  -optl-Wl,--export=hs_init \
  -optl-Wl,--export=handle_mcp_call \
  -optl-Wl,--allow-undefined \
  -o plugin.wasm Main.hs

# Via cabal
wasm32-wasi-cabal build wasm-guest
```

## Justfile Targets

```just
# justfile

# === WASM Build ===

# Build Haskell WASM guest
build-wasm-guest:
    cd haskell && wasm32-wasi-cabal build wasm-guest
    cp haskell/dist-newstyle/build/wasm32-wasi/ghc-*/wasm-guest-*/x/wasm-guest/build/wasm-guest/wasm-guest.wasm \
       rust/exomonad-runtime/plugin.wasm

# Build Rust runtime
build-rust-runtime:
    cd rust && cargo build -p exomonad-runtime --release

# Build everything
build-wasm: build-wasm-guest build-rust-runtime

# === Code Generation ===

# Generate types from schema
gen-types:
    quicktype schema/effects.json schema/common.json \
        --lang rust \
        --visibility public \
        --derive-debug \
        --density dense \
        --out rust/exomonad-runtime/src/generated/effects.rs
    quicktype schema/effects.json schema/common.json \
        --lang haskell \
        --module ExoMonad.Generated.Effects \
        --just-types \
        --out haskell/wasm-guest/src/ExoMonad/Generated/Effects.hs

# Check generated types are up to date
check-gen-types:
    #!/usr/bin/env bash
    set -e
    just gen-types
    if ! git diff --quiet rust/exomonad-runtime/src/generated/ haskell/wasm-guest/src/ExoMonad/Generated/; then
        echo "Generated types are out of date. Run 'just gen-types' and commit."
        exit 1
    fi

# === Development ===

# Run runtime with hot reload
dev-runtime:
    cd rust && cargo watch -x 'run -p exomonad-runtime'

# Rebuild WASM on change
watch-wasm:
    cd haskell && watchexec -e hs -- wasm32-wasi-cabal build wasm-guest

# === Testing ===

# Run all WASM-related tests
test-wasm: test-wasm-guest test-rust-runtime test-contract

test-wasm-guest:
    cd haskell && cabal test wasm-guest-test

test-rust-runtime:
    cd rust && cargo test -p exomonad-runtime

test-contract:
    cd rust && cargo test -p exomonad-runtime --test contract_tests
```

## CI Pipeline

```yaml
# .github/workflows/wasm.yml
name: WASM Build

on:
  push:
    paths:
      - 'schema/**'
      - 'haskell/wasm-guest/**'
      - 'rust/exomonad-runtime/**'
  pull_request:
    paths:
      - 'schema/**'
      - 'haskell/wasm-guest/**'
      - 'rust/exomonad-runtime/**'

jobs:
  check-schema:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Install quicktype
        run: npm install -g quicktype
      - name: Check generated types
        run: just check-gen-types

  build-wasm-guest:
    runs-on: ubuntu-latest
    needs: check-schema
    steps:
      - uses: actions/checkout@v4
      - name: Setup GHC WASM
        uses: AuroraFoundation/setup-ghc-wasm@v1
      - name: Build WASM guest
        run: just build-wasm-guest
      - name: Upload artifact
        uses: actions/upload-artifact@v4
        with:
          name: wasm-guest
          path: rust/exomonad-runtime/plugin.wasm

  build-rust-runtime:
    runs-on: ubuntu-latest
    needs: check-schema
    steps:
      - uses: actions/checkout@v4
      - name: Setup Rust
        uses: dtolnay/rust-toolchain@stable
      - name: Build runtime
        run: just build-rust-runtime

  integration-test:
    runs-on: ubuntu-latest
    needs: [build-wasm-guest, build-rust-runtime]
    steps:
      - uses: actions/checkout@v4
      - name: Download WASM artifact
        uses: actions/download-artifact@v4
        with:
          name: wasm-guest
          path: rust/exomonad-runtime/
      - name: Setup Rust
        uses: dtolnay/rust-toolchain@stable
      - name: Run integration tests
        run: just test-contract
```

## Docker Build Integration

Add to existing `docker-bake.hcl`:

```hcl
target "wasm-guest" {
  dockerfile = "docker/base/Dockerfile.wasm-guest"
  context = "."
  tags = ["exomonad-wasm-guest:latest"]
  output = ["type=local,dest=./artifacts"]
}

target "exomonad-runtime" {
  dockerfile = "docker/base/Dockerfile.rust-runtime"
  context = "."
  tags = ["exomonad-runtime:latest"]
  args = {
    GIT_SHA = "${GIT_SHA}"
  }
}
```

```dockerfile
# docker/base/Dockerfile.wasm-guest
FROM ghcr.io/AuroraFoundation/ghc-wasm:9.10 AS builder

WORKDIR /build
COPY haskell/wasm-guest ./haskell/wasm-guest
COPY haskell/cabal.project ./haskell/
COPY schema ./schema

# Generate types
RUN npm install -g quicktype && \
    quicktype schema/effects.json schema/common.json \
      --lang haskell \
      --module ExoMonad.Generated.Effects \
      --just-types \
      --out haskell/wasm-guest/src/ExoMonad/Generated/Effects.hs

# Build
RUN cd haskell && wasm32-wasi-cabal build wasm-guest

# Extract artifact
FROM scratch AS artifact
COPY --from=builder /build/haskell/dist-newstyle/build/wasm32-wasi/ghc-*/wasm-guest-*/x/wasm-guest/build/wasm-guest/wasm-guest.wasm /plugin.wasm
```

## Dependency Management

### Haskell (wasm-guest)

Minimize dependencies for faster WASM compilation:

```cabal
build-depends:
  , base >= 4.16
  , extism-pdk      -- Required
  , freer-simple    -- Effect system
  , aeson           -- JSON (heavy, but necessary)
  , bytestring
  , text
  -- Avoid: network, unix, template-haskell
```

### Rust (exomonad-runtime)

```toml
[dependencies]
extism = "1.0"
tokio = { version = "1", features = ["full"] }
axum = "0.7"
serde = { version = "1", features = ["derive"] }
serde_json = "1"
octocrab = "0.32"
tracing = "0.1"
```

## Verification Commands

```bash
# Check WASM is reactor model
wasm-objdump -x plugin.wasm | grep -A5 "Export"
# Should show: hs_init, handle_mcp_call, etc.

# Check WASM imports (host functions)
wasm-objdump -x plugin.wasm | grep -A20 "Import"
# Should show: git_get_branch, github_list_issues, etc.

# Validate WASM
wasm-validate plugin.wasm

# Check size
ls -lh plugin.wasm
# Target: < 10MB for reasonable load times
```
