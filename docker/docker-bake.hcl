# BuildKit Bake orchestrator for ExoMonad Docker builds
#
# Usage:
#   docker buildx bake                    # Build all final images
#   docker buildx bake deps               # Build only dependency cacher layers
#   docker buildx bake rust-builder       # Build Rust crates only
#   docker buildx bake haskell-builder    # Build Haskell packages only
#
# Cache efficiency:
#   - Deps are built separately from source
#   - Source changes only rebuild the final stage, not deps
#   - cargo-chef for Rust, cabal freeze + dummy source for Haskell
#
# Version embedding:
#   docker buildx bake --set *.args.GIT_SHA=$(git rev-parse --short HEAD)

variable "REGISTRY" { default = "" }
variable "TAG" { default = "latest" }
variable "GIT_SHA" { default = "" }

# Default: build all final images
group "default" {
  targets = ["control-server", "claude-agent", "zellij"]
}

# Build only dependency cachers (for CI warmup)
group "deps" {
  targets = ["rust-cacher", "haskell-cacher"]
}

# =============================================================================
# Rust Dependency Caching (cargo-chef)
# =============================================================================

target "rust-planner" {
  dockerfile = "base/Dockerfile.rust-deps"
  target = "planner"
  context = ".."
}

target "rust-cacher" {
  dockerfile = "base/Dockerfile.rust-deps"
  target = "cacher"
  context = ".."
  contexts = {
    rust-planner = "target:rust-planner"
  }
  cache-from = ["type=registry,ref=${REGISTRY}exomonad-rust-deps:cache"]
  cache-to = ["type=registry,ref=${REGISTRY}exomonad-rust-deps:cache,mode=max"]
}

target "rust-builder" {
  dockerfile = "base/Dockerfile.rust-deps"
  target = "builder"
  context = ".."
  contexts = {
    rust-cacher = "target:rust-cacher"
  }
  args = {
    GIT_SHA = GIT_SHA
  }
}

# =============================================================================
# Haskell Dependency Caching (cabal freeze + dummy source)
# =============================================================================

target "haskell-freeze" {
  dockerfile = "base/Dockerfile.haskell-deps"
  target = "freeze"
  context = ".."
}

target "haskell-cacher" {
  dockerfile = "base/Dockerfile.haskell-deps"
  target = "cacher"
  context = ".."
  contexts = {
    haskell-freeze = "target:haskell-freeze"
  }
  cache-from = ["type=registry,ref=${REGISTRY}exomonad-haskell-deps:cache"]
  cache-to = ["type=registry,ref=${REGISTRY}exomonad-haskell-deps:cache,mode=max"]
}

target "haskell-builder" {
  dockerfile = "base/Dockerfile.haskell-deps"
  target = "builder"
  context = ".."
  contexts = {
    haskell-cacher = "target:haskell-cacher"
  }
  args = {
    GIT_SHA = GIT_SHA
  }
}

# =============================================================================
# Final Images (inject pre-built binaries via contexts)
# =============================================================================

target "control-server" {
  dockerfile = "control-server/Dockerfile"
  context = ".."
  contexts = {
    haskell-builder = "target:haskell-builder"
    rust-builder = "target:rust-builder"
  }
  tags = ["${REGISTRY}exomonad-control-server:${TAG}"]
  args = {
    GIT_SHA = GIT_SHA
  }
}

target "claude-agent" {
  dockerfile = "claude-agent/Dockerfile"
  context = ".."
  contexts = {
    haskell-builder = "target:haskell-builder"
    rust-builder = "target:rust-builder"
  }
  tags = ["${REGISTRY}exomonad-agent:${TAG}"]
  args = {
    GIT_SHA = GIT_SHA
  }
}

target "zellij" {
  dockerfile = "zellij/Dockerfile"
  context = ".."
  contexts = {
    rust-builder = "target:rust-builder"
  }
  tags = ["${REGISTRY}exomonad-zellij:${TAG}"]
  args = {
    GIT_SHA = GIT_SHA
  }
}
