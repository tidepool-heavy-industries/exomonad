# BuildKit Bake orchestrator for ExoMonad Docker builds
#
# Usage (preferred - auto-injects GIT_SHA):
#   ./build                    # Build all final images
#   ./build --load             # Build and load into local daemon
#   ./build deps               # Build only dependency cacher layers
#   ./build control-server     # Build specific target
#
# Usage (direct - requires manual GIT_SHA):
#   export GIT_SHA=$(git rev-parse --short HEAD)
#   docker buildx bake -f docker/docker-bake.hcl
#
# Cache efficiency:
#   - Deps are built separately from source
#   - Source changes only rebuild the final stage, not deps
#   - cargo-chef for Rust, cabal freeze + dummy source for Haskell
#
# Version embedding:
#   GIT_SHA is read from environment (set by ./build wrapper or manually)

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
  dockerfile = "docker/base/Dockerfile.rust-deps"
  target = "planner"
  context = "."
}

target "rust-cacher" {
  dockerfile = "docker/base/Dockerfile.rust-deps"
  target = "cacher"
  context = "."
  contexts = {
    rust-planner = "target:rust-planner"
  }
}

target "rust-builder" {
  dockerfile = "docker/base/Dockerfile.rust-deps"
  target = "builder"
  context = "."
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
  dockerfile = "docker/base/Dockerfile.haskell-deps"
  target = "freeze"
  context = "."
}

target "haskell-cacher" {
  dockerfile = "docker/base/Dockerfile.haskell-deps"
  target = "cacher"
  context = "."
  contexts = {
    haskell-freeze = "target:haskell-freeze"
  }
}

target "haskell-builder" {
  dockerfile = "docker/base/Dockerfile.haskell-deps"
  target = "builder"
  context = "."
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
  dockerfile = "docker/control-server/Dockerfile"
  context = "."
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
  dockerfile = "docker/claude-agent/Dockerfile"
  context = "."
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
  dockerfile = "docker/zellij/Dockerfile"
  context = "."
  contexts = {
    rust-builder = "target:rust-builder"
  }
  tags = ["${REGISTRY}exomonad-zellij:${TAG}"]
  args = {
    GIT_SHA = GIT_SHA
  }
}
