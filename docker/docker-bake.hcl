# BuildKit Bake orchestrator for ExoMonad Docker builds
#
# Usage (preferred - auto-injects GIT_SHA):
#   ./build                    # Build all final images
#   ./build --load             # Build and load into local daemon
#   ./build control-server     # Build specific target
#
# Usage (direct - requires manual GIT_SHA):
#   export GIT_SHA=$(git rev-parse --short HEAD)
#   docker buildx bake -f docker/docker-bake.hcl
#
# Cache strategy:
#   - BuildKit cache mounts persist across builds (global to builder)
#   - Even if source/cabal files change, already-built deps are reused
#   - dist-newstyle cached for incremental compilation
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

# =============================================================================
# Common Base Image (shared user/tools/entrypoint)
# =============================================================================

target "common-base" {
  dockerfile = "docker/base/Dockerfile.common-base"
  context = "."
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
# Haskell Build (BuildKit cache mounts - no separate cacher stage needed)
# =============================================================================

target "haskell-builder" {
  dockerfile = "docker/base/Dockerfile.haskell-deps"
  target = "builder"
  context = "."
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
    common-base = "target:common-base"
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
# Note: claude-agent does NOT use common-base because it uses 'agent' user
# with sudo pattern instead of 'user' with gosu pattern

target "zellij" {
  dockerfile = "docker/zellij/Dockerfile"
  context = "."
  contexts = {
    common-base = "target:common-base"
    rust-builder = "target:rust-builder"
  }
  tags = ["${REGISTRY}exomonad-zellij:${TAG}"]
  args = {
    GIT_SHA = GIT_SHA
  }
}
