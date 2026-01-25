# Stage 1: Haskell Builder
# Use GHC 9.12.2 for latest stable with full HLS support
# Platform controlled by docker-compose.yml (linux/amd64 for deployment)
FROM haskell:9.12.2-slim-bookworm AS builder

# Install system dependencies for build
# zlib1g-dev, libffi-dev, libgmp-dev are required for Haskell compilation
RUN apt-get update && apt-get install -y \
    libgmp-dev \
    libffi-dev \
    zlib1g-dev \
    pkg-config \
    git \
    curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# ============================================================================
# CACHE OPTIMIZATION: BuildKit cache mounts for Hackage dependencies
# Note: --only-dependencies doesn't work with monorepos (all local packages
# need source). Instead we rely on cache mounts to speed up rebuilds.
# ============================================================================

# Copy all Haskell source
COPY cabal.project ./
COPY haskell/ ./haskell/

# Remove Nix-specific zlib configuration (hardcoded /nix/store paths fail in Debian)
RUN sed -i '/package zlib/,+2d' cabal.project

# Build with BuildKit cache mounts for Hackage deps and build artifacts
# -O1 balances build speed and runtime performance
RUN --mount=type=cache,target=/root/.cabal/store \
    --mount=type=cache,target=/build/dist-newstyle \
    cabal update && \
    cabal build tidepool-control-server -j --ghc-options=-O1 && \
    cp $(cabal list-bin tidepool-control-server) /usr/local/bin/tidepool-control-server

# Stage 2: Rust Builder
# Build mantle-agent for hook handling
# Using latest stable Rust for edition2024 support (required by hangar-config)
FROM rust:slim-bookworm AS rust-builder

WORKDIR /build

# Install build dependencies
# perl and perl-base needed for OpenSSL build configuration
# build-essential provides gcc, make, and other build tools
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    perl \
    perl-base \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# ============================================================================
# CACHE OPTIMIZATION: Copy Cargo manifests BEFORE source code
# ============================================================================

# 1. Copy workspace Cargo.toml and Cargo.lock first
COPY rust/Cargo.toml rust/Cargo.lock ./rust/

# 2. Copy all workspace member Cargo.tomls
COPY rust/mantle-agent/Cargo.toml ./rust/mantle-agent/
COPY rust/mantle-shared/Cargo.toml ./rust/mantle-shared/
COPY rust/mantle-hub/Cargo.toml ./rust/mantle-hub/
COPY rust/hangar-config/Cargo.toml ./rust/hangar-config/
COPY rust/tui-sidebar/Cargo.toml ./rust/tui-sidebar/
COPY rust/tui-popup/Cargo.toml ./rust/tui-popup/
COPY rust/ssh-proxy/Cargo.toml ./rust/ssh-proxy/
COPY rust/effector/Cargo.toml ./rust/effector/

# 3. Create stub source files for dependency resolution
# Cargo needs these to compute the dependency graph
RUN mkdir -p rust/mantle-agent/src rust/mantle-shared/src rust/mantle-hub/src \
             rust/hangar-config/src rust/tui-sidebar/src rust/tui-popup/src \
             rust/ssh-proxy/src rust/effector/src && \
    echo "fn main() {}" > rust/mantle-agent/src/main.rs && \
    echo "" > rust/mantle-shared/src/lib.rs && \
    echo "fn main() {}" > rust/mantle-hub/src/main.rs && \
    echo "" > rust/hangar-config/src/lib.rs && \
    echo "fn main() {}" > rust/tui-sidebar/src/main.rs && \
    echo "fn main() {}" > rust/tui-popup/src/main.rs && \
    echo "fn main() {}" > rust/ssh-proxy/src/main.rs && \
    echo "" > rust/effector/src/lib.rs

# 4. Build dependencies only (cached unless Cargo.toml/lock change)
# The '|| true' handles the expected compilation failure of stubs
RUN --mount=type=cache,target=/root/.cargo/registry \
    --mount=type=cache,target=/root/.cargo/git \
    --mount=type=cache,target=/build/rust/target \
    cd rust && cargo build --release -p mantle-agent 2>/dev/null || true

# 5. Copy real source code
COPY rust/ ./rust/

# 6. Build the actual binary (deps already cached)
RUN --mount=type=cache,target=/root/.cargo/registry \
    --mount=type=cache,target=/root/.cargo/git \
    --mount=type=cache,target=/build/rust/target \
    cd rust && cargo build --release -p mantle-agent && \
    cp target/release/mantle-agent /usr/local/bin/mantle-agent

# Stage 3: Runtime
# Debian bookworm-slim provides a small, stable base for the runtime
# Platform controlled by docker-compose.yml (linux/amd64 for deployment)
FROM debian:bookworm-slim AS runtime

# ============================================================================
# CACHE OPTIMIZATION: Stable layers first, frequently-changing configs last
# ============================================================================

# 1. Install runtime dependencies (very stable, rarely changes)
RUN apt-get update && apt-get install -y \
    libgmp10 \
    libffi8 \
    zlib1g \
    ca-certificates \
    openssl \
    tini \
    gosu \
    git \
    curl \
    socat \
    lsof \
    procps \
    screen \
    openssh-client \
    && rm -rf /var/lib/apt/lists/*

# 2. Install Docker CLI (stable)
RUN curl -fsSL https://get.docker.com | sh && rm -rf /var/lib/apt/lists/*

# 3. Install cargo-binstall for binary installation (stable)
RUN curl -L --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash

# 4. Install Zellij via cargo-binstall (stable, version pinned by cargo-binstall)
ENV PATH="/root/.cargo/bin:${PATH}"
RUN cargo-binstall --no-confirm zellij && \
    cp /root/.cargo/bin/zellij /usr/local/bin/zellij && \
    chmod 755 /usr/local/bin/zellij

# 5. Install Claude Code CLI (stable)
RUN curl -fsSL https://claude.ai/install.sh | bash && \
    mkdir -p /root/.claude

# Add Claude Code to PATH
ENV PATH="/root/.local/bin:${PATH}"

# 6. Copy binaries from builders (change with code, but less often than configs)
COPY --from=builder /usr/local/bin/tidepool-control-server /usr/local/bin/tidepool-control-server
COPY --from=rust-builder /usr/local/bin/mantle-agent /usr/local/bin/mantle-agent

# 7. Create non-root user for running Claude Code (stable)
RUN useradd -m -u 1000 -s /bin/bash user

# 8. Create necessary directories with proper ownership (stable)
RUN mkdir -p /sockets /worktrees /var/log/tidepool /home/user/.claude /home/user/.claude-orchestrator && \
    chown -R user:user /sockets /worktrees /var/log/tidepool /home/user && \
    chmod 775 /sockets /worktrees /var/log/tidepool

# 9. Ensure Claude binary is accessible to non-root user (stable)
RUN chmod 755 /root/.local/bin/claude && \
    cp /root/.local/bin/claude /usr/local/bin/claude && \
    chmod 755 /usr/local/bin/claude

# 10. Environment variables (stable)
ENV TIDEPOOL_CONTROL_SOCKET=/sockets/control.sock
ENV TIDEPOOL_TUI_SOCKET=/sockets/tui.sock
ENV HANGAR_ROOT=/worktrees
ENV TERM=xterm-256color
ENV SHELL=/bin/bash
ENV ZELLIJ_CONFIG_DIR=/etc/tidepool/zellij

# ============================================================================
# FREQUENTLY CHANGING CONFIGS LAST (maximizes cache reuse)
# ============================================================================

# 11. Copy configurations (changes more often than code)
COPY docker/orchestrator/config/exomonad.kdl /etc/tidepool/zellij/layouts/exomonad.kdl
COPY docker/orchestrator/config/zellij.kdl /etc/tidepool/zellij/config.kdl

# 12. Copy scripts (changes more often than configs)
COPY docker/orchestrator/scripts/orchestrator-entrypoint.sh /usr/local/bin/orchestrator-entrypoint.sh
COPY scripts/docker-entrypoint-orchestrator.sh /usr/local/bin/docker-entrypoint-orchestrator.sh
COPY docker/orchestrator/scripts/start-claude.sh /usr/local/bin/start-claude.sh
COPY docker/orchestrator/scripts/zattach /usr/local/bin/zattach

# 13. Copy SSH private key for ssh-proxy (generated during pre-build)
COPY docker/orchestrator/ssh-keys/orchestrator_key /etc/ssh-proxy/orchestrator_key
RUN chmod 600 /etc/ssh-proxy/orchestrator_key

# 14. Ensure scripts are executable
RUN chmod +x /usr/local/bin/orchestrator-entrypoint.sh /usr/local/bin/docker-entrypoint-orchestrator.sh /usr/local/bin/start-claude.sh /usr/local/bin/zattach

# Stage 4: Control Server (minimal, just Haskell binary + runtime deps)
FROM debian:bookworm-slim AS control-server

# Install minimal runtime dependencies for Haskell binary
RUN apt-get update && apt-get install -y \
    libgmp10 \
    libffi8 \
    zlib1g \
    ca-certificates \
    tini \
    gosu \
    socat \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Install cargo-binstall for binary installation
RUN curl -L --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash

# Install Zellij for cross-container tab creation (connects to orchestrator's session)
ENV PATH="/root/.cargo/bin:${PATH}"
RUN cargo-binstall --no-confirm zellij && \
    cp /root/.cargo/bin/zellij /usr/local/bin/zellij && \
    chmod 755 /usr/local/bin/zellij

# Copy only the control-server binary
COPY --from=builder /usr/local/bin/tidepool-control-server /usr/local/bin/tidepool-control-server

# Environment variables
ENV TIDEPOOL_CONTROL_SOCKET=/sockets/control.sock
ENV TIDEPOOL_TUI_SOCKET=/sockets/tui.sock
# Zellij cross-container access (shared socket volume with orchestrator)
ENV XDG_RUNTIME_DIR=/run/user/1000
ENV ZELLIJ_SESSION_NAME=orchestrator

# Create non-root user matching orchestrator UID
RUN useradd -m -u 1000 -s /bin/bash user

# Create entrypoint script for permission fixing + stale socket cleanup
RUN cat > /usr/local/bin/control-server-entrypoint.sh <<'EOF'
#!/bin/bash
set -euo pipefail

# Fix volume ownership (Docker creates named volumes as root)
chown 1000:1000 /sockets
chmod 755 /sockets

# Ensure XDG_RUNTIME_DIR exists for Zellij socket access
# This is a shared volume with orchestrator for cross-container Zellij commands
mkdir -p /run/user/1000
chown 1000:1000 /run/user/1000
chmod 755 /run/user/1000

# Cleanup stale socket to prevent "Address already in use" errors
rm -f /sockets/control.sock /sockets/tui.sock

# Start server as user:1000
# control-server uses TIDEPOOL_CONTROL_SOCKET env var for socket path
exec gosu 1000:1000 tidepool-control-server
EOF

RUN chmod +x /usr/local/bin/control-server-entrypoint.sh

EXPOSE 7432

# Set working directory to user's home (control-server creates .tidepool/ here)
# Socket path comes from TIDEPOOL_CONTROL_SOCKET env var (absolute path to shared volume)
WORKDIR /home/user

# Run as root initially to fix permissions, entrypoint drops to user
ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/control-server-entrypoint.sh"]

# Stage 5: Orchestrator (Zellij + Claude Code, mounts control-server socket)
# Attach via: docker exec -it tidepool-orchestrator zellij attach orchestrator
FROM runtime AS orchestrator

WORKDIR /worktrees

# Entrypoint runs as root initially for setup, then drops to user via gosu
ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/docker-entrypoint-orchestrator.sh"]
CMD ["orchestrator"]

# Stage 6: Dev Target
FROM --platform=linux/amd64 builder AS dev
RUN cabal install haskell-language-server -j
CMD ["cabal", "repl", "tidepool-control-server"]
