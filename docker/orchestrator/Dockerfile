# Stage 1: Builder
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

# Copy everything (BuildKit cache mounts handle dependency caching)
COPY . .

# Remove Nix-specific zlib configuration from cabal.project
# The hardcoded /nix/store paths will fail in Debian.
# We delete the 'package zlib' line and the following 2 lines.
RUN sed -i '/package zlib/,+2d' cabal.project

# Build binary and dependencies, then extract
# BuildKit cache mounts provide fast rebuilds without complex layering.
# We build with -O1 for a balance between build speed and runtime performance.
# Extract binary in same RUN to access it while cache mount is active.
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

# Copy Rust workspace
COPY rust/ ./rust/

# Build mantle-agent with release optimizations
RUN cd rust && cargo build --release -p mantle-agent

# Stage 3: Runtime
# Debian bookworm-slim provides a small, stable base for the runtime
# Platform controlled by docker-compose.yml (linux/amd64 for deployment)
FROM debian:bookworm-slim AS runtime

# Install runtime dependencies
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
    && rm -rf /var/lib/apt/lists/*

# Install Docker CLI
RUN curl -fsSL https://get.docker.com | sh && rm -rf /var/lib/apt/lists/*

# Install cargo-binstall for binary installation
RUN curl -L --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash

# Install Zellij via cargo-binstall
# v0.43.0+ required for web server support
ARG ZELLIJ_VERSION=0.43.1
ENV PATH="/root/.cargo/bin:${PATH}"
RUN cargo-binstall --no-confirm --version ${ZELLIJ_VERSION} zellij && \
    cp /root/.cargo/bin/zellij /usr/local/bin/zellij && \
    chmod 755 /usr/local/bin/zellij

# Copy the binary from builder
COPY --from=builder /usr/local/bin/tidepool-control-server /usr/local/bin/tidepool-control-server

# Copy mantle-agent from rust-builder
COPY --from=rust-builder /build/rust/target/release/mantle-agent /usr/local/bin/mantle-agent

# Install Claude Code CLI
# The installer creates a binary named 'claude', not 'claude-code'
RUN curl -fsSL https://claude.ai/install.sh | bash && \
    mkdir -p /root/.claude

# Add Claude Code to PATH
ENV PATH="/root/.local/bin:${PATH}"

# Copy configurations and scripts
COPY docker/orchestrator/config/orchestrator.kdl /etc/tidepool/zellij/layouts/default.kdl
COPY docker/orchestrator/config/zellij.kdl /etc/tidepool/zellij/config.kdl
COPY docker/orchestrator/scripts/orchestrator-entrypoint.sh /usr/local/bin/orchestrator-entrypoint.sh
COPY scripts/docker-entrypoint-orchestrator.sh /usr/local/bin/docker-entrypoint-orchestrator.sh

# Ensure scripts are executable
RUN chmod +x /usr/local/bin/orchestrator-entrypoint.sh /usr/local/bin/docker-entrypoint-orchestrator.sh

# Environment variables
ENV TIDEPOOL_CONTROL_SOCKET=/sockets/control.sock
ENV TIDEPOOL_TUI_SOCKET=/sockets/tui.sock
ENV HANGAR_ROOT=/worktrees
ENV TERM=xterm-256color
ENV SHELL=/bin/bash
ENV ZELLIJ_CONFIG_DIR=/etc/tidepool/zellij

# Create non-root user for running Claude Code
# UID 1000 is standard for first non-root user
RUN useradd -m -u 1000 -s /bin/bash user

# Create necessary directories with proper ownership
RUN mkdir -p /sockets /worktrees /var/log/tidepool /home/user/.claude /home/user/.claude-orchestrator && \
    chown -R user:user /sockets /worktrees /var/log/tidepool /home/user && \
    chmod 775 /sockets /worktrees /var/log/tidepool

# Ensure Claude binary is accessible to non-root user
RUN chmod 755 /root/.local/bin/claude && \
    cp /root/.local/bin/claude /usr/local/bin/claude && \
    chmod 755 /usr/local/bin/claude

EXPOSE 7432 8080

WORKDIR /worktrees

# Entrypoint runs as root initially for setup, then drops to user via gosu
ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/docker-entrypoint-orchestrator.sh"]
CMD ["orchestrator"]

# Stage 4: Dev Target
FROM --platform=linux/amd64 builder AS dev
RUN cabal install haskell-language-server -j
CMD ["cabal", "repl", "tidepool-control-server"]
