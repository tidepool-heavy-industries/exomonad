# Dedicated Control Server Container
# MCP server (TCP 7432) + Zellij tab creation + docker-ctl integration

# =============================================================================
# Stage 1: Rust Builder (for tui-spawner)
# =============================================================================
FROM rust:bookworm AS rust-builder

# Build tui-spawner with minimal workspace
WORKDIR /build

# These layers are cached (expensive setup)
COPY rust/Cargo.lock ./
COPY rust/tui-spawner ./tui-spawner
COPY rust/tui-sidebar ./tui-sidebar
COPY rust/tui-popup ./tui-popup
COPY rust/docker-ctl ./docker-ctl
COPY rust/mantle-shared ./mantle-shared
COPY rust/hangar-config ./hangar-config

# Create minimal workspace Cargo.toml (only crates needed for tui-spawner and docker-ctl)
RUN echo '[workspace]\nresolver = "2"\nmembers = ["tui-spawner", "tui-sidebar", "tui-popup", "docker-ctl", "mantle-shared", "hangar-config"]' > Cargo.toml

# Cache bust AFTER setup, BEFORE build - invalidates only the build step
# Usage: docker compose build --build-arg RUST_CACHEBUST=$(date +%s) control-server
ARG RUST_CACHEBUST=0
RUN cargo build --release -p tui-spawner && \
    cargo build --release -p docker-ctl

# =============================================================================
# Stage 2: Haskell Builder
# =============================================================================
FROM haskell:9.12.2-slim-bookworm AS builder

RUN apt-get update && apt-get install -y \
    libgmp-dev \
    libffi-dev \
    zlib1g-dev \
    pkg-config \
    git \
    curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Copy all Haskell source
COPY cabal.project ./
COPY haskell/ ./haskell/

# Remove Nix-specific zlib configuration
RUN sed -i '/package zlib/,+2d' cabal.project

# Build with cache mounts
RUN --mount=type=cache,id=control-server-store,target=/root/.cabal/store \
    --mount=type=cache,id=control-server-dist,target=/build/dist-newstyle \
    cabal update && \
    cabal build tidepool-control-server -j --ghc-options=-O1 && \
    cp $(cabal list-bin tidepool-control-server) /usr/local/bin/tidepool-control-server

# =============================================================================
# Stage 2: Runtime
# =============================================================================
FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libgmp10 \
    libffi8 \
    zlib1g \
    ca-certificates \
    gosu \
    socat \
    curl \
    gpg \
    && rm -rf /var/lib/apt/lists/*

# Install GitHub CLI (required for gh_issue_* MCP tools)
RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | gpg --dearmor -o /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" > /etc/apt/sources.list.d/github-cli.list \
    && apt-get update && apt-get install -y gh \
    && rm -rf /var/lib/apt/lists/*

# Install Docker CLI for spawner communication
RUN curl -fsSL https://get.docker.com | sh && \
    rm -rf /var/lib/apt/lists/*

# Install cargo-binstall for binary installation
RUN curl -L --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash

# Install Zellij for cross-container tab creation
ENV PATH="/root/.cargo/bin:${PATH}"
RUN cargo-binstall --no-confirm zellij && \
    cp /root/.cargo/bin/zellij /usr/local/bin/zellij && \
    chmod 755 /usr/local/bin/zellij

# Copy tui-spawner binary (for FIFO-based popup spawning)
# Re-declare CACHEBUST to invalidate COPY --from when rust-builder rebuilds
ARG RUST_CACHEBUST=0
COPY --from=rust-builder /build/target/release/tui-spawner /usr/local/bin/tui-spawner

# Copy docker-ctl binary
COPY --from=rust-builder /build/target/release/docker-ctl /usr/local/bin/docker-ctl

# Copy control-server binary
COPY --from=builder /usr/local/bin/tidepool-control-server /usr/local/bin/tidepool-control-server

# Copy entrypoint
COPY docker/control-server/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

# Environment
ENV TIDEPOOL_CONTROL_SOCKET=/sockets/control.sock
ENV TIDEPOOL_TUI_SOCKET=/sockets/tui.sock
ENV XDG_RUNTIME_DIR=/run/user/1000
ENV ZELLIJ_SESSION_NAME=main

# Create non-root user matching container UID
RUN useradd -m -u 1000 -s /bin/bash user

# Create necessary directories
RUN mkdir -p /sockets /run/user/1000 && \
    chown -R user:user /sockets /run/user/1000

EXPOSE 7432

WORKDIR /home/user

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
