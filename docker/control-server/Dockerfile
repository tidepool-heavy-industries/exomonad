# Dedicated Control Server Container
# MCP server (TCP 7432) + Zellij tab creation + docker-spawner integration

# =============================================================================
# Stage 1: Haskell Builder
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
RUN --mount=type=cache,target=/root/.cabal/store \
    --mount=type=cache,target=/build/dist-newstyle \
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
    tini \
    gosu \
    socat \
    curl \
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

# Install bd CLI for beads task tracking
RUN cargo-binstall --no-confirm beads-rs && \
    cp /root/.cargo/bin/bd /usr/local/bin/bd && \
    chmod 755 /usr/local/bin/bd

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

ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/entrypoint.sh"]
