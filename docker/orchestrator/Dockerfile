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

# Stage 2: Runtime
# Debian bookworm-slim provides a small, stable base for the runtime
# Platform controlled by docker-compose.yml (linux/amd64 for deployment)
FROM debian:bookworm-slim AS runtime

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libgmp10 \
    libffi8 \
    zlib1g \
    ca-certificates \
    tini \
    gosu \
    git \
    curl \
    socat \
    lsof \
    procps \
    && rm -rf /var/lib/apt/lists/*

# Install Docker CLI
RUN curl -fsSL https://get.docker.com | sh && rm -rf /var/lib/apt/lists/*

# Install Zellij
ARG ZELLIJ_VERSION=0.41.2
# Since we forced linux/amd64, we can hardcode the arch for Zellij too
RUN curl -L "https://github.com/zellij-org/zellij/releases/download/v${ZELLIJ_VERSION}/zellij-x86_64-unknown-linux-musl.tar.gz" | tar xz && \
    mv zellij /usr/local/bin/zellij && \
    chmod +x /usr/local/bin/zellij

# Copy the binary from builder
COPY --from=builder /usr/local/bin/tidepool-control-server /usr/local/bin/tidepool-control-server

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
ENV ZELLIJ_CONFIG_DIR=/etc/tidepool/zellij

# Create necessary directories
RUN mkdir -p /sockets /worktrees /var/log/tidepool && \
    chmod 777 /sockets /worktrees /var/log/tidepool

EXPOSE 7432 8080

WORKDIR /worktrees

ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/docker-entrypoint-orchestrator.sh"]
CMD ["orchestrator"]

# Stage 3: Dev Target
FROM --platform=linux/amd64 builder AS dev
RUN cabal install haskell-language-server -j
CMD ["cabal", "repl", "tidepool-control-server"]
