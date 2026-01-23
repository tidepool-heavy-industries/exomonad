# Stage 1: Builder
# Use GHC 9.6.6 for best GHC2021 support and compatibility
FROM haskell:9.6.6-slim AS builder

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

# Copy all files (respecting .dockerignore)
COPY . .

# Use BuildKit mounts for cabal store and dist-newstyle to speed up rebuilds.
# We build with -O1 for a balance between build speed and runtime performance.
# tidepool-control-server is the main orchestrator binary.
RUN --mount=type=cache,target=/root/.cabal/store \
    --mount=type=cache,target=/build/dist-newstyle \
    cabal update && \
    cabal build tidepool-control-server -j --O1

# Extract the binary using cabal list-bin
RUN cp $(cabal list-bin tidepool-control-server) /usr/local/bin/tidepool-control-server

# Stage 2: Runtime
# Debian bookworm-slim provides a small, stable base for the runtime
FROM debian:bookworm-slim AS runtime

# Install runtime dependencies
# tini: zombie reaping and signal handling
# gosu: step down from root to tidepool user
# socat/lsof: used by orchestrator for socket management and debugging
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

# Install Docker CLI (needed for the orchestrator to manage sibling containers)
RUN curl -fsSL https://get.docker.com | sh && rm -rf /var/lib/apt/lists/*

# Install Zellij
ARG ZELLIJ_VERSION=0.41.2
RUN ARCH=$(uname -m) && \
    if [ "$ARCH" = "x86_64" ]; then \
        ZELLIJ_ARCH="x86_64-unknown-linux-musl"; \
    elif [ "$ARCH" = "aarch64" ]; then \
        ZELLIJ_ARCH="aarch64-unknown-linux-musl"; \
    fi && \
    curl -L "https://github.com/zellij-org/zellij/releases/download/v${ZELLIJ_VERSION}/zellij-${ZELLIJ_ARCH}.tar.gz" | tar xz && \
    mv zellij /usr/local/bin/zellij && \
    chmod +x /usr/local/bin/zellij

# Copy the binary from builder
COPY --from=builder /usr/local/bin/tidepool-control-server /usr/local/bin/tidepool-control-server

# Copy configurations and scripts
# We use /etc/tidepool/zellij for global configs to avoid home directory issues
COPY docker/orchestrator/config/orchestrator.kdl /etc/tidepool/zellij/layouts/default.kdl
COPY docker/orchestrator/config/zellij.kdl /etc/tidepool/zellij/config.kdl
COPY docker/orchestrator/scripts/orchestrator-entrypoint.sh /usr/local/bin/orchestrator-entrypoint.sh
COPY scripts/docker-entrypoint-orchestrator.sh /usr/local/bin/docker-entrypoint-orchestrator.sh

# Ensure scripts are executable
RUN chmod +x /usr/local/bin/orchestrator-entrypoint.sh /usr/local/bin/docker-entrypoint-orchestrator.sh

# Environment variables for Tidepool components
ENV TIDEPOOL_CONTROL_SOCKET=/sockets/control.sock
ENV TIDEPOOL_TUI_SOCKET=/sockets/tui.sock
ENV HANGAR_ROOT=/worktrees
ENV TERM=xterm-256color
ENV ZELLIJ_CONFIG_DIR=/etc/tidepool/zellij

# Create necessary directories
RUN mkdir -p /sockets /worktrees /var/log/tidepool && \
    chmod 777 /sockets /worktrees /var/log/tidepool

# Expose port 7432 for external role-based MCP access
EXPOSE 7432

WORKDIR /worktrees

# Use tini for proper signal handling
# Use our custom entrypoint for UID/GID mapping and Auth Isolation
ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/docker-entrypoint-orchestrator.sh"]

# Default command starts the orchestrator
CMD ["orchestrator"]

# Stage 3: Dev Target (Optional)
# Provides a full GHC environment and HLS for containerized development
FROM builder AS dev
RUN cabal install haskell-language-server -j
CMD ["cabal", "repl", "tidepool-control-server"]