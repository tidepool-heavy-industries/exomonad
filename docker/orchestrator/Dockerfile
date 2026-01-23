# Stage 0: Cabal File Harvester
# This stage copies only the .cabal files to create a cache-friendly layer
# for dependency building.
FROM alpine:latest AS cabal-files
WORKDIR /app
COPY cabal.project .
# Copy the entire haskell directory
COPY haskell/ haskell/
# Find and delete everything that is NOT a .cabal file
# This is a robust way to keep the directory structure matching cabal.project
# without copying source code.
RUN find haskell -type f ! -name "*.cabal" -delete

# Stage 1: Builder
# Use GHC 9.12.2 for latest stable with full HLS support
# Force linux/amd64 to avoid cross-compilation issues on ARM hosts
FROM --platform=linux/amd64 haskell:9.12.2-slim AS builder

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

# Copy project definition and .cabal files from the harvester stage
# This layer ONLY changes if dependencies change.
COPY --from=cabal-files /app/cabal.project .
COPY --from=cabal-files /app/haskell haskell/

# Remove Nix-specific zlib configuration from cabal.project
# The hardcoded /nix/store paths will fail in Debian.
# We delete the 'package zlib' line and the following 2 lines.
RUN sed -i '/package zlib/,+2d' cabal.project

# Build dependencies (Cached Layer)
# This will only re-run if cabal.project or a .cabal file changes.
RUN --mount=type=cache,target=/root/.cabal/store \
    --mount=type=cache,target=/build/dist-newstyle \
    cabal update && \
    cabal build --only-dependencies tidepool-control-server -j

# Copy the rest of the source (Source Layer)
# This layer changes frequently (whenever source code changes).
COPY . .

# Build the binary
# We build with -O1 for a balance between build speed and runtime performance.
RUN --mount=type=cache,target=/root/.cabal/store \
    --mount=type=cache,target=/build/dist-newstyle \
    cabal build tidepool-control-server -j --O1

# Extract the binary using cabal list-bin
RUN cp $(cabal list-bin tidepool-control-server) /usr/local/bin/tidepool-control-server

# Stage 2: Runtime
# Debian bookworm-slim provides a small, stable base for the runtime
FROM --platform=linux/amd64 debian:bookworm-slim AS runtime

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

EXPOSE 7432

WORKDIR /worktrees

ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/docker-entrypoint-orchestrator.sh"]
CMD ["orchestrator"]

# Stage 3: Dev Target
FROM --platform=linux/amd64 builder AS dev
RUN cabal install haskell-language-server -j
CMD ["cabal", "repl", "tidepool-control-server"]
