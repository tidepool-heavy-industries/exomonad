# Dockerfile for Claude Code Agent
# No SSH - uses docker exec via docker-spawner for remote commands

# =============================================================================
# Stage 1: Rust Builder (for mantle-agent, effector, tui-spawner, bd)
# =============================================================================
FROM --platform=linux/amd64 rust:bookworm AS rust-builder
WORKDIR /build

# Build bd CLI from source to match container's glibc
RUN cargo install beads-rs --locked

# Install build dependencies
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    perl \
    perl-base \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Copy Rust workspace
COPY rust/ ./rust/

# Build mantle-agent, effector, and tui-spawner
# Use cache mounts for cargo to speed up rebuilds
RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/build/rust/target \
    cd rust && \
    cargo build --release -p mantle-agent -p effector -p tui-spawner && \
    cp target/release/mantle-agent /mantle-agent && \
    cp target/release/effector /effector && \
    cp target/release/tui-spawner /tui-spawner

# =============================================================================
# Stage 2: Haskell Builder (for tidepool-control-server)
# =============================================================================
FROM haskell:9.12.2-slim-bookworm AS haskell-builder

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
    cp $(cabal list-bin tidepool-control-server) /tidepool-control-server

# =============================================================================
# Stage 3: Main Agent Image
# =============================================================================
FROM --platform=linux/amd64 debian:bookworm-slim

# Avoid prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install core dependencies (no SSH)
RUN apt-get update && apt-get install -y \
    tini \
    git \
    curl \
    ca-certificates \
    gnupg \
    lsb-release \
    build-essential \
    libgmp-dev \
    libffi-dev \
    libncurses-dev \
    libtinfo-dev \
    zlib1g-dev \
    pkg-config \
    libssl-dev \
    xz-utils \
    sudo \
    python3 \
    socat \
    procps \
    gosu \
    && rm -rf /var/lib/apt/lists/*

# Install Node.js 20.x
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get install -y nodejs

# Install Docker CLI for spawner communication (tui-spawner needs this)
RUN curl -fsSL https://get.docker.com | sh && \
    rm -rf /var/lib/apt/lists/*

# Install cargo-binstall for binary installation
RUN curl -L --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash

# Install Zellij for cross-container tab creation
ENV PATH="/root/.cargo/bin:${PATH}"
RUN cargo-binstall --no-confirm zellij && \
    cp /root/.cargo/bin/zellij /usr/local/bin/zellij && \
    chmod 755 /usr/local/bin/zellij

# Create non-root user 'agent' with UID 1000
RUN useradd -m -s /bin/bash -u 1000 agent \
    && echo "agent ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

# Pre-seed Claude Code onboarding to avoid interactive prompts
RUN mkdir -p /home/agent/.claude && \
    echo '{"hasCompletedOnboarding":true}' > /home/agent/.claude.json && \
    chown -R agent:agent /home/agent/.claude /home/agent/.claude.json

# Set up toolchain installation for agent user
USER agent
WORKDIR /home/agent

# Install Claude Code CLI
RUN curl -fsSL https://claude.ai/install.sh | bash

# Install GHCup (Haskell toolchain)
ENV GHCUP_INSTALL_SKIP_MSYS=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_GHC_VERSION=9.6.4 BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.2.1 BOOTSTRAP_HASKELL_INSTALL_STACK=1 BOOTSTRAP_HASKELL_INSTALL_HLS=1 sh

# Install Rust toolchain (rustup)
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

# Set PATH for agent user
ENV PATH="/home/agent/.local/bin:/home/agent/.ghcup/bin:/home/agent/.cargo/bin:${PATH}"

# Back to root to copy binaries
USER root

# Copy Rust binaries
COPY --from=rust-builder /mantle-agent /usr/local/bin/mantle-agent
COPY --from=rust-builder /effector /usr/local/bin/effector
COPY --from=rust-builder /tui-spawner /usr/local/bin/tui-spawner
COPY --from=rust-builder /usr/local/cargo/bin/bd /usr/local/bin/bd

# Copy Haskell binaries
COPY --from=haskell-builder /tidepool-control-server /usr/local/bin/tidepool-control-server

# Set permissions
RUN chmod +x /usr/local/bin/mantle-agent /usr/local/bin/effector /usr/local/bin/tui-spawner /usr/local/bin/tidepool-control-server /usr/local/bin/bd

# Set up entrypoint script
COPY docker/claude-agent/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

# Create workspace directory
RUN mkdir -p /workspace && chown agent:agent /workspace

# Default environment
ENV MANTLE_CONTROL_SOCKET=/home/agent/.tidepool/sockets/control.sock
ENV XDG_RUNTIME_DIR=/run/user/1000
ENV ZELLIJ_SESSION_NAME=main

# Switch back to agent user for execution
USER agent
WORKDIR /workspace

# Entrypoint uses tini for proper signal handling
ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/entrypoint.sh"]

# Default command: launch Claude Code with bypassPermissions
CMD ["claude", "--permission-mode", "bypassPermissions"]
