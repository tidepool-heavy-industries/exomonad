# Build mantle-agent for Linux inside Docker
# From tidepool root: docker build -t tidepool/claude-code:latest -f rust/mantle/Dockerfile .

FROM rustlang/rust:nightly-bookworm AS builder

WORKDIR /build

# Copy workspace files
COPY rust/Cargo.toml rust/Cargo.lock ./
COPY rust/mantle ./mantle
COPY rust/mantle-agent ./mantle-agent
COPY rust/mantle-hub ./mantle-hub
COPY rust/mantle-shared ./mantle-shared

# Build mantle-agent (debug for faster iteration)
RUN cargo build -p mantle-agent

# Runtime stage
FROM debian:bookworm-slim

# Install system dependencies for Node.js, ghcup, and development
RUN apt-get update && apt-get install -y \
    ca-certificates \
    curl \
    wget \
    git \
    # ghcup dependencies
    build-essential \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libtinfo-dev \
    zlib1g-dev \
    # Process management (ps, top, pkill, killall, pgrep, pstree, fuser)
    procps \
    psmisc \
    # Network tools
    iproute2 \
    iputils-ping \
    netcat-openbsd \
    dnsutils \
    # Text processing & viewing
    vim \
    nano \
    less \
    jq \
    # System utilities
    tree \
    htop \
    strace \
    lsof \
    file \
    zip \
    unzip \
    rsync \
    # Modern CLI replacements
    ripgrep \
    fd-find \
    bat \
    # Database & monitoring
    sqlite3 \
    watch \
    # Documentation
    man-db \
    && curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get install -y nodejs \
    && npm install -g @anthropic-ai/claude-code \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user (Claude Code refuses --dangerously-skip-permissions as root)
RUN useradd -m -s /bin/bash user

# Pre-create directories with correct ownership
# This ensures mounted volumes inherit proper permissions
RUN mkdir -p /home/user/.claude /home/user/.stack /home/user/.local/bin && \
    chown -R user:user /home/user/.claude /home/user/.stack /home/user/.local/bin

# Switch to user for all remaining operations (no more root needed)
USER user
WORKDIR /home/user

# Configure git identity for commits inside container
RUN git config --global user.email "claude@tidepool.labs" && \
    git config --global user.name "Claude (Tidepool)" && \
    git config --global init.defaultBranch main

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_GHC_VERSION=latest
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=latest
ENV BOOTSTRAP_HASKELL_INSTALL_STACK=1

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH="/home/user/.local/bin:/home/user/.ghcup/bin:${PATH}"

# Pre-warm Stack cache with the actual url-shortener template
# This downloads GHC, resolver snapshot, and common dependencies during image build
# Do this as 'user' so cache builds up in /home/user/.stack
COPY --chown=user:user types-first-dev/template-repo-copy /tmp/warmup
WORKDIR /tmp/warmup
RUN stack build && cd .. && rm -rf warmup
WORKDIR /home/user

# Copy binaries to user's local bin (no root needed)
COPY --from=builder --chown=user:user /build/target/debug/mantle-agent /home/user/.local/bin/

# Copy entrypoint script
COPY --chown=user:user rust/mantle/entrypoint.sh /home/user/.local/bin/entrypoint.sh
RUN chmod +x /home/user/.local/bin/entrypoint.sh

ENTRYPOINT ["/home/user/.local/bin/entrypoint.sh"]
