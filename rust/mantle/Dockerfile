# Build mantle-agent for Linux inside Docker
# From rust/ directory: docker build -t mantle-agent:latest -f mantle/Dockerfile .

FROM rustlang/rust:nightly-bookworm AS builder

WORKDIR /build

# Copy workspace files
COPY Cargo.toml Cargo.lock ./
COPY mantle ./mantle
COPY mantle-agent ./mantle-agent
COPY mantle-hub ./mantle-hub
COPY mantle-shared ./mantle-shared

# Build mantle-agent (debug for faster iteration)
RUN cargo build -p mantle-agent

# Runtime stage
FROM debian:bookworm-slim

# Install system dependencies for Node.js, ghcup, and development
RUN apt-get update && apt-get install -y \
    ca-certificates \
    curl \
    git \
    # ghcup dependencies
    build-essential \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libtinfo-dev \
    zlib1g-dev \
    && curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get install -y nodejs \
    && npm install -g @anthropic-ai/claude-code \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user (Claude Code refuses --dangerously-skip-permissions as root)
RUN useradd -m -s /bin/bash user

# Pre-create .claude directory with correct ownership
# This ensures mounted volumes inherit proper permissions
RUN mkdir -p /home/user/.claude && chown -R user:user /home/user/.claude

# Install ghcup, GHC, and cabal as user (do this early for cache efficiency)
USER user
WORKDIR /home/user

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_GHC_VERSION=latest
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=latest
ENV BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH="/home/user/.ghcup/bin:${PATH}"

# Switch back to root to copy binaries
USER root

COPY --from=builder /build/target/debug/mantle-agent /usr/local/bin/

# Copy entrypoint script that registers MCP server if decision tools are provided
COPY mantle/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

# Run as non-root user
USER user
WORKDIR /home/user

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
