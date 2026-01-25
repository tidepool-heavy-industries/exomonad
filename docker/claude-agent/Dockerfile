# Dockerfile for Claude Code Agent

# Stage 1: Rust Builder (Multi-stage)
FROM --platform=linux/amd64 rust:slim-bookworm AS rust-builder
WORKDIR /build

# Install build dependencies
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    perl \
    perl-base \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

COPY rust/ ./rust/
# Use cache mounts for cargo to speed up rebuilds
RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/build/rust/target \
    cd rust && cargo build --release -p mantle-agent -p effector && \
    cp target/release/mantle-agent /mantle-agent && \
    cp target/release/effector /effector

# Stage 2: Main Agent Image
FROM --platform=linux/amd64 ubuntu:22.04

# Avoid prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install core dependencies
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
    openssh-server \
    && rm -rf /var/lib/apt/lists/*

# Install Node.js 20.x
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get install -y nodejs

# Configure sshd
RUN mkdir -p /var/run/sshd /root/.ssh
RUN sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin prohibit-password/' /etc/ssh/sshd_config && \
    sed -i 's/#PubkeyAuthentication yes/PubkeyAuthentication yes/' /etc/ssh/sshd_config && \
    sed -i 's/#PasswordAuthentication yes/PasswordAuthentication no/' /etc/ssh/sshd_config

# Copy orchestrator's public key (generated during pre-build)
COPY docker/claude-agent/orchestrator.pub /root/.ssh/authorized_keys
RUN chmod 600 /root/.ssh/authorized_keys && \
    chmod 700 /root/.ssh

# Expose SSH port (internal Docker network only)
EXPOSE 22

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

# Back to root to copy mantle-agent
USER root

# Copy pre-built binaries from Rust builder stage
COPY --from=rust-builder /mantle-agent /usr/local/bin/mantle-agent
COPY --from=rust-builder /effector /usr/local/bin/effector
RUN chmod +x /usr/local/bin/mantle-agent /usr/local/bin/effector

# Set up entrypoint script
COPY docker/claude-agent/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

# Create workspace directory
RUN mkdir -p /workspace && chown agent:agent /workspace

# Default environment for mantle-agent communication
ENV MANTLE_CONTROL_SOCKET=/home/agent/.tidepool/sockets/control.sock

# Switch back to agent user for execution
USER agent
WORKDIR /workspace

# Entrypoint uses tini for proper signal handling
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

# Default command: launch Claude Code with bypassPermissions
CMD ["claude", "--permission-mode", "bypassPermissions"]