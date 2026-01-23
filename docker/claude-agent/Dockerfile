# Dockerfile for Claude Code Agent
FROM ubuntu:22.04

# Avoid prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install core dependencies
# tini: for proper signal handling as PID 1
# git: for worktree management
# curl, ca-certificates: for downloading toolchains
# build-essential, libgmp-dev, etc: for Haskell/Rust compilation
# sudo: to allow non-root user to install additional packages if needed
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
    && rm -rf /var/lib/apt/lists/*

# Install Node.js 20.x
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get install -y nodejs

# Install Claude Code CLI globally
ARG CLAUDE_CODE_VERSION=0.29.0
RUN npm install -g @anthropic-ai/claude-code@${CLAUDE_CODE_VERSION}

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

# Install GHCup (Haskell toolchain)
# Installing GHC 9.6.4 and Cabal
ENV GHCUP_INSTALL_SKIP_MSYS=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_GHC_VERSION=9.6.4 BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.2.1 BOOTSTRAP_HASKELL_INSTALL_STACK=1 BOOTSTRAP_HASKELL_INSTALL_HLS=1 sh

# Install Rust toolchain (rustup)
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

# Set PATH for agent user to include GHCup and Rust
ENV PATH="/home/agent/.ghcup/bin:/home/agent/.cargo/bin:${PATH}"

# Back to root to copy system-level binaries and entrypoint
USER root

# Copy pre-built mantle-agent binary from host
# (Assumes mantle-agent is in the build context at 'bin/mantle-agent')
COPY bin/mantle-agent /usr/local/bin/mantle-agent
RUN chmod +x /usr/local/bin/mantle-agent

# Set up entrypoint script
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

# Create workspace directory with correct permissions
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
