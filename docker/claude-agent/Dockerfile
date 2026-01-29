# Claude Code Agent Container
# No SSH - uses docker exec via docker-ctl for remote commands
#
# Uses pre-built binaries from builder contexts:
#   - haskell-builder: exomonad-control-server
#   - rust-builder: exomonad, effector, tui-spawner
#
# Build with docker buildx bake (see docker/docker-bake.hcl)

FROM common-base

# Avoid prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install agent-specific dependencies
# (removed git, curl, ca-certificates, etc. as they are in common-base)
RUN apt-get update && apt-get install -y --no-install-recommends \
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

# Rename 'user' to 'agent' to match existing agent environment
# and grant sudo access
RUN usermod -l agent user && \
    groupmod -n agent user && \
    usermod -d /home/agent -m agent && \
    echo "agent ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

# Update environment variables to match new user
ENV HOME=/home/agent
ENV USER=agent

# Install Node.js 20.x
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get install -y --no-install-recommends nodejs \
    && rm -rf /var/lib/apt/lists/*

# Install Gemini CLI
RUN npm install -g @google/gemini-cli

# Create non-root user 'agent' setup
# Note: User 'agent' exists now (renamed from 'user'), but we need to ensure
# directories and permissions are correct.

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
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_GHC_VERSION=9.12.2 BOOTSTRAP_HASKELL_CABAL_VERSION=3.14.1.1 BOOTSTRAP_HASKELL_INSTALL_STACK=1 BOOTSTRAP_HASKELL_INSTALL_HLS=1 sh

# Install Rust toolchain (rustup)
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

# Set PATH for agent user
ENV PATH="/home/agent/.local/bin:/home/agent/.ghcup/bin:/home/agent/.cargo/bin:${PATH}"

# Back to root to copy binaries
USER root

# Copy pre-built Rust binaries from builder context
COPY --from=rust-builder /app/rust/target/release/exomonad /usr/local/bin/exomonad
COPY --from=rust-builder /app/rust/target/release/effector /usr/local/bin/effector
COPY --from=rust-builder /app/rust/target/release/tui-spawner /usr/local/bin/tui-spawner

# Copy pre-built Haskell binaries from builder context
COPY --from=haskell-builder /out/exomonad-control-server /usr/local/bin/exomonad-control-server

# Set permissions
RUN chmod +x /usr/local/bin/exomonad \
             /usr/local/bin/effector \
             /usr/local/bin/tui-spawner \
             /usr/local/bin/exomonad-control-server

# Set up entrypoint script
COPY docker/claude-agent/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

# Create workspace directory
RUN mkdir -p /workspace && chown agent:agent /workspace

# Default environment
ENV EXOMONAD_CONTROL_SOCKET=/home/agent/.exomonad/sockets/control.sock
ENV XDG_RUNTIME_DIR=/run/user/1000
ENV ZELLIJ_SESSION_NAME=main
ENV COLORTERM=truecolor

# Switch back to agent user for execution
USER agent
WORKDIR /workspace

# Entrypoint (init: true in docker-compose.yml provides signal handling)
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

# Default command: launch Claude Code with bypassPermissions and full debug logging
CMD ["claude", "--permission-mode", "bypassPermissions", "--debug", "--verbose"]

# OCI label for version tracking (at end to not bust cache on GIT_SHA changes)
ARG GIT_SHA=unknown
LABEL org.opencontainers.image.revision=${GIT_SHA}
