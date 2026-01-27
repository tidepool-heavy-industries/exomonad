# Dedicated Control Server Container
# MCP server (TCP 7432) + Zellij tab creation + docker-ctl integration
#
# Uses pre-built binaries from builder contexts:
#   - haskell-builder: exomonad-control-server
#   - rust-builder: docker-ctl, tui-spawner
#
# Build with docker buildx bake (see docker/docker-bake.hcl)

ARG GIT_SHA=unknown
FROM common-base
ARG GIT_SHA
LABEL org.opencontainers.image.revision=${GIT_SHA}

# Install additional runtime dependencies (on top of common-base)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgmp10 \
    libffi8 \
    zlib1g \
    socat \
    gpg \
    && rm -rf /var/lib/apt/lists/*

# Install GitHub CLI (required for gh_issue_* MCP tools)
RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | gpg --dearmor -o /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" > /etc/apt/sources.list.d/github-cli.list \
    && apt-get update && apt-get install -y --no-install-recommends gh \
    && rm -rf /var/lib/apt/lists/*

# Install Docker CLI for spawner communication
RUN curl -fsSL https://get.docker.com | sh && \
    rm -rf /var/lib/apt/lists/*

# Install cargo-binstall for binary installation
# Set CARGO_HOME explicitly to root's cargo since we run as root during build
ENV CARGO_HOME=/root/.cargo
RUN curl -L --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash

# Install Zellij for cross-container tab creation
ENV PATH="/root/.cargo/bin:${PATH}"
RUN cargo-binstall --no-confirm zellij && \
    cp /root/.cargo/bin/zellij /usr/local/bin/zellij && \
    chmod 755 /usr/local/bin/zellij

# Copy pre-built binaries from builder contexts
# These come from the BuildKit contexts defined in docker-bake.hcl
COPY --from=haskell-builder /out/exomonad-control-server /usr/local/bin/exomonad-control-server
COPY --from=rust-builder /app/rust/target/release/docker-ctl /usr/local/bin/docker-ctl
COPY --from=rust-builder /app/rust/target/release/tui-spawner /usr/local/bin/tui-spawner

# Verify binaries are executable
RUN chmod +x /usr/local/bin/exomonad-control-server \
             /usr/local/bin/docker-ctl \
             /usr/local/bin/tui-spawner

# Copy service-specific entrypoint wrapper
COPY docker/control-server/entrypoint.sh /usr/local/bin/service-entrypoint.sh
RUN chmod +x /usr/local/bin/service-entrypoint.sh

# Environment (extends common-base)
ENV EXOMONAD_CONTROL_SOCKET=/sockets/control.sock
ENV EXOMONAD_TUI_SOCKET=/sockets/tui.sock
ENV ZELLIJ_SESSION_NAME=main
# Override GIT_SAFE_DIRECTORY for repo bind mount
ENV GIT_SAFE_DIRECTORY=/repo

# Create necessary directories (user already exists from common-base)
RUN mkdir -p /sockets && chown -R user:user /sockets

EXPOSE 7432

WORKDIR /home/user

# Use service entrypoint which calls common entrypoint internally
ENTRYPOINT ["/usr/local/bin/service-entrypoint.sh"]
